%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_auth).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([lookup_user/3]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-include("ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: proplist()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Node, Options]) ->
    put(callid, Node),
    process_flag(trap_exit, true),
    lager:debug("starting new fs auth listener for ~s", [Node]),
    case freeswitch:bind(Node, directory) of
        ok ->
            lager:debug("bound to directory request on ~s", [Node]),
            {ok, #state{node=Node, options=Options}};
        {error, Reason} ->
            lager:warning("failed to bind to directory requests on ~s: ~p", [Node, Reason]),
            {stop, Reason};
        timeout ->
            lager:error("failed to bind to directory requests on ~s: timeout", [Node]),
            {stop, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]}, #state{node=Node}=State) ->
    case props:get_value(<<"sip_auth_method">>, Data) of
        <<"REGISTER">> ->
            lager:debug("received fetch request (~s) for sip registration creds from ~s", [ID, Node]);
        Else ->
            lager:debug("received fetch request for ~s (~s) user creds from ~s", [Else, ID, Node])
    end,
    case {props:get_value(<<"Event-Name">>, Data), props:get_value(<<"action">>, Data)} of
        {<<"REQUEST_PARAMS">>, <<"sip_auth">>} ->
            %% TODO: move this to a supervisor somewhere....
            spawn(?MODULE, lookup_user, [Node, ID, Data]),
            {noreply, State, hibernate};
        _Other ->
            {ok, Resp} = ecallmgr_fs_xml:empty_response(),
            _ = freeswitch:fetch_reply(Node, ID, Resp),
            lager:debug("ignoring request from ~s for ~p", [Node, _Other]),
            {noreply, State}
    end;

handle_info({fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    lager:debug("sending empyt reply for request (~s) for ~s ~s from ~s", [ID, _Section, _Something, Node]),
    {ok, Resp} = ecallmgr_fs_xml:empty_response(),
    _ = freeswitch:fetch_reply(Node, ID, Resp),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{node=Node}) ->
    lager:debug("fs auth ~s termination: ~p", [Node, _Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec lookup_user/3 :: (atom(), ne_binary(), proplist()) ->  'ok' | 'timeout' | {'error', _}.
lookup_user(Node, ID, Data) ->
    put(callid, ID),
    %% build req for rabbit
    AuthRealm = props:get_value(<<"domain">>, Data, props:get_value(<<"Auth-Realm">>, Data)),
    AuthUser = props:get_value(<<"user">>, Data, props:get_value(<<"Auth-User">>, Data)),
    Method = props:get_value(<<"sip_auth_method">>, Data),
    lager:debug("looking up credentials of ~s@~s for a ~s", [AuthUser, AuthRealm, Method]),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,[{<<"Msg-ID">>, ID}
                                    ,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
                                    ,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
                                    ,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Data)}
                                    ,{<<"Method">>, Method}
                                    ,{<<"Auth-User">>, AuthUser}
                                    ,{<<"Auth-Realm">>, AuthRealm}
                                    ,{<<"Media-Server">>, wh_util:to_binary(Node)}
                                    | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                   ]
                                  ,fun wapi_authn:publish_req/1
                                  ,fun wapi_authn:resp_v/1),
    case ReqResp of
        {error, _R} ->
            lager:debug("authn request lookup failed: ~p", [_R]),
            {ok, RespXML} = ecallmgr_fs_xml:route_not_found(),
            freeswitch:fetch_reply(Node, ID, iolist_to_binary(RespXML));
        {ok, RespJObj} ->
            {ok, Xml} = ecallmgr_fs_xml:authn_resp_xml(
                          wh_json:set_value(<<"Auth-Realm">>, AuthRealm
                                            ,wh_json:set_value(<<"Auth-User">>, AuthUser, RespJObj))
                         ),
            lager:debug("sending XML to ~w: ~s", [Node, Xml]),
            freeswitch:fetch_reply(Node, ID, iolist_to_binary(Xml))
    end.
