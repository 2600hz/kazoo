%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Directory lookups from FS
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_authn).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([handle_sucessful_registration/2]).
-export([lookup_user/4]).
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
                ,options = [] :: wh_proplist()
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

-spec handle_sucessful_registration/2 :: (proplist(), atom()) -> 'ok'.
handle_sucessful_registration(Props, Node) ->
    lager:debug("received registration event"),
    ecallmgr_registrar:reg_success(Props, Node),
    publish_register_event(Props),
    ok.

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
    bind_to_events(freeswitch:version(Node), Node),
    case bind_to_directory(Node) of
        ok -> {ok, #state{node=Node, options=Options}};
        {error, Reason} -> {stop, Reason}
    end.

bind_to_events({ok, <<"mod_kazoo", _/binary>>}, Node) ->
    ok = freeswitch:event(Node, ['CUSTOM', 'sofia::register']);
bind_to_events(_, Node) ->
    ok = freeswitch:event(Node, ['CUSTOM', 'sofia::register']),    
    gproc:reg({p, l, {call_event, Node, <<"sofia::register">>}}).

bind_to_directory(Node) ->
    case freeswitch:bind(Node, directory) of
        ok ->
            lager:debug("bound to directory request on ~s", [Node]),
            ok;
        {ok, _Resp} ->
            lager:debug("bound to directory request on ~s: ~s", [Node, _Resp]),
            ok;
        {error, Reason}=E ->
            lager:warning("failed to bind to directory requests on ~s: ~p", [Node, Reason]),
            E;
        timeout ->
            lager:error("failed to bind to directory requests on ~s: timeout", [Node]),
            {error, timeout}
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
handle_info({event, [_ | Props]}, #state{node=Node}=State) ->
    _ = case props:get_value(<<"Event-Subclass">>, Props, props:get_value(<<"Event-Name">>, Props)) of
            <<"sofia::register">> -> spawn(?MODULE, handle_sucessful_registration, [Props, Node]);
            _ -> ok
        end,
    {noreply, State};
handle_info({fetch, directory, <<"domain">>, <<"name">>, _Value, ID, [undefined | Data]}, #state{node=Node}=State) ->    
    case props:get_value(<<"sip_auth_method">>, Data) of
        <<"REGISTER">> ->
            lager:debug("received fetch request (~s) for sip registration creds from ~s", [ID, Node]);
        Else ->
            lager:debug("received fetch request for ~s (~s) user creds from ~s", [Else, ID, Node])
    end,
    case {props:get_value(<<"Event-Name">>, Data), props:get_value(<<"action">>, Data)} of
        {<<"REQUEST_PARAMS">>, <<"sip_auth">>} ->
            Method = props:get_value(<<"sip_auth_method">>, Data),
            spawn(?MODULE, lookup_user, [Node, ID, Method, Data]),
            {noreply, State, hibernate};
        {<<"REQUEST_PARAMS">>, <<"reverse-auth-lookup">>} ->
            spawn(?MODULE, lookup_user, [Node, ID, <<"reverse-lookup">>, Data]),
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
    lager:debug("got ~p", [_Info]),
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
-spec lookup_user/4 :: (atom(), ne_binary(), ne_binary(), wh_proplist()) ->
                               fs_handlecall_ret().
lookup_user(Node, ID, Method,  Data) ->
    put(callid, ID),
    %% build req for rabbit
    DomainName = props:get_value(<<"domain">>, Data, props:get_value(<<"Auth-Realm">>, Data)),
    UserId = props:get_value(<<"user">>, Data, props:get_value(<<"Auth-User">>, Data)),
    lager:debug("looking up credentials of ~s@~s for a ~s", [UserId, DomainName, Method]),
    Req = [{<<"Msg-ID">>, ID}
           ,{<<"To">>, ecallmgr_util:get_sip_to(Data)}
           ,{<<"From">>, ecallmgr_util:get_sip_from(Data)}
           ,{<<"Orig-IP">>, ecallmgr_util:get_orig_ip(Data)}
           ,{<<"Method">>, Method}
           ,{<<"Auth-User">>, UserId}
           ,{<<"Auth-Realm">>, DomainName}
           ,{<<"Media-Server">>, wh_util:to_binary(Node)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,props:filter_undefined(Req)
                                  ,fun wapi_authn:publish_req/1
                                  ,fun wapi_authn:resp_v/1),
    {ok, Xml} = handle_lookup_resp(ReqResp, DomainName, UserId, Method),
    lager:debug("sending XML to ~w: ~s", [Node, Xml]),
    freeswitch:fetch_reply(Node, ID, iolist_to_binary(Xml)).    

handle_lookup_resp({error, _R}, _, _, _) ->
    lager:debug("authn request lookup failed: ~p", [_R]),
    ecallmgr_fs_xml:route_not_found();
handle_lookup_resp({ok, RespJObj}, DomainName, UserId, <<"reverse-lookup">>) ->
    Props = [{<<"Domain-Name">>, DomainName}
             ,{<<"User-ID">>, UserId}
            ],
    ecallmgr_fs_xml:reverse_authn_resp_xml(wh_json:set_values(Props, RespJObj));
handle_lookup_resp({ok, RespJObj}, DomainName, UserId, _) ->
    Props = [{<<"Domain-Name">>, DomainName}
             ,{<<"User-ID">>, UserId}
            ],
    ecallmgr_fs_xml:authn_resp_xml(wh_json:set_values(Props, RespJObj)).
    
-spec publish_register_event/1 :: (wh_proplist()) -> 'ok'.
publish_register_event(Data) ->
    ApiProp = lists:foldl(fun(K, Api) ->
                                  case props:get_value(wh_util:to_lower_binary(K), Data) of
                                      undefined ->
                                          case props:get_value(K, Data) of
                                              undefined -> Api;
                                              V -> [{K, V} | Api]
                                          end;
                                      V -> [{K, V} | Api]
                                  end
                          end
                          ,[{<<"Event-Timestamp">>, round(wh_util:current_tstamp())}
                            ,{<<"Call-ID">>, get(callid)}
                            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)]
                          ,wapi_registration:success_keys()),
    lager:debug("sending successful registration"),
    wh_amqp_worker:cast(?ECALLMGR_AMQP_POOL
                        ,ApiProp
                        ,fun wapi_registration:publish_success/1
                       ).
