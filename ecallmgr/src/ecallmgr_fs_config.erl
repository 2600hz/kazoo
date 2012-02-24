%%%-------------------------------------------------------------------
%%% @relaodaclor Edouard Swiac <edouard@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Send config commands to FS
%%% @end
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_config).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, 
        handle_config_req/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FS_TIMEOUT, 5000).

-include("ecallmgr.hrl").

-record(state, {
          node = undefined :: atom()
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
init([Node, _Options]) ->
    ?LOG_SYS("starting new fs config listener for ~s", [Node]),
    process_flag(trap_exit, true),

    erlang:monitor_node(Node, true),
    {ok, #state{node=Node}, 0}.

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
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({fetch, configuration, <<"configuration">>, <<"name">>, Conf, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    ?LOG_START(ID, "received acls switch config request from ~s", [Node]),
    {ok, ConfigReqPid} = ecallmgr_fs_config_sup:start_req(Node, ID, fsconf_to_sysconf(Conf), _Data),
    erlang:monitor(process, ConfigReqPid),
    ?LOG_END("replying acls switch config request from ~s", [Node]),
    {noreply, State};

handle_info({_Fetch, _Section, _Something, _Key, _Value, ID, [undefined | _Data]}, #state{node=Node}=State) ->
    ?LOG_START(ID, "received switch config request for ~s ~s from ~s", [ _Section, _Something, Node]),
    _ = freeswitch:fetch_reply(Node, ID, ?EMPTYRESPONSE),
    ?LOG_END("ignoring request ~s from ~s", [props:get_value(<<"Event-Name">>, _Data), Node]),
    {noreply, State};

handle_info(timeout, #state{node=Node}=State) ->
    Type = {bind, config},

    {foo, Node} ! Type,
    receive
        ok ->
            ?LOG_SYS("bound to config request on ~s", [Node]),
            {noreply, State};
        {error, Reason} ->
            ?LOG_SYS("failed to bind to config requests on ~s, ~p", [Node, Reason]),
            {stop, Reason, State}
    after ?FS_TIMEOUT ->
            ?LOG_SYS("timed out binding to config requests on ~s", [Node]),
            {stop, timeout, State}
    end;

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
terminate(_Reason, _State) ->
    ?LOG_SYS("fs config ~p termination", [_Reason]),
    ok.

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
-spec handle_config_req/4 :: (Node, ID, Conf, Data) -> {ok, pid()} when
      Node :: atom(),
      ID :: binary(),
      Conf :: binary(),
      Data :: proplist().
handle_config_req(Node, ID, Conf, _Data) ->
    Pid = spawn_link(fun() -> 
          put(callid, ID),
          try
              {ok, AclResp} = ecallmgr_config:get(Conf),
              ?LOG(ID, "received ecallmgr_config response for ~p", [Conf]),
              {ok, Xml} = ecallmgr_fs_xml:config_resp_xml(AclResp),
              ?LOG_END(ID, "sending XML to ~w: ~s", [Node, Xml]),
              _ = freeswitch:fetch_reply(Node, ID, Xml)
          catch 
            throw:_T ->
                ?LOG("config request failed: thrown ~w", [_T]);
            erro:_E ->
                ?LOG("config request failed: error ~w", [_E])
            end
        end),
    {ok, Pid}.

%%% FS conf keys are not necessarily the same as we store them, remap it
fsconf_to_sysconf(FsConf) ->
  case FsConf of
    <<"acl.conf">> -> <<"acls">>
  end.
