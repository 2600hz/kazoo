%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agent).

-behaviour(gen_listener).

%% API
-export([start_link/2]).

%% Listener callbacks
-export([handle_status_update/2
         ,handle_sync_req/2
         ,handle_sync_resp/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("acdc.hrl").

-type agent_status() :: 'init' |      % agent is starting up
                        'ready' |     % agent is ready to connect
                        'trying' |    % agent is trying to connect
                        'connected' | % agent is connected
                        'paused'.     % agent is away
-record(state, {
          status = 'init' :: agent_status()
         ,current_call :: whapps_call:call()
         ,agent_id :: ne_binary()
         ,agent_db :: ne_binary()
         ,account_id :: ne_binary()
         ,endpoints :: wh_json:json_objects()
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id = wh_util:rand_hex_binary() :: ne_binary()
         ,sync_timer :: reference()
         }).

%%%===================================================================
%%% Defines for different functionality
%%%===================================================================

-define(SYNC_TIMER_MESSAGE, sync_timeout).
-define(SYNC_TIMER_TIMEOUT, 5000).

-define(BINDINGS(AcctDb, AgentId), [{self, []}
                                    ,{agent, [{agent_db, AcctDb}
                                              ,{agent_id, AgentId}
                                             ]}
                                   ]).

-define(RESPONDERS, [{{?MODULE, handle_agent_status}
                      ,{<<"agents">>, <<"status_update">>}
                     }
                     ,{{?MODULE, handle_sync_req}
                       ,{<<"agents">>, <<"sync_req">>}
                      }
                     ,{{?MODULE, handle_sync_resp}
                       ,{<<"agent">>, <<"sync_resp">>}
                      }
                    ]).
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
start_link(AcctDb, AgentJObj) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    gen_listener:start_link(?MODULE
                            ,[{bindings, ?BINDINGS(AcctDb, AgentId)}
                              ,{responders, ?RESPONDERS}
                             ]
                            ,[AcctDb, AgentJObj]).

-spec handle_status_update/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_status_update(_JObj, _Props) ->
    ok.

-spec handle_sync_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_sync_req(_JObj, _Props) ->
    ok.

handle_sync_resp(JObj, Props) ->
    gen_listener:cast(props:get_value(server, Props), {sync_resp, JObj}).

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
init([AcctDb, AgentJObj]) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    put(callid, AgentId),

    gen_listener:cast(self(), load_endpoints),
    gen_listener:cast(self(), send_sync_event),

    {ok, #state{
       status = 'init'
       ,agent_db = AcctDb
       ,agent_id = AgentId
       ,account_id = wh_json:get_value(<<"pvt_account_id">>, AgentJObj)
      }}.

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
    lager:debug("unhandled call from ~p: ~p", [_From, _Request]),
    {reply, {error, unhandled_call}, State}.

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
handle_cast(load_endpoints, #state{
              agent_db=AcctDb
              ,agent_id=AgentId
             }=State) ->
    {noreply, State#state{endpoints=acdc_util:get_endpoints(AcctDb, AgentId)}};
handle_cast(send_sync_event, #state{
              account_id=AcctId
              ,agent_id=AgentId
              ,my_id=ProcessId
             }=State) ->
    wapi_agent:publish_sync_req(create_sync_api(AcctId, AgentId, ProcessId)),
    {noreply, State#state{sync_timer=start_sync_timer()}};
handle_cast({sync_resp, RespJObj}, #state{sync_timer=Ref}=State) ->
    _ = erlang:cancel_timer(Ref),
    lager:debug("recv sync resp: ~p", [RespJObj]),
    {noreply, State#state{sync_timer=undefined}};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info({timeout, Ref, ?SYNC_TIMER_MESSAGE}, #state{sync_timer=Ref}=State) ->
    lager:debug("sync timer timeout, moving to ready"),
    {noreply, State#state{status=ready, sync_timer=undefined}};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, #state{status=Status}) ->
    {reply, [{status, Status}]}.

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
-spec create_sync_api/3 :: (ne_binary(), ne_binary(), ne_binary()) ->
                                   wh_json:json_object().
create_sync_api(AcctId, AgentId, ProcessId) ->
    wh_json:from_list([{<<"Account-ID">>, AcctId}
                       ,{<<"Agent-ID">>, AgentId}
                       ,{<<"Process-ID">>, ProcessId}
                       ,{<<"Msg-ID">>, wh_util:rand_hex_binary(6)}
                      ]).

-spec start_sync_timer/0 :: () -> reference().
start_sync_timer() ->
    erlang:start_timer(?SYNC_TIMER_TIMEOUT, self(), ?SYNC_TIMER_MESSAGE).
