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
-export([start_link/3
         ,member_connect_resp/2
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

-record(state, {
         call :: whapps_call:call()
         ,acdc_queue_id :: ne_binary() % the ACDc Queue ID
         ,msg_queue_id :: ne_binary() % the AMQP Queue ID of the ACDc Queue process
         ,agent_id :: ne_binary()
         ,agent_db :: ne_binary()
         ,account_id :: ne_binary()
         ,fsm_pid :: pid()
         ,agent_queues :: [ne_binary(),...] | []
         ,endpoints :: wh_json:json_objects()
         ,last_connect :: wh_now() % last connection
         ,last_attempt :: wh_now() % last attempt to connect
         ,my_id :: ne_binary()
         ,timer_ref :: reference()
         ,sync_resp :: wh_json:json_object() % furthest along resp
         }).

%%%===================================================================
%%% Defines for different functionality
%%%===================================================================

%% On init, an aget process sends a sync_req and waits SYNC_TIMER_TIMEOUT ms
%% The agent process checks its list of received 
-define(SYNC_TIMER_MESSAGE, sync_timeout).
-define(SYNC_TIMER_TIMEOUT, 5000).

%% After receiving sync_resp, if the resp status requires waiting, SYNC_WAIT_TIMER_TIMEOUT
%% pauses the agent process, then restarts the sync process (send sync_req, start
%% SYNC_TIMER_TIMEOUT, collect sync_resp(s), etc
-define(SYNC_WAIT_TIMER_MESSAGE, sync_wait_timeout).
-define(SYNC_WAIT_TIMER_TIMEOUT, 5000).

%% When in the wrapup status, how long does an agent wait before going back to ready
-define(WRAPUP_TIMER_MESSAGE, wrapup_timeout).
-define(WRAPUP_TIMER_TIMEOUT, 60000).

%% When an agent is paused (on break, logged out, etc)
-define(PAUSED_TIMER_MESSAGE, paused_timeout).

-define(BINDINGS(AcctDb, AgentId), [{self, []}
                                    ,{acdc_agent, [{agent_db, AcctDb}
                                                   ,{agent_id, AgentId}
                                                  ]}
                                   ]).

-define(RESPONDERS, [{{acdc_agent_handler, handle_agent_status}
                      ,{<<"agent">>, <<"status_update">>}
                     }
                     ,{{acdc_agent_handler, handle_sync_req}
                       ,{<<"agents">>, <<"sync_req">>}
                      }
                     ,{{acdc_agent_handler, handle_sync_resp}
                       ,{<<"agent">>, <<"sync_resp">>}
                      }
                     ,{{acdc_agent_handler, handle_call_event}
                       ,{<<"call_event">>, <<"*">>}
                      }
                     ,{{acdc_agent_handler, handle_member_message}
                       ,{<<"member">>, <<"*">>}
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
start_link(Supervisor, AcctDb, AgentJObj) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),

    case wh_json:get_value(<<"queues">>, AgentJObj) of
        undefined ->
            lager:debug("agent ~s in ~s has no queues, ignoring"),
            ignore;
        [] ->
            lager:debug("agent ~s in ~s has no queues, ignoring"),
            ignore;
        Queues ->
            gen_listener:start_link(?MODULE
                                    ,[{bindings, ?BINDINGS(AcctDb, AgentId)}
                                      ,{responders, ?RESPONDERS}
                                     ]
                                    ,[Supervisor, AcctDb, AgentJObj, Queues]
                                   )
    end.

-spec member_connect_resp/2 :: (pid(), wh_json:json_object()) -> 'ok'.
member_connect_resp(Srv, ReqJObj) ->
    gen_listener:cast(Srv, {member_connect_resp, ReqJObj}).

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
init([Supervisor, AcctDb, AgentJObj, Queues]) ->
    AgentId = wh_json:get_value(<<"_id">>, AgentJObj),
    put(callid, AgentId),

    {ok, FSMPid} = acdc_agent_sup:start_fsm(Supervisor, AcctDb, AgentId),

    gen_listener:cast(self(), load_endpoints),
    gen_listener:cast(self(), send_sync_event),

    {ok, #state{
       agent_db = AcctDb
       ,agent_id = AgentId
       ,account_id = wh_json:get_value(<<"pvt_account_id">>, AgentJObj)
       ,agent_queues = Queues
       ,my_id = list_to_binary([wh_util:to_binary(node()), "-", pid_to_list(self())])
       ,fsm_pid = FSMPid
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
handle_cast({member_connect_resp, ReqJObj}, #state{
              last_connect=LastConnect
              ,last_attempt=LastAttempt
              ,my_id=MyId
             }=State) ->
    
    {noreply, State};
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
handle_info(_Info, #state{}=State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all messages from the message bus
%%
%% @spec handle_info(JObj, State) -> {reply, Proplist} |
%%                                   ignore
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{fsm_pid=FSM}) ->
    {reply, [{fsm_pid, FSM}]}.

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
    lager:debug("agent process going down: ~p", [_Reason]).

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
