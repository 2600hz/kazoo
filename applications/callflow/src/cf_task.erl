%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_task).

-behaviour(gen_listener).

-export([start_link/3
         ,relay_amqp/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("callflow.hrl").

-record(state, {call :: whapps_call:call()
                ,callback :: fun()
                ,args :: list()
                ,pid :: pid()
                ,ref ::reference()
                ,queue :: api_binary()
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'call', [{'callid', CallId}]}
                           ,{'self', []}
                          ]).
-define(RESPONDERS, [{{?MODULE, 'relay_amqp'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the listener and binds to the call channel destroy events
%% @end
%%--------------------------------------------------------------------
-spec start_link(whapps_call:call(), fun(), list()) -> startlink_ret().
start_link(Call, Fun, Args) ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[Call, Fun, Args]
                           ).

%%--------------------------------------------------------------------
%% @doc
%% Handles call events (typically triggerred by a freeswitch event)
%% For the purposes of the singular hook listener, we are only interested in
%% CHANNEL_DESTROY.
%% @end
%%--------------------------------------------------------------------
-spec relay_amqp(wh_json:object(), wh_proplist()) -> any().
relay_amqp(JObj, Props) ->
    Pid = props:get_value('cf_task_pid', Props),
    whapps_call_command:relay_event(Pid, JObj).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the listener, and sends the init hook
%%--------------------------------------------------------------------
-spec init([fun()]) -> {'ok', state()}.
init([Call, Callback, Args]) ->
    lager:debug("started event listener for cf_task"),
    {'ok', #state{call=Call
                  ,callback=Callback
                  ,args=Args
                 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), term(), state()) ->
                         {'reply', {'error', 'not_implemented'}, state()}.
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), state()) ->
                         {'noreply', state()} |
                         {'stop', 'normal', state()}.
handle_cast({'gen_listener', {'created_queue', Q}}, State) ->
    {'noreply', State#state{queue=Q}};
handle_cast({'gen_listener', {'is_consuming', 'true'}}, State) ->
    lager:debug("ready to recv events, launching the task"),
    {'noreply', launch_task(State)};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), state()) -> {'noreply', state()}.
handle_info({'DOWN', Ref, 'process', Pid, Reason}
            ,#state{ref=Ref
                    ,pid=Pid
                   }=State
           ) ->
    lager:debug("task in ~p (~p) exited with reason: ~p", [Pid, Ref, Reason]),
    {'stop', 'normal', State};
handle_info(Info, State) ->
    lager:debug("unhandled message: ~p", [Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), state()) -> {'reply', wh_proplist()}.
handle_event(_JObj, #state{pid=Pid}) ->
    {'reply', [{'cf_task_pid', Pid}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), state()) -> any().
terminate(_Reason, _State) ->
    lager:debug("callflow task terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), state(), term()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec launch_task(state()) -> state().
launch_task(#state{queue=Q
                   ,call=Call
                   ,callback=Callback
                   ,args=Args
                  }=State) ->
    Self = self(),
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                           wh_amqp_channel:consumer_pid(Self),
                           Funs = [{fun whapps_call:kvs_store/3, 'consumer_pid', Self}
                                   ,{fun whapps_call:set_controller_queue/2, Q}
                                  ],
                           apply(Callback, Args ++ [whapps_call:exec(Funs, Call)])
                   end),
    lager:debug("watching task execute in ~p (~p)", [Pid, Ref]),
    State#state{pid=Pid, ref=Ref}.
