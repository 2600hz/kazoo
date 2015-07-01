%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_history).

-behaviour(gen_server).

-export([start_link/0]).
-export([add_command/2, add_command/3]).
-export([update_consumer_tag/3]).
-export([remove/1]).
-export([get/1]).
-export([add_exchange/1]).
-export([list_exchanges/0]).
-export([list_consume/1]).
-export([is_consuming/2
         ,is_bound/4
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(TAB, ?MODULE).

-record(state, {consumers = sets:new()
                ,exchanges = dict:new()
                ,connections = sets:new()
               }).

-record(wh_amqp_history, {timestamp = os:timestamp() :: wh_now() | '_'
                          ,consumer :: api_pid() | '_'
                          ,command :: wh_amqp_command() | '_'
                         }).
-type wh_amqp_history() :: #wh_amqp_history{}.

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
start_link() -> gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec add_command(wh_amqp_assignment(), wh_amqp_command()) -> 'ok'.
-spec add_command(wh_amqp_assignment(), wh_amqp_command(), 'sync' | 'async') -> 'ok'.
add_command(Assignment, Command) ->
    add_command(Assignment, Command, 'async').

add_command(#wh_amqp_assignment{consumer=Consumer}, Command, Method) ->
    MatchSpec = [{#wh_amqp_history{consumer=Consumer
                                   ,command=Command
                                   ,_='_'
                                  }
                  ,[]
                  ,['true']
                 }],
    case ets:select_count(?TAB, MatchSpec) =:= 0 of
        'false' ->
            lager:debug("not tracking history for known command ~s from ~p"
                        ,[element(1, Command), Consumer]
                       );
        'true' ->
            send_command(Consumer, Command, Method)
    end.

send_command(Consumer, Command, 'async') ->
    gen_server:cast(?MODULE, {'command', Consumer, Command});
send_command(Consumer, Command, 'sync') ->
    gen_server:call(?MODULE, {'command', Consumer, Command}).

-spec update_consumer_tag(pid(), ne_binary(), ne_binary()) -> 'ok'.
update_consumer_tag(Consumer, OldTag, NewTag) ->
    gen_server:cast(?MODULE, {'update_consumer_tag', Consumer, OldTag, NewTag}).

-spec remove(wh_amqp_assignment() | pid() | 'undefined') -> 'ok'.
remove(#wh_amqp_assignment{consumer=Consumer}) -> remove(Consumer);
remove(Consumer) when is_pid(Consumer) ->
    gen_server:cast(?MODULE, {'remove', Consumer});
remove(_) -> 'ok'.

-spec get(api_pid()) -> wh_amqp_commands().
get('undefined') -> [];
get(Consumer) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,_='_'
                              },
    [Command
     || #wh_amqp_history{command=Command}
            <- ets:match_object(?TAB, Pattern)
    ].

-spec add_exchange(wh_amqp_exchange()) -> 'ok'.
add_exchange(#'exchange.declare'{}=Exchange) ->
    gen_server:cast(?MODULE, {'add_exchange', Exchange}).

-spec list_exchanges() -> wh_amqp_exchanges().
list_exchanges() ->
    gen_server:call(?MODULE, 'list_exchanges').

-spec list_consume(pid()) -> wh_amqp_commands().
list_consume(Consumer) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'basic.consume'{_='_'}
                               ,_='_'
                              },
    [Command
     || #wh_amqp_history{command=Command} <- ets:match_object(?TAB, Pattern)
    ].

-spec is_consuming(pid(), ne_binary()) -> boolean().
is_consuming(Consumer, Queue) ->
    MatchSpec = [{#wh_amqp_history{consumer=Consumer
                                   ,command=#'basic.consume'{queue=Queue
                                                             ,_='_'
                                                            }
                                   ,_='_'
                                  }
                  ,[]
                  ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

-spec is_bound(pid(), ne_binary(), ne_binary(), ne_binary()) -> boolean().
is_bound(Consumer, Exchange, Queue, RoutingKey) when is_pid(Consumer) ->
    MatchSpec = [{#wh_amqp_history{consumer=Consumer
                                   ,command=#'queue.bind'{queue=Queue
                                                          ,exchange=Exchange
                                                          ,routing_key=RoutingKey
                                                          ,_='_'
                                                         }
                                   ,_='_'
                                  }
                  ,[]
                  ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

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
%%                     {'stop', Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    wh_util:put_callid(?MODULE),
    _ = ets:new(?TAB, ['named_table'
                       ,{'keypos', #wh_amqp_history.timestamp}
                       ,'protected'
                       ,'ordered_set'
                      ]),
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, Reply, State} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'command', Consumer, #'queue.unbind'{}=Unbind}, _From, State) ->
    unbind_queue(Consumer, Unbind),
    {'reply', 'ok', State};
handle_call('list_exchanges', {Connection, _}, #state{exchanges=Exchanges
                                                      ,connections=Connections
                                                     }=State) ->
    _Ref = monitor('process', Connection),
    {'reply', [Exchange || {_, Exchange} <- dict:to_list(Exchanges)]
     ,State#state{connections=sets:add_element(Connection, Connections)}};
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'update_consumer_tag', Consumer, OldTag, NewTag}, State) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'basic.consume'{consumer_tag=OldTag
                                                         ,_='_'}
                               ,_='_'},
    _ = update_consumer_tag(ets:match_object(?TAB, Pattern, 1), NewTag),
    {'noreply', State};
handle_cast({'command', Consumer, #'queue.delete'{queue=Queue}}, State) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'queue.declare'{queue=Queue
                                                         ,_='_'
                                                        }
                               ,_='_'
                              },
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State};
handle_cast({'command', Consumer, #'queue.unbind'{}=Unbind}, State) ->
    unbind_queue(Consumer, Unbind),
    {'noreply', State};
handle_cast({'command', Consumer, #'basic.cancel'{consumer_tag=CTag}}, State) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'basic.consume'{consumer_tag=CTag
                                                         ,_='_'}
                               ,_='_'},
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State};
handle_cast({'command', Consumer, Command}, #state{consumers=Consumers}=State) ->
    _ = ets:insert(?TAB, #wh_amqp_history{consumer=Consumer
                                          ,command=Command}),
    case sets:is_element(Consumer, Consumers) of
        'true' -> {'noreply', State};
        'false' ->
            _Ref = monitor('process', Consumer),
            {'noreply', State#state{consumers=sets:add_element(Consumer, Consumers)}}
    end;
handle_cast({'remove', Consumer}, #state{consumers=Consumers}=State) ->
    Pattern = #wh_amqp_history{consumer=Consumer, _='_'},
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State#state{consumers=sets:del_element(Consumer, Consumers)}};
handle_cast({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}
            ,#state{exchanges=Exchanges
                    ,connections=Connections}=State) ->
    _ = [(catch wh_amqp_connection:new_exchange(Connection, Exchange))
         || Connection <- sets:to_list(Connections)
        ],
    {'noreply', State#state{exchanges=dict:store(Name, Exchange, Exchanges)}};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'remove_history', Pid}, State) ->
    _ = remove(Pid),
    {'noreply', State};
handle_info({'DOWN', _, 'process', Pid, _Reason}
            ,#state{connections=Connections}=State) ->
    case sets:is_element(Pid, Connections) of
        'true' ->
            lager:debug("connection ~p went down: ~p", [Pid, _Reason]),
            {'noreply', State#state{connections=sets:del_element(Pid, Connections)}};
        'false' ->
            %% Allow wh_amqp_assignments time to get the history so it
            %% can cleanly remove queues/consumers/ect
            lager:debug("removing AMQP history for consumer ~p in 2.5s: ~p"
                        ,[Pid, _Reason]),
            erlang:send_after(2500, self(), {'remove_history', Pid}),
            {'noreply', State}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
    lager:debug("AMQP history terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec update_consumer_tag({wh_amqp_history(), ets:continuation()} | '$end_of_table', ne_binary()) -> 'ok'.
update_consumer_tag('$end_of_table', _) -> 'ok';
update_consumer_tag({[#wh_amqp_history{timestamp=Timestamp
                                       ,command=Command}
                     ], Continuation}, NewTag) ->
    Props = [{#wh_amqp_history.command, Command#'basic.consume'{consumer_tag=NewTag}}],
    _ = ets:update_element(?TAB, Timestamp, Props),
    update_consumer_tag(ets:match_object(Continuation), NewTag).

-spec unbind_queue(pid(), #'queue.unbind'{}) -> 'ok'.
unbind_queue(Consumer, #'queue.unbind'{queue=Queue
                                       ,exchange=Exchange
                                       ,routing_key=RoutingKey
                                      }) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'queue.bind'{queue=Queue
                                                      ,exchange=Exchange
                                                      ,routing_key=RoutingKey
                                                      ,_='_'
                                                     }
                               ,_='_'
                              },
    ets:match_delete(?TAB, Pattern),
    'ok'.
