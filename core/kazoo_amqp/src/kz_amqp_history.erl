%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_history).
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

-include("kz_amqp_util.hrl").

-define(SERVER, ?MODULE).

-define(TAB, ?MODULE).

-record(state, {consumers = sets:new() :: sets:set(pid())
               ,exchanges = dict:new() :: dict:dict(kz_term:ne_binary(), kz_amqp_exchange())
               ,connections = sets:new() :: sets:set(pid())
               }).
-type state() :: #state{}.

-record(kz_amqp_history, {timestamp = os:timestamp() :: kz_time:now() | '_'
                         ,consumer :: kz_term:api_pid() | '_'
                         ,command :: kz_amqp_command() | '_'
                         }).
-type kz_amqp_history() :: #kz_amqp_history{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec add_command(kz_amqp_assignment(), kz_amqp_command()) -> 'ok'.
add_command(Assignment, Command) ->
    add_command(Assignment, Command, 'async').

-spec add_command(kz_amqp_assignment(), kz_amqp_command(), 'sync' | 'async') -> 'ok'.
add_command(#kz_amqp_assignment{consumer=Consumer}, Command, Method) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
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
    gen_server:cast(?SERVER, {'command', Consumer, Command});
send_command(Consumer, Command, 'sync') ->
    gen_server:call(?SERVER, {'command', Consumer, Command}).

-spec update_consumer_tag(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_consumer_tag(Consumer, OldTag, NewTag) ->
    gen_server:cast(?SERVER, {'update_consumer_tag', Consumer, OldTag, NewTag}).

-spec remove(kz_amqp_assignment() | pid() | 'undefined') -> 'ok'.
remove(#kz_amqp_assignment{consumer=Consumer}) -> remove(Consumer);
remove(Consumer) when is_pid(Consumer) ->
    gen_server:cast(?SERVER, {'remove', Consumer});
remove(_) -> 'ok'.

-spec get(kz_term:api_pid()) -> kz_amqp_commands().
get('undefined') -> [];
get(Consumer) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,_='_'
                              },
    [Command
     || #kz_amqp_history{command=Command}
            <- ets:match_object(?TAB, Pattern)
    ].

-spec add_exchange(kz_amqp_exchange()) -> 'ok'.
add_exchange(#'exchange.declare'{}=Exchange) ->
    gen_server:call(?SERVER, {'add_exchange', Exchange}, ?MILLISECONDS_IN_DAY).

-spec list_exchanges() -> kz_amqp_exchanges().
list_exchanges() ->
    gen_server:call(?SERVER, 'list_exchanges').

-spec list_consume(pid()) -> kz_amqp_commands().
list_consume(Consumer) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{_='_'}
                              ,_='_'
                              },
    [Command
     || #kz_amqp_history{command=Command} <- ets:match_object(?TAB, Pattern)
    ].

-spec is_consuming(pid(), kz_term:ne_binary()) -> boolean().
is_consuming(Consumer, Queue) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
                                  ,command=#'basic.consume'{queue=Queue
                                                           ,_='_'
                                                           }
                                  ,_='_'
                                  }
                 ,[]
                 ,['true']
                 }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

-spec is_bound(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_bound(Consumer, Exchange, Queue, RoutingKey) when is_pid(Consumer) ->
    MatchSpec = [{#kz_amqp_history{consumer=Consumer
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

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?MODULE),
    _ = ets:new(?TAB, ['named_table'
                      ,{'keypos', #kz_amqp_history.timestamp}
                      ,'protected'
                      ,'ordered_set'
                      ]),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'command', Consumer, #'queue.unbind'{}=Unbind}, _From, State) ->
    unbind_queue(Consumer, Unbind),
    {'reply', 'ok', State};
handle_call('list_exchanges', {Connection, _}, #state{exchanges=Exchanges
                                                     ,connections=Connections
                                                     }=State) ->
    _Ref = monitor('process', Connection),
    {'reply', [Exchange || {_, Exchange} <- dict:to_list(Exchanges)]
    ,State#state{connections=sets:add_element(Connection, Connections)}};
handle_call({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}
           , _From
           ,#state{exchanges=Exchanges
                  ,connections=Connections}=State) ->
    case dict:find(Name, Exchanges) of
        'error' ->
            _ = [(catch kz_amqp_connection:new_exchange(Connection, Exchange))
                 || Connection <- sets:to_list(Connections)
                ],
            {'reply', 'ok', State#state{exchanges=dict:store(Name, Exchange, Exchanges)}};
        {'ok', _Exits} -> {'reply', 'ok', State}
    end;
handle_call(_Msg, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'update_consumer_tag', Consumer, OldTag, NewTag}, State) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{consumer_tag=OldTag
                                                       ,_='_'}
                              ,_='_'},
    _ = update_consumer_tag(ets:match_object(?TAB, Pattern, 1), NewTag),
    {'noreply', State};
handle_cast({'command', Consumer, #'queue.delete'{queue=Queue}}, State) ->
    Pattern = #kz_amqp_history{consumer=Consumer
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
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'basic.consume'{consumer_tag=CTag
                                                       ,_='_'}
                              ,_='_'},
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State};
handle_cast({'command', Consumer, Command}, #state{consumers=Consumers}=State) ->
    _ = ets:insert(?TAB, #kz_amqp_history{consumer=Consumer
                                         ,command=Command}),
    case sets:is_element(Consumer, Consumers) of
        'true' -> {'noreply', State};
        'false' ->
            _Ref = monitor('process', Consumer),
            {'noreply', State#state{consumers=sets:add_element(Consumer, Consumers)}}
    end;
handle_cast({'remove', Consumer}, #state{consumers=Consumers}=State) ->
    Pattern = #kz_amqp_history{consumer=Consumer, _='_'},
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State#state{consumers=sets:del_element(Consumer, Consumers)}};
handle_cast({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}
           ,#state{exchanges=Exchanges
                  ,connections=Connections}=State) ->
    _ = [(catch kz_amqp_connection:new_exchange(Connection, Exchange))
         || Connection <- sets:to_list(Connections)
        ],
    {'noreply', State#state{exchanges=dict:store(Name, Exchange, Exchanges)}};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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
            %% Allow kz_amqp_assignments time to get the history so it
            %% can cleanly remove queues/consumers/etc
            lager:debug("removing AMQP history for consumer ~p in 2.5s: ~p"
                       ,[Pid, _Reason]),
            erlang:send_after(2500, self(), {'remove_history', Pid}),
            {'noreply', State}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("AMQP history terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_consumer_tag({kz_amqp_history(), ets:continuation()} | '$end_of_table', kz_term:ne_binary()) -> 'ok'.
update_consumer_tag('$end_of_table', _) -> 'ok';
update_consumer_tag({[#kz_amqp_history{timestamp=Timestamp
                                      ,command=Command}
                     ], Continuation}, NewTag) ->
    Props = [{#kz_amqp_history.command, Command#'basic.consume'{consumer_tag=NewTag}}],
    _ = ets:update_element(?TAB, Timestamp, Props),
    update_consumer_tag(ets:match_object(Continuation), NewTag).

-spec unbind_queue(pid(), #'queue.unbind'{}) -> 'ok'.
unbind_queue(Consumer, #'queue.unbind'{queue=Queue
                                      ,exchange=Exchange
                                      ,routing_key=RoutingKey
                                      }) ->
    Pattern = #kz_amqp_history{consumer=Consumer
                              ,command=#'queue.bind'{queue=Queue
                                                    ,exchange=Exchange
                                                    ,routing_key=RoutingKey
                                                    ,_='_'
                                                    }
                              ,_='_'
                              },
    ets:match_delete(?TAB, Pattern),
    'ok'.
