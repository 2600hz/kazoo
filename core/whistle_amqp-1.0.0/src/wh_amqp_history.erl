%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_history).

-behaviour(gen_server).

-export([start_link/0]).
-export([is_consuming/2]).
-export([basic_consumers/1]).
-export([command/2]).
-export([remove/1]).
-export([get/1]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("amqp_util.hrl").

-define(TAB, ?MODULE).

-record(state, {consumers = sets:new()}).

-record(wh_amqp_history, {timestamp = now()
                          ,consumer
                          ,command}).

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

-spec is_consuming(pid(), ne_binary()) -> boolean().
is_consuming(Consumer, Queue) ->
    MatchSpec =[{#wh_amqp_history{consumer=Consumer
                                  ,command=#'basic.consume'{queue=Queue
                                                            ,_='_'}
                                 ,_='_'}
                 ,[]
                 ,['true']
                }],
    ets:select_count(?TAB, MatchSpec) =/= 0.

-spec basic_consumers(pid()) -> wh_amqp_commands().
basic_consumers(Consumer) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'basic.consume'{_='_'}
                               ,_='_'},
    [Command 
     || #wh_amqp_history{command=Command} <- ets:match_object(?TAB, Pattern)
    ].

-spec command(wh_amqp_assignment(), wh_amqp_commands()) -> 'ok'.
command(#wh_amqp_assignment{consumer=Consumer}, Command) ->
    MatchSpec =[{#wh_amqp_history{consumer=Consumer
                                  ,command=Command
                                  ,_='_'}
                 ,[]
                 ,['true']
                }],
    case ets:select_count(?TAB, MatchSpec) =:= 0 of
        'false' -> 
            lager:debug("not tracking history for known command ~s from ~p"
                        ,[element(1, Command), Consumer]);
        'true' -> 
            gen_server:cast(?MODULE, {'command', Consumer, Command})
    end.

-spec remove(wh_amqp_assignment() | pid() | 'undefined') -> 'ok'.
remove(#wh_amqp_assignment{consumer=Consumer}) -> remove(Consumer);
remove(Consumer) when is_pid(Consumer) ->
    gen_server:cast(?MODULE, {'remove', Consumer});
remove(_) -> 'ok'.

-spec get(pid()) -> wh_amqp_commands().
get(Consumer) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,_='_'},
    [Command 
     || #wh_amqp_history{command=Command} 
            <- ets:match_object(?TAB, Pattern)
    ].

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
    put(callid, ?LOG_SYSTEM_ID),
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
handle_cast({'command', Consumer, #'queue.delete'{queue=Queue}}, State) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'queue.declare'{queue=Queue
                                                         ,_='_'}
                               ,_='_'},
    _ = ets:match_delete(?TAB, Pattern),
    {'noreply', State};
handle_cast({'command', Consumer, #'queue.unbind'{queue=Queue
                                                  ,exchange=Exchange
                                                  ,routing_key=RoutingKey}}, State) ->
    Pattern = #wh_amqp_history{consumer=Consumer
                               ,command=#'queue.bind'{queue=Queue
                                                      ,exchange=Exchange
                                                      ,routing_key=RoutingKey
                                                      ,_='_'}
                               ,_='_'},
    _ = ets:match_delete(?TAB, Pattern),
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
handle_info({'DOWN', _, 'process', Consumer, _Reason}, State) ->
    lager:notice("consumer ~p went down without removing AMQP history: ~p"
                 ,[Consumer, _Reason]),
    _ = remove(Consumer),
    {'noreply', State};
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

