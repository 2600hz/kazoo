%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_history).
-behaviour(gen_server).

-export([start_link/0]).
-export([add_command/2]).
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

-record(state, {exchanges = dict:new() :: dict:dict(kz_term:ne_binary(), kz_amqp_exchange())
               ,connections = sets:new() :: sets:set(pid())
               }).
-type state() :: #state{}.

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
add_command(#kz_amqp_assignment{consumer=Consumer}, Command) ->
    case kz_amqp_history_ets:is_existing_command(Consumer, Command) of
        'false' -> handle_command(Consumer, Command);
        'true' ->
            lager:debug("not tracking history for known command ~s from ~p"
                       ,[element(1, Command), Consumer]
                       )
    end.

-spec handle_command(pid(), kz_amqp_command()) -> 'ok'.
handle_command(Consumer, #'queue.delete'{queue=Queue}) ->
    _ = kz_amqp_history_ets:delete_consumer_queue(Consumer, Queue),
    'ok';
handle_command(Consumer, #'queue.unbind'{}=Unbind) ->
    _ = kz_amqp_history_ets:unbind_queue(Consumer, Unbind),
    'ok';
handle_command(Consumer, #'basic.cancel'{consumer_tag=Tag}) ->
    _ = kz_amqp_history_ets:delete_consumer_consume(Consumer, Tag),
    'ok';
handle_command(Consumer, Command) ->
    kz_amqp_history_ets:add_consumer_command(Consumer, Command).

-spec update_consumer_tag(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_consumer_tag(Consumer, OldTag, NewTag) ->
    handle_update_consumer_tag(Consumer, OldTag, NewTag).

-spec remove(kz_amqp_assignment() | kz_term:api_pid()) -> 'ok'.
remove(#kz_amqp_assignment{consumer=Consumer}) -> remove(Consumer);
remove(Consumer) when is_pid(Consumer) ->
    _ = kz_amqp_history_ets:delete_consumer(Consumer),
    'ok';
remove(_) -> 'ok'.

-spec get(kz_term:api_pid()) -> kz_amqp_commands().
get('undefined') -> [];
get(Consumer) ->
    kz_amqp_history_ets:get_consumer_history(Consumer).

-spec add_exchange(kz_amqp_exchange()) -> 'ok'.
add_exchange(#'exchange.declare'{}=Exchange) ->
    gen_server:call(?SERVER, {'add_exchange', Exchange}, ?MILLISECONDS_IN_DAY).

-spec list_exchanges() -> kz_amqp_exchanges().
list_exchanges() ->
    gen_server:call(?SERVER, 'list_exchanges').

-spec list_consume(pid()) -> kz_amqp_commands().
list_consume(Consumer) ->
    kz_amqp_history_ets:get_consumer_basic_consumes(Consumer).

-spec is_consuming(pid(), kz_term:ne_binary()) -> boolean().
is_consuming(Consumer, Queue) ->
    kz_amqp_history_ets:is_consumer_queue_consuming(Consumer, Queue).

-spec is_bound(pid(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_bound(Consumer, Exchange, Queue, RoutingKey) when is_pid(Consumer) ->
    kz_amqp_history_ets:is_consumer_queue_bound_to_exchange(Consumer, Queue, Exchange, RoutingKey).

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
    _ = kz_amqp_history_ets:create(),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('list_exchanges', {Connection, _}, #state{exchanges=Exchanges
                                                     ,connections=Connections
                                                     }=State) ->
    _Ref = monitor('process', Connection),
    {'reply', [Exchange || {_, Exchange} <- dict:to_list(Exchanges)]
    ,State#state{connections=sets:add_element(Connection, Connections)}};
handle_call({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}
           , _From
           ,#state{exchanges=Exchanges
                  ,connections=Connections
                  }=State) ->
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
handle_cast({'add_exchange', #'exchange.declare'{exchange=Name}=Exchange}
           ,#state{exchanges=Exchanges
                  ,connections=Connections
                  }=State) ->
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
handle_info({'DOWN', _, 'process', Pid, _Reason}
           ,#state{connections=Connections}=State
           ) ->
    case sets:is_element(Pid, Connections) of
        'true' ->
            lager:debug("connection ~p went down: ~p", [Pid, _Reason]),
            {'noreply', State#state{connections=sets:del_element(Pid, Connections)}};
        'false' ->
            %% Consumer will be torn down when kz_amqp_assignments is done processing it
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
-spec handle_update_consumer_tag(pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_update_consumer_tag(Consumer, OldTag, NewTag) ->
    lager:debug("updating consumer ~p tag ~p to ~p", [Consumer, OldTag, NewTag]),
    ConsumerTags = kz_amqp_history_ets:get_consumer_commands_by_tag(Consumer, OldTag),
    update_old_tags(ConsumerTags, NewTag).

-spec update_old_tags([kz_amqp_history_ets:history()], kz_term:ne_binary()) -> 'ok'.
update_old_tags(History, NewTag) ->
    _ = [kz_amqp_history_ets:update_command_tag(H, NewTag) || H <- History],
    'ok'.
