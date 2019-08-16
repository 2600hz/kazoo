%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc This process runs on each node in the Kazoo cluster. It collects information
%%% on each node, and regularly sends the information the stats application.
%%% For `ecallmgr' nodes, it also collects `ecallmgr' information, and
%%% sip events statistics.
%%%
%%% @author Stephen Gibberd <stephen.gibberd@2600hz.com>
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_stats).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).
-export([increment_counter/1
        ,increment_counter/2
        ,send_counter/2
        ,send_absolute/2
        ,stop/0
        ,getdb/0
        ]).

-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(SERVER, ?MODULE).

-define(SEND_INTERVAL, 10 * ?MILLISECONDS_IN_SECOND).

-record(state, {variables=[]
               ,sip=[]
               ,send_stats=?SEND_INTERVAL
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
start_link() -> start_link(?SEND_INTERVAL).

-spec start_link(pos_integer()) -> kz_types:startlink_ret().
start_link(Send_stats) ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [Send_stats], []).

-spec stop() -> 'ok'.
stop() ->
    gen_server:cast(?SERVER, 'stop').

-spec getdb() -> state().
getdb() ->
    gen_server:call(?SERVER, 'get_db').

-spec increment_counter(any()) -> 'ok'.
increment_counter(Item) -> send_counter(Item, 1).

-spec increment_counter(any(), any()) -> 'ok'.
increment_counter(Realm, Item) ->
    gen_server:cast(?SERVER, {'add', Realm, Item, 1}).

-spec send_counter(any(), pos_integer()) -> 'ok'.
send_counter(Item, Value) when is_integer(Value) ->
    gen_server:cast(?SERVER, {'add', Item, Value}).

-spec send_absolute(any(), any()) -> 'ok'.
send_absolute(Item, Value) ->
    gen_server:cast(?SERVER, {'store', Item, Value}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Send_stats]) ->
    erlang:send_after(Send_stats, self(), {'send_stats', Send_stats}),
    {'ok', #state{send_stats=Send_stats}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('get_db', _From, State) ->
    {'reply', State, State};
handle_call(Other,_From,State) ->
    lager:debug("call ~p",[Other]),
    {'reply', 'ok', State}.


%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('stop', State) ->
    {'stop', 'ok', State};
handle_cast({Operation, Key, Val}, State) when Operation == 'add';
                                               Operation == 'store' ->
    {'noreply', store_value(Operation, Key, Val, State)};
handle_cast({Operation,Realm,Key,Val}, State) when Operation == 'add';
                                                   Operation == 'store' ->
    {'noreply', store_value(Operation, Realm, Key, Val, State)};
handle_cast(_,State) ->
    {'noreply',State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'send_stats', SendStats}=Info,State) ->
    send_stats(State#state.variables, State#state.sip),
    erlang:send_after(SendStats, self(), Info),
    {'noreply', State};
handle_info(_Info, State) ->
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
terminate(_Reason, _State) -> 'ok'.

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

%%% #state{}  stores statistics in the variables field, and sip event
%%% statistics in the sip field. store_value() either increments the existing
%%% value or sets the value.

store_value('add', Key, Value, State=#state{variables=Var}) when is_integer(Value) ->
    NewValue =  props:get_value(Key, Var, 0) + Value,
    State#state{variables = lists:keystore(Key, 1, Var, {Key, NewValue})};
store_value('store', Key, Value, State=#state{variables=Var}) ->
    State#state{variables = lists:keystore(Key, 1, Var, {Key, Value})}.

%%% Sip events are stored according to the sip realm/domain
store_value(Operation, Realm, Key, Value ,State=#state{sip=SipList}) when is_integer(Value) ->
    NewData = case props:get_value(Realm, SipList) of
                  'undefined' -> [{Key, Value}];
                  RealmData ->
                      NewValue =
                          case Operation == 'add' of
                              'true' -> props:get_value(Key, RealmData, 0) + Value;
                              'false' -> Value
                          end,
                      lists:keystore(Key, 1, RealmData, {Key, NewValue})
              end,
    State#state{sip=lists:keystore(Realm, 1, SipList, {Realm, NewData})}.

%%% send_stats collects and sends the statistics to the kazoo_stats_master
%%% process. All node (VMs) send memory usage
send_stats(VarList, SipList) when is_list(VarList), is_list(SipList) ->
    Vals = [{'nodename', node()},
            {'memory-total', erlang:memory(total)},
            {'memory-processes', erlang:memory(processes)},
            {'memory-system', erlang:memory(system)},
            {'memory-atom', erlang:memory(atom)},
            {'memory-binary', erlang:memory(binary)},
            {'memory-code', erlang:memory(code)},
            {'erlang-version', erlang:system_info(system_version)--"\n"},
            {'memory-ets', erlang:memory(ets)}
           ]
        ++ VarList
        ++ get_ecallmgr_values(VarList)
        ++ get_sip_values(SipList),
    send(Vals).

get_sip_values([]) -> [];
get_sip_values(SipList) ->
    [{<<"sip">>, SipList}].

get_ecallmgr_values(VarList) ->
    case atom_to_list(node()) of
        "ecallmgr@" ++ _ ->
            get_ecallmgr_values2(VarList);
        _ -> []
    end.

get_ecallmgr_values2(VarList) ->
    RegFail = case {props:get_value("register-attempt",VarList),
                    props:get_value("register-success",VarList)}
              of
                  {'undefined', _} -> 0;
                  {_, 'undefined'} -> 0;
                  {Reg, RegSucc} -> lists:max([Reg-RegSucc, 0])
              end,
    %% Sums the total reductions for all ecallmgr processes.
    Procs = [process_info(X) ||
                {_, X, _ ,_} <- supervisor:which_children('ecallmgr_sup')
                    ,is_pid(X)
            ],
    Reduction = lists:sum([props:get_value('reductions', X)
                           || X <- Procs
                                  ,is_list(X)
                          ]),
    [{'reduction',Reduction}
    ,{'register-fail',RegFail}
    ,{'processes', length(processes())}
    ].

send(RawPayload) ->
%%%   lager:debug("trying to convert to json ~n~p",[RawPayload]),
    Payload = kz_json:encode(recursive_from_proplist(RawPayload)),
%%%   lager:debug("sending to stats_master ~p",[Payload]),
    kz_amqp_util:targeted_publish(<<"statistics">>,Payload).

%% Prepares nested proplists to convert to JSON
-spec recursive_from_proplist(any()) -> kz_json:object().
recursive_from_proplist([]) -> kz_json:new();
recursive_from_proplist(List) when is_list(List) ->
    case lists:all(fun is_integer/1, List) of
        'true' -> List;
        'false' ->
            kz_json:from_list([{kz_term:to_binary(K)
                               ,recursive_from_proplist(V)}
                               || {K,V} <- List
                              ])
    end;
recursive_from_proplist(Other) -> Other.
