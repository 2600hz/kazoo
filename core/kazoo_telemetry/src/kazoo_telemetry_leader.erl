%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_telemetry_leader).

-behaviour(gen_server).

-export([start_link/0
        ,leader/0
        ,node_info/0
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_telemetry.hrl").

-define(SERVER, ?MODULE).

-record(state, {leader = 'undefined' :: node() | 'undefined'
               ,leader_poll_tref :: reference()
               ,responders = [] :: kz_term:ne_binaries()
               ,startup = 0 :: non_neg_integer()
               }).

-type state() :: #state{}.

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type oldest_kztm_node() :: 'undefined' |
                            {node(), kz_time:gregorian_seconds()}.

-define(NODE_INFO_BINDING, <<"kz_nodes.node.info">>).
-define(NODE_KEY, <<"telemetry">>).


%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc return the cluster's oldest node
%% @end
%%------------------------------------------------------------------------------
-spec leader() -> node().
leader() ->
    case kztm_oldest_node() of
        'undefined' -> node();
        {Node, _} -> Node
    end.

-spec kztm_oldest_node() -> oldest_kztm_node().
kztm_oldest_node() ->
    MatchSpec = [{#kz_node{node='$1'
                          ,node_info='$2'
                          ,_ = '_'
                          }
                 ,[]
                 ,[{{'$1','$2'}}]
                 }],
    determine_kztm_oldest_node(MatchSpec).

-spec determine_kztm_oldest_node(ets:match_spec()) -> oldest_kztm_node().
determine_kztm_oldest_node(MatchSpec) ->
    Results = ets:select('kz_nodes', MatchSpec),
    lists:foldl(fun determine_kztm_oldest_node_fold/2, 'undefined', Results).

-spec determine_kztm_oldest_node_fold({node(), kz_term:api_object()}, oldest_kztm_node()) -> oldest_kztm_node().
determine_kztm_oldest_node_fold({_Node, 'undefined'}, Acc) ->
    Acc;
determine_kztm_oldest_node_fold({Node, Info}, Acc) ->
    NodeTs = kz_json:get_integer_value([?NODE_KEY, <<"Startup">>], Info, 'undefined'),
    case Acc of
        'undefined' when NodeTs =:= 'undefined' -> Acc;
        'undefined' -> {Node, NodeTs};
        {_, Startup} when NodeTs < Startup -> {Node, NodeTs};
        _ -> Acc
    end.

-spec node_info() -> {kz_term:ne_binary(), kz_json:object()}.
node_info() ->
    Info = [{<<"Startup">>, startup()}
           ,{<<"leader">>, leader()}
           ],
    {?NODE_KEY, kz_json:from_list(Info)}.

-spec startup() -> non_neg_integer().
startup() ->
    gen_server:call(?SERVER, 'startup').

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = kz_util:set_startup(),
    _ = kazoo_bindings:bind(?NODE_INFO_BINDING
                           ,'kazoo_telemetry_leader'
                           ,'node_info'),
    lager:notice("starting kazoo_telemetry leader"),
    LeaderCheck = erlang:start_timer(?TM_LEADER_TICK, self(), 'leader_poll'),
    {'ok', #state{responders=?TM_RESPONDERS
                 ,leader_poll_tref = LeaderCheck
                 ,startup = get('$startup')
                 }}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call('startup', _From, #state{startup=Startup}=State) ->
    {'reply', Startup, State};
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('leader_change', #state{leader=OldLeader}=State) ->
    NewState = State#state{leader=leader()},
    case NewState#state.leader =:= node() of
        'false' ->
            lager:debug("telemetry leader is now ~s.", [NewState#state.leader]),
            _ = maybe_stop_responders(NewState, OldLeader =:= node()),
            {'noreply', NewState};
        'true' ->
            lager:debug("elected telemetry leader starting responders"),
            _ = lists:foreach(fun responder_start_fold/1, State#state.responders),
            {'noreply', NewState}
    end;
handle_cast(_Msg, State) ->
    {'noreply', State}.

-spec responder_start_fold(kz_term:ne_binary()) -> 'ok'.
responder_start_fold(App) ->
    case (kz_term:to_atom(App)):start_link() of
        {'error', _R} ->
            lager:debug("issue starting telemetry responder: ~p", [_R]),
            'ok';
        _ -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'timeout', _Pid, 'leader_poll'}, State) ->
    _ = maybe_change_leader(State#state.leader =/= leader()),
    Timer = erlang:start_timer(?TM_LEADER_TICK, self(), 'leader_poll'),
    NewState = State#state{leader_poll_tref=Timer},
    {'noreply', NewState};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc trigger a leader change
%% @end
%%------------------------------------------------------------------------------
-spec maybe_change_leader(boolean()) -> 'ok'.
maybe_change_leader('true') -> gen_server:cast(?SERVER, 'leader_change');
maybe_change_leader(_) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc trigger a leader change
%% @end
%%------------------------------------------------------------------------------
-spec maybe_stop_responders(state(), boolean()) -> 'ok'.
maybe_stop_responders(_State, 'false') -> 'ok';
maybe_stop_responders(#state{responders=Responders}, _) ->
    lists:foldl(fun(R, _) -> (kz_term:to_atom(R)):stop() end, [], Responders),
    'ok'.
