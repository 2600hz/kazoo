%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(amqp_leader_proc).
-behaviour(gen_server).

-compile({no_auto_import,[node/1]}).

%% API functions
-export([start/6
        ,start_link/6
        ,leader_call/2, leader_call/3
        ,leader_cast/2
        ,call/2, call/3
        ,cast/2
        ,reply/2
        ]).

-export([alive/1
        ,down/1
        ,candidates/1
        ,workers/1
        ,broadcast/3
        ,leader_node/1
        ]).

-export([s/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name                   :: atom()
               ,leader                 :: sign() | undefined
               ,role                   :: role() | undefined
               ,elected = 0            :: integer()
               ,restarted = 0          :: integer()
               ,callback_module        :: atom()
               ,callback_state         :: any()
               ,down = []              :: kz_term:atoms()
               ,candidates = [node()]  :: kz_term:atoms()
               }).

-record(sign, {elected                 :: integer()
              ,restarted               :: integer()
              ,node = node()           :: atom()
              ,name                    :: atom()
              ,sync                    :: any()
              }).

-record(?MODULE, {from :: sign()
                 ,msg :: join | {leader, sign()} | {sync} | {from_leader, any()}
                 }).

-type role() :: 'candidate' | 'leader'.
-type state() :: #state{}.
-type sign() :: #sign{}.
-type routine_ret() :: state() | {state(), routines()} | {'stop', any()}.
-type routine() :: {fun((state(), any()) -> routine_ret()), any()}.
-type routines() :: [routine()].
-type recipient() :: pid()
                   | atom()
                   | {atom(), atom()}
                   | {state(), atom()}
                   | {atom(), sign()}
                   | sign()
                   | kz_term:ne_binary().
-type from() :: any().

-include("amqp_leader.hrl").

-define(is_leader, State#state.role =:= 'leader').
-define(from_leader, (State#state.role =/= 'leader')
        andalso
          (From#sign.node =:= State#state.leader#sign.node
           andalso From#sign.elected =:= State#state.leader#sign.elected
           andalso From#sign.restarted =:= State#state.leader#sign.restarted
          )
       ).
%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start(Name, CandidateNodes, OptArgs, Mod, Arg, Options) ->
    start_it('start', Name, CandidateNodes, OptArgs, Mod, Arg, Options).

-spec start_link(atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start_link(Name, CandidateNodes, OptArgs, Mod, Arg, Options) ->
    start_it('start_link', Name, CandidateNodes, OptArgs, Mod, Arg, Options).

-spec start_it(atom(), atom(), kz_term:atoms(), list(), atom(), list(), list()) -> kz_types:startlink_ret().
start_it(StartFun, Name, CandidateNodes, OptArgs, Mod, Arg, Options) ->
    gen_server:StartFun({'local', Name}, ?MODULE, [Name, CandidateNodes, OptArgs, Mod, Arg, Options], []).

-spec leader_call(atom(), any()) -> any().
leader_call(Name, Request) ->
    leader_call(Name, Request, 5000).

-spec leader_call(atom(), any(), integer() | 'infinity') -> any().
leader_call(Name, Request, Timeout) ->
    call(Name, {'leader_call', Request}, Timeout).

-spec leader_cast(atom(), any()) -> 'ok'.
leader_cast(Name, Request) ->
    gen_server:cast(Name, {'leader_cast', Request}).

-spec cast(atom(), any()) -> 'ok'.
cast(Name, Request) ->
    gen_server:cast(Name, Request).

-spec call(atom(), any()) -> any().
call(Name, Request) ->
    call(Name, Request, 5000).

-spec call(atom(), any(), integer() | 'infinity') -> any().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

-spec reply({pid(), any()}, any()) -> term().
reply({Pid, _} = From, Reply) when is_pid(Pid)
                                   andalso erlang:node(Pid) =:= node() ->
    gen_server:reply(From, Reply);
reply({From, Tag}, Reply) ->
    send(From, {Tag, Reply}).

-spec alive(sign()) -> kz_term:atoms().
alive(_) -> [node()].
%%alive(#sign{candidates = Candidates}) ->
%%    Candidates.

-spec down(sign()) -> kz_term:atoms().
down(_) -> [].

-spec workers(sign()) -> kz_term:atoms().
workers(_) -> [].

-spec candidates(sign()) -> kz_term:atoms().
candidates(_) -> [node()].

-type msg() :: {'from_leader', any()}.

-spec broadcast(msg(), kz_term:atoms(), sign()) -> sign().
broadcast(Msg, _Nodes, #sign{name = Name} = Sign) ->
    send({Name, 'broadcast'}, Msg),
    Sign.

-spec leader_node(sign()) -> atom().
leader_node(#sign{node = Node}) -> Node.

-spec s(any()) -> {pid(), state()}.
s(Name) ->
    gen_server:call(Name, s).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', state()}.
init([Name, _CandidateNodes, _OptArgs, Mod, Arg, _Options]) ->
    kz_log:put_callid(kapi_leader:queue()),
    gen_server:cast(self(), {'init', Arg}),
    State = #state{name = Name
                  ,callback_module = Mod
                  },
    {'ok', State}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), {pid(), any()}, state()) -> kz_types:handle_call_ret_state(state()).
handle_call(s, _, State) ->
    {reply, {self(), State}, State};
handle_call({'leader_call', Msg}, From, State) when ?is_leader ->
    Routines = [{fun call_handle_leader_call/2, {From, Msg}}
               ],
    noreply(State, Routines);
handle_call({'leader_call', Msg}, From, #state{name = Name} = State) ->
    send(leader(State), {'leader_call', {{Name, node()}, From}, Msg}),
    noreply(State, []);
handle_call(Call, From, State) ->
    Routines = [{fun call_handle_call/2, {From, Call}}
               ],
    noreply(State, Routines).

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'init', Arg}, State) ->
    init(State, Arg);
handle_cast({'leader_cast', Msg}, State) when ?is_leader ->
    Routines = [{fun call_handle_leader_cast/2, Msg}
               ],
    noreply(State, Routines);
handle_cast({'leader_cast', Msg}, State) ->
    send(leader(State), {'leader_cast', Msg}),
    noreply(State, []);
handle_cast(Msg, State) ->
    Routines = [{fun call_handle_cast/2, Msg}
               ],
    noreply(State, Routines).

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(#?MODULE{from = From, msg = 'join'} = Msg, State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun announce_leader/2, {From, 'me'}}
               ,{fun add_candidates/2, From}
               ,{fun call_elected/2, From}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{from = From, msg = 'join'} = Msg, State) ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun add_candidates/2, From}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{msg = {'leader', Me}} = Msg, #state{leader = Me} = State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun increase_elected/2, []}
               ,{fun set_leader/2, 'me'}
               ,{fun announce_leader/2, {'broadcast', sign(State)}}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{msg = {'leader', NotMe}} = Msg, State) when ?is_leader ->
    lager:debug("message ~p", [Msg]),
    case NotMe > sign(State) of
        'true' ->
            Routines = [{fun set_leader/2, NotMe}
                       ,{fun set_role/2, 'candidate'}
                       ,{fun announce_leader/2, {'broadcast', 'me'}}
                       ,{fun surrender/2, NotMe}
                       ,{fun call_surrendered/2, 'undefined'}
                       ],
            noreply(State, Routines);
        'false' ->
            Routines = [{fun announce_leader/2, {NotMe, 'me'}}
                       ],
            noreply(State, Routines)
    end;

handle_info(#?MODULE{from = From, msg = {'leader', NewLeader}} = Msg, State) when ?from_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun set_leader/2, NewLeader},
                {fun call_surrendered/2, 'undefined'}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{from = From, msg = {'sync'}} = Msg, State) when ?from_leader ->
    lager:debug("message ~p", [Msg]),
    Routines = [{fun call_surrendered/2, 'undefined'}
               ,{fun add_candidates/2, From}
               ],
    noreply(State, Routines);

handle_info(#?MODULE{from = From, msg = Msg}, State) when ?from_leader ->
    Routines = [{fun call_from_leader/2, Msg}
               ],
    noreply(State, Routines);

handle_info({'leader_call', From, Request}, State) when ?is_leader ->
    Routines = [{fun call_handle_leader_call/2, {From, Request}}
               ],
    noreply(State, Routines);

handle_info({{Pid, _} = From, Reply}, State) when is_pid(Pid)
                                                  andalso erlang:node(Pid) =:= node() ->
    gen_server:reply(From, Reply),
    noreply(State, []);

handle_info({'DOWN', Node}, State) when ?is_leader ->
    Routines = [{fun call_handle_DOWN/2, Node}
               ],
    noreply(State, Routines);

handle_info({'DOWN', Node} = Msg, #state{leader = Leader} = State) ->
    lager:debug("message ~p", [Msg]),
    case node(Leader) of
        Node ->
            Routines = [{fun increase_elected/2, []}
                       ,{fun set_leader/2, 'me'}
                       ,{fun set_role/2, 'leader'}
                       ,{fun set_sync/2, sync(Leader)}
                       ,{fun call_elected/2, 'undefined'}
                       ,{fun announce_leader/2, {'broadcast', Leader}}
                       ],
            noreply(State, Routines);
        _ ->
            Routines = [],
            noreply(State, Routines)
    end;

handle_info(Info, State) ->
    Routines = [{fun call_handle_info/2, Info}
               ],
    noreply(State, Routines).

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, State) ->
    send({name(State), 'broadcast'}, {'DOWN', node()}),
    'ok'.

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
-spec init(state(), any()) -> routine_ret().
init(#state{callback_module = Mod, name = Name} = State, Arg) ->
    'true' = amqp_leader_listener:is_ready(),
    case catch Mod:init(Arg) of
        {'stop', _Reason} = Stop -> Stop;
        {'ignore', _, _} -> 'ignore';
        {'EXIT', Reason} -> {'stop', Reason};
        {'ok', ModState} ->
            NewState = State#state{callback_state = ModState},
            send({Name, 'broadcast'}, #?MODULE{from=sign(NewState), msg='join'}),
            receive
                #?MODULE{msg = {'leader', Leader}, from = Leader} ->
                    Routines = [{fun set_leader/2, Leader}
                               ,{fun set_role/2, 'candidate'}
                               ,{fun add_candidates/2, Leader}
                               ,{fun call_surrendered/2, 'undefined'}
                               ,{fun announce_leader/2, {Leader, 'me'}}
                               ],
                    noreply(NewState, Routines)
            after
                3000 ->
                    Routines = [{fun set_leader/2, 'me'}
                               ,{fun set_role/2, 'leader'}
                               ,{fun call_elected/2, 'undefined'}
                               ,{fun announce_leader/2, {'broadcast', 'me'}}
                               ],
                    noreply(NewState, Routines)
            end;
        Else ->
            lager:warning("~s initialization bad return ~p", [Mod, Else]),
            {'stop', {'bad_return_value', Else}}
    end.

-spec surrender(state(), sign()) -> state().
surrender(State, To) ->
    send({State, 'broadcast'}, #?MODULE{from = sign(State), msg = {'leader', To}}),
    set_role(State, 'candidate').

-spec call_surrendered(state(), sign() | 'undefined') -> state().
call_surrendered(#state{leader = Leader} = State, Leader) ->
    State;
call_surrendered(#state{callback_module = Mod} = State, 'undefined') ->
    LeaderSync = sync(State),
    {'ok', NewModState} = Mod:surrendered(callback_state(State), LeaderSync, State),
    set_callback_state(State, NewModState);
call_surrendered(State, OldLeader) ->
    LeaderSync = sync(State),
    case sync(OldLeader) =:= LeaderSync of
        'true' ->
            State;
        'false' ->
            call_surrendered(State, 'undefined')
    end.

-spec call_elected(state(), sign() | atom()) -> state().
call_elected(State, #sign{} = From) ->
    call_elected(State, node(From));
call_elected(#state{callback_module = Mod, leader = Leader} = State, Node) when ?is_leader ->
    {Action, Sync, ModState} = Mod:elected(callback_state(State), leader(State), Node),
    State1 = set_leader(State, Leader#sign{sync = Sync}),
    State2 = set_callback_state(State1, ModState),
    send({name(State), Node}, #?MODULE{from = sign(State2), msg = {'sync'}}),
    case Action of
        'ok' ->
            send({name(State2), 'broadcast'}, #?MODULE{from = sign(State2), msg = {'from_leader', Sync}});
        'reply' -> 'ok'
    end,
    State2.

-spec call_handle_info(state(), any()) -> routine_ret().
call_handle_info(State, Info) ->
    case exec_callback(State, {'handle_info', [Info]}) of
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_handle_call(state(), {from(), any()}) -> routine_ret().
call_handle_call(State, {From, Call}) ->
    case exec_callback(State, {'handle_call', [Call, From]}) of
        {'reply', Reply, ModState} ->
            reply(From, Reply),
            set_callback_state(State, ModState);
        {'stop', Reason, Reply, ModState} ->
            reply(From, Reply),
            {set_callback_state(State, ModState), [{'stop', Reason}]};
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_handle_cast(state(), any()) -> routine_ret().
call_handle_cast(State, Msg) ->
    case exec_callback(State, {'handle_cast', [Msg]}) of
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_handle_leader_call(state(), {from(), any()}) -> routine_ret().
call_handle_leader_call(State, {From, Msg}) when ?is_leader ->
    case exec_callback(State, {'handle_leader_call', [Msg, From]}) of
        {'reply', Reply, Broadcast, ModState} ->
            reply(From, Reply),
            send({name(State), 'broadcast'}, #?MODULE{from = leader(State), msg = Broadcast}),
            {set_callback_state(State, ModState), [{fun announce_leader/2, {'broadcast', 'me'}}]};
        {'reply', Reply, ModState} ->
            reply(From, Reply),
            set_callback_state(State, ModState);
        {'stop', Reason, Reply, ModState} ->
            reply(From, Reply),
            {set_callback_state(State, ModState), [{'stop', Reason}]};
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_handle_leader_cast(state(), any()) -> routine_ret().
call_handle_leader_cast(State, Msg) when ?is_leader ->
    case exec_callback(State, {'handle_leader_cast', [Msg]}) of
        {'ok', Broadcast, ModState} ->
            send({State, 'broadcast'}, Broadcast),
            set_callback_state(State, ModState);
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_from_leader(state(), any()) -> routine_ret().
call_from_leader(State, Sync) when not (?is_leader) ->
    case exec_callback(State, {'from_leader', [Sync]}) of
        {'ok', ModState} ->
            set_callback_state(State, ModState);
        {'noreply', ModState} ->
            set_callback_state(State, ModState);
        {'stop', Reason, ModState} ->
            {set_callback_state(State, ModState), [{'stop', Reason}]}
    end.

-spec call_handle_DOWN(state(), atom()) -> state().
call_handle_DOWN(State, Node) when ?is_leader ->
    case exec_callback(State, {'handle_DOWN', [Node]}) of
        {'ok', ModState} ->
            set_callback_state(State, ModState);
        {'ok', Msg, ModState} ->
            send({State, 'broadcast'}, Msg),
            set_callback_state(State, ModState)
    end.

-type callback_ret() :: {'reply', any(), any(), any()}
                      | {'reply', any(), any()}
                      | {'stop', any(), any(), any()}
                      | {'stop', any(), any()}
                      | {'noreply', any()}
                      | {'ok', any(), any()}
                      | {'ok', any()}.
-spec exec_callback(state(), {atom(), list()}) -> callback_ret().
exec_callback(#state{callback_module = Mod} = State, {Callback, Args}) ->
    ModState = callback_state(State),
    erlang:apply(Mod, Callback, Args ++ [ModState, leader(State)]).

-spec noreply(state(), routines()) -> state() | {'stop', any(), state()}.
noreply(#state{} = State, []) ->
    {'noreply', State};
noreply(#state{} = State, [{'stop', Reason} | _]) ->
    {'stop', Reason, State};
noreply(#state{}, [#state{} = NewState | Rest]) ->
    noreply(NewState, Rest);
noreply(#state{} = State, [{Fun, Data} | Rest]) when is_function(Fun, 2) ->
    case Fun(State, Data) of
        #state{} = NewState ->
            noreply(NewState, Rest);
        {#state{} = NewState, Routines} ->
            noreply(NewState, Routines ++ Rest);
        _ ->
            noreply(State, Rest)
    end.

-spec increase_elected(state(), []) -> state().
increase_elected(#state{elected = Elected} = State, []) ->
    State#state{elected = Elected + 1}.

-spec add_candidates(state(), sign() | kz_term:atoms()) -> state().
add_candidates(#state{} = State, #sign{node = Node}) ->
    add_candidates(State, [Node]);
add_candidates(#state{candidates = MyCandidates} = State, Candidates) ->
    State#state{candidates = lists:usort(MyCandidates ++ Candidates)}.

-spec set_role(state(), role()) -> state().
set_role(State, Role) -> State#state{role = Role}.

-spec set_leader(state(), 'me' | sign()) -> state().
set_leader(State, 'me') -> set_leader(State, sign(State));
set_leader(State, Leader) -> State#state{leader = Leader}.

-spec leader(state()) -> sign().
leader(#state{leader = Leader}) -> Leader.

-spec name(state() | sign()) -> atom().
name(#state{name = Name}) -> Name;
name(#sign{name = Name}) -> Name.

-spec set_callback_state(state(), any()) -> state().
set_callback_state(#state{} = State, CallbackState) -> State#state{callback_state = CallbackState}.

-spec callback_state(state()) -> state().
callback_state(#state{callback_state = State}) -> State.

-spec sync(state() | sign()) -> any().
sync(#state{leader = Leader}) -> sync(Leader);
sync(#sign{sync = Sync}) -> Sync;
sync(_) -> 'undefined'.

-spec set_sync(state(), any()) -> state().
set_sync(#state{leader = Leader} = State, Sync) ->
    State#state{leader = Leader#sign{sync = Sync}}.

-spec announce_leader(state(), {recipient(), 'me' | sign()}) -> state().
announce_leader(State, {To, 'me'}) ->
    announce_leader(State, {To, sign(State)});
announce_leader(State, {To, #sign{} = From}) ->
    send({State, To}, #?MODULE{from = From, msg = {'leader', leader(State)}}),
    State.

-spec send(recipient(), any()) -> 'ok'.
send(Pid, Msg) when is_atom(Pid); node() =:= erlang:node(Pid); node() =:= element(2, Pid) ->
    lager:debug("local message ~p: ~p", [Msg, Pid]),
    Pid ! Msg;
send({Name, Node}, Msg) when is_atom(Name)
                             andalso is_atom(Node) ->
    Route = kapi_leader:route(Name, Node),
    send(Route, Msg);
send({#state{name = Name}, Node}, Msg) ->
    send({Name, Node}, Msg);
send({Name, #sign{node = Node}}, Msg) ->
    send({Name, Node}, Msg);
send(Pid, Msg) when is_pid(Pid) ->
    Route = kapi_leader:route(Pid),
    send(Route, Msg);
send(#sign{name = Name, node = Node}, Msg) ->
    send({Name, Node}, Msg);
send(Route, Msg) when is_binary(Route) ->
    lager:debug("amqp message ~p: ~p", [Msg, Route]),
    Props = [{<<"Message">>, kz_term:to_hex_binary(erlang:term_to_binary(Msg))}
             | kz_api:default_headers(<<"leader">>, <<"message">>, ?APP_NAME, ?APP_VERSION)
            ],
    kapi_leader:publish_req(Route, Props).

-spec node(sign() | pid()) -> atom().
node(#sign{node = Node}) when is_atom(Node) -> Node.

-spec sign(state()) -> sign().
sign(#state{elected = Elected, restarted = Restarted, name = Name
            %% ,candidates = Candidates
           } = State) ->
    #sign{elected = Elected
         ,restarted = -Restarted
         ,name = Name
         ,sync = sync(State)
          %% ,candidates = Candidates
         }.
