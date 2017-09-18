%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Store routing keys/pid bindings. When a binding is fired,
%%% pass the payload to the pid for evaluation, accumulating
%%% the results for the response to the running process.
%%%
%%% foo.erl -> bind("module.init").
%%% *** Later ***
%%% module.erl
%%%   init() -> run("module.init", {some, "payload", 4, <<"You">>}).
%%%                foo ! Payload,
%%%                receive -> Resp
%%%   init() <- [Resp]
%%%   init() -> Decides what to do with responses
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kazoo_bindings).
-behaviour(gen_server).

%% API
-export([start_link/0
        ,bind/3, bind/4
        ,unbind/3, unbind/4
        ,map/2, map/3, pmap/2, pmap/3
        ,fold/2, fold/3
        ,flush/0, flush/1, flush_mod/1
        ,filter/1
        ,stop/0
        ,modules_loaded/0
        ,is_ready/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/2
        ,all/2
        ,succeeded/2
        ,failed/2
        ,matches/2
        ]).

%% Helper Function for calling map/3
-export([candidates/1
        ]).

-export([rt_options/0, rt_options/1]).

%% Helper Functions for debugging
-export([bindings/0, bindings/1, bindings/2
        ]).

%% ETS Persistence
-export([table_id/0
        ,table_options/0
        ,find_me_function/0
        ,gift_data/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_bindings.hrl").

-define(SERVER, ?MODULE).

%% {<<"foo.bar.#">>, [<<"foo">>, <<"bar">>, <<"#">>], queue:queue(), <<"foo.bar">>}

-type payload() :: any().
-type fold_results() :: payload().
-type map_results() :: list().

-type matches_fun() :: fun((ne_binaries(), ne_binaries()) -> boolean()).
-type candidates_fun() :: fun((ne_binary()) -> kz_bindings()).

-record(kz_responder, {module :: atom()
                      ,function :: atom()
                      ,payload :: any()
                      }).
-type kz_responder() :: #kz_responder{}.
-type kz_responders() :: [kz_responder()].

-record(kz_binding, {binding :: ne_binary() | '_'
                    ,binding_parts :: ne_binaries() | '_'
                    ,binding_responders = queue:new() :: queue:queue() | '_'
                                                         %% queue -> [#kz_responder{}]
                    ,binding_prefix :: api_binary() | '$1' | '_'
                    }).
-type kz_binding() :: #kz_binding{}.
-type kz_bindings() :: [kz_binding()].

-type kz_rt_options() :: kz_proplist().
-type kz_rt_option() :: 'candidates' | 'matches'.

-record(state, {bindings = [] :: kz_bindings()
               ,has_ets = false :: boolean()
               }).
-type state() :: #state{}.

-export_type([kz_responder/0
             ,kz_responders/0
             ,kz_binding/0
             ,kz_bindings/0
             ,candidates_fun/0
             ,matches_fun/0
             ,payload/0
             ,map_results/0
             ,fold_results/0
             ,kz_rt_option/0
             ,kz_rt_options/0
             ]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc
%% return [ {Result, Payload1} ], a list of tuples, the first element
%% of which is the result of the bound handler, and the second element
%% is the payload, possibly modified
%% @end
%%--------------------------------------------------------------------
-spec map(ne_binary(), payload()) -> map_results().
-spec map(ne_binary(), payload(), kz_rt_options()) -> map_results().
map(Routing, Payload) ->
    map_processor(Routing, Payload, rt_options()).

map(Routing, Payload, Options) ->
    map_processor(Routing, Payload, rt_options(Options)).

-spec pmap(ne_binary(), payload()) -> map_results().
-spec pmap(ne_binary(), payload(), kz_rt_options()) -> map_results().
pmap(Routing, Payload) ->
    pmap_processor(Routing, Payload, rt_options()).

pmap(Routing, Payload, Options) ->
    pmap_processor(Routing, Payload, rt_options(Options)).

-spec get_binding_candidates(ne_binary()) -> kz_bindings().
get_binding_candidates(Routing) ->
    case binary:split(Routing, <<".">>, ['global']) of
        [Vsn, Action | _] ->
            get_binding_candidates(Vsn, Action);
        _Segments ->
            ets:match(table_id(), '$1')
    end.

%% supports Crossbar-optimized bindings, vs selecting the whole table
-spec get_binding_candidates(ne_binary(), ne_binary()) -> kz_bindings().
get_binding_candidates(Vsn, Action) ->
    ets:select(table_id(), [{#kz_binding{binding_prefix='$1'
                                        ,_='_'
                                        }
                            ,[{'orelse'
                              ,{'=:=', '$1', <<Vsn/binary, ".", Action/binary>>}
                              ,{'orelse'
                               ,{'=:=', '$1', <<"*.", Action/binary>>}
                               ,{'orelse'
                                ,{'=:=', '$1', <<Vsn/binary, ".*">>}
                                ,{'=:=', '$1', <<"*.*">>}
                                }
                               }
                              }
                             ]
                            ,['$_']
                            }]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% return the modified Payload after it has been threaded through
%% all matching bindings
%% @end
%%--------------------------------------------------------------------
-spec fold(ne_binary(), payload()) -> fold_results().
fold(Routing, Payload) ->
    fold_processor(Routing, Payload, rt_options()).

-spec fold(ne_binary(), payload(), kz_rt_options()) -> fold_results().
fold(Routing, Payload, Options) ->
    fold_processor(Routing, Payload, rt_options(Options)).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec any(kz_proplist(), function()) -> boolean().
any(Res, F) when is_list(Res),
                 is_function(F, 1)
                 ->
    lists:any(F, Res).

-spec all(kz_proplist(), function()) -> boolean().
all(Res, F) when is_list(Res),
                 is_function(F, 1)
                 ->
    lists:all(F, Res).

-spec failed(map_results(), function()) -> map_results().
failed(Res, F) when is_list(Res),
                    is_function(F, 1)
                    ->
    [R || R <- Res, F(R)].

-spec succeeded(map_results(), function()) -> map_results().
succeeded(Res, F) when is_list(Res),
                       is_function(F, 1)
                       ->
    [R || R <- Res, F(R)].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Match routing patterns. * matches 1 slot, # 0 or more.
%% Note: matching only accepts wilcards on first argument (asymetric).
%% @end
%%
%% <<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>
%%
%%--------------------------------------------------------------------
-spec matches(ne_binaries(), ne_binaries()) -> boolean().

%% if both are empty, we made it!
matches([], []) -> 'true';
matches([<<"#">>], []) -> 'true';

matches([<<"#">>, <<"*">>], []) -> 'false';
matches([<<"#">>, <<"*">>], [<<>>]) -> 'false';
matches([<<"#">>, <<"*">>], [_]) -> 'true'; % match one item:  #.* matches foo
matches([<<"#">>, <<"*">>, K],[_, K]) -> 'true';
matches([<<"#">>], [_]) -> 'true';

matches([<<"#">>, <<"#">> | Bs], Rs) ->
    matches([<<"#">> | Bs], Rs);
matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    matches(Bs, []);           % so keep checking by stipping of the first #

%% if one runs out without a wildcard, no matchy
matches([], [_|_]) -> 'false'; % foo.*   foo
matches([_|_], []) -> 'false';
matches([_|_], [<<>>]) -> 'false';

%% * matches one segment only
matches([<<"*">> | Bs], [_|Rs]) ->
    matches(Bs, Rs); % so ignore what the routing segment is and continue

matches([<<"#">>, B | Bs], [B, B | Rs]) ->
    matches(Bs, Rs)
        orelse matches([<<"#">>|Bs], [B | Rs]);

%% # can match 0 or more segments
matches([<<"#">>, B | Bs], [B | Rs]) ->
    %% Since the segment in B could be repeated later in the Routing Key,
    %% we need to bifurcate here but we'll short circuit if this was indeed
    %% the end of the # matching
    %% see binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)

    case lists:member(B, Rs) of
        'true' ->
            matches(Bs, Rs)
                orelse matches([<<"#">> | Bs], Rs)
                orelse matches([<<"#">>, B | Bs], Rs);
        'false' ->
            matches(Bs, Rs)
    end;

matches([<<"#">>, <<"*">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs);

matches([<<"#">> | _]=Bs, [_ | Rs]) ->
    matches(Bs, Rs); % otherwise leave the # in to continue matching

%% if the segments match, continue
matches([B | Bs], [B | Rs]) ->
    matches(Bs, Rs);
%% otherwise no match
matches(_, _) -> 'false'.

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec stop() -> 'ok'.
stop() -> gen_server:cast(?SERVER, 'stop').

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result()].
-spec bind(ne_binary() | ne_binaries(), atom(), atom()) ->
                  bind_result() | bind_results().
-spec bind(ne_binary() | ne_binaries(), atom(), atom(), any()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun) ->
    [bind(Binding, Module, Fun) || Binding <- Bindings];
bind(Binding, Module, Fun) when is_binary(Binding) ->
    bind(Binding, Module, Fun, 'undefined').

bind([_|_]=Bindings, Module, Fun, Payload) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) ->
    lager:debug("adding binding ~s for ~s:~s (~p)", [Binding, Module, Fun, Payload]),
    gen_server:call(?SERVER, {'bind', Binding, Module, Fun, Payload}, 'infinity').

-type unbind_result() :: {'ok', 'deleted_binding' | 'updated_binding'} |
                         {'error', 'not_found'}.
-type unbind_results() :: [unbind_result()].

-spec unbind(ne_binary() | ne_binaries(), atom(), atom()) ->
                    unbind_result() | unbind_results().
-spec unbind(ne_binary() | ne_binaries(), atom(), atom(), any()) ->
                    unbind_result() | unbind_results().
unbind([_|_]=Bindings, Module, Fun) ->
    [unbind(Binding, Module, Fun) || Binding <- Bindings];
unbind(Binding, Module, Fun) when is_binary(Binding) ->
    unbind(Binding, Module, Fun, 'undefined').

unbind([_|_]=Bindings, Module, Fun, Payload) ->
    [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings];
unbind(Binding, Module, Fun, Payload) ->
    lager:debug("removing binding ~s for ~s:~s (~p)", [Binding, Module, Fun, Payload]),
    gen_server:call(?SERVER, {'unbind', Binding, Module, Fun, Payload}, 'infinity').

-spec flush() -> 'ok'.
flush() -> gen_server:cast(?SERVER, 'flush').

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> gen_server:cast(?SERVER, {'flush', Binding}).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(Module) -> gen_server:cast(?SERVER, {'flush_mod', Module}).

-type filter_fun() :: fun((ne_binary(), atom(), atom(), any()) -> boolean()).
-spec filter(filter_fun()) -> 'ok'.
filter(Predicate) when is_function(Predicate, 4) ->
    gen_server:cast(?SERVER, {'filter', Predicate}).

-spec modules_loaded() -> atoms().
modules_loaded() ->
    ets:foldl(fun modules_loaded_fold/2, [], table_id()).

-spec modules_loaded_fold(kz_binding(), atoms()) -> atoms().
modules_loaded_fold(#kz_binding{binding_responders=Responders}, Acc) ->
    props:unique([M
                  || #kz_responder{module=M} <- queue:to_list(Responders)
                 ])
        ++ Acc.

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() ->
    ['set', 'named_table', 'protected', {'keypos', #kz_binding.binding}].

-spec find_me_function() -> api_pid().
find_me_function() ->  whereis(?SERVER).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

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
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_util:put_callid(?LOG_SYSTEM_ID),
    lager:debug("starting bindings server"),
    {'ok', #state{}}.

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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call('is_ready', _From, #state{has_ets='true'}=State) ->
    {'reply', 'true', State};
handle_call('is_ready', _From, State) ->
    {'reply', 'false', State};
handle_call('current_bindings', _, #state{bindings=Bs}=State) ->
    {'reply', Bs, State};
handle_call({'bind', Binding, Mod, Fun, Payload}, _, #state{}=State) ->
    Resp = maybe_add_binding(Binding, Mod, Fun, Payload),
    lager:debug("maybe add binding ~s: ~p", [Binding, Resp]),
    {'reply', Resp, State};
handle_call({'unbind', Binding, Mod, Fun, Payload}, _, #state{}=State) ->
    Resp = maybe_rm_binding(Binding, Mod, Fun, Payload),
    lager:debug("maybe rm binding ~s: ~p", [Binding, Resp]),
    {'reply', Resp, State}.

-spec maybe_add_binding(ne_binary(), atom(), atom(), any()) ->
                               'ok' |
                               {'error', 'exists'}.
maybe_add_binding(Binding, Mod, Fun, Payload) ->
    Responder = #kz_responder{module=Mod
                             ,function=Fun
                             ,payload=Payload
                             },

    case ets:lookup(table_id(), Binding) of
        [] ->
            case binary:split(Binding, <<".">>, ['global']) of
                [Vsn, Action | _] = Pieces ->
                    lager:debug("adding new optimized ~s", [Binding]),
                    add_optimized_binding(Binding, Responder, Pieces, Vsn, Action);
                Pieces ->
                    lager:debug("adding new regular ~s", [Binding]),
                    add_binding(Binding, Responder, Pieces, 'undefined')
            end,
            'ok';
        [#kz_binding{binding_responders=Responders}=Bind] ->
            case queue:member(Responder, Responders) of
                'true' ->
                    lager:debug("responder exists for ~s", [Binding]),
                    {'error', 'exists'};
                'false' ->
                    Bind1 = Bind#kz_binding{binding_responders=queue:in(Responder, Responders)},
                    lager:debug("adding responder to existing binding ~s", [Binding]),
                    ets:insert(table_id(), Bind1),
                    'ok'
            end
    end.

-spec maybe_rm_binding(ne_binary(), atom(), atom(), any()) ->
                              {'ok', 'deleted_binding' | 'updated_binding'} |
                              {'error', 'not_found'}.
maybe_rm_binding(Binding, Mod, Fun, Payload) ->
    Responder = #kz_responder{module=Mod
                             ,function=Fun
                             ,payload=Payload
                             },
    case ets:lookup(table_id(), Binding) of
        [] -> {'error', 'not_found'};
        [#kz_binding{}=Bind] ->
            maybe_rm_responder(Binding, Responder, Bind)
    end.

-spec maybe_rm_responder(ne_binary(), kz_responder(), kz_binding()) ->
                                {'ok', 'deleted_binding' | 'updated_binding'} |
                                {'error', 'not_found'}.
maybe_rm_responder(Binding, Responder, #kz_binding{binding_responders=Responders}=Bind) ->
    case queue:member(Responder, Responders) of
        'false' ->
            {'error', 'not_found'};
        'true' ->
            NewResponders = queue:filter(fun(QueueResponder) -> QueueResponder =/= Responder end, Responders),
            case queue:len(NewResponders) of
                0 ->
                    ets:delete_object(table_id(), Bind),
                    {'ok', 'deleted_binding'};
                _Len ->
                    ets:update_element(table_id(), Binding, {#kz_binding.binding_responders, NewResponders}),
                    {'ok', 'updated_binding'}
            end
    end.

-spec add_optimized_binding(ne_binary(), kz_responder(), ne_binaries(), ne_binary(), ne_binary()) -> boolean().
add_optimized_binding(Binding, Responder, Pieces, Vsn, Action) ->
    Prefix = <<Vsn/binary, ".", Action/binary>>,
    add_binding(Binding, Responder, Pieces, Prefix).

-spec add_binding(ne_binary(), kz_responder(), ne_binaries(), api_binary()) -> boolean().
add_binding(Binding, Responder, Pieces, Prefix) ->
    Bind = #kz_binding{binding=Binding
                      ,binding_parts=lists:reverse(Pieces)
                      ,binding_responders=queue:in(Responder, queue:new())
                      ,binding_prefix=Prefix
                      },
    ets:insert_new(table_id(), Bind).

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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast('flush', #state{}=State) ->
    ets:delete_all_objects(table_id()),
    {'noreply', State, 'hibernate'};
handle_cast({'flush', Key}, #state{}=State) ->
    _ = ets:delete(table_id(), Key),
    {'noreply', State};
handle_cast({'flush_mod', Mod}, State) ->
    lager:debug("trying to flush ~s", [Mod]),
    ets:foldl(fun(El, _) -> flush_mod(Mod, El) end, 'ok', table_id()),
    {'noreply', State};
handle_cast({'filter', Predicate}, State) ->
    filter_bindings(Predicate),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec flush_mod(atom(), kz_binding()) -> boolean().
flush_mod(ClientMod, #kz_binding{binding=Binding
                                ,binding_responders=Responders
                                }) ->
    Filtered = queue:filter(fun(#kz_responder{module=Mod}) -> ClientMod =/= Mod end, Responders),
    case queue:len(Filtered) =:= queue:len(Responders) of
        'true' -> 'false'; %% nothing to update
        'false' ->
            case queue:len(Filtered) of
                0 ->
                    lager:debug("no more responders, removing ~s", [Binding]),
                    ets:delete(table_id(), Binding);
                _Len ->
                    lager:debug("removing mod ~s from ~s", [ClientMod, Binding]),
                    ets:update_element(table_id(), Binding, {#kz_binding.binding_responders, Filtered})
            end
    end.

-type filter_updates() :: [{ne_binary(), {pos_integer(), queue:queue()}}] | [].
-spec filter_bindings(filter_fun()) -> 'ok'.
-spec filter_bindings(filter_fun(), ne_binary() | '$end_of_table', filter_updates(), ne_binaries()) -> 'ok'.
filter_bindings(Predicate) ->
    filter_bindings(Predicate, ets:first(table_id()), [], []).

filter_bindings(_Predicate, '$end_of_table', Updates, Deletes) ->
    _ = [ets:delete(table_id(), DeleteKey) || DeleteKey <- Deletes],
    _ = [ets:update_element(table_id(), Key, Update) || {Key, Update} <- Updates],
    'ok';
filter_bindings(Predicate, Key, Updates, Deletes) ->
    [#kz_binding{binding=Binding
                ,binding_responders=Responders
                }
    ] = ets:lookup(table_id(), Key),
    NewResponders = queue:filter(fun(#kz_responder{module=M
                                                  ,function=F
                                                  ,payload=P
                                                  }) ->
                                         Predicate(Binding, M, F, P)
                                 end, Responders),
    case queue:len(NewResponders) of
        0 ->
            filter_bindings(Predicate
                           ,ets:next(table_id(), Key)
                           ,Updates
                           ,[Key | Deletes]
                           );
        _Len ->
            filter_bindings(Predicate
                           ,ets:next(table_id(), Key)
                           ,[{Key, {#kz_binding.binding_responders, NewResponders}}
                             | Updates
                            ]
                           ,Deletes
                           )
    end.

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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER',_TableId, _From, _Gift}, State) ->
    lager:debug("recv transfer of control for ~s from ~p", [_TableId, _From]),
    {'noreply', State#state{has_ets='true'}};
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _) ->
    lager:debug("bindings server terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If a binding_result uses 'eoq' for its response, the payload is
%% ignored and the subscriber is re-inserted into the queue, with the
%% previous payload being passed to the next invocation.
%% @end
%%--------------------------------------------------------------------
-spec fold_bind_results(kz_responders(), any(), ne_binary()) -> any().
fold_bind_results(_, {'error', _}=E, _) -> [E];
fold_bind_results([], Payload, _Route) -> Payload;
fold_bind_results(Responders, Payload, Route) ->
    fold_bind_results(Responders, Payload, Route, length(Responders), []).

-spec fold_bind_results(kz_responders(), any(), ne_binary(), non_neg_integer(), kz_responders()) -> any().
fold_bind_results([#kz_responder{module=M
                                ,function=F
                                ,payload='undefined'
                                }=Responder
                   | Responders]
                 ,[_|Tokens]=Payload
                 ,Route
                 ,RespondersLen
                 ,ReRunResponders) ->
    try erlang:apply(M, F, Payload) of
        'eoq' ->
            lager:debug("putting ~s to eoq", [M]),
            fold_bind_results(Responders, Payload, Route, RespondersLen, [Responder|ReRunResponders]);
        {'error', _E}=E ->
            lager:debug("error: ~p", [_E]),
            E;
        {'EXIT', {'undef', [{_M, _F, _A, _}|_]}} ->
            lager:debug("undefined function ~s:~s/~b", [M, F, length(Payload)]),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        {'EXIT', _E} ->
            lager:error("~s:~s/~p died unexpectedly: ~p", [M, F, length(Payload), _E]),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        'ok' ->
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        Pay1 ->
            fold_bind_results(Responders, [Pay1|Tokens], Route, RespondersLen, ReRunResponders)
    catch
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            log_function_clause(M, F, length(Payload), ST),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        'error':'undef' ->
            ST = erlang:get_stacktrace(),
            log_undefined(M, F, length(Payload), ST),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        _T:_E ->
            ST = erlang:get_stacktrace(),
            lager:error("excepted: ~s: ~p", [_T, _E]),
            kz_util:log_stacktrace(ST),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders)
    end;
fold_bind_results([], Payload, _Route, _RespondersLen, []) ->
    Payload;
fold_bind_results([], Payload, Route, RespondersLen, ReRunResponders) ->
    case length(ReRunResponders) of
        N when N < RespondersLen ->
            %% one or more pids weren't ready to operate on Payload, let them have another go
            fold_bind_results(lists:reverse(ReRunResponders), Payload, Route, N, []);
        RespondersLen ->
            %% If all Pids 'eoq'ed, ReRunQ will be the same queue, and Payload will be unchanged - exit the fold
            lager:error("loop detected for ~s, returning", [Route]),
            Payload
    end.

-spec log_undefined(atom(), atom(), non_neg_integer(), list()) -> 'ok'.
log_undefined(M, F, Length, [{M, F, _Args,_}|_]) ->
    lager:debug("undefined function ~s:~s/~b", [M, F, Length]);
log_undefined(M, F, Length, [{RealM, RealF, RealArgs,_}|_]) ->
    lager:debug("undefined function ~s:~s/~b", [RealM, RealF, length(RealArgs)]),
    lager:debug("in call ~s:~s/~b", [M, F, Length]);
log_undefined(M, F, Length, ST) ->
    lager:debug("undefined function ~s:~s/~b", [M, F, Length]),
    kz_util:log_stacktrace(ST).

log_function_clause(M, F, Length, [{M, F, _Args, _}|_]) ->
    lager:info("unable to find function clause for ~s:~s/~b", [M, F, Length]);
log_function_clause(M, F, Length, [{RealM, RealF, RealArgs, Where}|_ST]) ->
    lager:error("unable to find function clause for ~s:~s(~s) in ~s:~p"
               ,[RealM, RealF
                ,kz_binary:join([kz_term:to_binary(io_lib:format("~p",[A])) || A <- RealArgs], <<", ">>)
                ,props:get_value('file', Where), props:get_value('line', Where)
                ]
               ),
    lager:error("as part of ~s:~s/~p", [M, F, Length]),
    _ = [lager:error("st: ~p", [ST]) || ST <- _ST],
    'ok';
log_function_clause(M, F, Lenth, ST) ->
    lager:error("no matching function clause for ~s:~s/~p", [M, F, Lenth]),
    kz_util:log_stacktrace(ST).

-spec map_processor(ne_binary(), payload(), kz_rt_options()) -> map_results().
map_processor(Routing, Payload, Options) when not is_list(Payload) ->
    map_processor(Routing, [Payload], Options);
map_processor(Routing, Payload, Options) ->
    RoutingParts = routing_parts(Routing),
    lists:foldl(fun(Binding, Acc) ->
                        map_processor_fold(Binding, Acc, Payload, Routing, RoutingParts, Options)
                end
               ,[]
               ,kazoo_bindings_rt:candidates(Options, Routing)
               ).

-spec pmap_processor(ne_binary(), payload(), kz_rt_options()) -> map_results().
pmap_processor(Routing, Payload, Options) when not is_list(Payload) ->
    pmap_processor(Routing, [Payload], Options);
pmap_processor(Routing, Payload, Options) ->
    RoutingParts = routing_parts(Routing),
    lists:foldl(fun(Binding, Acc) ->
                        pmap_processor_fold(Binding, Acc, Payload, Routing, RoutingParts, Options)
                end
               ,[]
               ,kazoo_bindings_rt:candidates(Options, Routing)
               ).

-spec map_processor_fold(kz_binding(), map_results(), payload(), ne_binary(), ne_binaries(), kz_rt_options()) -> map_results().
map_processor_fold(#kz_binding{binding=Binding
                              ,binding_responders=Responders
                              }
                  ,Acc
                  ,Payload
                  ,Binding
                  ,_RoutingParts
                  ,_Options
                  ) ->
    lager:debug("exact match for ~s", [Binding]),
    map_responders(Acc, Responders, Payload);
map_processor_fold(#kz_binding{binding_parts=BParts
                              ,binding_responders=Responders
                              }
                  ,Acc
                  ,Payload
                  ,_Routing
                  ,RoutingParts
                  ,Options
                  ) ->
    case kazoo_bindings_rt:matches(Options, BParts, RoutingParts) of
        'false' -> Acc;
        'true' ->
            lager:debug("matched ~p to ~p", [BParts, RoutingParts]),
            map_responders(Acc, Responders, Payload)
    end.

-spec pmap_processor_fold(kz_binding(), map_results(), payload(), ne_binary(), ne_binaries(), kz_rt_options()) -> map_results().
pmap_processor_fold(#kz_binding{binding=Binding
                               ,binding_responders=Responders
                               }
                   ,Acc
                   ,Payload
                   ,Binding
                   ,_RoutingParts
                   ,_Options
                   ) ->
    lager:debug("exact match for ~s", [Binding]),
    map_responders(Acc, Responders, Payload);
pmap_processor_fold(#kz_binding{binding_parts=BParts
                               ,binding_responders=Responders
                               }
                   ,Acc
                   ,Payload
                   ,_Routing
                   ,RoutingParts
                   ,Options
                   ) ->
    case kazoo_bindings_rt:matches(Options, BParts, RoutingParts) of
        'false' -> Acc;
        'true' ->
            lager:debug("matched ~p to ~p", [BParts, RoutingParts]),
            pmap_responders(Acc, Responders, Payload)
    end.

-spec map_responders(map_results(), queue:queue(), payload()) -> map_results().
map_responders(Acc, Responders, Payload) ->
    [apply_map_responder(Responder, Payload)
     || Responder <- queue:to_list(Responders)
    ]
        ++ Acc.

-spec pmap_responders(map_results(), queue:queue(), payload()) -> map_results().
pmap_responders(Acc, Responders, Payload) ->
    plists:map(fun(R) -> apply_map_responder(R, Payload) end
              ,queue:to_list(Responders)
              ,[{'processes', 'schedulers'}]
              )
        ++ Acc.

-spec apply_map_responder(kz_responder(), payload()) -> any().
apply_map_responder(#kz_responder{module=M
                                 ,function=F
                                 ,payload=ResponderPayload
                                 }, MapPayload) ->
    Payload = maybe_merge_payload(ResponderPayload, MapPayload),
    try erlang:apply(M, F, Payload)
    catch
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            maybe_log_function_clause(M, F, Payload, ST),
            {'EXIT', {'function_clause', ST}};
        'error':'undef' ->
            ST = erlang:get_stacktrace(),
            maybe_log_undefined(M, F, Payload, ST),
            {'EXIT', {'undef', ST}};
        'error':Exp ->
            ST = erlang:get_stacktrace(),
            lager:error("exception: error:~p", [Exp]),
            kz_util:log_stacktrace(ST),
            {'EXIT', {Exp, ST}};
        _Type:Exp ->
            ST = erlang:get_stacktrace(),
            lager:error("exception: ~s:~p", [_Type, Exp]),
            kz_util:log_stacktrace(ST),
            {'EXIT', Exp}
    end.

-spec maybe_merge_payload(payload(), payload()) -> payload().
maybe_merge_payload('undefined', MapPayload) -> MapPayload;
maybe_merge_payload(ResponderPayload, MapPayload) -> [ResponderPayload|MapPayload].

-spec maybe_log_undefined(atom(), atom(), list(), list()) -> 'ok'.
maybe_log_undefined(M, F, Payload, [{M, F, Arity, _}|_])
  when is_integer(Arity),
       length(Payload) == Arity -> 'ok';
maybe_log_undefined(M, F, Payload, [{M, F, Payload, _}|_]) -> 'ok';
maybe_log_undefined(M, F, Payload, ST) ->
    log_undefined(M, F, length(Payload), ST).

-spec maybe_log_function_clause(atom(), atom(), list(), list()) -> 'ok'.
maybe_log_function_clause(M, F, Payload, [{M, F, Arity, _}|_])
  when is_integer(Arity),
       length(Payload) == Arity -> 'ok';
maybe_log_function_clause(M, F, Payload, [{M, F, Payload, _}|_]) -> 'ok';
maybe_log_function_clause(M, F, Payload, ST) ->
    log_function_clause(M, F, length(Payload), ST).

-spec fold_processor(ne_binary(), payload(), kz_rt_options()) -> fold_results().
fold_processor(Routing, Payload, Options) when not is_list(Payload) ->
    fold_processor(Routing, [Payload], Options);
fold_processor(Routing, Payload, Options) ->
    RoutingParts = routing_parts(Routing),
    Candidates = kazoo_bindings_rt:candidates(Options, Routing),

    [Reply|_] =
        lists:foldl(fun(#kz_binding{binding=Binding
                                   ,binding_parts=BParts
                                   ,binding_responders=Responders
                                   }
                       ,Acc
                       ) ->
                            case Binding =:= Routing
                                orelse kazoo_bindings_rt:matches(Options, BParts, RoutingParts)
                            of
                                'true' ->
                                    lager:debug("folding ~p to ~p", [Routing, Binding]),
                                    fold_bind_results(queue:to_list(Responders), Acc, Routing);
                                'false' -> Acc
                            end
                    end
                   ,Payload
                   ,Candidates
                   ),
    Reply.

-spec candidates(ne_binary()) -> kz_bindings().
candidates(Routing) ->
    get_binding_candidates(Routing).

-spec bindings() -> kz_bindings().
bindings() ->
    bindings(<<"#">>, rt_options()).

-spec bindings(ne_binary()) -> kz_bindings().
bindings(Routing) ->
    bindings(Routing, rt_options()).

-spec bindings(ne_binary(), kz_rt_options()) -> kz_bindings().
bindings(Routing, Opts) ->
    Options = rt_options(Opts),
    RoutingParts = routing_parts(Routing),
    ets:foldr(fun(#kz_binding{binding=Binding
                             ,binding_parts=BParts
                             }=Bind, Acc) ->
                      case Binding =:= Routing
                          orelse kazoo_bindings_rt:matches(Options, BParts, RoutingParts)
                      of
                          'true' -> [Bind | Acc];
                          'false' -> Acc
                      end
              end
             ,[]
             ,table_id()
             ).

-spec routing_parts(ne_binary()) -> ne_binaries().
routing_parts(Routing) ->
    lists:reverse(binary:split(Routing, <<".">>, ['global'])).

-spec rt_options() -> kz_rt_options().
rt_options() ->
    [{'candidates', fun get_binding_candidates/1}
    ,{'matches', fun matches/2}
    ].

-spec rt_options(kz_rt_options()) -> kz_rt_options().
rt_options(Options) ->
    props:insert_values(rt_options(), Options).

-spec is_ready() -> boolean().
is_ready() ->
    gen_server:call(?SERVER, 'is_ready').
