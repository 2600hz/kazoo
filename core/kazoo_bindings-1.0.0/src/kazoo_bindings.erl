%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2014, 2600Hz INC
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
         ,map/2
         ,fold/2
         ,flush/0, flush/1, flush_mod/1
         ,filter/1
         ,stop/0
         ,modules_loaded/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/2
         ,all/2
         ,succeeded/2
         ,failed/2
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

-ifdef(TEST).
%% PropEr needs to be included before eunit. Both modules create a ?LET macro,
%% but the PropEr one is the useful one. Also needs to be included before any
%% function definition because it includes functions.
-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").
-endif.

%% {<<"foo.bar.#">>, [<<"foo">>, <<"bar">>, <<"#">>], queue(), <<"foo.bar">>}

-type payload() :: term().

-record(kz_responder, {module :: atom()
                       ,function :: atom()
                       ,payload :: term()
                      }).
-type kz_responder() :: #kz_responder{}.

-record(kz_binding, {binding :: ne_binary()
                     ,binding_parts :: ne_binaries()
                     ,binding_responders = queue:new() :: queue()
                      %% queue -> [#kz_responder{}]
                     ,binding_prefix :: api_binary()
                    }).
-type kz_binding() :: #kz_binding{}.
-type kz_bindings() :: [kz_binding(),...] | [].

-record(state, {bindings = [] :: kz_bindings()}).

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
-type map_results() :: list().
-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    map_processor(Routing, Payload, get_binding_candidates(Routing)).

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
-type fold_results() :: payload().
-spec fold(ne_binary(), payload()) -> fold_results().
fold(Routing, Payload) ->
    fold_processor(Routing, Payload, get_binding_candidates(Routing)).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec any(wh_proplist(), function()) -> boolean().
any(Res, F) when is_list(Res),
                 is_function(F, 1)
                 ->
    lists:any(F, Res).

-spec all(wh_proplist(), function()) -> boolean().
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
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, 'stop').

-type bind_result() :: 'ok' |
                       {'error', 'exists'}.
-type bind_results() :: [bind_result(),...] | [].
-spec bind(ne_binary() | ne_binaries(), atom(), atom()) ->
                  bind_result() | bind_results().
bind([_|_]=Bindings, Module, Fun) ->
    [bind(Binding, Module, Fun) || Binding <- Bindings];
bind(Binding, Module, Fun) when is_binary(Binding) ->
    bind(Binding, Module, Fun, 'undefined').

bind([_|_]=Bindings, Module, Fun, Payload) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) ->
    lager:debug("adding binding ~s for ~s:~s (~p)", [Binding, Module, Fun, Payload]),
    gen_server:call(?MODULE, {'bind', Binding, Module, Fun, Payload}, 'infinity').

-type unbind_result() :: {'ok', 'deleted_binding' | 'updated_binding'} |
                         {'error', 'not_found'}.
-type unbind_results() :: [unbind_result(),...] | [].

-spec unbind(ne_binary() | ne_binaries(), atom(), atom()) ->
                    unbind_result() | unbind_results().
-spec unbind(ne_binary() | ne_binaries(), atom(), atom(), term()) ->
                    unbind_result() | unbind_results().
unbind([_|_]=Bindings, Module, Fun) ->
    [unbind(Binding, Module, Fun) || Binding <- Bindings];
unbind(Binding, Module, Fun) when is_binary(Binding) ->
    unbind(Binding, Module, Fun, 'undefined').

unbind([_|_]=Bindings, Module, Fun, Payload) ->
    [unbind(Binding, Module, Fun, Payload) || Binding <- Bindings];
unbind(Binding, Module, Fun, Payload) ->
    lager:debug("removing binding ~s for ~s:~s (~p)", [Binding, Module, Fun, Payload]),
    gen_server:call(?MODULE, {'unbind', Binding, Module, Fun, Payload}, 'infinity').


-spec flush() -> 'ok'.
flush() -> gen_server:cast(?MODULE, 'flush').

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> gen_server:cast(?MODULE, {'flush', Binding}).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(Module) -> gen_server:cast(?MODULE, {'flush_mod', Module}).

-type filter_fun() :: fun((ne_binary(), atom(), atom(), term()) -> boolean()).
-spec filter(filter_fun()) -> 'ok'.
filter(Predicate) when is_function(Predicate, 4) ->
    gen_server:cast(?MODULE, {'filter', Predicate}).

-spec modules_loaded() -> atoms().
modules_loaded() ->
    ets:foldl(fun(#kz_binding{binding_responders=Responders}, Acc) ->
                      props:unique([M || #kz_responder{module=M} <- queue:to_list(Responders)]) ++ Acc
              end, [], table_id()).

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> list().
table_options() ->
    ['set', 'named_table', 'protected', {'keypos', #kz_binding.binding}].

-spec find_me_function() -> api_pid().
find_me_function() ->  whereis(?MODULE).

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
init([]) ->
    put('callid', ?LOG_SYSTEM_ID),

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

-spec maybe_add_binding(ne_binary(), atom(), atom(), term()) -> 'ok' |
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

-spec maybe_rm_binding(ne_binary(), atom(), atom(), term()) ->
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
handle_cast('flush', #state{}=State) ->
    ets:delete_all_objects(table_id()),
    {'noreply', State, 'hibernate'};
handle_cast({'flush', Binding}, #state{}=State) ->
    ets:delete(table_id(), Binding),
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
            lager:debug("removing mod ~s from ~s", [ClientMod, Binding]),
            ets:update_element(table_id(), Binding, {3, Filtered})
    end.

-type filter_updates() :: [{ne_binary(), {pos_integer(), queue()}}] | [].
-spec filter_bindings(filter_fun()) -> 'ok'.
-spec filter_bindings(filter_fun(), ne_binary() | '$end_of_table', filter_updates(), ne_binaries()) -> 'ok'.
filter_bindings(Predicate) ->
    filter_bindings(Predicate, ets:first(table_id()), [], []).

filter_bindings(_Predicate, '$end_of_table', Updates, Deletes) ->
    [ets:delete(table_id(), DeleteKey) || DeleteKey <- Deletes],
    [ets:update_element(table_id(), Key, Update) || {Key, Update} <- Updates],
    'ok';
filter_bindings(Predicate, Key, Updates, Deletes) ->
    [#kz_binding{binding=Binding
                 ,binding_responders=Responders
                }] = ets:lookup(table_id(), Key),
    NewResponders = queue:filter(fun(#kz_responder{module=M
                                                   ,function=F
                                                   ,payload=P
                                                  }) ->
                                         Predicate(Binding, M, F, P)
                                 end, Responders),
    case queue:len(NewResponders) of
        0 -> filter_bindings(Predicate, ets:next(table_id(), Key), Updates, [Key | Deletes]);
        _Len -> filter_bindings(Predicate, ets:next(table_id(), Key), [{Key, {#kz_binding.binding_responders, NewResponders}} | Updates], Deletes)
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
handle_info({'ETS-TRANSFER',_TableId, _From, _Gift}, State) ->
    lager:debug("recv transfer of control for ~s from ~p", [_TableId, _From]),
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
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
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

matches([<<"#">> | Bs], []) -> % sadly, #.# would match foo, foo.bar, foo.bar.baz, etc
    matches(Bs, []);           % so keep checking by stipping of the first #

%% if one runs out without a wildcard, no matchy
matches([], [_|_]) -> 'false'; % foo.*   foo
matches([_|_], []) -> 'false';
matches([_|_], [<<>>]) -> 'false';

%% * matches one segment only
matches([<<"*">> | Bs], [_|Rs]) ->
    matches(Bs, Rs); % so ignore what the routing segment is and continue

%% # can match 0 or more segments
matches([<<"#">>, B | Bs], [B | Rs]) ->
    %% Since the segment in B could be repeated later in the Routing Key, we need to bifurcate here
    %% but we'll short circuit if this was indeed the end of the # matching
    %% see binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)

    case lists:member(B, Rs) of
        'true' ->
            matches(Bs, Rs) orelse matches([<<"#">> | Bs], Rs);
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
%% @private
%% @doc
%% If a binding_result uses 'eoq' for its response, the payload is
%% ignored and the subscriber is re-inserted into the queue, with the
%% previous payload being passed to the next invocation.
%% @end
%%--------------------------------------------------------------------
-spec fold_bind_results(queue() | kz_bindings(), term(), ne_binary()) -> term().
fold_bind_results(_, {'error', _}=E, _) -> [E];
fold_bind_results(Responders, Payload, Route) when is_list(Responders) ->
    fold_bind_results(Responders, Payload, Route, length(Responders), []);
fold_bind_results(Responders, Payload, Route) ->
    fold_bind_results(queue:to_list(Responders), Payload, Route, queue:len(Responders), []).

-spec fold_bind_results(kz_bindings(), term(), ne_binary(), non_neg_integer(), kz_bindings()) -> term().
fold_bind_results([#kz_responder{module=M
                                ,function=F
                                ,payload='undefined'
                                }=Responder
                   | Responders], [_|Tokens]=Payload, Route, RespondersLen, ReRunResponders) ->
    try apply_responder(Responder, Payload) of
        'eoq' ->
            lager:debug("putting ~s to eoq", [M]),
            fold_bind_results(Responders, Payload, Route, RespondersLen, [Responder|ReRunResponders]);
        {'error', _E}=E ->
            lager:debug("error: ~p", [_E]),
            E;
        {'EXIT', {'undef', [{_M, _F, _A, _}|_]}} ->
            ST = erlang:get_stacktrace(),
            log_undefined(M, F, length(Payload), ST),
            fold_bind_results(Responders, Payload, Route, RespondersLen, ReRunResponders);
        {'EXIT', _E} ->
            ST = erlang:get_stacktrace(),
            lager:debug("~s:~s/~p died unexpectedly: ~p", [M, F, length(Payload), _E]),
            wh_util:log_stacktrace(ST),
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
            lager:debug("excepted: ~s: ~p", [_T, _E]),
            wh_util:log_stacktrace(ST),
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
            lager:debug("loop detected for ~s, returning", [Route]),
            Payload
    end.

-spec apply_responder(kz_responder(), term()) -> term().
apply_responder(#kz_responder{module=M
                             ,function=F
                             ,payload='undefined'
                             }, Payload) ->
    apply(M, F, Payload);
apply_responder(#kz_responder{module=M
                             ,function=F
                             ,payload=ResponderPayload
                             }, Payload) ->
    apply(M, F, [ResponderPayload | Payload]).

-spec log_undefined(atom(), atom(), non_neg_integer(), list()) -> 'ok'.
log_undefined(M, F, Length, [{M, F, _Args,_}|_]) ->
    lager:debug("undefined function ~s:~s/~b", [M, F, Length]);
log_undefined(M, F, Length, [{RealM, RealF, RealArgs,_}|_]) ->
    lager:debug("undefined function ~s:~s/~b", [RealM, RealF, length(RealArgs)]),
    lager:debug("in call ~s:~s/~b", [M, F, Length]);
log_undefined(M, F, Length, ST) ->
    lager:debug("undefined function ~s:~s/~b", [M, F, Length]),
    wh_util:log_stacktrace(ST).

log_function_clause(M, F, Length, [{M, F, _Args, _}|_]) ->
    lager:debug("unable to find function clause for ~s:~s/~b", [M, F, Length]);
log_function_clause(M, F, Length, [{RealM, RealF, RealArgs, Where}|_]) ->
    lager:debug("unable to find function clause for ~s:~s(~p) in ~s:~p"
                ,[RealM, RealF, RealArgs, props:get_value('file', Where), props:get_value('line', Where)]
               ),
    lager:debug("as part of ~s:~s/~p", [M, F, Length]);
log_function_clause(M, F, Lenth, ST) ->
    lager:debug("no matching function clause for ~s:~s/~p", [M, F, Lenth]),
    wh_util:log_stacktrace(ST).

-spec map_processor(ne_binary(), payload(), kz_bindings()) -> any().
map_processor(Routing, Payload, Bindings) when not is_list(Payload) ->
    map_processor(Routing, [Payload], Bindings);
map_processor(Routing, Payload, Bindings) ->
    RoutingParts = lists:reverse(binary:split(Routing, <<".">>, ['global'])),
    Map = fun(Responder) ->
                  apply_responder(Responder, Payload)
          end,

    lists:foldl(fun(#kz_binding{binding=Binding
                                ,binding_responders=Responders
                               }, Acc) when Binding =:= Routing ->
                        lager:debug("exact match for ~s", [Routing]),
                        [catch Map(Responder) || Responder <- queue:to_list(Responders)] ++ Acc;
                   (#kz_binding{binding_parts=BParts
                                ,binding_responders=Responders
                               }, Acc) ->
                        case matches(BParts, RoutingParts) of
                            'true' ->
                                lager:debug("matched ~p to ~p", [BParts, RoutingParts]),
                                [catch Map(Responder) || Responder <- queue:to_list(Responders)] ++ Acc;
                            'false' -> Acc
                        end
                end, [], Bindings).

-spec fold_processor(ne_binary(), payload(), kz_bindings()) -> fold_results().
fold_processor(Routing, Payload, Bindings) when not is_list(Payload) ->
    fold_processor(Routing, [Payload], Bindings);
fold_processor(Routing, Payload, Bindings) ->
    RoutingParts = lists:reverse(binary:split(Routing, <<".">>, ['global'])),

    [Reply|_] = lists:foldl(
                  fun(#kz_binding{binding=Binding
                                 ,binding_parts=BParts
                                 ,binding_responders=Responders
                                 }, Acc) ->
                          case Binding =:= Routing orelse matches(BParts, RoutingParts) of
                              'true' ->
                                  lager:debug("routing ~s matches ~s", [Routing, Binding]),
                                  fold_bind_results(Responders, Acc, Routing);
                              'false' -> Acc
                          end
                  end, Payload, Bindings),
    Reply.

%% EUNIT and PropEr TESTING %%
-ifdef(TEST).
-spec binding_matches(ne_binary(), ne_binary()) -> boolean().
binding_matches(B, R) when erlang:byte_size(B) > 0 andalso erlang:byte_size(R) > 0 ->
    matches(lists:reverse(binary:split(B, <<".">>, ['global']))
            ,lists:reverse(binary:split(R, <<".">>, ['global']))).

-define(ROUTINGS, [ <<"foo.bar.zot">>, <<"foo.quux.zot">>, <<"foo.bar.quux.zot">>, <<"foo.zot">>, <<"foo">>, <<"xap">>]).

-define(BINDINGS, [
                   {<<"#">>, [true, true, true, true, true, true]}
                   ,{<<"foo.*.zot">>, [true, true, false, false, false, false]}
                   ,{<<"foo.#.zot">>, [true, true, true, true, false, false]}
                   ,{<<"*.bar.#">>, [true, false, true, false, false, false]}
                   ,{<<"*">>, [false, false, false, false, true, true]}
                   ,{<<"#.tow">>, [false, false, false, false, false, false]}
                   ,{<<"#.quux.zot">>, [false, true, true, false, false, false]}
                   ,{<<"xap.#">>, [false, false, false, false, false, true]}
                   ,{<<"#.*">>, [true, true, true, true, true, true]}
                   ,{<<"#.bar.*">>, [true, false, false, false, false, false]}
                  ]).

bindings_match_test() ->
    lists:foreach(fun({B, _}=Expected) ->
                          Actual = lists:foldr(fun(R, Acc) -> [binding_matches(B, R) | Acc] end, [], ?ROUTINGS),
                          ?assertEqual(Expected, {B, Actual})
                  end, ?BINDINGS).

weird_bindings_test() ->
    ?assertEqual(true, binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)),
    ?assertEqual(true, binding_matches(<<"#.*">>, <<"foo">>)),
    ?assertEqual(true, binding_matches(<<"#.*">>, <<"foo.bar">>)),
    ?assertEqual(false, binding_matches(<<"foo.#.*">>, <<"foo">>)),
    %% ?assertEqual(false, binding_matches(<<"#.*">>, <<>>)),
    ?assertEqual(true, binding_matches(<<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>)),
    ok.

%%% PropEr tests
%% Checks that the patterns for paths (a.#.*.c) match or do not
%% match a given expanded path.
prop_expands() ->
    ?FORALL(Paths, expanded_paths(),
            ?WHENFAIL(io:format("Failed on ~p~n",[Paths]),
                      lists:all(fun(X) -> X end, %% checks if all true
                                [binding_matches(Pattern, Expanded) =:= Expected ||
                                    {Pattern, Expanded, Expected} <- Paths])
                     )).

%%% GENERATORS

%% Gives a list of paths that were expanded, some of them to fail on purpose,
%% some of them not to.
expanded_paths() ->
    ?LET(P, path(),
         begin
             B = list_to_binary(P),
             ?LET({{Expanded1, IsRight1},{Expanded2, IsRight2}},
                  {wrong(P), right(P)},
                  [{B, list_to_binary(Expanded1), IsRight1},
                   {B, list_to_binary(Expanded2), IsRight2}])
         end).

%% Tries to make a pattern wrong. Will not always suceed because a pattern
%% like "#" can be anything at all.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong(Path) ->
    ?LET(P, Path, wrong(P, true, [])).

%% Will expand the patterns according to the rules so they should always match
%%
%% Returns {Str, ShouldMatchOriginal}.
right(Path) ->
    ?LET(P, Path, {right1(P), true}).

%% Here's why some patterns will always succeed even if we try to make them
%% wrong. In a given strign S, we could add segments, but some subpatterns
%% would have a chance to fix the problem we created. See a.*.#, which means
%% 'at least two segments'  but still matches (albeit wrongly) a.b if we drop
%% a section of the text, replace it by one, or add two of them. It can
%% technically be done, but we would need a strong lookahead for that.
%% This is especially the case of .#., which we will have to simply ignore.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong([], Bool, Acc) ->
    {lists:reverse(Acc), Bool};
wrong("*.#." ++ Rest, Bool, Acc) -> %% the # messes stuff up, can't invalidate
    wrong(Rest, Bool, Acc);
wrong("*.#", Bool, Acc) ->  %% same as above, end of string
    {lists:reverse(Acc), Bool};
wrong("*." ++ Rest, _Bool, Acc) ->
    wrong(Rest, false, Acc);
wrong(".*", _Bool, Acc) ->
    {lists:reverse(Acc), false};
wrong(".#." ++ Rest, Bool, Acc) -> %% can't make this one wrong
    wrong(Rest, Bool, [$.|Acc]);
wrong("#." ++ Rest, Bool, Acc) -> %% same, start of string
    wrong(Rest, Bool, Acc);
wrong(".#", Bool, Acc) -> %% same as above, end of string
    {lists:reverse(Acc), Bool};
wrong([Char|Rest], Bool, Acc) when Char =/= $*, Char =/= $# ->
    wrong(Rest, Bool, [Char|Acc]).

%% Returns an expanded string according to the rules
right1([]) -> [];
right1("*" ++ Rest) ->
    ?LET(S, segment(), S++right1(Rest));
right1(".#" ++ Rest) ->
    ?LET(X,
         union([
                "",
                ?LAZY(?LET(S, segment(), [$.]++S)),
                ?LAZY(?LET({A,B}, {segment(), segment()}, [$.]++A++[$.]++B)),
                ?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, [$.]++A++[$.]++B++[$.]++C))
               ]),
         X ++ right1(Rest));
right1("#." ++ Rest) ->
    ?LET(X,
         union([
                "",
                ?LAZY(?LET(S, segment(), S++[$.])),
                ?LAZY(?LET({A,B}, {segment(), segment()}, A++[$.]++B++[$.])),
                ?LAZY(?LET({A,B,C}, {segment(), segment(), segment()}, A++[$.]++B++[$.]++C++[$.]))
               ]),
         X ++ right1(Rest));
right1([Char|Rest]) ->
    [Char|right1(Rest)].

%% Building a basic pattern/path string
path() ->
    ?LET(Base, ?LAZY(weighted_union([{3,a()}, {1,b()}])),
         ?LET({H,T}, {union(["*.","#.",""]), union([".*",".#",""])},
              H ++ Base ++ T)).

a() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), markers()]))},
         X ++ [$.] ++ Y).

b() ->
    ?LET({X,Y}, {segment(), ?LAZY(union([b(), c()]))},
         X ++ [$.] ++ Y).

c() ->
    segment().

segment() ->
    ?SUCHTHAT(
       X,
       list(union([choose($a,$z), choose($A,$Z), choose($0,$9)])),
       length(X) =/= 0
      ).

markers() ->
    ?LET(S, ?LAZY(union([[$#, $., c()], [$*, $., b()]])), lists:flatten(S)).

-endif.
