%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
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
-module(crossbar_bindings).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,bind/3
         ,map/2
         ,fold/2
         ,flush/0, flush/1, flush_mod/1
         ,stop/0
         ,modules_loaded/0
        ]).

%% Helper Functions for Results of a map/2
-export([any/1
         ,all/1
         ,succeeded/1
         ,failed/1
        ]).

%% Internally-used functions
-export([map_processor/5
         ,fold_processor/5
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

%% {FullBinding, BindingPieces, QueueOfMods}
%% {<<"foo.bar.#">>, [<<"foo">>, <<"bar">>, <<"#">>], queue(), <<"foo.bar">>}
-type binding() :: {ne_binary(), ne_binaries(), queue(), ne_binary()}. %% queue(Module::atom() | pid())
-type bindings() :: [binding(),...] | [].

-type payload() :: path_tokens() | % mapping over path tokens in URI
                   [cb_context:context() | path_token() | 'undefined',...] |
                   cb_context:context() |
                   {cb_context:context(), wh_proplist()} | % v1_resource:rest_init/2
                   {'error', _} | % v1_util:execute_request/2
                   {wh_json:json_strings(), cb_context:context(), path_tokens()} |
                   {wh_datetime(), cowboy_req:req(), cb_context:context()} | % v1_resource:expires/2
                   {cowboy_req:req(), cb_context:context()}. % mapping over the request/context records

-record(state, {bindings = [] :: bindings()}).

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
-type map_results() :: [boolean() |
                        http_methods() |
                        {boolean() | 'halt', cb_context:context()}
                        ,...] | [].
-spec map(ne_binary(), payload()) -> map_results().
map(Routing, Payload) ->
    map_processor(Routing, Payload, get_bindings(Routing)).

get_bindings(Routing) ->
    [Vsn, Action | _] = binary:split(Routing, <<".">>, ['global']),

    ets:select(?MODULE, [{ {'_', '_', '_', '$1'}
                           ,[{'=:=', '$1', <<Vsn/binary, ".", Action/binary>>}]
                           ,['$_']
                         }]).
%% ets:match(?MODULE, ['$_']).

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
    fold_processor(Routing, Payload, get_bindings(Routing)).

%%-------------------------------------------------------------------
%% @doc
%% Helper functions for working on a result set of bindings
%% @end
%%-------------------------------------------------------------------
-spec any(wh_proplist()) -> boolean().
any(Res) when is_list(Res) -> lists:any(fun check_bool/1, Res).

-spec all(wh_proplist()) -> boolean().
all(Res) when is_list(Res) -> lists:all(fun check_bool/1, Res).

-spec failed(map_results()) -> map_results().
failed(Res) when is_list(Res) -> [R || R <- Res, filter_out_succeeded(R)].

-spec succeeded(map_results()) -> map_results().
succeeded(Res) when is_list(Res) -> [R || R <- Res, filter_out_failed(R)].

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:cast(?SERVER, 'stop').

-spec bind(ne_binary(), atom(), atom()) -> 'ok' | {'error', 'exists'}.
bind(Binding, Module, Fun) ->
    gen_server:call(?MODULE, {'bind', Binding, Module, Fun}, 'infinity').

-spec flush() -> 'ok'.
flush() -> gen_server:cast(?MODULE, 'flush').

-spec flush(ne_binary()) -> 'ok'.
flush(Binding) -> gen_server:cast(?MODULE, {'flush', Binding}).

-spec flush_mod(atom()) -> 'ok'.
flush_mod(CBMod) -> gen_server:cast(?MODULE, {'flush_mod', CBMod}).

-spec modules_loaded() -> atoms().
modules_loaded() -> gen_server:call(?MODULE, 'modules_loaded').

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
    process_flag('trap_exit', 'true'),
    put('callid', ?LOG_SYSTEM_ID),

    ets:new(?MODULE, ['named_table', 'protected']),

    lager:debug("starting bindings server"),

    spawn(fun() ->
                  put('callid', ?LOG_SYSTEM_ID),
                  [ maybe_init_mod(Mod)
                    || Mod <- whapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, [])
                  ]
          end),

    {'ok', #state{}}.

maybe_init_mod(ModBin) ->
    try (wh_util:to_atom(ModBin, 'true')):init() of
        _ -> 'ok'
    catch
        _E:_R ->
            lager:warning("failed to initialize ~s: ~p, ~p", [ModBin, _E, _R])
    end.

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
handle_call('modules_loaded', _, State) ->
    Mods = ets:foldl(fun( {_, _, MFs, _}, Acc) ->
                        [ K || {K, _} <- props:unique(queue:to_list(MFs))] ++ Acc
                       end, [], ?MODULE),
    {'reply', lists:usort(Mods), State};

handle_call('current_bindings', _, #state{bindings=Bs}=State) ->
    {'reply', Bs, State};

handle_call({'map', Routing, Payload, ReqId}, From, State) when not is_list(Payload) ->
    handle_call({'map', Routing, [Payload], ReqId}, From, State);
handle_call({'map', Routing, Payload, ReqId}, From , #state{bindings=Bs}=State) ->
    spawn(?MODULE, 'map_processor', [Routing, Payload, ReqId, From, Bs]),
    {'noreply', State};

handle_call({'fold', Routing, Payload, ReqId}, From, State) when not is_list(Payload) ->
    handle_call({'fold', Routing, [Payload], ReqId}, From, State);
handle_call({'fold', Routing, Payload, ReqId}, From, #state{bindings=Bs}=State) ->
    spawn(?MODULE, 'fold_processor', [Routing, Payload, ReqId, From, Bs]),
    {'noreply', State};

handle_call({'bind', Binding, Mod, Fun}, _, #state{}=State) ->
    MF = {Mod, Fun},

    case ets:lookup(?MODULE, Binding) of
        [] ->
            [Vsn, Action | _] = Pieces = binary:split(Binding, <<".">>, ['global']),
            Prefix = <<Vsn/binary, ".", Action/binary>>,

            Bind = {Binding, lists:reverse(Pieces), queue:in(MF, queue:new()), Prefix},

            ets:insert_new(?MODULE, Bind),
            {'reply', 'ok', State};
        [{_, _, Subscribers, Prefix}] ->
            case queue:member(MF, Subscribers) of
                'true' -> {'reply', {'error', 'exists'}, State};
                'false' ->
                    BParts = lists:reverse(binary:split(Binding, <<".">>, ['global'])),
                    Bind = {Binding, BParts, queue:in(MF, Subscribers), Prefix},

                    ets:insert(?MODULE, Bind),
                    {'reply', 'ok', State}
            end
    end.

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
    ets:delete_all_objects(?MODULE),
    {'noreply', State, 'hibernate'};
handle_cast({'flush', Binding}, #state{}=State) ->
    ets:delete(?MODULE, Binding),
    {'noreply', State};
handle_cast({'flush_mod', CBMod}, State) ->
    lager:debug("trying to flush ~s", [CBMod]),
    ets:foldl(fun(El, _) -> flush_mod(CBMod, El) end, 'ok', ?MODULE),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

flush_mod(CBMod, {Binding, _BParts, MFs, _Prefix}) ->
    Filtered = queue:filter(fun({Mod, _}) -> Mod =/= CBMod end, MFs),
    case queue:len(Filtered) =:= queue:len(MFs) of
        'true' -> 'ok'; %% nothing to update
        'false' ->
            lager:debug("removing mod ~s from ~s", [CBMod, Binding]),
            ets:update_element(?MODULE, Binding, {3, Filtered})
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
-spec fold_bind_results(queue() | [{atom(), atom()},...] | [], term(), ne_binary()) -> term().
fold_bind_results(_, {'error', _}=E, _) -> [E];
fold_bind_results(MFs, Payload, Route) when is_list(MFs) ->
    fold_bind_results(MFs, Payload, Route, length(MFs), []);
fold_bind_results(MFs, Payload, Route) ->
    fold_bind_results(queue:to_list(MFs), Payload, Route, queue:len(MFs), []).

-spec fold_bind_results([{atom(), atom()},...] | [], term(), ne_binary(), non_neg_integer(), [{atom(), atom()},...] | []) -> term().
fold_bind_results([{M,F}|MFs], [_|Tokens]=Payload, Route, MFsLen, ReRunQ) ->
    case catch apply(M, F, Payload) of
        'eoq' -> lager:debug("putting ~s to eoq", [M]), fold_bind_results(MFs, Payload, Route, MFsLen, [{M,F}|ReRunQ]);
        {'error', _E}=E -> lager:debug("error, E"), E;
        {'EXIT', _E} -> lager:debug("excepted: ignoring"), fold_bind_results(MFs, Payload, Route, MFsLen, ReRunQ);
        Pay1 ->
            fold_bind_results(MFs, [Pay1|Tokens], Route, MFsLen, ReRunQ)
    end;
fold_bind_results([], Payload, Route, MFsLen, ReRunQ) ->
    case length(ReRunQ) of
        0 -> Payload; % no one to re-run, return
        N when N < MFsLen ->
            %% one or more pids weren't ready to operate on Payload, let them have another go
            fold_bind_results(lists:reverse(ReRunQ), Payload, Route, N, []);
        MFsLen ->
            %% If all Pids 'eoq'ed, ReRunQ will be the same queue, and Payload will be unchanged - exit the fold
            lager:debug("loop detected for ~s, returning", [Route]),
            Payload
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Helpers for the result set helpers
%% @end
%%-------------------------------------------------------------------------
-spec check_bool({boolean(), term()} | boolean()) -> boolean().
check_bool({'true', _}) -> 'true';
check_bool('true') -> 'true';
check_bool(_) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_failed({boolean() | 'halt', _} | boolean() | term()) -> boolean().
filter_out_failed({'true', _}) -> 'true';
filter_out_failed('true') -> 'true';
filter_out_failed({'halt', _}) -> 'true';
filter_out_failed({'false', _}) -> 'false';
filter_out_failed('false') -> 'false';
filter_out_failed({'EXIT', _}) -> 'false';
filter_out_failed(Term) -> not wh_util:is_empty(Term).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec filter_out_succeeded({boolean() | 'halt', _} | boolean() | term()) -> boolean().
filter_out_succeeded({'true', _}) -> 'false';
filter_out_succeeded('true') -> 'false';
filter_out_succeeded({'halt', _}) -> 'true';
filter_out_succeeded({'false', _}) -> 'true';
filter_out_succeeded('false') -> 'true';
filter_out_succeeded({'EXIT', _}) -> 'true';
filter_out_succeeded(Term) -> wh_util:is_empty(Term).

-spec map_processor(ne_binary(), payload(), wh_json:json_strings()) -> any().
-spec map_processor(ne_binary(), payload(), ne_binary(), call_from(), wh_json:json_strings()) -> map_results().
map_processor(Routing, Payload, ReqId, From, Bs) ->
    put('callid', ReqId),
    Reply = map_processor(Routing, Payload, Bs),
    gen_server:reply(From, Reply).

map_processor(Routing, Payload, Bs) when not is_list(Payload) ->
    map_processor(Routing, [Payload], Bs);
map_processor(Routing, Payload, Bs) ->
    RoutingParts = lists:reverse(binary:split(Routing, <<".">>, ['global'])),
    Map = fun({Mod, Fun}) when is_atom(Mod) ->
                  apply(Mod, Fun, Payload)
          end,
    lists:foldl(fun({B, _, MFs, _Pre}, Acc) when B =:= Routing ->
                        lager:debug("exact match ~p to ~p", [B, Routing]),
                        [catch Map(MF) || MF <- queue:to_list(MFs)] ++ Acc;
                   ({_B, BParts, MFs, _Pre}, Acc) ->
                        case matches(BParts, RoutingParts) of
                            'true' ->
                                lager:debug("matched ~p to ~p", [BParts, RoutingParts]),
                                [catch Map(MF) || MF <- queue:to_list(MFs)] ++ Acc;
                            'false' -> Acc
                        end;
                   ([{B, _, MFs, _Pre}], Acc) when B =:= Routing ->
                        lager:debug("exact match ~p to ~p", [B, Routing]),
                        [catch Map(MF) || MF <- queue:to_list(MFs)] ++ Acc;
                   ([{_B, BParts, MFs, _Pre}], Acc) ->
                        case matches(BParts, RoutingParts) of
                            'true' ->
                                lager:debug("matched ~p to ~p", [BParts, RoutingParts]),
                                [catch Map(MF) || MF <- queue:to_list(MFs)] ++ Acc;
                            'false' -> Acc
                        end
                end, [], Bs).

-spec fold_processor(ne_binary(), payload(), wh_json:json_strings()) -> fold_results().
-spec fold_processor(ne_binary(), payload(), ne_binary(), call_from(), wh_json:json_strings()) -> 'ok'.
fold_processor(Routing, Payload, ReqId, From, Bs) ->
    put('callid', ReqId),
    Reply = fold_processor(Routing, Payload, Bs),
    gen_server:reply(From, Reply).

fold_processor(Routing, Payload, Bs) when not is_list(Payload) ->
    fold_processor(Routing, [Payload], Bs);
fold_processor(Routing, Payload, Bs) ->
    RoutingParts = lists:reverse(binary:split(Routing, <<".">>, ['global'])),

    [Reply|_] = lists:foldl(
                  fun({B, BParts, MFs, _Pre}, Acc) ->
                          case B =:= Routing orelse matches(BParts, RoutingParts) of
                              'true' ->
                                  lager:debug("routing ~s matches ~s", [Routing, B]),
                                  fold_bind_results(MFs, Acc, Routing);
                              'false' -> Acc
                          end;
                     ([{B, BParts, MFs, _Pre}], Acc) ->
                          case B =:= Routing orelse matches(BParts, RoutingParts) of
                              'true' ->
                                  lager:debug("routing ~s matches ~s", [Routing, B]),
                                  fold_bind_results(MFs, Acc, Routing);
                              'false' -> Acc
                          end
                  end, Payload, Bs),
    Reply.

%% EUNIT and PropEr TESTING %%
-ifdef(TEST).
-spec binding_matches(ne_binary(), ne_binary()) -> boolean().
binding_matches(B, R) when erlang:byte_size(B) > 0 andalso erlang:byte_size(R) > 0 ->
    matches(lists:reverse(binary:split(B, <<".">>, ['global']))
            ,lists:reverse(binary:split(R, <<".">>, ['global']))).


%% PropEr needs to be included before eunit. Both modules create a ?LET macro,
%% but the PropEr one is the useful one. Also needs to be included before any
%% function definition because it includes functions.
-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").

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
