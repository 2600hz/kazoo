%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Store routing keys/pid bindings. When a binding is fired,
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
%%%
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_bindings_tests).

-ifdef(PROPER).
-export([expanded_paths/0]).

- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% EUNIT and PropEr TESTING %%
-spec binding_matches(kz_term:ne_binary(), binary()) -> boolean().
binding_matches(B, R) ->
    BRev = lists:reverse(binary:split(B, <<".">>, ['global'])),
    RRev = lists:reverse(binary:split(R, <<".">>, ['global'])),

    kazoo_bindings:matches(BRev, RRev).

-define(ROUTINGS, [<<"foo.bar.zot">>
                  ,<<"foo.quux.zot">>
                  ,<<"foo.bar.quux.zot">>
                  ,<<"foo.zot">>
                  ,<<"foo">>
                  ,<<"xap">>
                  ]).

-define(BINDINGS, [{<<"#">>, ['true', 'true', 'true', 'true', 'true', 'true']}
                  ,{<<"foo.*.zot">>, ['true', 'true', 'false', 'false', 'false', 'false']}
                  ,{<<"foo.#.zot">>, ['true', 'true', 'true', 'true', 'false', 'false']}
                  ,{<<"*.bar.#">>, ['true', 'false', 'true', 'false', 'false', 'false']}
                  ,{<<"*">>, ['false', 'false', 'false', 'false', 'true', 'true']}
                  ,{<<"#.tow">>, ['false', 'false', 'false', 'false', 'false', 'false']}
                  ,{<<"#.quux.zot">>, ['false', 'true', 'true', 'false', 'false', 'false']}
                  ,{<<"xap.#">>, ['false', 'false', 'false', 'false', 'false', 'true']}
                  ,{<<"#.*">>, ['true', 'true', 'true', 'true', 'true', 'true']}
                  ,{<<"#.bar.*">>, ['true', 'false', 'false', 'false', 'false', 'false']}
                  ]).

bindings_match_test_() ->
    lists:map(fun({B, _}=Expected) ->
                      Actual = lists:foldr(fun(R, Acc) -> [binding_matches(B, R) | Acc] end, [], ?ROUTINGS),
                      ?_assertEqual(Expected, {B, Actual})
              end
             ,?BINDINGS
             ).

%% Left commented out because this was really useful for stepping through
%% individual tests and I want to keep it here for reference in the future
%% dbg_test() ->
%%     dbg:start(),

%%     dbg:tracer(),

%%     dbg:tpl(kazoo_bindings, [{'_', [], [$_]}]),
%%     dbg:p(all, c),


%%     Result = binding_matches(<<"W0.*.m.m.#">>, <<"W0.m.m.m.5">>),


%%     dbg:stop_clear(),
%%     dbg:stop(),
%%     ?assertEqual('true', Result).

weird_bindings_test_() ->
    [?_assertEqual('true', binding_matches(<<"#.A.*">>, <<"A.a.A.a">>))
    ,?_assertEqual('true', binding_matches(<<"#.*">>, <<"foo">>))
    ,?_assertEqual('true', binding_matches(<<"#.*">>, <<"foo.bar">>))
    ,?_assertEqual('false', binding_matches(<<"foo.#.*">>, <<"foo">>))
    ,?_assertEqual('false', binding_matches(<<"#.*">>, <<>>))
    ,?_assertEqual('true', binding_matches(<<"#.6.*.1.4.*">>, <<"6.a.a.6.a.1.4.a">>))
    ,?_assertEqual('true', binding_matches(<<"*.u.*.7.7.#">>, <<"i.u.e.7.7.7.a">>))
    ,?_assertEqual('true', binding_matches(<<"#.c.#.c.#">>, <<"c.c">>))
    ,?_assertEqual('true', binding_matches(<<"#.Z.*.9.0.#">>, <<"1.Z.7.9.0.9.a.0">>))
    ,?_assertEqual('true', binding_matches(<<"W0.*.m.m.#">>, <<"W0.m.m.m.5">>))
    ].

%%% PropEr tests
%% Checks that the patterns for paths (a.#.*.c) match or do not
%% match a given expanded path.
-ifdef(PROPER).

expands_test_() ->
    {"Running PropEr tests"
    ,{'timeout'
     ,50000
     ,?_assertEqual('true', proper:quickcheck(prop_expands(), [{'numtests', 10000}]))
     }
    }.

prop_expands() ->
    ?FORALL(Paths
           ,expanded_paths(),
            ?WHENFAIL(io:format("Failed on ~p~n", [Paths])
                     ,lists:all(fun kz_term:identity/1,
                                [binding_matches(Pattern, Expanded) =:= Expected
                                 || {Pattern, Expanded, Expected} <- Paths
                                ])
                     )
           ).

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
                   {B, list_to_binary(Expanded2), IsRight2}
                  ])
         end).

%% Tries to make a pattern wrong. Will not always succeed because a pattern
%% like "#" can be anything at all.
%%
%% Returns {Str, ShouldMatchOriginal}.
wrong(Path) ->
    ?LET(P, Path, wrong(P, 'true', [])).

%% Will expand the patterns according to the rules so they should always match
%%
%% Returns {Str, ShouldMatchOriginal}.
right(Path) ->
    ?LET(P, Path, {right1(P), 'true'}).

%% Here's why some patterns will always succeed even if we try to make them
%% wrong. In a given string S, we could add segments, but some subpatterns
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
    wrong(Rest, 'false', Acc);
wrong(".*", _Bool, Acc) ->
    {lists:reverse(Acc), 'false'};
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
