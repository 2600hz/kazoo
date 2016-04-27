%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
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
-module(kazoo_bindings_test).

-ifdef(PROPER).
- include_lib("proper/include/proper.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").
-include_lib("whistle/include/wh_types.hrl").

%% EUNIT and PropEr TESTING %%
-spec binding_matches(ne_binary(), ne_binary()) -> boolean().
binding_matches(B, R) when erlang:byte_size(B) > 0 andalso erlang:byte_size(R) > 0 ->
    kazoo_bindings:matches(
      lists:reverse(binary:split(B, <<".">>, ['global']))
      ,lists:reverse(binary:split(R, <<".">>, ['global']))
     ).

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

bindings_match_test() ->
    lists:foreach(fun({B, _}=Expected) ->
                          Actual = lists:foldr(fun(R, Acc) -> [binding_matches(B, R) | Acc] end, [], ?ROUTINGS),
                          ?assertEqual(Expected, {B, Actual})
                  end, ?BINDINGS).

weird_bindings_test() ->
    ?assertEqual('true', binding_matches(<<"#.A.*">>,<<"A.a.A.a">>)),
    ?assertEqual('true', binding_matches(<<"#.*">>, <<"foo">>)),
    ?assertEqual('true', binding_matches(<<"#.*">>, <<"foo.bar">>)),
    ?assertEqual('false', binding_matches(<<"foo.#.*">>, <<"foo">>)),
    %% ?assertEqual('false', binding_matches(<<"#.*">>, <<>>)),
    ?assertEqual('true', binding_matches(<<"#.6.*.1.4.*">>,<<"6.a.a.6.a.1.4.a">>)),
    ok.

%%% PropEr tests
%% Checks that the patterns for paths (a.#.*.c) match or do not
%% match a given expanded path.
-ifdef(PROPER).

expands_test_() ->
    {"Running PropEr tests"
     ,{'timeout'
       ,50000
       ,?_assertEqual('true'
                      ,proper:quickcheck(?MODULE:prop_expands()
                                         ,[{'numtests', 10000}]
                                        )
                     )
      }
    }.

prop_expands() ->
    ?FORALL(Paths
            ,expanded_paths(),
            ?WHENFAIL(io:format("Failed on ~p~n", [Paths])
                      ,lists:all(fun wh_util:identity/1,
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
