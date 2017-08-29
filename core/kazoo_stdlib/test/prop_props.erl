%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(prop_props).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_set_value() ->
    ?FORALL({KV, Before, After}
           ,{test_property(), test_proplist(), test_proplist()}
           ,?WHENFAIL(?debugFmt("failed: props:is_defined(~p, ~p ++ props:set_value(~p, ~p)).~n", [KV, Before, KV, After])
                     ,props:is_defined(KV, Before ++ props:set_value(KV, After))
                     )
           ).

prop_set_values() ->
    ?FORALL({KVs, Before, After}
           ,{unique_proplist(), test_proplist(), test_proplist()}
           ,?WHENFAIL(?debugFmt("Props = ~p ++ props:set_values(~p, ~p)~n", [Before, KVs, After])
                     ,begin
                          Props = Before ++ props:set_values(KVs, After),
                          lists:all(fun(KV) -> props:is_defined(KV, Props) end
                                   ,KVs
                                   )
                      end
                     )
           ).

prop_get_value() ->
    ?FORALL({Props, KV}
           ,test_proplist_and_kv()
           ,begin
                K = case is_tuple(KV) of 'true' -> element(1, KV); 'false' -> KV end,
                V = case is_tuple(KV) of 'true' -> element(2, KV); 'false' -> 'true' end,
                ?WHENFAIL(?debugFmt("~p = props:get_value(~p, ~p).~n"
                                   ,[V, K, Props]
                                   )
                         ,V =:= props:get_value(K, Props)
                         )
            end
           ).

prop_is_defined() ->
    ?FORALL({Props, Existing, NonExisting}
           ,test_proplist_and_keys()
           ,?WHENFAIL(?debugFmt("exists props:is_defined(~p, ~p)~nnot props:is_defined(~p, ~p)~n"
                               ,[Existing, Props, NonExisting, Props]
                               )
                     ,props:is_defined(Existing, Props)
                      andalso 'false' =:= props:is_defined(NonExisting, Props)
                     )
           ).

test_proplist() ->
    list(test_property()).

test_property() ->
    oneof([test_key()
          ,{test_key(), test_value()}
          ]).

%% TODO: generate recursive proplists and key paths to test get/set on nested proplists
test_value() -> any().

test_key() ->
    oneof([atom(), binary()]).

test_proplist_and_kv() ->
    ?LET(Props
        ,?SUCHTHAT(UniqueProps
                  ,?LET(GenProps, non_empty(test_proplist()), props:unique(GenProps))
                  ,is_list(UniqueProps)
                  )
        ,{Props, elements(Props)}
        ).

unique_proplist() ->
    ?LET(GenProps, non_empty(test_proplist()), props:unique(GenProps)).

test_proplist_and_keys() ->
    ?LET(Props
        ,?SUCHTHAT(UniqueProps
                  ,?LET(GenProps, non_empty(test_proplist()), props:unique(GenProps))
                  ,is_list(UniqueProps)
                  )
        ,{Props, element_of(Props), not_oneof(Props)}
        ).

element_of(Props) ->
    element_of(Props, rand:uniform()).

element_of([K], _) -> as_key(K);
element_of([K|_], Rand) when Rand < 0.5 -> as_key(K);
element_of([_|Rest], _) -> element_of(Rest, rand:uniform()).

as_key(A) when is_atom(A) -> A;
as_key({K, _}) -> K.

not_oneof(Props) ->
    ?LET(K
        ,test_key()
        ,(not lists:member(K, Props))
         andalso ('false' =:= lists:keyfind(K, 1, Props))
        ).

-endif.
