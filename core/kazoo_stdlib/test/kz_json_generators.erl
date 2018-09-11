-module(kz_json_generators).

-export([test_object/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

test_object() ->
    ?LET(JObj, object(), remove_duplicate_keys(JObj)).

remove_duplicate_keys(?EMPTY_JSON_OBJECT) -> ?EMPTY_JSON_OBJECT;
remove_duplicate_keys(?JSON_WRAPPER(_)=JObj) ->
    kz_json:foldl(fun no_dups/3, kz_json:new(), JObj);
remove_duplicate_keys(V) -> V.

no_dups(Key, ?JSON_WRAPPER(_)=JObj, Acc) ->
    DeDuped = remove_duplicate_keys(JObj),
    kz_json:set_value(Key, DeDuped, Acc);
no_dups(Key, Value, Acc) ->
    kz_json:set_value(Key, Value, Acc).
