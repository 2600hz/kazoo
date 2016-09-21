-ifndef(KZ_JSON_HRL).

-include_lib("kazoo/include/kz_types.hrl").

%% How do we wrap proplists to denote they're json objects?
%% -define(JSON_WRAPPER(Proplist), {struct, Proplist}).
%% -define(IS_JSON_GUARD(Obj), is_tuple(Obj)
%%         andalso element(1, Obj) =:= 'struct'
%%         andalso is_list(element(2, Obj))
%%        ).

-define(JSON_WRAPPER(Proplist), {Proplist}).

-define(EMPTY_JSON_OBJECT, ?JSON_WRAPPER([])).

-type object() :: ?JSON_WRAPPER(json_proplist()) | ?EMPTY_JSON_OBJECT.
-type objects() :: [object()].

-type json_string() :: ne_binary() | atom().
-type json_strings() :: [json_string()].
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].

-type key() :: json_string() | pos_integer().
-type keys() :: [key()].
-type path() :: keys() | key().
-type paths() :: [path()].

-type json_proplist_key() :: path().
-type json_proplist_kv(K, V) :: [{K, V}].
-type json_proplist_k(K) :: json_proplist_kv(K, json_term()).
-type json_proplist() :: json_proplist_kv(json_proplist_key(), json_term()).
-type json_proplists() :: [json_proplist(),...].

-type json_iolist() :: {'json', iolist()}.
-type json_term() :: boolean()
                   | json_string() | <<>>
                   | json_iolist()
                   | json_number()
                   | json_array()
                   | object()
                   | kz_date().
-type json_terms() :: [json_term()].
-type api_json_term() :: json_term() | 'undefined'.

-define(KZ_JSON_HRL, 'true').
-endif.
