-ifndef(WH_JSON_HRL).

-define(EMPTY_JSON_OBJECT, {'struct', []}).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("proper/include/proper.hrl").

-define(IS_JSON_GUARD(Obj), is_tuple(Obj)
        andalso element(1, Obj) =:= 'struct'
        andalso is_list(element(2, Obj))
       ).

-type json_object() :: {'struct', json_proplist()}.
-type json_objects() :: [json_object()].

-type json_string() :: ne_binary() | atom().
-type json_strings() :: [json_string()].
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].

-type json_proplist_kv(K, V) :: [{K, V},...] | [].
-type json_proplist_k(K) :: json_proplist_kv(K, json_term()).
-type json_proplist() :: json_proplist_kv(json_string(), json_term()).

-type json_iolist() :: {'json', iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist() | <<>>.
-type json_terms() :: [json_term()].

-define(WH_JSON_HRL, true).
-endif.

