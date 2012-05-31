-ifndef(WH_JSON_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("proper/include/proper.hrl").

%% How do we wrap proplists to denote they're json objects?
%% -define(JSON_WRAPPER(Proplist), {struct, Proplist}).
%% -define(IS_JSON_GUARD(Obj), is_tuple(Obj)
%%         andalso element(1, Obj) =:= 'struct'
%%         andalso is_list(element(2, Obj))
%%        ).

-define(JSON_WRAPPER(Proplist), {Proplist}).
-define(IS_JSON_GUARD(Obj), is_tuple(Obj)
       andalso is_list(element(1, Obj))
      ).

-define(EMPTY_JSON_OBJECT, ?JSON_WRAPPER([])).

-type json_object() :: ?JSON_WRAPPER(json_proplist()).
-type json_objects() :: [json_object()].

-type json_string() :: ne_binary() | atom() | pos_integer().
-type json_strings() :: [json_string()].
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].

-type json_proplist_key() :: json_string() | json_strings().
-type json_proplist_kv(K, V) :: [{K, V},...] | [].
-type json_proplist_k(K) :: json_proplist_kv(K, json_term()).
-type json_proplist() :: json_proplist_kv(json_proplist_key(), json_term()).

-type json_iolist() :: {'json', iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist() | <<>>.
-type json_terms() :: [json_term()].

-define(WH_JSON_HRL, true).
-endif.

