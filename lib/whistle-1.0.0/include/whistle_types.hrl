-ifndef(WHISTLE_TYPES_INCLUDED).

-type proplist() :: list(tuple(binary() | atom(), term())) | [].

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

%% -type iolist() :: [char() | binary() | iolist()].
%% -type iodata() :: iolist() | binary().
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_iolist() :: {json, iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().
-type json_objects() :: [json_object()] | [].
-type mochijson() :: json_object() | json_objects() | json_term() | [].

-type date() :: tuple(non_neg_integer(), non_neg_integer(), non_neg_integer()).
-type time() :: tuple(non_neg_integer(), non_neg_integer(), non_neg_integer()).
-type datetime() :: tuple(date(), time()).
-type iso_week() :: tuple(non_neg_integer(), integer()).
-type seconds() :: integer().
-type day_of_week() :: 1 | 2 | 3 | 4 | 5 | 6 | 7.

-define(WHISTLE_TYPES_INCLUDED, true).
-endif.
