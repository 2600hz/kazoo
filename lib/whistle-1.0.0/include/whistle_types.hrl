-ifndef(WHISTLE_TYPES_INCLUDED).

-type proplist() :: list(tuple(binary() | atom(), term())) | [].

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

-define(WHISTLE_TYPES_INCLUDED, true).
-endif.
