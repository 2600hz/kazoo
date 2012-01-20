-ifndef(WH_JSON_HRL).

-define(EMPTY_JSON_OBJECT, {'struct', []}).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("proper/include/proper.hrl").

-type json_string() :: atom() | ne_binary().
-type json_strings() :: [json_string()].
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_proplist() :: [{json_string(), json_term()}].
-type json_iolist() :: {'json', iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist() | <<>>.
-type json_terms() :: [json_term()].

-opaque json_object() :: {'struct', json_proplist()}.
-type json_objects() :: [json_object()].

-define(WH_JSON_HRL, true).
-endif.
