-ifndef(KAZOO_JSON_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% How do we wrap proplists to denote they're json objects?
%% -define(JSON_WRAPPER(Proplist), {struct, Proplist}).
%% -define(IS_JSON_GUARD(Obj), is_tuple(Obj)
%%         andalso element(1, Obj) =:= 'struct'
%%         andalso is_list(element(2, Obj))
%%        ).

-define(JSON_WRAPPER(Proplist), {Proplist}).

-define(EMPTY_JSON_OBJECT, ?JSON_WRAPPER([])).

-type non_null_json_term() :: boolean()
                            | json_string() | <<>>
                            | json_number()
                            | object()
                            | json_array().
%% Denotes term definitions for JSON.

-type json_term() :: non_null_json_term() | 'null'.
%% Denotes term definitions or null object for JSON.

-type flat_json_term() :: boolean()
                        | json_string() | <<>>
                        | json_number()
                        | json_array().
%% Denotes all valid term definitions for JSON for a flatten JSON.

-type api_json_term() :: json_term() | 'undefined'.
%% Denotes all valid term definitions or `undefined' for JSON.

-type json_terms() :: [json_term()].
%% Denotes all valid term definitions for JSON.

-type json_array()  :: json_terms().
%% Denotes array in JSON.

-ifdef(PROPER).
%% PropEr will blow up the atom table if running this in an interactive shell
-type json_string() :: kz_term:ne_binary().
-else.
-type json_string() :: kz_term:ne_binary() | atom().
-endif.
%% Denotes string in JSON.

-type json_number() :: integer() | float().
%% Denotes number in JSON.

-type object() :: ?JSON_WRAPPER(json_proplist()) | ?EMPTY_JSON_OBJECT.
%% Denotes JSON object Erlang representation, {@link json_proplist} wrapped in `{}'.

-type objects() :: [object()].
%% Denotes a list of {@link object()}.

-type flat_proplist() :: [{path(), flat_json_term()}].
%% Denotes a flatten version of JSON proplist, `[{full_path, value}]'.

-type flat_object() :: ?JSON_WRAPPER(flat_proplist()).
%% Denotes a JSON of flatten version of JSON proplist, same as {@link flat_proplist()} but wrapped in `{}'.

-type flat_objects() :: [flat_object()].
%% A list of flatten JSON objects.

-type key() :: json_string().
%% Denotes a JSON key.

-type keys() :: [key()].
%% Denotes a list of JSON keys.

-type path() :: keys() | key() | pos_integer() | [pos_integer()].
%% Denotes a path to (or n-th element of) a value in a JSON.

-type paths() :: [path()].
%% Denotes a list of paths (or n-th element of) a value in a JSON.

-type json_proplist() :: [{key(), json_term()}].
%% Denotes proplist of JSON object Erlang representation.
-type json_proplists() :: [json_proplist()].

-type encode_option() :: 'uescape'
                       | 'pretty'
                       | 'force_utf8'
                       | 'escape_forward_slashes'
                       | {'bytes_per_iter', non_neg_integer()}
                       | {'bytes_per_red', non_neg_integer()}.
%% `jiffy' encode option.

-type encode_options() :: [encode_option()].
%% `jiffy' encode option.

-define(KAZOO_JSON_HRL, 'true').
-endif.
