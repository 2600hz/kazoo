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
-type json_term() :: non_null_json_term() | 'null'.

-type flat_json_term() :: boolean()
                        | json_string() | <<>>
                        | json_number()
                        | json_array().

-type api_json_term() :: json_term() | 'undefined'.
-type json_terms() :: [json_term()].

-type json_array()  :: json_terms().
-type json_string() :: kz_term:ne_binary() | atom().
-type json_number() :: integer() | float().

-type object() :: ?JSON_WRAPPER(json_proplist()) | ?EMPTY_JSON_OBJECT.
-type objects() :: [object()].

-type flat_proplist() :: [{keys(), flat_json_term()}].
-type flat_object() :: ?JSON_WRAPPER(flat_proplist()).
-type flat_objects() :: [flat_object()].

-type key() :: json_string().
-type keys() :: [key(),...].
-type path() :: keys() | key() | pos_integer() | [pos_integer()].
-type paths() :: [path()].

-type json_proplist() :: [{key(), json_term()}] | [].
-type json_proplists() :: [json_proplist()].

-type encode_option() :: 'uescape'
                       | 'pretty'
                       | 'force_utf8'
                       | 'escape_forward_slashes'
                       | {'bytes_per_iter', non_neg_integer()}
                       | {'bytes_per_red', non_neg_integer()}.

-type encode_options() :: [encode_option()].

-define(KAZOO_JSON_HRL, 'true').
-endif.
