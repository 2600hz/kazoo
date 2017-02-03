-module(cb_cfg).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-include("crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cfg">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cfg">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.cfg">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET, ?HTTP_PUT].

-spec resource_exists() -> true.
resource_exists() -> true.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate(Context, cb_context:req_verb(Context)).

validate(Context, ?HTTP_GET) ->
    Path = kz_json:get_value(<<"key">>, cb_context:query_string(Context)),
    [Scope, Section, Key] = binary:split(Path, <<".">>, [global]),
    validate_read(Context, Scope, Section, Key);

validate(Context, ?HTTP_PUT) ->
    validate_access(Context, cb_context:is_superduper_admin(Context)).

validate_access(Context, false) ->
    cb_context:set_resp_status(Context, error);

validate_access(Context, true) ->
    Data = cb_context:req_data(Context),
    Path = kz_json:get_value(<<"key">>, Data),
    Value = kz_json:get_value(<<"value">>, Data),
    [Scope, Section, Key] = binary:split(Path, <<".">>, [global]),
    validate_write(Context, Scope, Section, Key, Value).

validate_read(Context, <<"system">>, Section, Key) ->
    Value = kapps_config:get(Section, Key),
    cb_context:set_resp_status(cb_context:set_resp_data(Context, Value), success);

validate_read(Context, <<"account">>, Section, Key) ->
    Value = kapps_account_config:get(cb_context:account_id(Context), Section, Key),
    cb_context:set_resp_status(cb_context:set_resp_data(Context, Value), success).

validate_write(Context, <<"system">>, Section, Key, Value) -> 
    kapps_config:set(Section, Key, Value),
    cb_context:set_resp_status(Context, success);

validate_write(Context, <<"account">>, Section, Key, Value) -> 
    kapps_account_config:set(cb_context:account_id(Context), Section, Key, Value),
    cb_context:set_resp_status(Context, success).
