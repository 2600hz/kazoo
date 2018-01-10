%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Devices module
%%%
%%% Handle client requests for device documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_quickcall).

-export([init/0
        ,allowed_methods/1
        ,resource_exists/1
        ,validate/2
        ,post/2
        ]).

-include("crossbar.hrl").

-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"v2_resource.allowed_methods.quickcall">>, 'allowed_methods'}
               ,{<<"v2_resource.resource_exists.quickcall">>, 'resource_exists'}
               ,{<<"v2_resource.validate.quickcall">>, 'validate'}
               ,{<<"v2_resource.execute.post.quickcall">>, 'post'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_Number) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_Number) -> 'true'.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, _Number) ->
    validate_quickcall(Context, cb_context:req_nouns(Context)).

validate_quickcall(Context, ?DEVICES_QCALL_NOUNS(DeviceId, _Number)) ->
    validate_quickcall(load_endpoint(Context, DeviceId, kz_device:type()));
validate_quickcall(Context, ?USERS_QCALL_NOUNS(UserId, _Number)) ->
    validate_quickcall(load_endpoint(Context, UserId, kzd_user:type())).

-spec validate_quickcall(cb_context:context()) -> cb_context:context().
validate_quickcall(Context) ->
    Context1 = crossbar_util:maybe_validate_quickcall(Context),
    case cb_context:has_errors(Context1) of
        'true' ->
            lager:info("validation of quickcall failed"),
            Context1;
        'false' -> maybe_originate(Context1, cb_context:req_verb(Context))
    end.

-spec maybe_originate(cb_context:context(), http_method()) -> cb_context:context().
maybe_originate(Context, ?HTTP_GET) ->
    cb_modules_util:maybe_originate_quickcall(Context);
maybe_originate(Context, ?HTTP_POST) ->
    Context.

-spec load_endpoint(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
load_endpoint(Context, EndpointId, EndpointType) ->
    crossbar_doc:load(EndpointId, Context, ?TYPE_CHECK_OPTION(EndpointType)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _Number) ->
    cb_modules_util:maybe_originate_quickcall(Context).
