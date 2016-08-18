%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @contributors:
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(cb_parked_calls).

-include("crossbar.hrl").

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate/1
        ]).

-define(MOD_CONFIG_CAT, <<"callflow.park">>).
-define(DB_DOC_NAME, kapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).

-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.parked_calls">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.parked_calls">>, ?MODULE, 'resource_exists'),
    crossbar_bindings:bind(<<"*.validate.parked_calls">>, ?MODULE, 'validate').

-spec allowed_methods() -> http_methods().
allowed_methods() -> [?HTTP_GET].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    Ctx1 = crossbar_doc:load(?DB_DOC_NAME, Context, [{'expected_type', ?DB_DOC_NAME}]),

    case cb_context:doc(Ctx1) of
        'undefined' ->
            Ctx2 = cb_context:set_resp_data(Ctx1, kz_json:new()),
            cb_context:set_resp_status(Ctx2, 'success');

        ValidDoc ->
            cb_context:set_resp_data(Ctx1, kz_json:public_fields(kz_json:normalize_jobj(ValidDoc)))
    end.
