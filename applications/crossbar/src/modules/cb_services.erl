%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_services).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,content_types_provided/2
        ,validate/1, validate/2
        ,get/1, get/2
        ,post/1, post/2
        ]).

-include("crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".services">>).

-define(CB_LIST, <<"services/crossbar_listing">>).
-define(AUDIT_LOG_LIST, <<"services/audit_logs_by_creation">>).

-define(PATH_PLAN, <<"plan">>).
-define(PATH_AUDIT, <<"audit">>).
-define(PATH_STATUS, <<"status">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.services">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.services">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.services">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.services">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.services">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.services">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.services">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.services">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?PATH_PLAN) ->
    [?HTTP_GET];
allowed_methods(?PATH_AUDIT) ->
    [?HTTP_GET];
allowed_methods(?PATH_STATUS) ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /services => []
%%    /services/foo => [<<"foo">>]
%%    /services/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> boolean().
resource_exists(?PATH_PLAN) -> 'true';
resource_exists(?PATH_AUDIT) -> 'true';
resource_exists(?PATH_STATUS) -> 'true';
resource_exists(_) -> 'false'.

-spec content_types_provided(cb_context:context(), path_token()) -> cb_context:context().
content_types_provided(Context, ?PATH_AUDIT) ->
    cb_context:add_content_types_provided(Context
                                         ,[{'to_json', ?JSON_CONTENT_TYPES}
                                          ,{'to_csv', ?CSV_CONTENT_TYPES}
                                          ]).

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /services mights load a list of service objects
%% /services/123 might load the service object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_services(Context, cb_context:req_verb(Context)).

validate_services(Context, ?HTTP_GET) -> load_services(Context);
validate_services(Context, ?HTTP_POST) ->
    BillingId = kz_json:get_value(<<"billing_id">>, cb_context:req_data(Context)),
    try kz_services:set_billing_id(BillingId, cb_context:account_id(Context)) of
        'undefined' ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, 'undefined'}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        ServicesRec ->
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, kz_services:public_json(ServicesRec)}
                               ,{fun cb_context:store/3, 'services', ServicesRec}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ])
    catch
        'throw':{Error, Reason} ->
            R = kz_json:set_value([<<"billing_id">>, <<"invalid">>], Reason, kz_json:new()),
            crossbar_util:response('error', kz_term:to_binary(Error), 400, R, Context)
    end.

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?PATH_PLAN) ->
    crossbar_util:response(kz_services:service_plan_json(cb_context:account_id(Context)), Context);
validate(Context, ?PATH_AUDIT) ->
    load_audit_logs(Context);
validate(Context0, ?PATH_STATUS) ->
    Context1 = load_services(Context0),
    case cb_context:resp_status(Context1) of
        'success' -> create_status_payload(Context1);
        _Else -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%------------------------------------------------------------------------------

-spec get(cb_context:context()) -> cb_context:context().
get(Context) ->
    Context.

-spec get(cb_context:context(), path_token()) -> cb_context:context().
get(Context, ?PATH_PLAN) ->
    Context;
get(Context, ?PATH_AUDIT) ->
    Context;
get(Context, ?PATH_STATUS) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    maybe_save_services(Context, cb_context:fetch(Context, 'services')).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?PATH_STATUS) ->
    maybe_update_status(Context).

-spec maybe_update_status(cb_context:context()) -> cb_context:context().
maybe_update_status(Context) ->
    case cb_context:is_superduper_admin(cb_context:auth_account_id(Context)) of
        'false' -> cb_context:add_system_error('forbidden', Context);
        'true' -> update_status(Context)
    end.

-spec update_status(cb_context:context()) -> cb_context:context().
update_status(Context) ->
    ReqData = cb_context:req_data(Context),
    Routines = [fun update_good_standing/2
               ,fun maybe_update_reason/2
               ,fun maybe_update_reason_code/2
               ],
    Update = lists:foldl(fun(F, J) -> F(J, ReqData) end
                        ,kz_services:to_json(cb_context:fetch(Context, 'services'))
                        ,Routines
                        ),
    Services = kz_services:save(kz_services:from_service_json(Update, 'false')),
    create_status_payload(
      cb_context:setters(Context
                        ,[{fun cb_context:set_doc/2, kz_services:public_json(Services)}
                         ,{fun cb_context:store/3, 'services', Services}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                         ])
     ).

-spec update_good_standing(kz_json:object(), kz_json:object()) -> kz_json:object().
update_good_standing(JObj, ReqData) ->
    case kz_json:get_ne_binary_value(<<"in_good_standing">>, ReqData) of
        <<"true">> -> kzd_services:set_status(JObj, kzd_services:status_good());
        <<"false">> -> kzd_services:set_status(JObj, kzd_services:status_delinquent())
    end.

-spec maybe_update_reason(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_update_reason(JObj, ReqData) ->
    case kz_json:get_ne_binary_value(<<"reason">>, ReqData) of
        'undefined' -> JObj;
        Reason -> kzd_services:set_reason(JObj, Reason)
    end.

-spec maybe_update_reason_code(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_update_reason_code(JObj, ReqData) ->
    case kz_json:get_number_value(<<"reason_code">>, ReqData) of
        'undefined' -> JObj;
        ReasonCore -> kzd_services:set_reason_code(JObj, ReasonCore)
    end.

maybe_save_services(Context, 'undefined') -> Context;
maybe_save_services(Context, Services) ->
    try kz_services:save(Services) of
        NewServices ->
            crossbar_util:response(kz_services:public_json(NewServices), Context)
    catch
        'throw':{Error, Reason} ->
            crossbar_util:response('error', kz_term:to_binary(Error), 500, Reason, Context)
    end.

-spec load_audit_logs(cb_context:context()) -> cb_context:context().
load_audit_logs(Context) ->
    Options = [{'mapper', crossbar_view:map_doc_fun()}
              ,'include_docs'
              ],
    crossbar_view:load_modb(Context, ?AUDIT_LOG_LIST, Options).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_services(cb_context:context()) -> cb_context:context().
load_services(Context) ->
    Services = kz_services:fetch(cb_context:account_id(Context)),
    cb_context:setters(Context
                      ,[{fun cb_context:set_doc/2, kz_services:public_json(Services)}
                       ,{fun cb_context:store/3, 'services', Services}
                       ,{fun cb_context:set_resp_data/2, kz_services:public_json(Services)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_status_payload(cb_context:context()) -> cb_context:context().
create_status_payload(Context) ->
    JObj = kz_services:to_json(cb_context:fetch(Context, 'services')),
    Status = kzd_services:status(JObj) =:= kzd_services:status_good(),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, create_status_payload(Status, JObj)}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

-spec create_status_payload(boolean(), kz_json:object()) -> kz_json:object().
create_status_payload('true', _JObj) ->
    kz_json:from_list([{<<"in_good_standing">>, 'true'}
                      ]);
create_status_payload('false', JObj) ->
    kz_json:from_list([{<<"in_good_standing">>, 'false'}
                      ,{<<"reason">>, kzd_services:reason(JObj)}
                      ,{<<"reason_code">>, kzd_services:reason_code(JObj)}
                      ]).
