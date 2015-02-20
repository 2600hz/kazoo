%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Rate limits
%%% /accounts/{account_id}/rate_limits - manip account's access lists
%%% /accounts/{account_id}/devices/{device_id}/rate_limits - manip device's access lists
%%%
%%% @end
%%% @contributors:
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cb_rate_limits).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate/1
         ,post/1
         ,delete/1
        ]).

-include("../crossbar.hrl").

-define(LISTING_BY_OWNER, <<"rate_limits/list_by_owner">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rate_limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rate_limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.rate_limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rate_limits">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.rate_limits">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /rate_limits => []
%%    /rate_limits/foo => [<<"foo">>]
%%    /rate_limits/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec only_if_reseller_or_master(cb_context:context()
                                 ,fun((cb_context:context()) -> cb_context:context())
                                ) -> cb_context:context().
only_if_reseller_or_master(Context, Continue) ->
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    case AuthAccountId =:= MasterAccount
         orelse wh_services:is_reseller(AuthAccountId) of
        'true' -> Continue(Context);
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /rate_limits mights load a list of metaflow objects
%% /rate_limits/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_rate_limits(Context, cb_context:req_verb(Context)).

-spec validate_rate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_rate_limits(Context, ?HTTP_GET) ->
    validate_get_rate_limits(Context, thing_id(Context));
validate_rate_limits(Context, ?HTTP_POST) ->
    only_if_reseller_or_master(Context, fun validate_post_rate_limits/1);
validate_rate_limits(Context, ?HTTP_DELETE) ->
    only_if_reseller_or_master(Context, fun (C) -> validate_delete_rate_limits(C, thing_id(C)) end).

-spec validate_post_rate_limits(cb_context:context()) -> cb_context:context().
validate_post_rate_limits(Context) ->
    case thing_type(Context) of
        <<"accounts">> ->
            cb_context:validate_request_data(<<"account_rate_limits">>, Context, fun validate_set_rate_limits/1);
        <<"devices">> ->
            cb_context:validate_request_data(<<"device_rate_limits">>, Context, fun validate_set_rate_limits/1);
        _Else ->
            crossbar_util:response_faulty_request(Context)
    end.

-spec thing_type(cb_context:context()) -> api_binary().
thing_type(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"rate_limits">>, []}, {Type, [_ThingId]} | _] ->
            Type;
        _Nouns -> 'undefined'
    end.

-spec thing_id(cb_context:context()) -> api_binary().
thing_id(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"rate_limits">>, []}, {<<"accounts">>, [AccountId]} | _] ->
            AccountId;
        [{<<"rate_limits">>, []}, {<<"devices">>, [DeviceId]} | _] ->
            DeviceId;
        _Nouns -> 'undefined'
    end.

-spec get_rate_limits_id_for_thing(cb_context:context(), ne_binary()) -> api_binaries().
get_rate_limits_id_for_thing(Context, ThingId) ->
    ViewOpt = [{'key', ThingId}],
    case couch_mgr:get_results(cb_context:account_db(Context), ?LISTING_BY_OWNER, ViewOpt) of
        {'ok', JObjs} ->
            lists:map(fun get_value/1, JObjs);
        {'error', _Err} ->
            lager:error("Can't load rate limits due to err: ~p", [_Err]),
            'undefined'
    end.

-spec get_value(wh_json:object()) -> api_binary().
get_value(JObj) ->
    wh_json:get_value(<<"value">>, JObj).

-spec validate_get_rate_limits(cb_context:context(), api_binary()) -> cb_context:context().
validate_get_rate_limits(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_get_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> crossbar_doc:handle_json_success(wh_json:new(), Context);
        [RateLimitsId] -> crossbar_doc:load(RateLimitsId, Context);
        RateLimitsIds ->
            AccountDb = cb_context:account_db(Context),
            lager:error("Found more than one result, please check ids(from db ~s): ~p ", [AccountDb, RateLimitsIds]),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec validate_delete_rate_limits(cb_context:context(), api_binary()) -> cb_context:context().
validate_delete_rate_limits(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_delete_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> crossbar_doc:handle_json_success(wh_json:new(), Context);
        [RateLimitsId] -> crossbar_doc:load(RateLimitsId, Context);
        RateLimitsIds ->
            AccountDb = cb_context:account_db(Context),
            lager:error("Found more than one result, please check ids(from db ~s): ~p ", [AccountDb, RateLimitsIds]),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec validate_set_rate_limits(cb_context:context()) ->
                                    cb_context:context().
-spec validate_set_rate_limits(cb_context:context(), api_binary()) ->
                                    cb_context:context().
validate_set_rate_limits(Context) ->
    lager:debug("rate limits data is valid, setting on thing"),
    validate_set_rate_limits(Context, thing_id(Context)).

validate_set_rate_limits(Context, 'undefined') ->
    lager:debug("no thing found"),
    crossbar_util:response_faulty_request(Context);
validate_set_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> Context;
        [RateLimitsId] -> crossbar_doc:load_merge(RateLimitsId, Context);
        RateLimitsIds ->
            AccountDb = cb_context:account_db(Context),
            lager:error("Found more than one result, please check ids(from db ~s): ~p ", [AccountDb, RateLimitsIds]),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec set_pvt_fields(cb_context:context()) -> cb_context:context().
set_pvt_fields(Context) ->
    AccountDb = cb_context:account_db(Context),
    ThingId = thing_id(Context),
    {'ok', JObj} = couch_mgr:open_cache_doc(AccountDb, ThingId),
    ThingType = wh_json:get_value(<<"pvt_type">>, JObj),
    QueryName = case ThingType of
                    <<"account">> -> wh_json:get_value(<<"realm">>, JObj);
                    <<"device">> -> wh_json:get_value([<<"sip">>, <<"username">>], JObj)
                end,
    Props = [{<<"pvt_type">>, <<"rate_limits">>}
             ,{<<"pvt_owner_id">>, ThingId}
             ,{<<"pvt_owner_type">>, ThingType}
             ,{<<"pvt_queryname">>, QueryName}
            ],
    cb_context:set_doc(Context, wh_json:set_values(Props, cb_context:doc(Context))).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(set_pvt_fields(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    lager:info("Loaded doc: ~p", [cb_context:doc(Context)]),
    crossbar_doc:delete(Context).
