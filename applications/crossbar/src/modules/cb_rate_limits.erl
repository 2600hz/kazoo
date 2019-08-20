%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc SBC Rate limits
%%% /accounts/{account_id}/rate_limits - manip account's access lists
%%% /accounts/{account_id}/devices/{device_id}/rate_limits - manip device's access lists
%%%
%%%
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_rate_limits).

-export([init/0
        ,allowed_methods/0
        ,authorize/1
        ,resource_exists/0
        ,validate/1
        ,post/1
        ,delete/1
        ]).

-include("crossbar.hrl").

-define(LISTING_BY_OWNER, <<"rate_limits/list_by_owner">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the bindings this module will respond to.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rate_limits">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rate_limits">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.validate.rate_limits">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rate_limits">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.rate_limits">>, ?MODULE, 'delete').

%%------------------------------------------------------------------------------
%% @doc Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    case thing_type_id(Context) of
        'undefined' -> 'false';
        _ ->
            {'ok', MasterAccount} = kapps_util:get_master_account_id(),
            AuthAccountId = cb_context:auth_account_id(Context),
            AuthAccountId =:= MasterAccount
                orelse kz_services_reseller:is_reseller(AuthAccountId)
    end.

-type thing_type() :: kz_term:ne_binary().
-type thing_id() :: kz_term:ne_binary().
-type thing_description() :: {thing_type(), thing_id()}.
-spec thing_type_id(cb_context:context()) -> thing_description() | 'undefined'.
thing_type_id(Context) ->
    case cb_context:req_nouns(Context) of
        [{<<"rate_limits">>, []}, {<<"accounts">> = Type, [Id]} | _] -> {Type, Id};
        [{<<"rate_limits">>, []}, {<<"devices">> = Type, [Id]} | _] -> {Type, Id};
        _ReqNouns -> 'undefined'
    end.

-spec thing_type(cb_context:context()) -> thing_type() | 'undefined'.
thing_type(Context) ->
    case thing_type_id(Context) of
        {Type, _} -> Type;
        'undefined' -> 'undefined'
    end.

-spec thing_id(cb_context:context()) -> thing_id() | 'undefined'.
thing_id(Context) ->
    case thing_type_id(Context) of
        {_, Id} -> Id;
        'undefined' -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Does the path point to a valid resource.
%% For example:
%%
%% ```
%%    /rate_limits => []
%%    /rate_limits/foo => [<<"foo">>]
%%    /rate_limits/foo/bar => [<<"foo">>, <<"bar">>]
%% '''
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /rate_limits might load a list of metaflow objects
%% /rate_limits/123 might load the metaflow object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_rate_limits(Context, cb_context:req_verb(Context)).

-spec validate_rate_limits(cb_context:context(), http_method()) -> cb_context:context().
validate_rate_limits(Context, ?HTTP_GET) ->
    validate_get_rate_limits(Context, thing_id(Context));
validate_rate_limits(Context, ?HTTP_POST) ->
    validate_post_rate_limits(Context);
validate_rate_limits(Context, ?HTTP_DELETE) ->
    validate_delete_rate_limits(Context, thing_id(Context)).

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

-spec get_rate_limits_id_for_thing(cb_context:context(), kz_term:ne_binary()) -> kz_term:api_binaries().
get_rate_limits_id_for_thing(Context, ThingId) ->
    ViewOpt = [{'key', ThingId}],
    case kz_datamgr:get_results(cb_context:account_db(Context), ?LISTING_BY_OWNER, ViewOpt) of
        {'ok', JObjs} ->
            [get_id(JB) || JB <- JObjs];
        {'error', _Err} ->
            lager:error("can't load rate limits due to err: ~p", [_Err]),
            'undefined'
    end.

-spec get_id(kz_json:object()) -> kz_term:api_binary().
get_id(JObj) ->
    kz_doc:id(JObj).

-spec validate_get_rate_limits(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_get_rate_limits(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_get_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> crossbar_doc:handle_json_success(kz_json:new(), Context);
        [RateLimitsId] -> crossbar_doc:load(RateLimitsId, Context, ?TYPE_CHECK_OPTION(<<"rate_limits">>));
        RateLimitsIds ->
            lager:error("found more than one result, please check ids(from db ~s): ~p"
                       ,[cb_context:account_db(Context), RateLimitsIds]
                       ),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec validate_delete_rate_limits(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_delete_rate_limits(Context, 'undefined') ->
    crossbar_util:response_faulty_request(Context);
validate_delete_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> crossbar_doc:handle_json_success(kz_json:new(), Context);
        [RateLimitsId] -> crossbar_doc:load(RateLimitsId, Context, ?TYPE_CHECK_OPTION(<<"rate_limits">>));
        RateLimitsIds ->
            lager:error("found more than one result, please check ids(from db ~s): ~p"
                       ,[cb_context:account_db(Context), RateLimitsIds]
                       ),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec validate_set_rate_limits(cb_context:context()) ->
                                      cb_context:context().
validate_set_rate_limits(Context) ->
    lager:debug("rate limits data is valid, setting on thing"),
    validate_set_rate_limits(Context, thing_id(Context)).

-spec validate_set_rate_limits(cb_context:context(), kz_term:api_binary()) ->
                                      cb_context:context().
validate_set_rate_limits(Context, 'undefined') ->
    lager:debug("no thing found"),
    crossbar_util:response_faulty_request(Context);
validate_set_rate_limits(Context, ThingId) ->
    case get_rate_limits_id_for_thing(Context, ThingId) of
        'undefined' -> crossbar_util:response('fatal', <<"data collection error">>, 503, Context);
        [] -> Context;
        [RateLimitsId] -> crossbar_doc:load_merge(RateLimitsId, Context, ?TYPE_CHECK_OPTION(<<"rate_limits">>));
        RateLimitsIds ->
            lager:error("found more than one result, please check ids(from db ~s): ~p"
                       ,[cb_context:account_db(Context), RateLimitsIds]
                       ),
            crossbar_util:response('fatal', <<"data collection error">>, 503, Context)
    end.

-spec set_pvt_fields(cb_context:context()) -> cb_context:context().
set_pvt_fields(Context) ->
    ThingId = thing_id(Context),
    {'ok', JObj} = kz_datamgr:open_cache_doc(cb_context:account_db(Context), ThingId),
    ThingType = kz_doc:type(JObj),

    Props = [{<<"pvt_type">>, <<"rate_limits">>}
            ,{<<"pvt_owner_id">>, ThingId}
            ,{<<"pvt_owner_type">>, ThingType}
            ,{<<"pvt_queryname">>, query_name(ThingType, JObj)}
            ],
    cb_context:set_doc(Context, kz_json:set_values(Props, cb_context:doc(Context))).

-spec query_name(kz_term:ne_binary(), kz_json:object()) -> kz_term:api_binary().
query_name(<<"account">>, JObj) ->
    kzd_accounts:realm(JObj);
query_name(<<"device">>, JObj) ->
    kzd_devices:sip_username(JObj).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is POST, execute the actual action, usually a db save.
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    crossbar_doc:save(set_pvt_fields(Context)).

%%------------------------------------------------------------------------------
%% @doc If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%------------------------------------------------------------------------------
-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    crossbar_doc:delete(Context).
