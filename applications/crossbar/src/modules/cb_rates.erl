%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Upload a rate deck, query rates for a given DID
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_rates).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1 ,allowed_methods/2
        ,resource_exists/0, resource_exists/1 ,resource_exists/2
        ,content_types_provided/1
        ,validate/1, validate/2, validate/3
        ,put/1
        ,post/2
        ,patch/2
        ,delete/2
        ]).

-include("crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(PVT_TYPE, <<"rate">>).
-define(NUMBER, <<"number">>).
-define(CB_LIST, <<"rates/crossbar_listing">>).
-define(RATEDECKS, <<"ratedecks">>).
-define(RATE_LOOKUP, <<"rates/lookup">>).

-define(NUMBER_RESP_FIELDS, [<<"Base-Cost">>
                            ,<<"E164-Number">>
                            ,<<"Prefix">>
                            ,<<"Rate">>
                            ,<<"Rate-ID">>
                            ,<<"Rate-Description">>
                            ,<<"Rate-Increment">>
                            ,<<"Rate-Minimum">>
                            ,<<"Rate-Name">>
                            ,<<"Ratedeck-ID">>
                            ,<<"Surcharge">>
                            ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = init_db(),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.rates">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.rates">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.rates">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.rates">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.execute.put.rates">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.rates">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.rates">>, ?MODULE, 'patch'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.rates">>, ?MODULE, 'delete'),
    'ok'.

init_db() ->
    _ = kz_datamgr:db_create(?KZ_RATES_DB),
    _ = kapps_maintenance:refresh(?KZ_RATES_DB),
    'ok'.

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

authorize(Context, [{<<"rates">>, [?RATEDECKS]}]) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> 'true';
        'false' -> {'stop', cb_context:add_system_error('forbidden', Context)}
    end;
authorize(_Context, [{<<"rates">>, [?NUMBER, _NumberToRate]}]) ->
    lager:debug("authorizing rate request for ~s", [_NumberToRate]),
    'true';
authorize(_Context, _Nouns) ->
    'false'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?RATEDECKS) ->
    [?HTTP_GET];
allowed_methods(_RateId) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods(?NUMBER, _PhoneNumber) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_RateId) -> 'true'.

-spec resource_exists(path_token(),path_token()) -> 'true'.
resource_exists(?NUMBER, _PhoneNumber) -> 'true'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    content_types_provided_by_verb(Context, cb_context:req_verb(Context)).

-spec content_types_provided_by_verb(cb_context:context(), http_method()) -> cb_context:context().
content_types_provided_by_verb(Context, ?HTTP_GET) ->
    lager:debug("adding csv ctp"),
    cb_context:add_content_types_provided(Context, [{'to_csv', ?CSV_CONTENT_TYPES}
                                                   ,{'to_json', ?JSON_CONTENT_TYPES}
                                                   ]);
content_types_provided_by_verb(Context, _Verb) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_rates(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?RATEDECKS) ->
    ratedecks_list(Context);
validate(Context, Id) ->
    validate_rate(Context, Id, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?NUMBER, Phonenumber) ->
    validate_number(Phonenumber, Context).

-spec validate_rates(cb_context:context(), http_method()) -> cb_context:context().
validate_rates(Context, ?HTTP_GET) ->
    summary(cb_context:set_account_db(Context, ratedeck_db(Context)), cb_context:req_value(Context, <<"prefix">>));
validate_rates(Context, ?HTTP_PUT) ->
    create(cb_context:set_account_db(Context, ratedeck_db(Context))).

-spec validate_rate(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_rate(Context, Id, ?HTTP_GET) ->
    read(Id, cb_context:set_account_db(Context, ratedeck_db(Context)));
validate_rate(Context, Id, ?HTTP_POST) ->
    update(Id, cb_context:set_account_db(Context, ratedeck_db(Context)));
validate_rate(Context, Id, ?HTTP_PATCH) ->
    validate_patch(Id, cb_context:set_account_db(Context, ratedeck_db(Context)));
validate_rate(Context, Id, ?HTTP_DELETE) ->
    read(Id, cb_context:set_account_db(Context, ratedeck_db(Context))).

-spec ratedeck_db(cb_context:context()) -> kz_term:ne_binary().
ratedeck_db(Context) ->
    RatedeckId = cb_context:req_value(Context, <<"ratedeck_id">>, ?KZ_RATES_DB),
    kzd_ratedeck:format_ratedeck_db(RatedeckId).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _RateId) ->
    crossbar_doc:save(Context).

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, _RateId) ->
    crossbar_doc:save(Context).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _RateId) ->
    crossbar_doc:delete(Context).

-spec validate_number(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_number(Phonenumber, Context) ->
    case knm_converters:is_reconcilable(Phonenumber) of
        'true' ->
            rate_for_number(knm_converters:normalize(Phonenumber), Context);
        'false' ->
            cb_context:add_validation_error(<<"number format">>
                                           ,<<"error">>
                                           ,kz_json:from_list([{<<"message">>, <<"Number is un-rateable">>}])
                                           ,Context
                                           )
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new instance with the data provided, if it is valid
%% @end
%%------------------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Load an instance from the database
%% @end
%%------------------------------------------------------------------------------
-spec read(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context, ?TYPE_CHECK_OPTION(<<"rate">>)).

%%------------------------------------------------------------------------------
%% @doc Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"rates">>, Context, OnSuccess).

%%------------------------------------------------------------------------------
%% @doc Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%------------------------------------------------------------------------------
-spec validate_patch(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec on_successful_validation(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    Doc = lists:foldl(fun(F, R) -> F(R) end
                     ,kzd_rates:from_json(cb_context:doc(Context))
                     ,[fun kzd_rates:set_type/1
                      ,fun ensure_routes_set/1
                      ]
                     ),
    cb_context:set_doc(Context, Doc);
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context, ?TYPE_CHECK_OPTION(<<"rate">>)).

-spec ensure_routes_set(kz_json:object()) -> kz_json:object().
ensure_routes_set(Rate) ->
    ensure_routes_set(Rate, kzd_rates:routes(Rate)).

-spec ensure_routes_set(kzd_rates:doc(), kz_term:api_ne_binaries()) -> kz_json:object().
ensure_routes_set(Rate, 'undefined') ->
    kzd_rates:set_default_route(Rate);
ensure_routes_set(Rate, []) ->
    kzd_rates:set_default_route(Rate);
ensure_routes_set(Rate, _Routes) ->
    Rate.

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec summary(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
summary(Context, 'undefined') ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2);
summary(Context, Prefix) ->
    crossbar_doc:load_view(<<"rates/lookup">>
                          ,[{'keys', kzdb_ratedeck:prefix_keys(Prefix)},'include_docs']
                          ,Context
                          ,fun normalize_rate_lookup/2
                          ).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_rate_lookup(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_rate_lookup(JObj, Acc) ->
    [kz_json:get_value(<<"doc">>, JObj)|Acc].

-spec rate_for_number(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
rate_for_number(Phonenumber, Context) ->
    Request = props:filter_undefined(
                [{<<"To-DID">>, Phonenumber}
                ,{<<"Send-Empty">>, 'true'}
                ,{<<"Msg-ID">>, cb_context:req_id(Context)}
                ,{<<"Account-ID">>, cb_context:account_id(Context)}
                ,{<<"Ratedeck-ID">>, cb_context:req_value(Context, <<"ratedeck_id">>)}
                ,{<<"Direction">>, cb_context:req_value(Context, <<"direction">>)}
                ,{<<"From-DID">>, cb_context:req_value(Context, <<"caller_id_number">>)}
                ,{<<"Resource-ID">>, cb_context:req_value(Context, <<"resource_id">>)}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]
               ),
    case kz_amqp_worker:call(Request
                            ,fun kapi_rate:publish_req/1
                            ,fun kapi_rate:resp_v/1
                            ,3 * ?MILLISECONDS_IN_SECOND
                            )
    of
        {'ok', Rate} ->
            lager:debug("found rate for ~s: ~p", [Phonenumber, Rate]),
            maybe_handle_rate(Phonenumber, Context, Rate);
        _E ->
            lager:debug("failed to query for number ~s: ~p", [Phonenumber, _E]),
            cb_context:add_system_error(<<"No rate found for this number">>, Context)
    end.

-spec maybe_handle_rate(kz_term:ne_binary(), cb_context:context(), kz_json:object()) ->
                               cb_context:context().
maybe_handle_rate(Phonenumber, Context, Rate) ->
    case kz_json:get_value(<<"Base-Cost">>, Rate) of
        'undefined' ->
            lager:debug("empty rate response for ~s", [Phonenumber]),
            cb_context:add_system_error(<<"No rate found for this number">>, Context);
        _BaseCost ->
            normalize_view(kz_json:set_value(<<"E164-Number">>, Phonenumber, Rate), Context)
    end.

-spec normalize_view(kz_json:object(),cb_context:context()) -> cb_context:context().
normalize_view(Rate, Context) ->
    crossbar_util:response(filter_view(Rate), Context).

-spec filter_view(kz_json:object()) -> kz_json:object().
filter_view(Rate) ->
    normalize_fields(
      kz_json:filter(fun filter_fields/1, kz_api:remove_defaults(Rate))
     ).

-spec filter_fields(tuple()) -> boolean().
filter_fields({K,_}) ->
    lists:member(K, ?NUMBER_RESP_FIELDS).

-spec normalize_fields(kz_json:object()) -> kz_json:object().
normalize_fields(Rate) ->
    kz_json:map(fun normalize_field/2, Rate).

-spec normalize_field(kz_json:path(), kz_json:json_term()) ->
                             {kz_json:path(), kz_json:json_term()}.
normalize_field(<<"Base-Cost">> = K, BaseCost) ->
    {K, kz_currency:units_to_dollars(BaseCost)};
normalize_field(K, V) ->
    {K, V}.

-spec ratedecks_list(cb_context:context()) -> cb_context:context().
ratedecks_list(Context) ->
    Props = [{<<"default_ratedeck">>, kapps_config:get_ne_binary(<<"hotornot">>, <<"default_ratedeck">>, ?KZ_RATES_DB)}
            ,{<<"ratedecks">>, kz_services_ratedecks:ratedecks()}
            ],
    cb_context:setters(Context, [{fun cb_context:set_resp_data/2, kz_json:from_list(Props)}
                                ,{fun cb_context:set_resp_status/2, 'success'}
                                ]).
