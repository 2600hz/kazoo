%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_resource_selectors).

-export([init/0
        ,authorize/1
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/4
        ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/4
        ,validate/1, validate/2, validate/3, validate/5
        ,post/2
        ,delete/2
        ]).

-export([normalize_view_results/2]).

-include("crossbar.hrl").

-define(SRS_LIST, <<"resource_selectors/resource_listing">>).
-define(SRS_SEARCH, <<"resource_selectors/crossbar_search">>).
-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".resource_selectors">>).
-define(SUPPRESS_SRS_NOTICE, kapps_config:get_is_true(?MOD_CONFIG_CAT, <<"suppress_selectors_change_notice">>, 'false')).
-define(NAME, <<"name">>).
-define(RESOURCE, <<"resource">>).
-define(RULES, <<"rules">>).

-define(DEFAULT_RULES, kz_json:new()).
-define(DOC_PVT_TYPE, <<"resource_selector">>).
-define(RULES_PVT_TYPE, <<"resource_selector_rules">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = [crossbar_bindings:bind(Binding, ?MODULE, F)
         || {Binding, F} <- [{<<"*.allowed_methods.resource_selectors">>, 'allowed_methods'}
                            ,{<<"*.resource_exists.resource_selectors">>, 'resource_exists'}
                            ,{<<"*.validate.resource_selectors">>, 'validate'}
                            ,{<<"*.execute.post.resource_selectors">>, 'post'}
                            ,{<<"*.execute.delete.resource_selectors">>, 'delete'}
                            ,{<<"*.authorize">>, 'authorize'}
                            ]
        ],
    ok.

-spec authorize(cb_context:context()) ->
                       boolean() | {'stop', cb_context:context()}.
authorize(Context) ->
    authorize(Context, cb_context:req_nouns(Context)).

-spec authorize(cb_context:context(), req_nouns()) ->
                       boolean() | {'stop', cb_context:context()}.
authorize(Context, [{<<"resource_selectors">>, _} | _]) ->
    case cb_context:account_id(Context) of
        'undefined' -> maybe_authorize_admin(Context);
        _AccountId -> 'true'
    end;
authorize(_Context, _Nouns) ->
    'false'.

-spec maybe_authorize_admin(cb_context:context()) ->
                                   'true' |
                                   {'stop', cb_context:context()}.
maybe_authorize_admin(Context) ->
    case cb_context:is_superduper_admin(Context) of
        'true' ->
            lager:debug("authz the request for global resources"),
            'true';
        'false' -> {'stop', Context}
    end.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?RULES) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?NAME) ->
    [?HTTP_GET];
allowed_methods(?RESOURCE) ->
    [?HTTP_GET];
allowed_methods(_UUID) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(?NAME, _SelectorName) ->
    [?HTTP_GET];
allowed_methods(?RESOURCE, _ResourceId) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token(), path_token(), path_token()) -> http_methods().
allowed_methods(?RESOURCE, _ResourceId, ?NAME, _SelectorName) ->
    [?HTTP_GET];
allowed_methods(?NAME, _SelectorName, ?RESOURCE, _ResourceId) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?RULES) -> 'true';
resource_exists(?NAME) -> 'true';
resource_exists(?RESOURCE) -> 'true';
resource_exists(_UUID) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(?NAME, _SelectorName) -> 'true';
resource_exists(?RESOURCE, _ResourceId) -> 'true'.

-spec resource_exists(path_token(), path_token(), path_token(), path_token()) -> 'true'.
resource_exists(?RESOURCE, _ResourceId, ?NAME, _SelectorName) -> 'true';
resource_exists(?NAME, _SelectorName, ?RESOURCE, _ResourceId) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    summary(set_selectors_db(Context), [], <<"resource_selectors/id_listing">>, 'false').

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?RULES) ->
    validate_rules(set_account_db(Context), cb_context:req_verb(Context));
validate(Context, ?NAME) ->
    summary(set_selectors_db(Context), [], <<"resource_selectors/name_listing">>, 'true');
validate(Context, ?RESOURCE) ->
    summary(set_selectors_db(Context), [], <<"resource_selectors/resource_listing">>, 'true');
validate(Context, UUID) ->
    validate_doc(set_selectors_db(Context), UUID, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, ?NAME, SelectorName) ->
    summary(set_selectors_db(Context), [SelectorName], <<"resource_selectors/name_resource_listing">>, 'true');
validate(Context, ?RESOURCE, ResourceId) ->
    summary(set_selectors_db(Context), [ResourceId], <<"resource_selectors/resource_name_listing">>, 'true').

-spec validate(cb_context:context(), path_token(), path_token(),  path_token(), path_token()) -> cb_context:context().
validate(Context, ?RESOURCE, ResourceId, ?NAME, SelectorName) ->
    summary(set_selectors_db(Context), [ResourceId, SelectorName], <<"resource_selectors/resource_name_id_listing">>, 'false');
validate(Context, ?NAME, SelectorName, ?RESOURCE, ResourceId) ->
    summary(set_selectors_db(Context), [ResourceId, SelectorName], <<"resource_selectors/resource_name_id_listing">>, 'false').

-spec validate_rules(cb_context:context(), http_method()) -> cb_context:context().
validate_rules(Context, ?HTTP_GET) ->
    load_rules(Context);
validate_rules(Context, ?HTTP_POST) ->
    OnSuccess = fun on_successful_rules_validation/1,
    cb_context:validate_request_data(<<"resource_selectors.rules">>, Context, OnSuccess).

-spec validate_doc(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_doc(Context, UUID, ?HTTP_GET) ->
    crossbar_doc:load(UUID, Context, ?TYPE_CHECK_OPTION(?DOC_PVT_TYPE));
validate_doc(Context, UUID, ?HTTP_POST) ->
    OnSuccess = fun(C) -> on_successful_doc_validation(C, UUID) end,
    cb_context:validate_request_data(<<"resource_selectors">>, Context, OnSuccess);
validate_doc(Context, UUID, ?HTTP_DELETE) ->
    crossbar_doc:load(UUID, Context, ?TYPE_CHECK_OPTION(?DOC_PVT_TYPE)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?RULES) ->
    post_rules(Context);
post(Context, _UUID) ->
    post_doc(Context).

-spec post_rules(cb_context:context()) -> cb_context:context().
post_rules(Context) ->
    crossbar_doc:save(Context).

-spec post_doc(cb_context:context()) -> cb_context:context().
post_doc(Context) ->
    crossbar_doc:save(Context).

-spec set_selectors_db(cb_context:context()) -> cb_context:context().
set_selectors_db(Context) ->
    case is_global_request(Context) of
        'true' ->
            {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
            MasterSelectorsDb = kz_util:format_resource_selectors_db(MasterAccountId),
            cb_context:set_account_db(Context, MasterSelectorsDb);
        'false' ->
            AccountId = cb_context:account_id(Context),
            SelectorsDb = kz_util:format_resource_selectors_db(AccountId),
            cb_context:set_account_db(Context, SelectorsDb)
    end.

-spec set_account_db(cb_context:context()) -> cb_context:context().
set_account_db(Context) ->
    case is_global_request(Context) of
        'true' ->
            {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
            cb_context:set_account_db(Context, MasterAccountDb);
        'false' -> Context
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _UUID) ->
    crossbar_doc:delete(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%------------------------------------------------------------------------------
-spec load_rules(cb_context:context()) -> cb_context:context().
load_rules(Context) ->
    crossbar_doc:load(?RULES_PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?RULES_PVT_TYPE)).

-spec summary(cb_context:context(), kz_term:api_binaries(), kz_term:api_binary(), boolean()) -> cb_context:context().
summary(Context, Prefix, ViewName, Reduce) ->
    case kz_term:is_true(Reduce) of
        'true' ->
            Options = [{'startkey', build_start_key(Context, Prefix)}
                      ,{'endkey', build_end_key(Context, Prefix)}
                      ,'group'
                      ],
            Fun = fun normalize_view_results/2;
        'false' ->
            Options = [{'startkey', build_start_key(Context, Prefix)}
                      ,{'endkey', build_end_key(Context, Prefix)}
                      ],
            Fun = fun normalize_resource_selector_result/2
    end,
    Context1 = crossbar_doc:load_view(ViewName, Options, Context, Fun),
    RespEnvelope = fix_envelope_start_keys(cb_context:resp_envelope(Context1), Prefix),
    cb_context:set_resp_envelope(Context1, RespEnvelope).

-spec build_start_key(cb_context:context(), kz_term:api_binaries()) -> kz_term:api_binaries().
build_start_key(Context, PrefixKeys) ->
    case cb_context:req_value(Context, <<"start_key">>) of
        'undefined' -> PrefixKeys;
        StartKey -> build_key(PrefixKeys, StartKey)
    end.

-spec build_end_key(cb_context:context(), kz_term:api_binaries()) -> kz_term:api_binaries().
build_end_key(Context, PrefixKeys) ->
    case cb_context:req_value(Context, <<"end_key">>) of
        'undefined' -> build_key(PrefixKeys, <<16#fff0/utf8>>);
        EndKey -> build_key(PrefixKeys, EndKey)
    end.

-spec build_key(kz_term:api_binaries(), kz_term:api_binary()) -> kz_term:api_binaries().
build_key(PrefixKeys, Suffix) ->
    lists:reverse([Suffix | lists:reverse(PrefixKeys)]).

-spec fix_envelope_start_keys(kz_json:object(), kz_term:api_binaries()) -> kz_json:object().
fix_envelope_start_keys(JObj, Prefix) ->
    lists:foldl(fun(K,J) ->
                        case {kz_json:get_value(K, J), Prefix} of
                            {'undefined', _} -> J;
                            {[], _} -> kz_json:delete_key(K, J);
                            {_, []} -> J;
                            {P, P} -> kz_json:delete_key(K, J);
                            {[P, Value], [P]} -> kz_json:set_value(K, Value, J);
                            {[P1, P2, Value], [P1, P2]} -> kz_json:set_value(K, Value, J)
                        end
                end
               ,JObj
               ,[<<"start_key">>, <<"next_start_key">>]
               ).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    Key = lists:last(kz_json:get_value(<<"key">>, JObj)),
    Value = kz_json:get_value(<<"value">>, JObj),
    [kz_json:from_list([{Key, Value}]) | Acc].

-spec normalize_resource_selector_result(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_resource_selector_result(JObj, Acc) ->
    Props = props:filter_undefined(
              [{<<"id">>, kz_json:get_value(<<"id">>, JObj)}
              ,{<<"name">>, kz_json:get_value([<<"value">>, <<"name">>], JObj)}
              ,{<<"resource">>, kz_json:get_value([<<"value">>, <<"resource">>], JObj)}
              ,{<<"selector">>, kz_json:get_value([<<"value">>, <<"selector">>], JObj)}
              ,{<<"value">>, kz_json:get_value([<<"value">>, <<"value">>], JObj)}
              ,{<<"start_time">>, kz_json:get_value([<<"value">>, <<"stop_time">>], JObj)}
              ,{<<"stop_time">>, kz_json:get_value([<<"value">>, <<"stop_time">>], JObj)}
              ]),
    [kz_json:from_list(Props) | Acc].

-spec on_successful_rules_validation(cb_context:context()) -> cb_context:context().
on_successful_rules_validation(Context) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(?RULES_PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?RULES_PVT_TYPE))).

-spec on_successful_doc_validation(cb_context:context(), path_token()) -> cb_context:context().
on_successful_doc_validation(Context, UUID) ->
    maybe_handle_load_failure(crossbar_doc:load_merge(UUID, Context, ?TYPE_CHECK_OPTION(?DOC_PVT_TYPE))).

-spec is_global_request(cb_context:context()) -> boolean().
is_global_request(Context) ->
    case cb_context:account_id(Context) of
        'undefined' ->
            lager:debug("request is for global resources"),
            'true';
        AccountId ->
            lager:debug("request is for local account ~s", [AccountId]),
            'false'
    end.

-spec maybe_handle_load_failure(cb_context:context()) ->
                                       cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) ->
                                       cb_context:context().
maybe_handle_load_failure(Context, 404) ->
    JObj = kz_doc:set_type(kz_doc:set_id(cb_context:req_data(Context),?RULES_PVT_TYPE), ?RULES_PVT_TYPE),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_doc:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.
