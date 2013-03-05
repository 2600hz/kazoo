%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CDR
%%% Read only access to CDR docs
%%%
%%% @end
%%% @contributors
%%%   Edouard Swiac
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_cdrs).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,validate/1, validate/2
        ]).

-include("src/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".cdrs">>).

-define(CB_LIST_BY_USER, <<"cdrs/listing_by_owner">>).
-define(CB_LIST, <<"cdrs/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.cdrs">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.cdrs">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.cdrs">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.cdrs">>, ?MODULE, validate).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/cdr/' can only accept GET
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    ['GET'].
allowed_methods(_) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(#cb_context{}=Context) ->
    CTPs = [{to_json, [{<<"application">>, <<"json">>}]}
            ,{to_csv, [{<<"application">>, <<"octet-stream">>}]}
           ],
    cb_context:add_content_types_provided(Context, CTPs).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_cdr_summary(Context).
validate(#cb_context{req_verb = <<"get">>}=Context, CDRId) ->
    load_cdr(CDRId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of accounts, each summarized.  Or a specific
%% account summary.
%% @end
%%--------------------------------------------------------------------
-spec load_cdr_summary(cb_context:context()) -> cb_context:context().
load_cdr_summary(#cb_context{req_nouns=[_, {?WH_ACCOUNTS_DB, [_AID]} | _]}=Context) ->
    lager:debug("loading cdrs for account ~s", [_AID]),
    case create_view_options(undefined, Context) of
        {ok, ViewOptions} -> load_view(?CB_LIST, ViewOptions, Context);
        {error, C} -> C
    end;
load_cdr_summary(#cb_context{req_nouns=[_, {<<"users">>, [UserId] } | _]}=Context) ->
    lager:debug("loading cdrs for user ~s", [UserId]),
    case create_view_options(UserId, Context) of
        {ok, ViewOptions} -> load_view(?CB_LIST_BY_USER, ViewOptions, Context);
        {error, C} -> C
    end;
load_cdr_summary(Context) ->
    lager:debug("invalid URL chain for cdr summary request"),
    cb_context:add_system_error(faulty_request, Context).

load_view(View, ViewOptions, #cb_context{query_json=JObj}=Context) ->
    {G, L} = case cb_modules_util:is_superduper_admin(Context) of
                 false -> {false, false};
                 true ->
                     {whapps_config:get_is_true(?CONFIG_CAT, <<"calculate_internal_cost_for_global_resources">>, false)
                      ,whapps_config:get_is_true(?CONFIG_CAT, <<"calcuate_internal_cost_for_local_resources">>, false)
                     }
             end,
    crossbar_doc:load_view(View
                           ,maybe_load_doc(G, L, ViewOptions)
                           ,Context#cb_context{query_json=wh_json:delete_keys([<<"created_to">>
                                                                                   ,<<"created_from">>
                                                                              ], JObj)}
                           ,fun(CDR, Acc) -> normalize_view_results(CDR, Acc, G, L) end
                          ).

maybe_load_doc(true, _, ViewOptions) ->
    [include_doc | ViewOptions];
maybe_load_doc(_, true, ViewOptions) ->
    [include_doc | ViewOptions];
maybe_load_doc(_, _, ViewOptions) ->
    ViewOptions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a CDR document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_cdr(ne_binary(), cb_context:context()) -> cb_context:context().
load_cdr(CdrId, Context) ->
    crossbar_doc:load(CdrId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects(), boolean(), boolean()) -> wh_json:objects().
normalize_view_results(JObj, Acc, false, false) ->
    [filter_cdr_fields(wh_json:get_value(<<"value">>, JObj))|Acc];
normalize_view_results(JObj, Acc, G, L) ->
    V = filter_cdr_fields(wh_json:get_value(<<"value">>, JObj)),
    case wh_json:get_value(<<"doc">>, JObj) of
        undefined -> [V | Acc];
        Doc -> update_cdr_fields(Acc, G, L, V, Doc)
    end.

update_cdr_fields(Acc, true, _, V, Cdr) ->
    case wh_json:get_value(<<"pvt_internal_cost">>, Cdr) of
        undefined -> update_cdr_fields(Acc, V, Cdr);
        Cost -> [wh_json:set_value(<<"internal_cost">>, Cost, V) | Acc]
    end.

update_cdr_fields(Acc, V, Cdr) ->
    case wh_json:get_value([<<"custom_channel_vars">>, <<"rate_id">>], Cdr) of
        undefined -> [V | Acc];
        RateId -> add_internal_cost(Acc, V, Cdr, RateId)
    end.

add_internal_cost(Acc, V, Cdr, RateId) ->
    case couch_mgr:open_doc(?WH_RATES_DB, RateId) of
        {ok, RateDoc} ->
            Cost = calc_internal_cost(RateDoc, Cdr),

            _ = spawn(fun() ->
                              AcctDb = wh_json:get_value(<<"pvt_account_db">>, Cdr),
                              couch_mgr:save_doc(AcctDb, wh_json:set_value(<<"pvt_internal_cost">>, Cost, Cdr))
                      end),

            [wh_json:set_value(<<"internal_cost">>, Cost, V) | Acc];
        _ -> [V | Acc]
    end.

calc_internal_cost(RateDoc, Cdr) ->
    Rate = wh_json:get_float_value(<<"pvt_rate_cost">>, RateDoc, 0.0),
    RateIncr = wh_json:get_integer_value(<<"rate_increment">>, RateDoc, 60),
    RateMin = wh_json:get_integer_value(<<"rate_minimum">>, RateDoc, 60),
    Surcharge = wh_json:get_float_value(<<"rate_surcharge">>, RateDoc, 0.0),
    BillingSecs = wh_json:get_integer_value(<<"billing_seconds">>, Cdr, 0),

    wht_util:calculate_cost(Rate, RateIncr, RateMin, Surcharge, BillingSecs).

filter_cdr_fields(JObj) ->
    JObj.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_view_options(api_binary(), cb_context:context()) ->
                                       {'ok', wh_proplist()} |
                                       {'error', cb_context:context()}.
create_view_options(OwnerId, #cb_context{query_json=JObj}=Context) ->
    MaxRange = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"maximum_range">>, 6048000),
    To = wh_json:get_integer_value(<<"created_to">>, JObj, wh_util:current_tstamp()),
    From = wh_json:get_integer_value(<<"created_from">>, JObj, wh_util:current_tstamp() - MaxRange),
    Diff = To - From,
    case {Diff < 0, Diff > MaxRange} of
        {true, _} ->
            Message = <<"created_from is prior to created_to">>,
            {error, cb_context:add_validation_error(<<"created_from">>, <<"date_range">>, Message, Context)};
        {_, true} ->
            Message = <<"created_to is more than ", (wh_util:to_binary(MaxRange))/binary, " seconds from created_from">>,
            {error, cb_context:add_validation_error(<<"created_from">>, <<"date_range">>, Message, Context)};
        {false, false} when OwnerId =:= undefined ->
            {ok, [{<<"startkey">>, From}, {<<"endkey">>, To}]};
        {false, false} ->
            {ok, [{<<"startkey">>, [OwnerId, From]}, {<<"endkey">>, [OwnerId, To]}]}
    end.
