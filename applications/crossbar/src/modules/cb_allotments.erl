%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Dinkor Media Group (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(cb_allotments).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ,post/1
        ]).

-include("crossbar.hrl").
-include_lib("kazoo_stdlib/include/kazoo_json.hrl").

-define(LIST_CONSUMED, <<"allotments/consumed">>).
-define(PVT_TYPE, <<"limits">>).
-define(CONSUMED, <<"consumed">>).
-define(PVT_ALLOTMENTS, <<"pvt_allotments">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.allotments">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.allotments">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.allotments">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.post.allotments">>, ?MODULE, 'post'),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_POST].
allowed_methods(?CONSUMED) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?CONSUMED) -> 'true'.

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
validate(Context) ->
    validate_allotments(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?CONSUMED) ->
    validate_consumed(Context, cb_context:req_verb(Context)).

-spec validate_allotments(cb_context:context(), http_method()) -> cb_context:context().
validate_allotments(Context, ?HTTP_GET) ->
    load_allotments(Context);
validate_allotments(Context, ?HTTP_POST) ->
    cb_context:validate_request_data(<<"allotments">>, Context, fun on_successful_validation/1).

-spec validate_consumed(cb_context:context(), http_method()) -> cb_context:context().
validate_consumed(Context, ?HTTP_GET) ->
    C = load_allotments(Context),
    load_consumed(C, cb_context:resp_status(C)).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    update_allotments(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_allotments(cb_context:context()) -> cb_context:context().
load_allotments(Context) ->
    C = crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(C) =:= 'success'
        andalso not kz_json:is_empty(kz_json:get_json_value(?PVT_ALLOTMENTS, cb_context:doc(C), kz_json:new()))
    of
        'false' ->
            Msg = <<"allotments are not configured for this account yet">>,
            crossbar_util:response_400(Msg, kz_json:new(), Context);
        'true' ->
            cb_context:set_resp_data(C, kz_json:get_json_value(?PVT_ALLOTMENTS, cb_context:doc(C)))
    end.


-spec load_consumed(cb_context:context(), crossbar_status()) -> cb_context:context().
load_consumed(Context, 'success') ->
    Allotments = kz_json:get_json_value(?PVT_ALLOTMENTS, cb_context:doc(Context)),
    Mode = get_consumed_mode(Context),
    {ContextResult, _, Result} = kz_json:foldl(fun foldl_consumed/3, {Context, Mode, kz_json:new()}, Allotments),
    case cb_context:resp_status(ContextResult) of
        'success' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, Result}
                                        ]);
        _Error -> ContextResult
    end;
load_consumed(Context, _) -> Context.

-type mode() :: {ne_binary(), gregorian_seconds(), gregorian_seconds()}.

-spec foldl_consumed(ne_binary(), kz_json:object(), CMA) -> CMA when CMA :: {cb_context:context(), mode(), kz_json:objects()}.
foldl_consumed(Classification, ValueJObj, {Context, {CycleMode, From0, To0}=Mode, Acc}) ->
    {Cycle, From, To} = case CycleMode =:= <<"cycle">>
                            andalso kz_json:get_ne_binary_value(<<"cycle">>, ValueJObj)
                        of
                            'false' -> {CycleMode, From0, To0};
                            Value -> {Value, cycle_start(Value, From0), cycle_end(Value, To0)}
                        end,
    Options = [{'range_keymap', [Classification]}
              ,{'created_from', From}
              ,{'created_to', To}
              ,{'group_level', 1}
              ],
    C1 = crossbar_view:load_modb(Context, ?LIST_CONSUMED, Options),
    case cb_context:resp_status(C1) of
        'success' ->
            {C1, Mode, normalize_result(Cycle, From, To, Acc, cb_context:doc(C1))};
        CtxErr -> {CtxErr, Mode, Acc}
    end.


-spec normalize_result(api_ne_binary(), gregorian_seconds(), gregorian_seconds(), kz_json:object(), kz_json:objects())
                      -> kz_json:object().
normalize_result(_Cycle, _From, _To, Acc, []) -> Acc;
normalize_result(Cycle, From, To, Acc, [Head|Tail]) ->
    [Classification] = kz_json:get_value(<<"key">>, Head),
    Consumed = kz_json:get_value(<<"value">>, Head),
    Acc1 = case kz_json:get_value(Classification, Acc) of
               'undefined' ->
                   Value = kz_json:set_values(
                             [{<<"cycle">>, Cycle}
                             ,{<<"consumed_from">>, From}
                             ,{<<"consumed_to">>, To}
                             ,{<<"consumed">>, Consumed}
                             ], kz_json:new()),
                   kz_json:set_value(Classification, Value, Acc);
               AccValue ->
                   AccConsumed = kz_json:get_integer_value(<<"consumed">>, AccValue),
                   kz_json:set_value([Classification, <<"consumed">>], AccConsumed + Consumed, Acc)
           end,
    normalize_result(Cycle, From, To, Acc1, Tail).

-spec get_consumed_mode(cb_context:context()) -> mode().
get_consumed_mode(Context) ->
    case
        {maybe_req_seconds(Context, <<"created_from">>)
        ,maybe_req_seconds(Context, <<"created_to">>)
        }
    of
        {'undefined', 'undefined'} ->
            NowDateTime = kz_time:now_s(),
            {<<"cycle">>, NowDateTime, NowDateTime};
        {From, 'undefined'} -> {<<"cycle">>, From, From};
        {'undefined', To} -> {<<"cycle">>, To, To};
        {From, To} -> {<<"manual">>, From, To}
    end.

-spec maybe_req_seconds(cb_context:context(), api_binary()) -> api_seconds().
maybe_req_seconds(Context, Key) ->
    Val = cb_context:req_value(Context, Key),
    case kz_term:safe_cast(Val, 'undefined', fun kz_term:to_integer/1) of
        'undefined' -> 'undefined';
        T when T > 0 -> T;
        _ -> 'undefined'
    end.

-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    case is_allowed(Context) of
        'true' ->
            maybe_create_limits_doc(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)));
        'false' ->
            Msg = <<"sub-accounts of non-master resellers must contact the reseller to change their allotments">>,
            crossbar_util:response_400(Msg, kz_json:new(), Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_allowed(cb_context:context()) -> boolean().
is_allowed(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    IsSystemAdmin = kz_util:is_system_admin(AuthAccountId),
    {'ok', MasterAccount} = kapps_util:get_master_account_id(),
    case kz_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            lager:debug("allowing reseller to update allotments"),
            'true';
        MasterAccount ->
            lager:debug("allowing direct account to update allotments"),
            'true';
        _Else when IsSystemAdmin ->
            lager:debug("allowing system admin to update allotments"),
            'true';
        _Else ->
            lager:debug("sub-accounts of non-master resellers must contact the reseller to change their allotments"),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_create_limits_doc(cb_context:context()) -> cb_context:context().
-spec maybe_create_limits_doc(cb_context:context(), pos_integer()) -> cb_context:context().
maybe_create_limits_doc(Context) ->
    maybe_create_limits_doc(Context, cb_context:resp_error_code(Context)).

maybe_create_limits_doc(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = kz_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}, {<<"_id">>, ?PVT_TYPE}]),
    JObj = kz_json_schema:add_defaults(kz_json:merge_jobjs(NewLimits, kz_doc:public_fields(Data)), <<"limits">>),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_doc:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_create_limits_doc(Context, _RespCode) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_allotments(cb_context:context()) -> cb_context:context().
update_allotments(Context) ->
    Doc = cb_context:doc(Context),
    Allotments = cb_context:req_data(Context),
    NewDoc = kz_json:set_value(?PVT_ALLOTMENTS, Allotments, Doc),
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, NewDoc)),
    cb_context:set_resp_data(Context1, Allotments).

-spec cycle_start(api_ne_binary(), kz_datetime() | gregorian_seconds()) -> gregorian_seconds().
cycle_start(Cycle, Seconds) when is_integer(Seconds) ->
    cycle_start(Cycle, calendar:gregorian_seconds_to_datetime(Seconds));
cycle_start(<<"monthly">>, {{Year, Month, _}, _}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}});
cycle_start(<<"weekly">>, {Date, _}) ->
    calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}) -
        (calendar:day_of_the_week(Date) - 1) * ?SECONDS_IN_DAY;
cycle_start(<<"daily">>,  {{Year, Month, Day}, _}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}});
cycle_start(<<"hourly">>, {{Year, Month, Day}, {Hour, _, _}}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}});
cycle_start(<<"minutely">>, {{Year, Month, Day}, {Hour, Min, _}}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, 0}});
cycle_start(_, {{Year, Month, Day}, {Hour, _, _}}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, 0, 0}}).

-spec cycle_end(api_ne_binary(), kz_datetime() | gregorian_seconds()) -> gregorian_seconds().
cycle_end(Cycle, Seconds) when is_integer(Seconds) ->
    cycle_end(Cycle, calendar:gregorian_seconds_to_datetime(Seconds));
cycle_end(<<"monthly">>, {{Year, Month, _}, _}) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    calendar:datetime_to_gregorian_seconds({{Year, Month, LastDay}, {23, 59, 59}}) + 1;
cycle_end(<<"weekly">>, DateTime)   -> cycle_start(<<"weekly">>, DateTime)   + ?SECONDS_IN_WEEK;
cycle_end(<<"daily">>, DateTime)    -> cycle_start(<<"daily">>, DateTime)    + ?SECONDS_IN_DAY;
cycle_end(<<"hourly">>, DateTime)   -> cycle_start(<<"hourly">>, DateTime)   + ?SECONDS_IN_HOUR;
cycle_end(<<"minutely">>, DateTime) -> cycle_start(<<"minutely">>, DateTime) + ?SECONDS_IN_MINUTE;
cycle_end(_, DateTime)              -> cycle_start(<<"hourly">>, DateTime)   + ?SECONDS_IN_HOUR.
