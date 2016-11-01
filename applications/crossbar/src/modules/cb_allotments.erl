%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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
-include_lib("kazoo_json/include/kazoo_json.hrl").

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
    load_consumed(Context).

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
    Context1 = maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))),
    Allotments = kz_json:get_json_value(?PVT_ALLOTMENTS, cb_context:doc(Context1), kz_json:new()),
    cb_context:set_resp_data(Context1, Allotments).


-spec load_consumed(cb_context:context()) -> cb_context:context().
load_consumed(Context) ->
    Allotments = kz_json:get_json_value(?PVT_ALLOTMENTS
                                       ,cb_context:doc(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)))
                                       ,kz_json:new()),
    Mode = get_consumed_mode(Context),
    {ContextResult, _, Result} = kz_json:foldl(fun foldl_consumed/3, {Context, Mode, kz_json:new()}, Allotments),
    case cb_context:resp_status(ContextResult) of
        'success' ->
            cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}
                                        ,{fun cb_context:set_resp_data/2, Result}
                                        ]);
        _Error -> ContextResult
    end.

-type mode() :: {'cycle', kz_datetime()} |
                {'manual', api_seconds(), api_seconds()}.

-spec foldl_consumed(api_binary(), kz_json:object(), CMA) -> CMA when
      CMA :: {cb_context:context(), mode(), kz_json:objects()}.
foldl_consumed(Classification, Value, {Context, Mode, Acc}) ->
    case create_viewoptions(Context, Classification, Value, Mode) of
        {Cycle, ViewOptions} ->
            [_, From] = props:get_value('startkey', ViewOptions),
            [_, To] = props:get_value('endkey', ViewOptions),
            ContextResult = crossbar_doc:load_view(?LIST_CONSUMED
                                                  ,props:insert_value({'group_level', 1},ViewOptions)
                                                  ,Context
                                                  ),
            Acc1 = normalize_result(Cycle, From, To, Acc, cb_context:doc(ContextResult)),
            {ContextResult, Mode, Acc1};
        ContextErr -> {ContextErr, Mode, Acc}
    end.


-spec normalize_result(api_binary(), gregorian_seconds(), gregorian_seconds(), kz_json:object(), kz_json:objects())
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

-spec create_viewoptions(cb_context:context(), api_binary(), kz_json:object(), mode()) -> {api_binary(), kz_proplist()} |
                                                                                          cb_context:context().
create_viewoptions(Context, Classification, JObj, {'cycle', DateTime}) ->
    Cycle = kz_json:get_value(<<"cycle">>, JObj),
    From = cycle_start(Cycle, DateTime),
    To = cycle_end(Cycle, DateTime),
    case cb_modules_util:range_modb_view_options(Context, [Classification], [], From, To) of
        {'ok', ViewOptions} -> {Cycle, ViewOptions};
        ContextErr -> ContextErr
    end;

create_viewoptions(Context, Classification, _JObj, {'manual', From, To}) ->
    case cb_modules_util:range_modb_view_options(Context, [Classification], [], From, To) of
        {'ok', ViewOptions} -> {<<"manual">>, ViewOptions};
        ContextErr -> ContextErr
    end.

-spec get_consumed_mode(cb_context:context()) -> mode().
get_consumed_mode(Context) ->
    case
        {maybe_req_seconds(Context, <<"created_from">>)
        ,maybe_req_seconds(Context, <<"created_to">>)
        }
    of
        {'undefined', 'undefined'} -> {'cycle', calendar:universal_time()};
        {From, 'undefined'} -> {'cycle', calendar:gregorian_seconds_to_datetime(From)};
        {'undefined', To} -> {'cycle', calendar:gregorian_seconds_to_datetime(To)};
        {From, To} -> {'manual', From, To}
    end.

-spec maybe_req_seconds(cb_context:context(), api_binary()) -> api_seconds().
maybe_req_seconds(Context, Key) ->
    T = cb_context:req_value(Context, Key),
    case kz_util:is_empty(T) of
        'true' -> 'undefined';
        'false' -> kz_util:to_integer(T)
    end.

-spec on_successful_validation(cb_context:context()) -> cb_context:context().
on_successful_validation(Context) ->
    case is_allowed(Context) of
        'true' -> maybe_handle_load_failure(crossbar_doc:load(?PVT_TYPE, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)));
        'false' -> crossbar_util:response_400(<<"sub-accounts of non-master resellers must contact the reseller to change their allotments">>, kz_json:new(), Context)
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
-spec maybe_handle_load_failure(cb_context:context()) -> cb_context:context().
-spec maybe_handle_load_failure(cb_context:context(), pos_integer()) -> cb_context:context().
maybe_handle_load_failure(Context) ->
    maybe_handle_load_failure(Context, cb_context:resp_error_code(Context)).

maybe_handle_load_failure(Context, 404) ->
    Data = cb_context:req_data(Context),
    NewLimits = kz_json:from_list([{<<"pvt_type">>, ?PVT_TYPE}
                                  ,{<<"_id">>, ?PVT_TYPE}
                                  ]),
    JObj = kz_json_schema:add_defaults(kz_json:merge_jobjs(NewLimits, kz_json:public_fields(Data))
                                      ,<<"limits">>
                                      ),

    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, kz_json:public_fields(JObj)}
                       ,{fun cb_context:set_doc/2, crossbar_doc:update_pvt_parameters(JObj, Context)}
                       ]);
maybe_handle_load_failure(Context, _RespCode) -> Context.

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

-spec cycle_start(ne_binary(), kz_datetime() | gregorian_seconds()) -> gregorian_seconds().
cycle_start(Cycle, Seconds) when is_integer(Seconds) -> cycle_start(Cycle, calendar:gregorian_seconds_to_datetime(Seconds));
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
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, 0}}).

-spec cycle_end(ne_binary(), kz_datetime() | gregorian_seconds()) -> gregorian_seconds().
cycle_end(Cycle, Seconds) when is_integer(Seconds) -> cycle_end(Cycle, calendar:gregorian_seconds_to_datetime(Seconds));
cycle_end(<<"monthly">>, {{Year, Month, _}, _}) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    calendar:datetime_to_gregorian_seconds({{Year, Month, LastDay}, {23, 59, 59}}) + 1;
cycle_end(<<"weekly">>, DateTime) ->   cycle_start(<<"weekly">>, DateTime) + ?SECONDS_IN_WEEK;
cycle_end(<<"daily">>, DateTime) ->    cycle_start(<<"daily">>, DateTime) + ?SECONDS_IN_DAY;
cycle_end(<<"hourly">>, DateTime) ->   cycle_start(<<"hourly">>, DateTime) + ?SECONDS_IN_HOUR;
cycle_end(<<"minutely">>, DateTime) -> cycle_start(<<"minutely">>, DateTime) + ?SECONDS_IN_MINUTE.
