%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_phone_number).

-export([fetch/1, fetch/2
        ,save/1
        ,delete/1
        ,release/1
        ,new/1, new/2
        ]).

-export([to_json/1
        ,to_public_json/1
        ,from_json/1, from_json_with_options/2
        ,is_phone_number/1
        ]).

-export([setters/2
        ,new/0
        ,number/1, set_number/2
        ,number_db/1
        ,assign_to/1, set_assign_to/2
        ,assigned_to/1, set_assigned_to/2
        ,prev_assigned_to/1, set_prev_assigned_to/2
        ,used_by/1, set_used_by/2
        ,features/1, features_available/1, features_list/1, set_features/2
        ,feature/2, set_feature/3
        ,state/1, set_state/2
        ,reserve_history/1, add_reserve_history/2, unwind_reserve_history/1
        ,ported_in/1, set_ported_in/2
        ,module_name/1, set_module_name/2
        ,carrier_data/1, set_carrier_data/2, update_carrier_data/2
        ,region/1, set_region/2
        ,auth_by/1, set_auth_by/2, is_authorized/1
        ,dry_run/1, set_dry_run/2
        ,batch_run/1, set_batch_run/2
        ,locality/1, set_locality/2
        ,doc/1, update_doc/2, reset_doc/2
        ,modified/1, set_modified/2
        ,created/1, set_created/2
        ,is_billable/1
        ]).

-export([list_attachments/2]).

-ifndef(TEST).
-export([push_stored/0]).
-endif.

-include("knm.hrl").
-include_lib("kazoo_json/include/kazoo_json.hrl").

-record(knm_phone_number, {number :: ne_binary()
                          ,number_db :: ne_binary()
                          ,assign_to :: api_ne_binary()
                          ,assigned_to :: api_ne_binary()
                          ,prev_assigned_to :: api_ne_binary()
                          ,used_by :: api_ne_binary()
                          ,features = kz_json:new() :: kz_json:object()
                          ,state :: ne_binary()
                          ,reserve_history = [] :: ne_binaries()
                          ,ported_in = 'false' :: boolean()
                          ,module_name = knm_carriers:default_carrier() :: ne_binary()
                          ,carrier_data = kz_json:new() :: kz_json:object()
                          ,region :: ne_binary()
                          ,auth_by :: api_ne_binary()
                          ,dry_run = 'false' :: boolean()
                          ,batch_run = 'false' :: boolean()
                          ,locality :: kz_json:object()
                          ,doc = kz_json:new() :: kz_json:object()
                          ,modified :: gregorian_seconds()
                          ,created :: gregorian_seconds()
                          ,is_billable = 'false' :: boolean()
                          ,is_dirty = 'false' :: boolean()
                          }).
-opaque knm_phone_number() :: #knm_phone_number{}.

-type knm_phone_numbers() :: [knm_phone_number(), ...].

-export_type([knm_phone_number/0
             ,knm_phone_numbers/0
             ,set_function/0
             ,set_functions/0
             ]).

-define(BULK_BATCH_WRITES,
        kapps_config:get_is_true(?KNM_CONFIG_CAT, <<"should_bulk_batch_writes">>, false)).

-define(PORTING_MODULE_NAME,
        kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"porting_module_name">>, ?CARRIER_LOCAL)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(ne_binary()) -> knm_phone_number().
new(DID) ->
    new(DID, knm_number_options:default()).

-spec new(ne_binary(), knm_number_options:options()) -> knm_phone_number().
new(DID, Options0) ->
    Options = case knm_number_options:state(Options0) of
                  ?NUMBER_STATE_PORT_IN -> [{'module_name', ?PORTING_MODULE_NAME} | Options0];
                  _ -> Options0
              end,
    {'ok', PhoneNumber} =
        setters(new(),
                [{fun set_number/2, knm_converters:normalize(DID)}
                 | knm_number_options:to_phone_number_setters(Options)
                ]),
    PhoneNumber.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> knm_phone_number_return().
-spec fetch(ne_binary(), knm_number_options:options()) -> knm_phone_number_return().

fetch(Num) ->
    fetch(Num, knm_number_options:default()).

-ifdef(TEST).
fetch(?TEST_CREATE_NUM, _Options) ->
    {'error', 'not_found'};
fetch(?TEST_AVAILABLE_NUM, Options) ->
    handle_fetched_result(?AVAILABLE_NUMBER, Options);
fetch(?TEST_IN_SERVICE_NUM, Options) ->
    handle_fetched_result(?IN_SERVICE_NUMBER, Options);
fetch(?TEST_IN_SERVICE_WITH_HISTORY_NUM, Options) ->
    handle_fetched_result(?IN_SERVICE_WITH_HISTORY_NUMBER, Options);
fetch(?BW_EXISTING_DID, Options) ->
    handle_fetched_result(?BW_EXISTING_JSON, Options);
fetch(?TEST_OLD_NUM, Options) ->
    JObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_in.json"))),
    handle_fetched_result(JObj, Options);
fetch(?TEST_OLD2_NUM, Options) ->
    JObj = kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_2_in.json"))),
    handle_fetched_result(JObj, Options);
fetch(_DID, _Options) ->
    {'error', 'not_found'}.
-else.
fetch(Num, Options) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    case fetch(NumberDb, NormalizedNum, Options) of
        {'ok', JObj} -> handle_fetched_result(JObj, Options);
        {'error', _R}=Error ->
            lager:debug("failed to open ~s in ~s: ~p", [NormalizedNum, NumberDb, _R]),
            Error
    end.

fetch(NumberDb, NormalizedNum, Options) ->
    case knm_number_options:batch_run(Options) of
        'true' -> kz_datamgr:open_doc(NumberDb, NormalizedNum);
        'false' -> kz_datamgr:open_cache_doc(NumberDb, NormalizedNum)
    end.
-endif.

-spec handle_fetched_result(kz_json:object(), knm_number_options:options()) ->
                                   {'ok', knm_phone_number()}.
handle_fetched_result(JObj, Options) ->
    PhoneNumber = from_json_with_options(JObj, Options),
    case is_authorized(PhoneNumber) of
        'true' -> {'ok', PhoneNumber};
        'false' -> knm_errors:unauthorized()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_phone_number()) -> knm_phone_number().
save(#knm_phone_number{dry_run='true'}=PhoneNumber) ->
    lager:debug("dry_run-ing btw"),
    PhoneNumber;
save(#knm_phone_number{is_dirty = false}=PhoneNumber) ->
    lager:debug("not dirty: skipping save"),
    PhoneNumber;
save(PhoneNumber) ->
    Routines = [fun save_to_number_db/1
               ,fun handle_assignment/1
               ],
    {'ok', NewPhoneNumber} = setters(PhoneNumber, Routines),
    NewPhoneNumber.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% To call only from knm_number:delete/2 (only for sysadmins).
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number()) -> knm_phone_number().
delete(#knm_phone_number{dry_run='true'}=PhoneNumber) ->
    lager:debug("dry_run-ing btw"),
    PhoneNumber;
delete(PhoneNumber) ->
    Routines = [fun try_delete_number_doc/1
               ,fun try_maybe_remove_number_from_account/1
               ,{fun set_state/2, ?NUMBER_STATE_DELETED}
               ],
    {'ok', NewPhoneNumber} = setters(PhoneNumber, Routines),
    NewPhoneNumber.

try_delete_number_doc(PN) ->
    case delete_number_doc(PN) of
        {'ok', _}=Ok -> Ok;
        {'error', _R} ->
            lager:debug("number doc for ~s not removed: ~p", [number(PN), _R]),
            {'ok', PN}
    end.

try_maybe_remove_number_from_account(PN) ->
    case maybe_remove_number_from_account(PN) of
        {'ok', _}=Ok -> Ok;
        {'error', _R} ->
            lager:debug("account doc for ~s not removed: ~p", [number(PN), _R]),
            {'ok', PN}
    end.

-spec release(knm_phone_number()) -> knm_phone_number().
-spec release(knm_phone_number(), ne_binary()) -> knm_phone_number().
release(PhoneNumber) ->
    release(PhoneNumber, state(PhoneNumber)).

release(PhoneNumber, ?NUMBER_STATE_RELEASED) ->
    PhoneNumber;
release(PhoneNumber, ?NUMBER_STATE_DELETED) ->
    PhoneNumber;
release(PhoneNumber, ?NUMBER_STATE_RESERVED) ->
    authorize_release(PhoneNumber);
release(PhoneNumber, ?NUMBER_STATE_PORT_IN) ->
    authorize_release(PhoneNumber);
release(PhoneNumber, ?NUMBER_STATE_IN_SERVICE) ->
    authorize_release(PhoneNumber);
release(PN, FromState) ->
    To = ?NUMBER_STATE_RELEASED,
    knm_errors:invalid_state_transition(PN, FromState, To).

-spec authorize_release(knm_phone_number()) -> knm_phone_number().
-spec authorize_release(knm_phone_number(), ne_binary()) -> knm_phone_number().
authorize_release(PhoneNumber) ->
    authorize_release(PhoneNumber, auth_by(PhoneNumber)).

-ifdef(TEST).
authorize_release(PhoneNumber, ?KNM_DEFAULT_AUTH_BY) ->
    authorized_release(PhoneNumber);
authorize_release(PhoneNumber, ?MASTER_ACCOUNT_ID) ->
    authorized_release(PhoneNumber);
authorize_release(_PhoneNumber, _AuthBy) ->
    knm_errors:unauthorized().
-else.
authorize_release(PhoneNumber, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    authorized_release(PhoneNumber);
authorize_release(PhoneNumber, AuthBy) ->
    AssignedTo = assigned_to(PhoneNumber),
    case kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
        'false' -> knm_errors:unauthorized();
        'true' -> authorized_release(PhoneNumber)
    end.
-endif.

-spec authorized_release(knm_phone_number()) -> knm_phone_number().
authorized_release(PhoneNumber) ->
    ReleasedState = knm_config:released_state(?NUMBER_STATE_AVAILABLE),
    Routines = [{fun set_features/2, kz_json:new()}
               ,{fun set_doc/2, kz_json:private_fields(doc(PhoneNumber))}
               ,{fun set_prev_assigned_to/2, assigned_to(PhoneNumber)}
               ,{fun set_assigned_to/2, 'undefined'}
               ,{fun set_state/2, ReleasedState}
               ],
    {'ok', NewPhoneNumber} = setters(PhoneNumber, Routines),
    NewPhoneNumber.

%%--------------------------------------------------------------------
%% @public
%% @doc Returns same fields view phone_numbers.json returns.
%%--------------------------------------------------------------------
-spec to_public_json(knm_phone_number()) -> kz_json:object().
to_public_json(Number) ->
    JObj = to_json(Number),
    State = {<<"state">>, state(Number)},
    UsedBy = {<<"used_by">>, used_by(Number)},
    Features = {<<"features">>, features_list(Number)},
    ReadOnly =
        kz_json:from_list(
          props:filter_empty(
            [{<<"created">>, kz_doc:created(JObj)}
            ,{<<"modified">>, kz_doc:modified(JObj)}
            ,State
            ,UsedBy
            ,Features
            ,{<<"features_available">>, features_available(Number)}
            ])
         ),
    Values = props:filter_empty(
               [State
               ,UsedBy
               ,Features
               ]),
    Root = kz_json:set_values(Values, kz_json:public_fields(JObj)),
    kz_json:set_value(<<"_read_only">>, ReadOnly, Root).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_json(knm_phone_number()) -> kz_json:object().
to_json(#knm_phone_number{doc=JObj}=N) ->
    kz_json:from_list(
      [{<<"_id">>, number(N)}
      ,{?PVT_DB_NAME, number_db(N)}
      ,{?PVT_STATE, state(N)}
      ,{?PVT_PORTED_IN, ported_in(N)}
      ,{?PVT_MODULE_NAME, module_name(N)}
      ,{?PVT_MODIFIED, modified(N)}
      ,{?PVT_CREATED, created(N)}
      ,{?PVT_IS_BILLABLE, is_billable(N)}
      ,{?PVT_TYPE, <<"number">>}
       | kz_json:to_proplist(sanitize_public_fields(JObj))
      ]
      ++
          props:filter_empty(
            [{?PVT_ASSIGNED_TO, assigned_to(N)}
            ,{?PVT_AUTH_BY, auth_by(N)}
            ,{?PVT_PREVIOUSLY_ASSIGNED_TO, prev_assigned_to(N)}
            ,{?PVT_USED_BY, used_by(N)}
            ,{?PVT_FEATURES, features(N)}
            ,{?PVT_FEATURES_AVAILABLE, features_available(N)}
            ,{?PVT_RESERVE_HISTORY, reserve_history(N)}
            ,{?PVT_CARRIER_DATA, carrier_data(N)}
            ,{?PVT_REGION, region(N)}
            ])
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json(kz_json:object()) -> knm_phone_number().
from_json(JObj) ->
    Features =
        case kz_json:get_value(?PVT_FEATURES, JObj) of
            'undefined' -> kz_json:new();
            FeaturesList when is_list(FeaturesList) ->
                lists:foldl(fun (F, A) -> features_fold(F, A, JObj) end, kz_json:new(), FeaturesList);
            FeaturesJObj -> FeaturesJObj
        end,
    Now = kz_util:current_tstamp(),
    IsBillable = kz_json:is_true(?PVT_IS_BILLABLE, JObj, 'undefined'),
    UsedBy = kz_json:get_value(?PVT_USED_BY, JObj),
    {'ok', PhoneNumber} =
        setters(new(),
                [{fun set_number/2, knm_converters:normalize(kz_doc:id(JObj))}
                ,{fun set_assigned_to/3, kz_json:get_value(?PVT_ASSIGNED_TO, JObj), UsedBy}
                ,{fun set_prev_assigned_to/2, kz_json:get_value(?PVT_PREVIOUSLY_ASSIGNED_TO, JObj)}
                ,{fun set_features/2, Features}
                ,{fun set_state/2, kz_json:get_first_defined([?PVT_STATE, ?PVT_STATE_LEGACY], JObj)}
                ,{fun set_reserve_history/2, kz_json:get_value(?PVT_RESERVE_HISTORY, JObj, [])}
                ,{fun set_ported_in/2, kz_json:is_true(?PVT_PORTED_IN, JObj, 'false')}
                ,{fun set_module_name/3, kz_json:get_value(?PVT_MODULE_NAME, JObj), IsBillable}
                ,{fun set_carrier_data/2, kz_json:get_value(?PVT_CARRIER_DATA, JObj)}
                ,{fun set_region/2, kz_json:get_value(?PVT_REGION, JObj)}
                ,{fun set_auth_by/2, kz_json:get_value(?PVT_AUTH_BY, JObj)}
                ,{fun set_doc/2, sanitize_public_fields(JObj)}
                ,{fun set_modified/2, kz_doc:modified(JObj, Now)}
                ,{fun set_created/2, kz_doc:created(JObj, Now)}
                ]),
    PhoneNumber.

%% Handle 3.22 -> 4.0 features migration.
%% Note: if a feature matches here that means it was enabled in 3.22.
features_fold(?FEATURE_FORCE_OUTBOUND, Acc, JObj) ->
    Data = kz_json:is_true(?FEATURE_FORCE_OUTBOUND, JObj),
    kz_json:set_value(?FEATURE_FORCE_OUTBOUND, Data, Acc);
features_fold(?FEATURE_RINGBACK, Acc, JObj) ->
    Data = kz_json:from_list(
             props:filter_undefined(
               [{<<"early">>, kz_json:get_ne_value([?FEATURE_RINGBACK, <<"early">>], JObj)}
               ,{<<"transfer">>, kz_json:get_ne_value([?FEATURE_RINGBACK, <<"early">>], JObj)}
               ])),
    kz_json:set_value(?FEATURE_RINGBACK, Data, Acc);
features_fold(?FEATURE_PREPEND, Acc, JObj) ->
    Name = kz_json:get_ne_value([?FEATURE_PREPEND, <<"name">>], JObj),
    Data = kz_json:from_list([{<<"enabled">>, true}
                             ,{<<"name">>, Name}
                             ]),
    kz_json:set_value(?FEATURE_PREPEND, Data, Acc);
features_fold(?FEATURE_CNAM_OUTBOUND, Acc, JObj) ->
    DisplayName = kz_json:get_ne_binary_value([?FEATURE_CNAM, ?CNAM_DISPLAY_NAME], JObj),
    Data = kz_json:from_list([{?CNAM_DISPLAY_NAME, DisplayName}]),
    kz_json:set_value(?FEATURE_CNAM_OUTBOUND, Data, Acc);
features_fold(?CNAM_INBOUND_LOOKUP, Acc, _) ->
    Data = kz_json:from_list([{?CNAM_INBOUND_LOOKUP, true}]),
    kz_json:set_value(?FEATURE_CNAM_INBOUND, Data, Acc);
features_fold(<<"dash_e911">>=Feature, Acc, JObj) ->
    Data = kz_json:get_value(Feature, JObj),
    kz_json:set_value(?FEATURE_E911, Data, Acc);
features_fold(<<"vitelity_e911">>=Feature, Acc, JObj) ->
    Data = kz_json:get_value(Feature, JObj),
    kz_json:set_value(?FEATURE_E911, Data, Acc);
features_fold(FeatureKey, Acc, JObj) ->
    %% Encompasses at least:
    %%   ?FEATURE_PORT & ?FEATURE_FAILOVER
    Data = kz_json:get_ne_value(FeatureKey, JObj, kz_json:new()),
    kz_json:set_value(FeatureKey, Data, Acc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json_with_options(kz_json:object(), knm_phone_number() | knm_number_options:options()) ->
                                    knm_phone_number().
from_json_with_options(JObj, Options)
  when is_list(Options) ->
    Updates = [{fun set_assign_to/2, knm_number_options:assign_to(Options)}
               %% See knm_number_options:default/0 for these 3.
              ,{fun set_dry_run/2, knm_number_options:dry_run(Options, 'false')}
              ,{fun set_batch_run/2, knm_number_options:batch_run(Options, 'false')}
              ,{fun set_auth_by/2, knm_number_options:auth_by(Options, ?KNM_DEFAULT_AUTH_BY)}
              ],
    {'ok', PhoneNumber} = setters(from_json(JObj), Updates),
    PhoneNumber;
from_json_with_options(JObj, PhoneNumber) ->
    Options = [{'dry_run', dry_run(PhoneNumber)}
              ,{'batch_run', batch_run(PhoneNumber)}
              ,{'auth_by', auth_by(PhoneNumber)}
              ],
    from_json_with_options(JObj, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new() -> knm_phone_number().
new() ->
    #knm_phone_number{}.

-spec is_phone_number(any()) -> boolean().
is_phone_number(#knm_phone_number{}) -> 'true';
is_phone_number(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters(knm_phone_number(), set_functions()) -> knm_phone_number_return().
setters(Number, Routines) ->
    try lists:foldl(fun setters_fold/2, Number, Routines) of
        {'ok', _N}=Ok -> Ok;
        {'error', _R}=Error -> Error;
        Result -> {'ok', Result}
    catch
        'throw':{'stop', Error} -> Error;
        'error':'function_clause' ->
            {_M, FName, [_PhoneNumber,Arg|_], _Info} = hd(erlang:get_stacktrace()),
            lager:error("~s failed, argument: ~p", [FName, Arg]),
            kz_util:log_stacktrace(),
            {'error', FName};
        'error':Reason -> {'error', Reason}
    end.

-type set_function() :: fun((knm_phone_number()) -> setter_acc()) |
                        fun((knm_phone_number(), V) -> setter_acc()) |
                        {fun((knm_phone_number(), V) -> setter_acc()), V} |
                        {fun((knm_phone_number(), K, V) -> setter_acc()), [K | V,...]} |
                        {fun((knm_phone_number(), K, V) -> setter_acc()), K, V}.
-type set_functions() :: [set_function()].

-type setter_acc() :: knm_phone_number_return() |
                      knm_phone_number().

-spec setters_fold(set_function(), setter_acc()) -> setter_acc().
setters_fold(_, {'error', _R}=Error) ->
    throw({'stop', Error});
setters_fold({Fun, Key, Value}, PhoneNumber) when is_function(Fun, 3) ->
    setters_fold_apply(Fun, [PhoneNumber, Key, Value]);
setters_fold({Fun, Value}, PhoneNumber) when is_function(Fun, 2) ->
    setters_fold_apply(Fun, [PhoneNumber, Value]);
setters_fold(Fun, PhoneNumber) when is_function(Fun, 1) ->
    setters_fold_apply(Fun, [PhoneNumber]).

-spec setters_fold_apply(set_function(), nonempty_list()) -> setter_acc().
setters_fold_apply(Fun, [{'ok',PhoneNumber}|Args]) ->
    setters_fold_apply(Fun, [PhoneNumber|Args]);
setters_fold_apply(Fun, Args) ->
    erlang:apply(Fun, Args).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number(knm_phone_number()) -> ne_binary().
number(#knm_phone_number{number=Num}) -> Num.

-spec set_number(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_number(N, <<"+",_:8,_/binary>>=NormalizedNum) ->
    NumberDb = knm_converters:to_db(NormalizedNum),
    case {N#knm_phone_number.number, N#knm_phone_number.number_db} of
        {NormalizedNum, NumberDb} -> N;
        _ ->
            N#knm_phone_number{is_dirty = true
                              ,number = NormalizedNum
                              ,number_db = NumberDb
                              }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec number_db(knm_phone_number()) -> ne_binary().
number_db(#knm_phone_number{number_db=NumberDb}) ->
    NumberDb.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assign_to(knm_phone_number()) -> api_ne_binary().
assign_to(#knm_phone_number{assign_to=AssignTo}) ->
    AssignTo.

-spec set_assign_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_assign_to(N=#knm_phone_number{assign_to = V}, V) -> N;
set_assign_to(N, AssignTo='undefined') ->
    N#knm_phone_number{is_dirty = true
                      ,assign_to = AssignTo
                      };
set_assign_to(N, AssignTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{is_dirty = true
                      ,assign_to = AssignTo
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(knm_phone_number()) -> api_ne_binary().
assigned_to(#knm_phone_number{assigned_to=AssignedTo}) ->
    AssignedTo.

-spec set_assigned_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_assigned_to(N=#knm_phone_number{assigned_to = V}, V) -> N;
set_assigned_to(N, AssignedTo='undefined') ->
    N#knm_phone_number{is_dirty = true
                      ,assigned_to = AssignedTo
                      ,used_by = 'undefined'
                      };
set_assigned_to(N, AssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{is_dirty = true
                      ,assigned_to = AssignedTo
                      ,used_by = 'undefined'
                      }.

-spec set_assigned_to(knm_phone_number(), api_ne_binary(), api_ne_binary()) -> knm_phone_number().
set_assigned_to(N=#knm_phone_number{assigned_to = V}, V, UsedBy) ->
    set_used_by(N, UsedBy);
set_assigned_to(N0, AssignedTo='undefined', UsedBy) ->
    N = set_used_by(N0, UsedBy),
    N#knm_phone_number{assigned_to = AssignedTo};
set_assigned_to(N0, AssignedTo=?MATCH_ACCOUNT_RAW(_), UsedBy) ->
    N = set_used_by(N0, UsedBy),
    N#knm_phone_number{assigned_to = AssignedTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prev_assigned_to(knm_phone_number()) -> api_ne_binary().
prev_assigned_to(#knm_phone_number{prev_assigned_to=PrevAssignedTo}) ->
    PrevAssignedTo.

-spec set_prev_assigned_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_prev_assigned_to(N=#knm_phone_number{prev_assigned_to = V}, V) -> N;
set_prev_assigned_to(N, PrevAssignedTo='undefined') ->
    N#knm_phone_number{is_dirty = true
                      ,prev_assigned_to = PrevAssignedTo
                      };
set_prev_assigned_to(N, PrevAssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    N#knm_phone_number{is_dirty = true
                      ,prev_assigned_to = PrevAssignedTo
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec used_by(knm_phone_number()) -> api_ne_binary().
used_by(#knm_phone_number{used_by=UsedBy}) -> UsedBy.

-spec set_used_by(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_used_by(N=#knm_phone_number{used_by = V}, V) -> N;
set_used_by(N, UsedBy='undefined') ->
    N#knm_phone_number{is_dirty = true
                      ,used_by = UsedBy
                      };
set_used_by(N, UsedBy=?NE_BINARY) ->
    N#knm_phone_number{is_dirty = true
                      ,used_by = UsedBy
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec features(knm_phone_number()) -> kz_json:object().
features(#knm_phone_number{features=Features}) -> Features.

-spec features_list(knm_phone_number()) -> ne_binaries().
features_list(N) ->
    sets:to_list(sets:from_list(kz_json:get_keys(features(N)))).

-spec features_available(knm_phone_number()) -> ne_binaries().
features_available(N) ->
    knm_providers:available_features(N).

-spec set_features(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_features(N, Features) ->
    'true' = kz_json:is_json_object(Features),
    case kz_json:are_equal(N#knm_phone_number.features, Features) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,features = Features
                              }
    end.

-spec feature(knm_phone_number(), ne_binary()) -> kz_json:api_json_term().
feature(Number, Feature) ->
    kz_json:get_ne_value(Feature, features(Number)).

-spec set_feature(knm_phone_number(), ne_binary(), kz_json:json_term()) ->
                         knm_phone_number().
set_feature(N, Feature=?NE_BINARY, Data) ->
    Features = kz_json:set_value(Feature, Data, features(N)),
    set_features(N, Features). %% Sets is_dirty.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec state(knm_phone_number()) -> api_ne_binary().
state(#knm_phone_number{state=State}) -> State.

-spec set_state(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_state(N=#knm_phone_number{state = V}, V) -> N;
set_state(N, State)
  when State =:= ?NUMBER_STATE_PORT_IN;
       State =:= ?NUMBER_STATE_PORT_OUT;
       State =:= ?NUMBER_STATE_DISCOVERY;
       State =:= ?NUMBER_STATE_IN_SERVICE;
       State =:= ?NUMBER_STATE_RELEASED;
       State =:= ?NUMBER_STATE_RESERVED;
       State =:= ?NUMBER_STATE_AVAILABLE;
       State =:= ?NUMBER_STATE_DISCONNECTED;
       State =:= ?NUMBER_STATE_DELETED;
       State =:= ?NUMBER_STATE_AGING
       ->
    N#knm_phone_number{is_dirty = true
                      ,state = State
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reserve_history(knm_phone_number()) -> ne_binaries().
reserve_history(#knm_phone_number{reserve_history=History}) -> History.

-spec set_reserve_history(knm_phone_number(), ne_binaries()) -> knm_phone_number().
set_reserve_history(N, History) when is_list(History) ->
    Cons = fun (A, PN) -> add_reserve_history(PN, A) end,
    lists:foldr(Cons, N#knm_phone_number{reserve_history=[]}, History).

-spec add_reserve_history(knm_phone_number(), ne_binary()) -> knm_phone_number().
add_reserve_history(#knm_phone_number{reserve_history=[AccountId|_]}=N
                   ,?MATCH_ACCOUNT_RAW(AccountId)
                   ) ->
    N;
add_reserve_history(#knm_phone_number{reserve_history=ReserveHistory}=N
                   ,?MATCH_ACCOUNT_RAW(AccountId)
                   ) ->
    N#knm_phone_number{is_dirty = true
                      ,reserve_history=[AccountId | ReserveHistory]
                      }.

-spec unwind_reserve_history(knm_phone_number()) -> knm_phone_number().
unwind_reserve_history(PN) ->
    ReserveHistory = PN#knm_phone_number.reserve_history,
    case lists:delete(prev_assigned_to(PN), reserve_history(PN)) of
        ReserveHistory -> PN;
        NewReserveHistory ->
            set_reserve_history(PN, NewReserveHistory)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ported_in(knm_phone_number()) -> boolean().
ported_in(#knm_phone_number{ported_in=Ported}) -> Ported.

-spec set_ported_in(knm_phone_number(), boolean()) -> knm_phone_number().
set_ported_in(N=#knm_phone_number{ported_in = V}, V) -> N;
set_ported_in(N, Ported) when is_boolean(Ported) ->
    N#knm_phone_number{is_dirty = true
                      ,ported_in = Ported
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec module_name(knm_phone_number()) -> api_ne_binary().
module_name(#knm_phone_number{module_name = Name}) -> Name.

-spec set_module_name(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_module_name(N0, ?CARRIER_LOCAL=Name) ->
    Feature =
        case feature(N0, ?FEATURE_LOCAL) of
            'undefined' -> kz_json:new();
            LocalFeature -> LocalFeature
        end,
    N = set_feature(N0, ?FEATURE_LOCAL, Feature),
    case N0#knm_phone_number.module_name =:= Name of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,module_name = Name
                              }
    end;
%% knm_bandwidth is deprecated, updating to the new module
set_module_name(N, <<"wnm_bandwidth">>) ->
    set_module_name(N, <<"knm_bandwidth2">>);
set_module_name(N, <<"wnm_", Name/binary>>) ->
    set_module_name(N, <<"knm_", Name/binary>>);
set_module_name(N, Name=?NE_BINARY) ->
    IsBillable = knm_carriers:is_number_billable(N#knm_phone_number{module_name = Name}),
    case {N#knm_phone_number.module_name, N#knm_phone_number.is_billable} of
        {Name, IsBillable} -> N;
        _ ->
            N#knm_phone_number{is_dirty = true
                              ,module_name = Name
                              ,is_billable = IsBillable
                              }
    end.

-spec set_module_name(knm_phone_number(), ne_binary(), api_boolean()) -> knm_phone_number().
%% Some old docs have these as module name
set_module_name(N, <<"undefined">>, IsBillable) ->
    set_module_name(N, ?CARRIER_LOCAL, IsBillable);
set_module_name(N, 'undefined', IsBillable) ->
    set_module_name(N, ?CARRIER_LOCAL, IsBillable);
set_module_name(N0, Name, IsBillable)
  when is_boolean(IsBillable) ->
    N = set_module_name(N0, Name),
    %% Do not override is_billable when field is already set on doc.
    case N#knm_phone_number.is_billable of
        IsBillable -> N;
        _ ->
            N#knm_phone_number{is_dirty = true
                              ,is_billable = IsBillable
                              }
    end;
set_module_name(N0, Name, 'undefined') ->
    set_module_name(N0, Name).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec carrier_data(knm_phone_number()) -> kz_json:object().
carrier_data(#knm_phone_number{carrier_data=Data}) -> Data.

-spec set_carrier_data(knm_phone_number(), api_object()) -> knm_phone_number().
set_carrier_data(N=#knm_phone_number{carrier_data = undefined}, undefined) -> N;
set_carrier_data(N, 'undefined') ->
    set_carrier_data(N, kz_json:new());
set_carrier_data(N, Data) ->
    'true' = kz_json:is_json_object(Data),
    case kz_json:are_equal(N#knm_phone_number.carrier_data, Data) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,carrier_data = Data
                              }
    end.

-spec update_carrier_data(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_carrier_data(N=#knm_phone_number{carrier_data = Data}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(JObj, Data),
    case kz_json:are_equal(N#knm_phone_number.carrier_data, Updated) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,carrier_data = Updated
                              }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec region(knm_phone_number()) -> api_ne_binary().
region(#knm_phone_number{region=Region}) -> Region.

-spec set_region(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_region(N=#knm_phone_number{region = V}, V) -> N;
set_region(N, Region='undefined') ->
    N#knm_phone_number{is_dirty = true
                      ,region = Region
                      };
set_region(N, Region=?NE_BINARY) ->
    N#knm_phone_number{is_dirty = true
                      ,region = Region
                      }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec auth_by(knm_phone_number()) -> api_ne_binary().
auth_by(#knm_phone_number{auth_by=AuthBy}) -> AuthBy.

-spec set_auth_by(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_auth_by(N, AuthBy='undefined') ->
    N#knm_phone_number{auth_by=AuthBy};
set_auth_by(N, AuthBy=?KNM_DEFAULT_AUTH_BY) ->
    N#knm_phone_number{auth_by=AuthBy};
set_auth_by(N, ?MATCH_ACCOUNT_RAW(AuthBy)) ->
    N#knm_phone_number{auth_by=AuthBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(knm_phone_number()) -> boolean().
dry_run(#knm_phone_number{dry_run=DryRun}) -> DryRun.

-spec set_dry_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_dry_run(N, DryRun) when is_boolean(DryRun) ->
    N#knm_phone_number{dry_run=DryRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec batch_run(knm_phone_number()) -> boolean().
batch_run(#knm_phone_number{batch_run=BatchRun}) -> BatchRun.

-spec set_batch_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_batch_run(N, BatchRun) when is_boolean(BatchRun) ->
    N#knm_phone_number{batch_run=BatchRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec locality(knm_phone_number()) -> kz_json:object().
locality(#knm_phone_number{locality=Locality}) -> Locality.

-spec set_locality(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_locality(N, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    case kz_json:are_equal(JObj, N#knm_phone_number.locality) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,locality = JObj
                              }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec doc(knm_phone_number()) -> kz_json:object().
doc(#knm_phone_number{doc=Doc}) -> Doc.

-spec set_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_doc(N, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    case kz_json:are_equal(JObj, N#knm_phone_number.doc) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,doc = JObj
                              }
    end.

-spec update_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_doc(N=#knm_phone_number{doc = Doc}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(kz_json:public_fields(JObj), Doc),
    Data = kz_json:delete_key(<<"id">>, Updated),
    case kz_json:are_equal(Data, N#knm_phone_number.doc) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,doc = Data
                              }
    end.

-spec reset_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
reset_doc(N=#knm_phone_number{doc = Doc}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(kz_json:public_fields(JObj), kz_json:private_fields(Doc)),
    Data = kz_json:delete_key(<<"id">>, Updated),
    case kz_json:are_equal(Data, N#knm_phone_number.doc) of
        true -> N;
        false ->
            N#knm_phone_number{is_dirty = true
                              ,doc = Data
                              }
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec modified(knm_phone_number()) -> gregorian_seconds().
modified(#knm_phone_number{is_dirty = true}) -> kz_util:current_tstamp();
modified(#knm_phone_number{modified = Modified}) -> Modified.

-spec set_modified(knm_phone_number(), gregorian_seconds()) -> knm_phone_number().
set_modified(PN, Modified)
  when is_integer(Modified), Modified > 0 ->
    PN#knm_phone_number{modified=Modified}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec created(knm_phone_number()) -> gregorian_seconds().
created(#knm_phone_number{created=Created}) -> Created.

-spec set_created(knm_phone_number(), gregorian_seconds()) -> knm_phone_number().
set_created(PN, Created)
  when is_integer(Created), Created > 0 ->
    PN#knm_phone_number{created=Created}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_billable(knm_phone_number()) -> boolean().
is_billable(#knm_phone_number{is_billable = IsBillable}) -> IsBillable.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(knm_phone_number(), ne_binary()) -> {'ok', kz_json:object()} |
                                                           {'error', any()}.
list_attachments(PhoneNumber, AuthBy) ->
    AssignedTo = assigned_to(PhoneNumber),
    case state(PhoneNumber) == ?NUMBER_STATE_PORT_IN
        andalso kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true')
    of
        'true' -> {'ok', kz_doc:attachments(doc(PhoneNumber), kz_json:new())};
        'false' -> {'error', 'unauthorized'}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Sanitize phone number docs fields and remove deprecated fields
%% @end
%%--------------------------------------------------------------------
-spec sanitize_public_fields(kz_json:object()) -> kz_json:object().
sanitize_public_fields(JObj) ->
    Keys = [<<"id">>
           ,<<"used_by">>
           ],
    kz_json:delete_keys(Keys, kz_json:public_fields(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(knm_phone_number()) -> boolean().
-ifdef(TEST).
is_authorized(#knm_phone_number{auth_by = ?KNM_DEFAULT_AUTH_BY}) -> 'true';
is_authorized(#knm_phone_number{auth_by = 'undefined'}) -> 'false';
is_authorized(#knm_phone_number{assigned_to = 'undefined'
                               ,assign_to = AssignTo
                               ,auth_by = AuthBy
                               }) ->
    is_in_account_hierarchy(AuthBy, AssignTo);
is_authorized(#knm_phone_number{assigned_to = AssignedTo
                               ,auth_by = AuthBy
                               }) ->
    is_in_account_hierarchy(AuthBy, AssignedTo).
-else.
is_authorized(#knm_phone_number{auth_by = ?KNM_DEFAULT_AUTH_BY}) ->
    lager:info("bypassing auth"),
    'true';
is_authorized(#knm_phone_number{auth_by = 'undefined'}) -> 'false';
is_authorized(#knm_phone_number{assigned_to = 'undefined'
                               ,assign_to = 'undefined'
                               ,auth_by = AuthBy
                               }) ->
    lager:debug("assigns all undefined, checking if auth is super duper"),
    kz_util:is_system_admin(AuthBy);
is_authorized(#knm_phone_number{assigned_to = 'undefined'
                               ,assign_to = AssignTo
                               ,auth_by = AuthBy
                               }) ->
    is_in_account_hierarchy(AuthBy, AssignTo);
is_authorized(#knm_phone_number{assigned_to = AssignedTo
                               ,auth_by = AuthBy
                               }) ->
    is_in_account_hierarchy(AuthBy, AssignedTo).
-endif.

-spec is_in_account_hierarchy(ne_binary(), ne_binary()) -> boolean().
-ifdef(TEST).
is_in_account_hierarchy(AuthBy, AccountId) ->
    ?LOG_DEBUG("is authz ~s ~s", [AuthBy, AccountId]),
    (AccountId =:= ?RESELLER_ACCOUNT_ID
     orelse AccountId =:= ?MASTER_ACCOUNT_ID
    )
        andalso (AuthBy =:= ?RESELLER_ACCOUNT_ID
                 orelse AuthBy =:= ?MASTER_ACCOUNT_ID
                ).
-else.
is_in_account_hierarchy(AuthBy, AccountId) ->
    ?LOG_DEBUG("is authz ~s ~s", [AuthBy, AccountId]),
    kz_util:is_in_account_hierarchy(AuthBy, AccountId, 'true').
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_to_number_db(knm_phone_number()) -> knm_phone_number().
-ifdef(TEST).
save_to_number_db(PhoneNumber) -> PhoneNumber.
-else.
save_to_number_db(PhoneNumber) ->
    NumberDb = number_db(PhoneNumber),
    JObj = to_json(PhoneNumber),
    case datamgr_save(PhoneNumber, NumberDb, JObj) of
        {'ok', Doc} -> from_json_with_options(Doc, PhoneNumber);
        {'error', 'not_found'} ->
            lager:debug("creating new db '~s' for number '~s'", [NumberDb, number(PhoneNumber)]),
            'true' = kz_datamgr:db_create(NumberDb),
            {'ok',_} = kz_datamgr:revise_doc_from_file(NumberDb, ?APP, <<"views/numbers.json">>),
            save_to_number_db(PhoneNumber);
        {'error', E} ->
            lager:error("failed to save ~s in ~s: ~p", [number(PhoneNumber), NumberDb, E]),
            knm_errors:database_error(E, PhoneNumber)
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_assignment(knm_phone_number()) -> knm_phone_number().
handle_assignment(PhoneNumber) ->
    ?LOG_DEBUG("handling assignment for ~s", [number(PhoneNumber)]),
    unassign_from_prev(assign(PhoneNumber)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes sure number is assigned to assigned_to by creating number doc
%% that may not yet exist in AssignedTo DB.
%% @end
%%--------------------------------------------------------------------
-spec assign(knm_phone_number()) -> knm_phone_number().
assign(PhoneNumber) ->
    AssignedTo = assigned_to(PhoneNumber),
    case kz_util:is_empty(AssignedTo) of
        'true' -> PhoneNumber;
        'false' -> assign(PhoneNumber, AssignedTo)
    end.

-spec assign(knm_phone_number(), ne_binary()) -> knm_phone_number().
-ifdef(TEST).
assign(PhoneNumber, _AssignedTo) ->
    PhoneNumber.
-else.
assign(PhoneNumber, AssignedTo) ->
    AccountDb = kz_util:format_account_db(AssignedTo),
    case datamgr_save(PhoneNumber, AccountDb, to_json(PhoneNumber)) of
        {'error', E} ->
            lager:error("failed to assign number ~s to ~s", [number(PhoneNumber), AccountDb]),
            knm_errors:assign_failure(PhoneNumber, E);
        {'ok', JObj} ->
            lager:debug("assigned number ~s to ~s", [number(PhoneNumber), AccountDb]),
            from_json_with_options(JObj, PhoneNumber)
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes sure number is unassigned from prev_assigned_to by removing
%% number doc that may still exist in PrevAssignedTo DB.
%% @end
%%--------------------------------------------------------------------
-spec unassign_from_prev(knm_phone_number()) -> knm_phone_number().
unassign_from_prev(PhoneNumber) ->
    PrevAssignedTo = prev_assigned_to(PhoneNumber),
    case kz_util:is_empty(PrevAssignedTo) of
        'false' -> unassign_from_prev(PhoneNumber, PrevAssignedTo);
        'true' ->
            lager:debug("prev_assigned_to is empty for ~s, ignoring", [number(PhoneNumber)]),
            PhoneNumber
    end.

-spec unassign_from_prev(knm_phone_number(), ne_binary()) -> knm_phone_number().
-ifdef(TEST).
unassign_from_prev(PhoneNumber, _PrevAssignedTo) ->
    PhoneNumber.
-else.
unassign_from_prev(#knm_phone_number{assigned_to = PrevAssignedTo} = PhoneNumber
        ,PrevAssignedTo
        ) ->
    lager:debug("prev_assigned_to is same as assigned_to, not unassign-ing from prev"),
    PhoneNumber;
unassign_from_prev(PhoneNumber, PrevAssignedTo) ->
    Num = number(PhoneNumber),
    case get_number_in_account(PrevAssignedTo, Num) of
        {'ok', _} -> do_unassign_from_prev(PhoneNumber, PrevAssignedTo);
        {'error', 'not_found'} ->
            lager:debug("number ~s was not found in ~s, no need to unassign from prev"
                       ,[Num, PrevAssignedTo]),
            PhoneNumber;
        {'error', _R} -> do_unassign_from_prev(PhoneNumber, PrevAssignedTo)
    end.

-spec do_unassign_from_prev(knm_phone_number(), ne_binary()) -> knm_phone_number().
do_unassign_from_prev(PhoneNumber, PrevAssignedTo) ->
    PrevAccountDb = kz_util:format_account_db(PrevAssignedTo),
    case kz_datamgr:del_doc(PrevAccountDb, to_json(PhoneNumber)) of
        {'ok', _} ->
            lager:debug("successfully unassign_from_prev number ~s from ~s"
                       ,[number(PhoneNumber), PrevAssignedTo]),
            PhoneNumber;
        {'error', E} ->
            lager:error("failed to unassign from prev number ~s from ~s"
                       ,[number(PhoneNumber), PrevAssignedTo]),
            knm_errors:assign_failure(PhoneNumber, E)
    end.

-spec get_number_in_account(ne_binary(), ne_binary()) ->
                                   {'ok', kz_json:object()} |
                                   {'error', any()}.
get_number_in_account(AccountId, Num) ->
    AccountDb = kz_util:format_account_db(AccountId),
    kz_datamgr:open_cache_doc(AccountDb, Num).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete_number_doc(knm_phone_number()) -> knm_phone_number_return().
delete_number_doc(Number) ->
    NumberDb = number_db(Number),
    JObj = to_json(Number),
    case kz_datamgr:del_doc(NumberDb, JObj) of
        {'error', _R}=E -> E;
        {'ok', _} -> {'ok', Number}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_number_from_account(knm_phone_number()) -> knm_phone_number_return().
maybe_remove_number_from_account(Number) ->
    AssignedTo = assigned_to(Number),
    case kz_util:is_empty(AssignedTo) of
        'true' ->
            lager:debug("assigned_to is empty for ~s, ignoring", [number(Number)]),
            {'ok', Number};
        'false' ->
            case kz_datamgr:del_doc(kz_util:format_account_db(AssignedTo), to_json(Number)) of
                {'error', _R}=E -> E;
                {'ok', _} -> {'ok', Number}
            end
    end.

-ifndef(TEST).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec datamgr_save(knm_phone_number(), ne_binary(), kz_json:object()) ->
                          {'ok', kz_json:object()} |
                          kz_data:data_error().
datamgr_save(PhoneNumber, Db, JObj) ->
    case batch_run(PhoneNumber)
        andalso ?BULK_BATCH_WRITES
    of
        'false' -> kz_datamgr:ensure_saved(Db, JObj);
        'true' -> store_maybe_push(Db, JObj)
    end.

store_maybe_push(Db, Doc) ->
    BelowMax = kz_datamgr:max_bulk_insert() - 1,
    case get(Db) of
        'undefined' ->
            put(Db, 1),
            put({Db,1}, [Doc]),
            {'ok', Doc};
        BelowMax ->
            store_doc(Db, Doc, BelowMax),
            push_stored(Db, {Db, BelowMax + 1});
        Count ->
            store_doc(Db, Doc, Count),
            {'ok', Doc}
    end.

store_doc(Db, Doc, Count) ->
    NewCount = Count + 1,
    put(Db, NewCount),
    put({Db,NewCount}, [Doc | get({Db,Count})]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec push_stored() -> 'ok'.
push_stored() ->
    case ?BULK_BATCH_WRITES of
        true ->
            lager:debug("pushing stored writes"),
            DBs = [Db || {Db=?NE_BINARY, I} <- get(),
                         is_integer(I)
                  ],
            lists:foreach(fun push_stored/1, DBs);
        false ->
            ok
    end.

push_stored(Db) ->
    push_stored(Db, {Db, get(Db)}).

push_stored(Db, Key) ->
    Docs = get(Key),
    erase(Key),
    erase(Db),
    R = kz_datamgr:save_docs(Db, Docs),
    element(1, R) =:= 'error'
        andalso lager:debug("save_docs ~p failed: ~p"
                           ,[[kz_doc:id(Doc) || Doc <- Docs]
                            ,element(2,R)
                            ]),
    R.
-endif.
