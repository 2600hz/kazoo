%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz INC
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
        ,new/1
        ]).

-export([to_json/1
        ,to_public_json/1
        ,from_json/1, from_json_with_options/2
        ,from_number/1, from_number_with_options/2
        ,is_phone_number/1
        ]).

-export([setters/2
        ,number/1
        ,number_db/1
        ,assign_to/1, set_assign_to/2
        ,assigned_to/1, set_assigned_to/2
        ,prev_assigned_to/1, set_prev_assigned_to/2
        ,used_by/1, set_used_by/2
        ,features/1, features_list/1, set_features/2
        ,feature/2, set_feature/3
        ,features_allowed/1, features_denied/1
        ,state/1, set_state/2
        ,reserve_history/1, add_reserve_history/2, unwind_reserve_history/1
        ,ported_in/1, set_ported_in/2
        ,module_name/1, set_module_name/2
        ,carrier_data/1, set_carrier_data/2, update_carrier_data/2
        ,region/1, set_region/2
        ,auth_by/1, set_auth_by/2
        ,is_authorized/1, is_admin/1
        ,dry_run/1, set_dry_run/2
        ,batch_run/1, set_batch_run/2
        ,mdn_run/1, set_mdn_run/2
        ,locality/1, set_locality/2
        ,doc/1, update_doc/2, reset_doc/2
        ,modified/1, set_modified/2
        ,created/1, set_created/2
        ]).

-export([list_attachments/2]).

-ifndef(TEST).
-export([push_stored/0]).
-endif.

-include("knm.hrl").
-include_lib("kazoo_json/include/kazoo_json.hrl").

%% Used by from_json/1
-define(DEFAULT_FEATURES, kz_json:new()).
-define(DEFAULT_RESERVE_HISTORY, []).
-define(DEFAULT_PORTED_IN, false).
-define(DEFAULT_MODULE_NAME, knm_carriers:default_carrier()).
-define(DEFAULT_CARRIER_DATA, kz_json:new()).
-define(DEFAULT_DOC, kz_json:new()).
-define(DEFAULT_FEATURES_ALLOWED, []).
-define(DEFAULT_FEATURES_DENIED, []).

-record(knm_phone_number, {number :: ne_binary()
                          ,number_db :: ne_binary()
                          ,assign_to :: api_ne_binary()
                          ,assigned_to :: api_ne_binary()
                          ,prev_assigned_to :: api_ne_binary()
                          ,used_by :: api_ne_binary()
                          ,features :: kz_json:object()
                          ,state :: ne_binary()
                          ,reserve_history :: ne_binaries()
                          ,ported_in :: boolean()
                          ,module_name :: ne_binary()
                          ,carrier_data :: kz_json:object()
                          ,region :: api_ne_binary()
                          ,auth_by :: api_ne_binary()
                          ,dry_run = 'false' :: boolean()
                          ,batch_run = 'false' :: boolean()
                          ,mdn_run = false :: boolean()
                          ,locality :: api_object()
                          ,doc :: kz_json:object()
                          ,modified :: gregorian_seconds()
                          ,created :: gregorian_seconds()
                          ,is_dirty = 'false' :: boolean()
                          ,features_allowed :: ne_binaries()
                          ,features_denied :: ne_binaries()
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

-define(PORT_IN_MODULE_NAME,
        kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"port_in_module_name">>, ?CARRIER_LOCAL)).

-define(DIRTY(PN), begin
                       lager:debug("dirty"),
                       PN#knm_phone_number{is_dirty = true}
                   end).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec new(knm_numbers:collection()) -> knm_numbers:collection().
new(T=#{todo := Nums, options := Options}) ->
    Setters = new_setters(Options),
    PNs = [do_new(DID, Setters) || DID <- Nums],
    knm_numbers:ok(PNs, T).

-spec new_setters(knm_number_options:options()) -> set_functions().
new_setters(Options) ->
    knm_number_options:to_phone_number_setters(
      case knm_number_options:state(Options) of
          ?NUMBER_STATE_PORT_IN -> [{'module_name', ?PORT_IN_MODULE_NAME} | Options];
          _ -> Options
      end
     ).

-spec do_new(ne_binary(), set_functions()) -> knm_phone_number().
do_new(DID, Setters) ->
    {ok, PN} = setters(from_number(DID), Setters),
    PN.

%% @public
-spec from_number(ne_binary()) -> knm_phone_number().
from_number(DID) ->
    from_json(kz_doc:set_id(kz_json:new(), DID)).

%% @public
-spec from_number_with_options(ne_binary(), knm_number_options:options()) -> knm_phone_number().
from_number_with_options(DID, Options) ->
    do_new(DID, new_setters(Options)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> knm_phone_number_return();
           (knm_numbers:collection()) -> knm_numbers:collection().
-spec fetch(ne_binary(), knm_number_options:options()) -> knm_phone_number_return().

fetch(?NE_BINARY=Num) ->
    fetch(Num, knm_number_options:default());
fetch(T0=#{todo := Nums, options := Options}) ->
    Pairs = group_by_db(lists:usort([knm_converters:normalize(Num) || Num <- Nums])),
    F = fun ({NumberDb, NormalizedNums}, T) ->
                case fetch_in(NumberDb, NormalizedNums, Options) of
                    {error, R} ->
                        lager:warning("bulk read failed (~p): ~p", [R, NormalizedNums]),
                        knm_numbers:ko(NormalizedNums, R, T);
                    {ok, JObjs} when is_list(JObjs) -> bulk_fetch(T, JObjs);
                    {ok, JObj} -> do_handle_fetch(T, JObj)
                end
        end,
    lists:foldl(F, T0, Pairs).

-ifdef(TEST).
fetch_in(NumberDb, Nums, _Options) ->
    true = lists:all(fun (Num) -> NumberDb =:= knm_converters:to_db(Num) end, Nums),
    {ok, [test_fetch_in(Num) || Num <- Nums]}.

test_fetch_in(Num) ->
    Data = case test_fetch(Num) of
               {error, not_found} -> {<<"error">>, <<"not_found">>};
               {ok, JObj} -> {<<"doc">>, JObj}
           end,
    kz_json:from_list([{<<"key">>, Num}, Data]).
-else.
fetch_in(NumberDb, [Num], Options) ->
    fetch(NumberDb, Num, Options);
fetch_in(NumberDb, Nums, Options) ->
    case knm_number_options:batch_run(Options) of
        true -> kz_datamgr:open_docs(NumberDb, Nums);
        false -> kz_datamgr:open_cache_docs(NumberDb, Nums)
    end.
-endif.

bulk_fetch(T0, JObjs) ->
    F = fun (JObj, T) ->
                Num = kz_json:get_ne_value(<<"key">>, JObj),
                case kz_json:get_ne_value(<<"doc">>, JObj) of
                    undefined ->
                        R = kz_json:get_ne_value(<<"error">>, JObj),
                        knm_numbers:ko(Num, kz_term:to_atom(R, true), T);
                    Doc ->
                        do_handle_fetch(T, Doc)
                end
        end,
    lists:foldl(F, T0, JObjs).

do_handle_fetch(T=#{options := Options}, Doc) ->
    case knm_number:attempt(fun handle_fetch/2, [Doc, Options]) of
        {ok, PN} -> knm_numbers:ok(PN, T);
        {error, R} -> knm_numbers:ko(kz_doc:id(Doc), R, T)
    end.

group_by_db(Nums) ->
    F = fun (Num, M) ->
                Key = knm_converters:to_db(Num),
                M#{Key => [Num | maps:get(Key, M, [])]}
        end,
    maps:to_list(lists:foldl(F, #{}, Nums)).

-ifdef(TEST).
fetch(Num, Options) ->
    case test_fetch(Num) of
        {error, _}=E -> E;
        {ok, JObj} -> handle_fetch(JObj, Options)
    end.

test_fetch(?TEST_CREATE_NUM) ->
    {error, not_found};
test_fetch(?TEST_AVAILABLE_NUM) ->
    {ok, ?AVAILABLE_NUMBER};
test_fetch(?TEST_IN_SERVICE_BAD_CARRIER_NUM) ->
    {ok, ?IN_SERVICE_BAD_CARRIER_NUMBER};
test_fetch(?TEST_IN_SERVICE_NUM) ->
    {ok, ?IN_SERVICE_NUMBER};
test_fetch(?TEST_IN_SERVICE_MDN) ->
    {ok, ?IN_SERVICE_MDN};
test_fetch(?TEST_IN_SERVICE_WITH_HISTORY_NUM) ->
    {ok, ?IN_SERVICE_WITH_HISTORY_NUMBER};
test_fetch(?BW_EXISTING_DID) ->
    {ok, ?BW_EXISTING_JSON};
test_fetch(?TEST_TELNYX_NUM) ->
    {ok, ?TELNYX_NUMBER};
test_fetch(?TEST_OLD_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_1_in.json")))};
test_fetch(?TEST_OLD2_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_2_in.json")))};
test_fetch(?TEST_OLD3_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_3_in.json")))};
test_fetch(?TEST_OLD4_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_4_in.json")))};
test_fetch(?TEST_OLD5_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_5_in.json")))};
test_fetch(?TEST_OLD6_NUM) ->
    {ok, kz_json:decode(list_to_binary(knm_util:fixture("old_vsn_6_in.json")))};
test_fetch(_DID=?NE_BINARY) ->
    {error, not_found}.
-else.
fetch(Num=?NE_BINARY, Options) ->
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),
    case fetch(NumberDb, NormalizedNum, Options) of
        {'ok', JObj} -> handle_fetch(JObj, Options);
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

-spec handle_fetch(kz_json:object(), knm_number_options:options()) -> {ok, knm_phone_number()}.
handle_fetch(JObj, Options) ->
    PN = from_json_with_options(JObj, Options),
    case is_authorized(PN)
        andalso is_mdn_for_mdn_run(PN, Options)
    of
        true -> {ok, PN};
        false -> knm_errors:unauthorized()
    end.

is_mdn_for_mdn_run(#knm_phone_number{auth_by = ?KNM_DEFAULT_AUTH_BY}, _) ->
    lager:debug("mdn check disabled by auth_by"),
    true;
is_mdn_for_mdn_run(PN, Options) ->
    IsMDN = ?CARRIER_MDN =:= module_name(PN),
    %% Equal to: IsMDN xnor IsMDNRun
    case knm_number_options:mdn_run(Options) of
        false -> not IsMDN;
        true ->
            _ = IsMDN
                andalso lager:debug("~s is an mdn", [number(PN)]),
            IsMDN
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_phone_number()) -> knm_phone_number();
          (knm_numbers:collection()) -> knm_numbers:collection().
save(T0=#{todo := PNs, options := Options}) ->
    case knm_number_options:dry_run(Options) of
        true ->
            lager:debug("dry_run-ing btw"),
            knm_numbers:ok(PNs, T0);
        false ->
            F = fun (PN, T) ->
                        case knm_number:attempt(fun save/1, [PN]) of
                            {error, R} -> knm_numbers:ko(number(PN), R, T);
                            NewPN -> knm_numbers:ok(NewPN, T)
                        end
                end,
            lists:foldl(F, T0, PNs)
    end;
save(PN=#knm_phone_number{is_dirty = false}) ->
    lager:debug("not dirty, skip saving ~s", [number(PN)]),
    PN;
save(PN) ->
    Routines = [fun save_to_number_db/1
               ,fun handle_assignment/1
               ],
    {'ok', NewPN} = setters(PN, Routines),
    NewPN.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% To call only from knm_numbers:delete/2 (only for sysadmins).
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number()) -> knm_phone_number();
            (knm_numbers:collection()) -> knm_numbers:collection().
delete(T0=#{todo := PNs, options := Options}) ->
    case knm_number_options:dry_run(Options) of
        true ->
            lager:debug("dry_run-ing btw"),
            knm_numbers:ok(PNs, T0);
        false ->
            F = fun (PN, T) ->
                        case knm_number:attempt(fun delete/1, [PN]) of
                            {error, R} -> knm_numbers:ko(number(PN), R, T);
                            NewPN -> knm_numbers:ok(NewPN, T)
                        end
                end,
            lists:foldl(F, T0, PNs)
    end;
delete(PN) ->
    lager:debug("deleting permanently ~s", [number(PN)]),
    Routines = [fun try_delete_number_doc/1
               ,fun try_maybe_remove_number_from_account/1
               ,{fun set_state/2, ?NUMBER_STATE_DELETED}
               ],
    {'ok', NewPN} = setters(PN, Routines),
    NewPN.

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

-spec release(knm_phone_number()) -> knm_phone_number();
             (knm_numbers:collection()) -> knm_numbers:collection().
-spec release(knm_phone_number(), ne_binary()) -> knm_phone_number().
release(T0=#{todo := PNs}) ->
    F = fun (PN, T) ->
                case knm_number:attempt(fun release/1, [PN]) of
                    {error, Reason} -> knm_numbers:ko(number(PN), Reason, T);
                    NewPN -> knm_numbers:ok(NewPN, T)
                end
        end,
    lists:foldl(F, T0, PNs);
release(PN) ->
    release(PN, state(PN)).

release(PN, ?NUMBER_STATE_RELEASED) -> PN;
release(PN, ?NUMBER_STATE_RESERVED) -> authorize_release(PN);
release(PN, ?NUMBER_STATE_PORT_IN) -> authorize_release(PN);
release(PN, ?NUMBER_STATE_IN_SERVICE) -> authorize_release(PN);
release(PN, FromState) ->
    To = ?NUMBER_STATE_RELEASED,
    knm_errors:invalid_state_transition(PN, FromState, To).

-spec authorize_release(knm_phone_number()) -> knm_phone_number().
-spec authorize_release(knm_phone_number(), ne_binary()) -> knm_phone_number().
authorize_release(PN) ->
    authorize_release(PN, auth_by(PN)).

-ifdef(TEST).
authorize_release(PN, ?KNM_DEFAULT_AUTH_BY) -> authorized_release(PN);
authorize_release(PN, ?MASTER_ACCOUNT_ID) -> authorized_release(PN);
authorize_release(_PN, _AuthBy) -> knm_errors:unauthorized().
-else.
authorize_release(PN, ?KNM_DEFAULT_AUTH_BY) ->
    lager:info("bypassing auth"),
    authorized_release(PN);
authorize_release(PN, AuthBy) ->
    AssignedTo = assigned_to(PN),
    case kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true') of
        'false' -> knm_errors:unauthorized();
        'true' -> authorized_release(PN)
    end.
-endif.

-spec authorized_release(knm_phone_number()) -> knm_phone_number().
authorized_release(PN) ->
    ReleasedState = knm_config:released_state(?NUMBER_STATE_AVAILABLE),
    Routines = [{fun set_features/2, ?DEFAULT_FEATURES}
               ,{fun set_doc/2, kz_json:private_fields(doc(PN))}
               ,{fun set_prev_assigned_to/2, assigned_to(PN)}
               ,{fun set_assigned_to/2, undefined}
               ,{fun set_state/2, ReleasedState}
               ],
    {'ok', NewPN} = setters(PN, Routines),
    NewPN.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns same fields view phone_numbers.json returns.
%% @end
%%--------------------------------------------------------------------
-spec to_public_json(knm_phone_number()) -> kz_json:object().
to_public_json(PN) ->
    JObj = to_json(PN),
    State = {<<"state">>, state(PN)},
    UsedBy = {<<"used_by">>, used_by(PN)},
    Features = {<<"features">>, features_list(PN)},
    ReadOnly =
        kz_json:from_list(
          props:filter_empty(
            [{<<"created">>, kz_doc:created(JObj)}
            ,{<<"modified">>, kz_doc:modified(JObj)}
            ,State
            ,UsedBy
            ,Features
            ,{<<"features_available">>, knm_providers:available_features(PN)}
            ,{<<"features_allowed">>, features_allowed(PN)}
            ,{<<"features_denied">>, features_denied(PN)}
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
to_json(PN=#knm_phone_number{doc=JObj}) ->
    kz_json:from_list(
      [{<<"_id">>, number(PN)}
      ,{?PVT_DB_NAME, number_db(PN)}
      ,{?PVT_STATE, state(PN)}
      ,{?PVT_PORTED_IN, ported_in(PN)}
      ,{?PVT_MODULE_NAME, module_name(PN)}
      ,{?PVT_MODIFIED, modified(PN)}
      ,{?PVT_CREATED, created(PN)}
      ,{?PVT_TYPE, <<"number">>}
       | kz_json:to_proplist(sanitize_public_fields(JObj))
      ]
      ++
          props:filter_empty(
            [{?PVT_ASSIGNED_TO, assigned_to(PN)}
            ,{?PVT_AUTH_BY, auth_by(PN)}
            ,{?PVT_PREVIOUSLY_ASSIGNED_TO, prev_assigned_to(PN)}
            ,{?PVT_USED_BY, used_by(PN)}
            ,{?PVT_FEATURES, features(PN)}
            ,{?PVT_FEATURES_ALLOWED, features_allowed(PN)}
            ,{?PVT_FEATURES_DENIED, features_denied(PN)}
            ,{?PVT_RESERVE_HISTORY, reserve_history(PN)}
            ,{?PVT_CARRIER_DATA, carrier_data(PN)}
            ,{?PVT_REGION, region(PN)}
            ])
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec from_json(kz_json:object()) -> knm_phone_number().
from_json(JObj0) ->
    JObj = maybe_update_rw_features(JObj0),
    Features =
        case kz_json:get_ne_value(?PVT_FEATURES, JObj) of
            'undefined' -> ?DEFAULT_FEATURES;
            FeaturesList when is_list(FeaturesList) -> migrate_features(FeaturesList, JObj);
            FeaturesJObj -> FeaturesJObj
        end,
    {ok, PN} =
        setters(#knm_phone_number{}
               ,[{fun set_number/2, knm_converters:normalize(kz_doc:id(JObj))}
                ,{fun set_assigned_to/3
                 ,kz_json:get_value(?PVT_ASSIGNED_TO, JObj)
                 ,kz_json:get_value(?PVT_USED_BY, JObj)
                 }
                ,{fun set_prev_assigned_to/2, kz_json:get_value(?PVT_PREVIOUSLY_ASSIGNED_TO, JObj)}
                ,{fun set_features/2, maybe_rename_features(Features)}
                ,{fun set_state/2, kz_json:get_first_defined([?PVT_STATE, ?PVT_STATE_LEGACY], JObj)}
                ,{fun set_reserve_history/2, kz_json:get_value(?PVT_RESERVE_HISTORY, JObj, ?DEFAULT_RESERVE_HISTORY)}
                ,{fun set_ported_in/2, kz_json:is_true(?PVT_PORTED_IN, JObj, ?DEFAULT_PORTED_IN)}
                ,{fun set_module_name/2, kz_json:get_value(?PVT_MODULE_NAME, JObj, ?DEFAULT_MODULE_NAME)}
                ,{fun set_carrier_data/2, kz_json:get_value(?PVT_CARRIER_DATA, JObj, ?DEFAULT_CARRIER_DATA)}
                ,{fun set_region/2, kz_json:get_value(?PVT_REGION, JObj)}
                ,{fun set_auth_by/2, kz_json:get_value(?PVT_AUTH_BY, JObj)}
                ,{fun set_doc/2, sanitize_public_fields(JObj)}
                ,{fun set_modified/2, kz_doc:modified(JObj)}
                ,{fun set_created/2, kz_doc:created(JObj)}
                ,{fun set_features_allowed/2, kz_json:get_list_value(?PVT_FEATURES_ALLOWED, JObj, ?DEFAULT_FEATURES_ALLOWED)}
                ,{fun set_features_denied/2, kz_json:get_list_value(?PVT_FEATURES_DENIED, JObj, ?DEFAULT_FEATURES_DENIED)}
                ]),
    PN.

%% Handle moving away from provider-specific E911
maybe_rename_features(Features) ->
    Fs = kz_json:delete_keys([?LEGACY_DASH_E911, ?LEGACY_VITELITY_E911], Features),
    case {kz_json:get_ne_value(?LEGACY_DASH_E911, Features)
         ,kz_json:get_ne_value(?LEGACY_VITELITY_E911, Features)
         }
    of
        {undefined, undefined} -> Features;
        {Dash, undefined} -> kz_json:set_value(?FEATURE_E911, Dash, Fs);
        {undefined, Vitelity} -> kz_json:set_value(?FEATURE_E911, Vitelity, Fs);
        {_Dash, Vitelity} -> kz_json:set_value(?FEATURE_E911, Vitelity, Fs)
    end.

maybe_update_rw_features(JObj) ->
    case {kz_json:get_ne_value(?LEGACY_DASH_E911, JObj)
         ,kz_json:get_ne_value(?LEGACY_VITELITY_E911, JObj)
         }
    of
        {undefined, undefined} -> JObj;
        {Dash, undefined} -> kz_json:set_value(?FEATURE_E911, Dash, JObj);
        {undefined, Vitelity} -> kz_json:set_value(?FEATURE_E911, Vitelity, JObj);
        {_Dash, Vitelity} -> kz_json:set_value(?FEATURE_E911, Vitelity, JObj)
    end.

%% Handle 3.22 -> 4.0 features migration.
migrate_features(FeaturesList, JObj) ->
    F = fun (Feature, A) -> features_fold(Feature, A, JObj) end,
    lists:foldl(F, ?DEFAULT_FEATURES, FeaturesList).

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
    Data = kz_json:from_list([{<<"enabled">>, kz_json:is_true([?FEATURE_PREPEND, <<"enabled">>], JObj)}
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
features_fold(?LEGACY_DASH_E911=Feature, Acc, JObj) ->
    Data = kz_json:get_value(Feature, JObj),
    kz_json:set_value(?FEATURE_E911, Data, Acc);
features_fold(?LEGACY_VITELITY_E911=Feature, Acc, JObj) ->
    Data = kz_json:get_value(Feature, JObj),
    kz_json:set_value(?FEATURE_E911, Data, Acc);
features_fold(?LEGACY_TELNYX_E911=Feature, Acc, JObj) ->
    Data = kz_json:get_value(Feature, JObj),
    kz_json:set_value(?FEATURE_E911, Data, Acc);
features_fold(FeatureKey, Acc, JObj) ->
    %% Encompasses at least:
    %%   ?FEATURE_PORT & ?FEATURE_FAILOVER
    Data = kz_json:get_ne_value(FeatureKey, JObj, kz_json:new()),
    lager:debug("emcompassed ~s", [FeatureKey, kz_json:encode(Data)]),
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
               %% See knm_number_options:default/0 for these 4.
              ,{fun set_dry_run/2, knm_number_options:dry_run(Options, 'false')}
              ,{fun set_batch_run/2, knm_number_options:batch_run(Options, 'false')}
              ,{fun set_mdn_run/2, knm_number_options:mdn_run(Options)}
              ,{fun set_auth_by/2, knm_number_options:auth_by(Options, ?KNM_DEFAULT_AUTH_BY)}
              ],
    {'ok', PN} = setters(from_json(JObj), Updates),
    PN;
from_json_with_options(JObj, PN) ->
    Options = [{'dry_run', dry_run(PN)}
              ,{'batch_run', batch_run(PN)}
              ,{mdn_run, mdn_run(PN)}
              ,{'auth_by', auth_by(PN)}
              ],
    from_json_with_options(JObj, Options).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_phone_number(any()) -> boolean().
is_phone_number(#knm_phone_number{}) -> 'true';
is_phone_number(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec setters(knm_phone_number(), set_functions()) -> knm_phone_number_return();
             (knm_numbers:collection(), set_functions()) -> knm_numbers:collection().
setters(T0=#{todo := PNs}, Routines) ->
    F = fun (PN, T) ->
                case setters(PN, Routines) of
                    {ok, NewPN} -> knm_numbers:ok(NewPN, T);
                    {error, R} -> knm_numbers:ko(number(PN), R, T)
                end
        end,
    lists:foldl(F, T0, PNs);
setters(PN, Routines) ->
    try lists:foldl(fun setters_fold/2, PN, Routines) of
        {'ok', _N}=Ok -> Ok;
        {'error', _R}=Error -> Error;
        Result -> {'ok', Result}
    catch
        'throw':{'stop', Error} -> Error;
        'error':'function_clause' ->
            ST = erlang:get_stacktrace(),
            {FName, Arg} =
                case ST of
                    [{lists, foldl, [Name|_aPN], Arg2}|_] -> {Name, Arg2};
                    [{_M, Name, [_aPN,Arg2|_], _Info}|_] -> {Name, Arg2}
                end,
            lager:error("~s failed, argument: ~p", [FName, Arg]),
            kz_util:log_stacktrace(ST),
            {'error', FName};
        'error':Reason ->
            kz_util:log_stacktrace(),
            {'error', Reason}
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
setters_fold({Fun, Key, Value}, PN) when is_function(Fun, 3) ->
    setters_fold_apply(Fun, [PN, Key, Value]);
setters_fold({Fun, Value}, PN) when is_function(Fun, 2) ->
    setters_fold_apply(Fun, [PN, Value]);
setters_fold(Fun, PN) when is_function(Fun, 1) ->
    setters_fold_apply(Fun, [PN]).

-spec setters_fold_apply(set_function(), nonempty_list()) -> setter_acc().
setters_fold_apply(Fun, [{'ok',PN}|Args]) ->
    setters_fold_apply(Fun, [PN|Args]);
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
set_number(PN, <<"+",_:8,_/binary>>=NormalizedNum) ->
    NumberDb = knm_converters:to_db(NormalizedNum),
    case {PN#knm_phone_number.number, PN#knm_phone_number.number_db} of
        {undefined, undefined} ->
            PN#knm_phone_number{number = NormalizedNum
                               ,number_db = NumberDb
                               };
        {NormalizedNum, NumberDb} -> PN;
        _ ->
            ?DIRTY(PN#knm_phone_number{number = NormalizedNum
                                      ,number_db = NumberDb
                                      })
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

%% This is not stored on number doc
-spec set_assign_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_assign_to(PN=#knm_phone_number{assign_to = V}, V) -> PN;
set_assign_to(PN, AssignTo=undefined) ->
    PN#knm_phone_number{assign_to = AssignTo};
set_assign_to(PN, AssignTo=?MATCH_ACCOUNT_RAW(_)) ->
    PN#knm_phone_number{assign_to = AssignTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec assigned_to(knm_phone_number()) -> api_ne_binary().
assigned_to(#knm_phone_number{assigned_to=AssignedTo}) ->
    AssignedTo.

-spec set_assigned_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_assigned_to(PN=#knm_phone_number{assigned_to = V}, V) -> PN;
set_assigned_to(PN, AssignedTo='undefined') ->
    ?DIRTY(PN#knm_phone_number{assigned_to = AssignedTo
                              ,used_by = 'undefined'
                              });
set_assigned_to(PN, AssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    ?DIRTY(PN#knm_phone_number{assigned_to = AssignedTo
                              ,used_by = 'undefined'
                              }).

%% This is used only by from_json/1
-spec set_assigned_to(knm_phone_number(), api_ne_binary(), api_ne_binary()) -> knm_phone_number().
set_assigned_to(PN0, AssignedTo='undefined', UsedBy) ->
    PN = set_used_by(PN0, UsedBy),
    PN#knm_phone_number{assigned_to = AssignedTo};
set_assigned_to(PN0, AssignedTo=?MATCH_ACCOUNT_RAW(_), UsedBy) ->
    PN = set_used_by(PN0, UsedBy),
    PN#knm_phone_number{assigned_to = AssignedTo}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec prev_assigned_to(knm_phone_number()) -> api_ne_binary().
prev_assigned_to(#knm_phone_number{prev_assigned_to=PrevAssignedTo}) ->
    PrevAssignedTo.

-spec set_prev_assigned_to(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_prev_assigned_to(PN=#knm_phone_number{prev_assigned_to = V}, V) -> PN;
set_prev_assigned_to(PN=#knm_phone_number{prev_assigned_to = undefined}
                    ,PrevAssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    PN#knm_phone_number{prev_assigned_to = PrevAssignedTo};
set_prev_assigned_to(PN, PrevAssignedTo='undefined') ->
    ?DIRTY(PN#knm_phone_number{prev_assigned_to = PrevAssignedTo});
set_prev_assigned_to(PN, PrevAssignedTo=?MATCH_ACCOUNT_RAW(_)) ->
    ?DIRTY(PN#knm_phone_number{prev_assigned_to = PrevAssignedTo}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec used_by(knm_phone_number()) -> api_ne_binary().
used_by(#knm_phone_number{used_by=UsedBy}) -> UsedBy.

-spec set_used_by(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_used_by(PN=#knm_phone_number{used_by = V}, V) -> PN;
set_used_by(PN=#knm_phone_number{used_by = undefined}, UsedBy=?NE_BINARY) ->
    PN#knm_phone_number{used_by = UsedBy};
set_used_by(PN, UsedBy='undefined') ->
    lager:debug("unassigning ~s from ~s", [number(PN), PN#knm_phone_number.used_by]),
    ?DIRTY(PN#knm_phone_number{used_by = UsedBy});
set_used_by(PN, UsedBy=?NE_BINARY) ->
    lager:debug("assigning ~s to ~s from ~s", [number(PN), UsedBy, PN#knm_phone_number.used_by]),
    ?DIRTY(PN#knm_phone_number{used_by = UsedBy}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec features(knm_phone_number()) -> kz_json:object().
features(#knm_phone_number{features=Features}) -> Features.

-spec features_list(knm_phone_number()) -> ne_binaries().
features_list(PN) ->
    lists:usort(kz_json:get_keys(features(PN))).

-spec set_features(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_features(PN=#knm_phone_number{features = undefined}, Features) ->
    true = kz_json:is_json_object(Features),
    PN#knm_phone_number{features = Features};
set_features(PN, Features) ->
    'true' = kz_json:is_json_object(Features),
    case kz_json:are_equal(PN#knm_phone_number.features, Features) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{features = Features})
    end.

-spec feature(knm_phone_number(), ne_binary()) -> kz_json:api_json_term().
feature(PN, Feature) ->
    kz_json:get_ne_value(Feature, features(PN)).

-spec set_feature(knm_phone_number(), ne_binary(), kz_json:json_term()) ->
                         knm_phone_number().
set_feature(PN0, Feature=?NE_BINARY, Data) ->
    Features = kz_json:set_value(Feature, Data, features(PN0)),
    PN = set_features(PN0, Features),
    PN#knm_phone_number.is_dirty
        andalso lager:debug("setting ~s feature ~s: ~s", [number(PN), Feature, kz_json:encode(Data)]),
    PN.


-spec set_features_allowed(knm_phone_number(), ne_binaries()) -> knm_phone_number().
set_features_allowed(PN=#knm_phone_number{features_allowed = undefined}, Features) ->
    true = lists:all(fun kz_term:is_ne_binary/1, Features),
    PN#knm_phone_number{features_allowed = Features};
set_features_allowed(PN, Features) ->
    true = lists:all(fun kz_term:is_ne_binary/1, Features),
    case lists:usort(PN#knm_phone_number.features_allowed) =:= lists:usort(Features) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{features_allowed = Features})
    end.

-spec set_features_denied(knm_phone_number(), ne_binaries()) -> knm_phone_number().
set_features_denied(PN=#knm_phone_number{features_denied = undefined}, Features) ->
    true = lists:all(fun kz_term:is_ne_binary/1, Features),
    PN#knm_phone_number{features_denied = Features};
set_features_denied(PN, Features) ->
    true = lists:all(fun kz_term:is_ne_binary/1, Features),
    case lists:usort(PN#knm_phone_number.features_denied) =:= lists:usort(Features) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{features_denied = Features})
    end.

-spec features_allowed(knm_phone_number()) -> ne_binaries().
-ifdef(TEST).
features_allowed(#knm_phone_number{number = ?TEST_TELNYX_NUM}) ->
    [<<"cnam">>, <<"e911">>, <<"failover">>, <<"force_outbound">>
    ,<<"prepend">>, <<"ringback">>, <<"carrier_name">>];
features_allowed(#knm_phone_number{features_allowed = Features}) -> Features.
-else.
features_allowed(#knm_phone_number{features_allowed = Features}) -> Features.
-endif.

-spec features_denied(knm_phone_number()) -> ne_binaries().
-ifdef(TEST).
features_denied(#knm_phone_number{number = ?TEST_TELNYX_NUM}) ->
    [<<"port">>, <<"failover">>];
features_denied(#knm_phone_number{features_denied = Features}) -> Features.
-else.
features_denied(#knm_phone_number{features_denied = Features}) -> Features.
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec state(knm_phone_number()) -> api_ne_binary().
state(#knm_phone_number{state=State}) -> State.

-spec set_state(knm_phone_number(), ne_binary()) -> knm_phone_number().
set_state(PN=#knm_phone_number{state = V}, V) -> PN;
set_state(PN=#knm_phone_number{state = undefined}, State) ->
    true = is_state(State),
    PN#knm_phone_number{state = State};
set_state(PN, State) ->
    true = is_state(State),
    lager:debug("updating state from ~s to ~s", [PN#knm_phone_number.state, State]),
    ?DIRTY(PN#knm_phone_number{state = State}).

is_state(State)
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
       -> true;
is_state(_) -> false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec reserve_history(knm_phone_number()) -> ne_binaries().
reserve_history(#knm_phone_number{reserve_history=History}) -> History.

-spec set_reserve_history(knm_phone_number(), ne_binaries()) -> knm_phone_number().
set_reserve_history(PN0, History) when is_list(History) ->
    Cons = fun add_reserve_history/2,
    lists:foldr(Cons, PN0#knm_phone_number{reserve_history=?DEFAULT_RESERVE_HISTORY}, History).

-spec add_reserve_history(api_ne_binary(), knm_phone_number()) -> knm_phone_number().
add_reserve_history(undefined, PN) -> PN;
add_reserve_history(?MATCH_ACCOUNT_RAW(AccountId)
                   ,PN=#knm_phone_number{reserve_history=[AccountId|_]}
                   ) -> PN;
add_reserve_history(?MATCH_ACCOUNT_RAW(AccountId)
                   ,PN=#knm_phone_number{reserve_history=ReserveHistory}
                   ) ->
    ?DIRTY(PN#knm_phone_number{reserve_history=[AccountId|ReserveHistory]}).

-spec unwind_reserve_history(knm_phone_number()) -> knm_phone_number();
                            (knm_numbers:collection()) -> knm_numbers:collection().
unwind_reserve_history(T=#{todo := PNs}) ->
    NewPNs = [unwind_reserve_history(PN) || PN <- PNs],
    knm_numbers:ok(NewPNs, T);
unwind_reserve_history(PN) ->
    ReserveHistory = reserve_history(PN),
    case lists:delete(prev_assigned_to(PN), ReserveHistory) of
        ReserveHistory -> PN;
        NewReserveHistory -> set_reserve_history(PN, NewReserveHistory)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ported_in(knm_phone_number()) -> boolean().
ported_in(#knm_phone_number{ported_in=Ported}) -> Ported.

-spec set_ported_in(knm_phone_number(), boolean()) -> knm_phone_number().
set_ported_in(PN=#knm_phone_number{ported_in = V}, V) -> PN;
set_ported_in(PN=#knm_phone_number{ported_in = undefined}, Ported)
  when is_boolean(Ported) ->
    PN#knm_phone_number{ported_in = Ported};
set_ported_in(PN, Ported) when is_boolean(Ported) ->
    lager:debug("updating ported_in from ~s to ~s", [PN#knm_phone_number.ported_in, Ported]),
    ?DIRTY(PN#knm_phone_number{ported_in = Ported}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec module_name(knm_phone_number()) -> api_ne_binary().
module_name(#knm_phone_number{module_name = Name}) -> Name.

-spec set_module_name(knm_phone_number(), ne_binary()) -> knm_phone_number().
%% knm_bandwidth is deprecated, updating to the new module
set_module_name(PN, <<"wnm_bandwidth">>) ->
    set_module_name(PN, <<"knm_bandwidth2">>);
set_module_name(PN, <<"wnm_", Name/binary>>) ->
    set_module_name(PN, <<"knm_", Name/binary>>);
%% Some old docs have these as module name
set_module_name(PN, <<"undefined">>) ->
    set_module_name(PN, ?CARRIER_LOCAL);
set_module_name(PN, 'undefined') ->
    set_module_name(PN, ?CARRIER_LOCAL);

set_module_name(PN, ?CARRIER_LOCAL=Name) ->
    set_module_name_local(PN, Name);
set_module_name(PN, ?CARRIER_MDN=Name) ->
    set_module_name_local(PN, Name);

set_module_name(PN=#knm_phone_number{module_name = Name}, Name=?NE_BINARY) -> PN;

set_module_name(PN0=#knm_phone_number{module_name = undefined}, Name=?NE_BINARY) ->
    PN = PN0#knm_phone_number{module_name = Name},
    Features = kz_json:delete_key(?FEATURE_LOCAL, features(PN)),
    set_features(PN, Features);

set_module_name(PN0, Name=?NE_BINARY) ->
    lager:debug("updating module_name from ~p to ~p", [PN0#knm_phone_number.module_name, Name]),
    PN = ?DIRTY(PN0#knm_phone_number{module_name = Name}),
    Features = kz_json:delete_key(?FEATURE_LOCAL, features(PN)),
    set_features(PN, Features).

set_module_name_local(PN0, Name) ->
    Feature =
        case feature(PN0, ?FEATURE_LOCAL) of
            'undefined' -> kz_json:new();
            LocalFeature -> LocalFeature
        end,
    PN = set_feature(PN0, ?FEATURE_LOCAL, Feature),
    case PN0#knm_phone_number.module_name of
        undefined -> PN#knm_phone_number{module_name = Name};
        Name -> PN;
        _ ->
            lager:debug("updating module_name from ~p to ~p", [PN#knm_phone_number.module_name, Name]),
            ?DIRTY(PN#knm_phone_number{module_name = Name})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec carrier_data(knm_phone_number()) -> kz_json:object().
carrier_data(#knm_phone_number{carrier_data=Data}) -> Data.

-spec set_carrier_data(knm_phone_number(), api_object()) -> knm_phone_number().
set_carrier_data(PN=#knm_phone_number{carrier_data = undefined}, undefined) ->
    set_carrier_data(PN, ?DEFAULT_CARRIER_DATA);
set_carrier_data(PN=#knm_phone_number{carrier_data = undefined}, Data) ->
    true = kz_json:is_json_object(Data),
    PN#knm_phone_number{carrier_data = Data};
set_carrier_data(PN, undefined) ->
    set_carrier_data(PN, ?DEFAULT_CARRIER_DATA);
set_carrier_data(PN, Data) ->
    'true' = kz_json:is_json_object(Data),
    case kz_json:are_equal(PN#knm_phone_number.carrier_data, Data) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{carrier_data = Data})
    end.

-spec update_carrier_data(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_carrier_data(PN=#knm_phone_number{carrier_data = Data}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(JObj, Data),
    case kz_json:are_equal(PN#knm_phone_number.carrier_data, Updated) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{carrier_data = Updated})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec region(knm_phone_number()) -> api_ne_binary().
region(#knm_phone_number{region=Region}) -> Region.

-spec set_region(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_region(PN=#knm_phone_number{region = V}, V) -> PN;
set_region(PN=#knm_phone_number{region = undefined}, Region=?NE_BINARY) ->
    PN#knm_phone_number{region = Region};
set_region(PN, Region='undefined') ->
    lager:debug("updating region from ~s to ~s", [PN#knm_phone_number.region, Region]),
    ?DIRTY(PN#knm_phone_number{region = Region});
set_region(PN, Region=?NE_BINARY) ->
    lager:debug("updating region from ~s to ~s", [PN#knm_phone_number.region, Region]),
    ?DIRTY(PN#knm_phone_number{region = Region}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec auth_by(knm_phone_number()) -> api_ne_binary().
auth_by(#knm_phone_number{auth_by=AuthBy}) -> AuthBy.

-spec set_auth_by(knm_phone_number(), api_ne_binary()) -> knm_phone_number().
set_auth_by(PN, AuthBy='undefined') ->
    PN#knm_phone_number{auth_by=AuthBy};
set_auth_by(PN, AuthBy=?KNM_DEFAULT_AUTH_BY) ->
    PN#knm_phone_number{auth_by=AuthBy};
set_auth_by(PN, ?MATCH_ACCOUNT_RAW(AuthBy)) ->
    PN#knm_phone_number{auth_by=AuthBy}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_admin(knm_phone_number() | api_ne_binary()) -> boolean().
-ifdef(TEST).
is_admin(#knm_phone_number{auth_by=AuthBy}) -> is_admin(AuthBy);
is_admin(?KNM_DEFAULT_AUTH_BY) -> true;
is_admin(?MASTER_ACCOUNT_ID) -> true;
is_admin(_) -> false.
-else.
is_admin(#knm_phone_number{auth_by=AuthBy}) -> is_admin(AuthBy);
is_admin(AuthBy) ->
    IsBypassed = ?KNM_DEFAULT_AUTH_BY =:= AuthBy,
    IsBypassed
        andalso lager:info("bypassing auth"),
    IsBypassed
        orelse kz_util:is_system_admin(AuthBy).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dry_run(knm_phone_number()) -> boolean().
dry_run(#knm_phone_number{dry_run=DryRun}) -> DryRun.

-spec set_dry_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_dry_run(PN, DryRun) when is_boolean(DryRun) ->
    PN#knm_phone_number{dry_run=DryRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec batch_run(knm_phone_number()) -> boolean().
batch_run(#knm_phone_number{batch_run=BatchRun}) -> BatchRun.

-spec set_batch_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_batch_run(PN, BatchRun) when is_boolean(BatchRun) ->
    PN#knm_phone_number{batch_run=BatchRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec mdn_run(knm_phone_number()) -> boolean().
mdn_run(#knm_phone_number{mdn_run=MDNRun}) -> MDNRun.

-spec set_mdn_run(knm_phone_number(), boolean()) -> knm_phone_number().
set_mdn_run(PN, MDNRun) when is_boolean(MDNRun) ->
    PN#knm_phone_number{mdn_run=MDNRun}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec locality(knm_phone_number()) -> kz_json:object().
locality(#knm_phone_number{locality=Locality}) -> Locality.

-spec set_locality(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_locality(PN=#knm_phone_number{locality = undefined}, JObj) ->
    true = kz_json:is_json_object(JObj),
    PN#knm_phone_number{locality = JObj};
set_locality(PN, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    case kz_json:are_equal(JObj, PN#knm_phone_number.locality) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{locality = JObj})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec doc(knm_phone_number()) -> kz_json:object().
doc(#knm_phone_number{doc=Doc}) -> Doc.

-spec set_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
set_doc(PN=#knm_phone_number{doc = undefined}, JObj) ->
    true = kz_json:is_json_object(JObj),
    PN#knm_phone_number{doc = JObj};
set_doc(PN, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    case kz_json:are_equal(JObj, PN#knm_phone_number.doc) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{doc = JObj})
    end.

-spec update_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
update_doc(PN=#knm_phone_number{doc = Doc}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(kz_json:public_fields(JObj), Doc),
    Data = kz_json:delete_key(<<"id">>, Updated),
    case kz_json:are_equal(Data, PN#knm_phone_number.doc) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{doc = Data})
    end.

-spec reset_doc(knm_phone_number(), kz_json:object()) -> knm_phone_number().
reset_doc(PN=#knm_phone_number{doc = Doc}, JObj) ->
    'true' = kz_json:is_json_object(JObj),
    Updated = kz_json:merge_recursive(kz_json:public_fields(JObj), kz_json:private_fields(Doc)),
    Data = maybe_update_rw_features(kz_json:delete_key(<<"id">>, Updated)),
    case kz_json:are_equal(Data, PN#knm_phone_number.doc) of
        true -> PN;
        false -> ?DIRTY(PN#knm_phone_number{doc = Data})
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec modified(knm_phone_number()) -> gregorian_seconds().
modified(#knm_phone_number{is_dirty = true}) -> kz_time:current_tstamp();
modified(#knm_phone_number{modified = undefined}) -> kz_time:current_tstamp();
modified(#knm_phone_number{modified = Modified}) -> Modified.

-spec set_modified(knm_phone_number(), gregorian_seconds()) -> knm_phone_number().
set_modified(PN=#knm_phone_number{modified = V}, V) -> PN;
set_modified(PN, Modified)
  when is_integer(Modified), Modified > 0 ->
    PN#knm_phone_number{modified=Modified}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec created(knm_phone_number()) -> gregorian_seconds().
created(#knm_phone_number{created = undefined}) -> kz_time:current_tstamp();
created(#knm_phone_number{created = Created}) -> Created.

-spec set_created(knm_phone_number(), gregorian_seconds()) -> knm_phone_number().
set_created(PN=#knm_phone_number{created = undefined}, undefined) ->
    PN#knm_phone_number{created = kz_time:current_tstamp()};
set_created(PN=#knm_phone_number{created = V}, V) -> PN;
set_created(PN, Created)
  when is_integer(Created), Created > 0 ->
    ?DIRTY(PN#knm_phone_number{created=Created}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list_attachments(knm_phone_number(), ne_binary()) -> {'ok', kz_json:object()} |
                                                           {'error', any()}.
list_attachments(PN, AuthBy) ->
    AssignedTo = assigned_to(PN),
    case state(PN) =:= ?NUMBER_STATE_PORT_IN
        andalso kz_util:is_in_account_hierarchy(AuthBy, AssignedTo, 'true')
    of
        'true' -> {'ok', kz_doc:attachments(doc(PN), kz_json:new())};
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
save_to_number_db(PN) -> PN.
-else.
save_to_number_db(PN=#knm_phone_number{state = ?NUMBER_STATE_DELETED}) ->
    lager:debug("deleted, skip saving ~s", [number(PN)]),
    PN;
save_to_number_db(PN) ->
    NumberDb = number_db(PN),
    JObj = to_json(PN),
    case datamgr_save(PN, NumberDb, JObj) of
        {'ok', Doc} -> from_json_with_options(Doc, PN);
        {'error', 'not_found'} ->
            lager:debug("creating new db '~s' for number '~s'", [NumberDb, number(PN)]),
            'true' = kz_datamgr:db_create(NumberDb),
            {'ok',_} = kz_datamgr:revise_doc_from_file(NumberDb, ?APP, <<"views/numbers.json">>),
            save_to_number_db(PN);
        {'error', E} ->
            lager:error("failed to save ~s in ~s: ~p", [number(PN), NumberDb, E]),
            knm_errors:database_error(E, PN)
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_assignment(knm_phone_number()) -> knm_phone_number().
handle_assignment(PN) ->
    ?LOG_DEBUG("handling assignment for ~s", [number(PN)]),
    unassign_from_prev(assign(PN)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes sure number is assigned to assigned_to by creating number doc
%% that may not yet exist in AssignedTo DB.
%% @end
%%--------------------------------------------------------------------
-spec assign(knm_phone_number()) -> knm_phone_number().
assign(PN) ->
    AssignedTo = assigned_to(PN),
    case kz_term:is_empty(AssignedTo) of
        'true' -> PN;
        'false' -> assign(PN, AssignedTo)
    end.

-spec assign(knm_phone_number(), ne_binary()) -> knm_phone_number().
-ifdef(TEST).
assign(PN, _AssignedTo) -> PN.
-else.
assign(PN, AssignedTo) ->
    AccountDb = kz_util:format_account_db(AssignedTo),
    case datamgr_save(PN, AccountDb, to_json(PN)) of
        {'error', E} ->
            lager:error("failed to assign number ~s to ~s", [number(PN), AccountDb]),
            knm_errors:assign_failure(PN, E);
        {'ok', JObj} ->
            lager:debug("assigned number ~s to ~s", [number(PN), AccountDb]),
            from_json_with_options(JObj, PN)
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
unassign_from_prev(PN) ->
    PrevAssignedTo = prev_assigned_to(PN),
    case kz_term:is_empty(PrevAssignedTo) of
        'false' -> unassign_from_prev(PN, PrevAssignedTo);
        'true' ->
            lager:debug("prev_assigned_to is empty for ~s, ignoring", [number(PN)]),
            PN
    end.

-spec unassign_from_prev(knm_phone_number(), ne_binary()) -> knm_phone_number().
-ifdef(TEST).
unassign_from_prev(PN, _PrevAssignedTo) -> PN.
-else.
unassign_from_prev(PN=#knm_phone_number{assigned_to = PrevAssignedTo}, PrevAssignedTo) ->
    lager:debug("prev_assigned_to is same as assigned_to, not unassign-ing from prev"),
    PN;
unassign_from_prev(PN, PrevAssignedTo) ->
    Num = number(PN),
    case get_number_in_account(PrevAssignedTo, Num) of
        {'error', 'not_found'} ->
            lager:debug("number ~s was not found in ~s, no need to unassign from prev"
                       ,[Num, PrevAssignedTo]),
            PN;
        {'ok', _} -> do_unassign_from_prev(PN, PrevAssignedTo);
        {'error', _R} -> do_unassign_from_prev(PN, PrevAssignedTo)
    end.

-spec do_unassign_from_prev(knm_phone_number(), ne_binary()) -> knm_phone_number().
do_unassign_from_prev(PN, PrevAssignedTo) ->
    PrevAccountDb = kz_util:format_account_db(PrevAssignedTo),
    case kz_datamgr:del_doc(PrevAccountDb, to_json(PN)) of
        {'ok', _} ->
            lager:debug("successfully unassign_from_prev number ~s from ~s"
                       ,[number(PN), PrevAssignedTo]),
            PN;
        {'error', E} ->
            lager:error("failed to unassign from prev number ~s from ~s"
                       ,[number(PN), PrevAssignedTo]),
            knm_errors:assign_failure(PN, E)
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
-ifdef(TEST).
delete_number_doc(PN) -> {ok, PN}.
-else.
delete_number_doc(PN) ->
    case kz_datamgr:del_doc(number_db(PN), number(PN)) of
        {'error', _R}=E -> E;
        {'ok', _} -> {'ok', PN}
    end.
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_number_from_account(knm_phone_number()) -> knm_phone_number_return().
-ifdef(TEST).
maybe_remove_number_from_account(PN) -> {ok, PN}.
-else.
maybe_remove_number_from_account(PN) ->
    AssignedTo = assigned_to(PN),
    Num = number(PN),
    case kz_term:is_empty(AssignedTo) of
        'true' ->
            lager:debug("assigned_to is empty for ~s, ignoring", [Num]),
            {'ok', PN};
        'false' ->
            case kz_datamgr:del_doc(kz_util:format_account_db(AssignedTo), Num) of
                {'error', _R}=E -> E;
                {'ok', _} -> {'ok', PN}
            end
    end.
-endif.

-ifndef(TEST).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec datamgr_save(knm_phone_number(), ne_binary(), kz_json:object()) ->
                          {'ok', kz_json:object()} |
                          kz_data:data_error().
datamgr_save(PN, Db, JObj) ->
    case batch_run(PN)
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
