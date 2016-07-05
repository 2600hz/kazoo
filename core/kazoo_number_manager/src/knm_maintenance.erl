%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_maintenance).

-include("knm.hrl").

-export([ fix_account_numbers/1
        , fix_accounts_numbers/1

        ,generate_numbers/3
        ]).

-define(TIME_BETWEEN_ACCOUNTS_MS
       ,kapps_config:get_integer(?KNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_NUMBERS_MS
       ,kapps_config:get_integer(?KNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(LOG(Format, Args),
        lager:debug(Format, Args),
        io:format(Format ++ "\n", Args)
       ).

-type number_to_fix() :: #{ key => ne_binary()
                          , old => knm_phone_number:knm_phone_number()
                          , fetched => knm_phone_number:knm_phone_number()
                          , account_db => ne_binary()
                          , account_id => ne_binary()
                          , trunkstore_numbers => ne_binaries()
                          , callflow_numbers => ne_binaries()
                          , fixes => list()
                          }.

%% API

%% @public
-spec fix_accounts_numbers([ne_binary()]) -> 'ok'.
fix_accounts_numbers(Accounts) ->
    foreach_pause_in_between(?TIME_BETWEEN_ACCOUNTS_MS, fun fix_account_numbers/1, Accounts).

%% @public
-spec fix_account_numbers(ne_binary()) -> 'ok'.
fix_account_numbers(AccountDb = ?MATCH_ACCOUNT_ENCODED(_)) ->
    ?LOG("########## fixing [~s] ##########", [AccountDb]),
    ?LOG("[~s] getting numbers", [AccountDb]),
    ToFix = get_phone_numbers(AccountDb),
    ?LOG("[~s] start fixing numbers", [AccountDb]),
    foreach_pause_in_between(?TIME_BETWEEN_NUMBERS_MS, fun maybe_fix_number/1, ToFix),
    kz_datamgr:flush_cache_doc(AccountDb, ?KNM_PHONE_NUMBERS_DOC);
fix_account_numbers(Account = ?NE_BINARY) ->
    fix_account_numbers(kz_util:format_account_db(Account)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec foreach_pause_in_between(non_neg_integer(), fun(), list()) -> 'ok'.
foreach_pause_in_between(_, _, []) -> 'ok';
foreach_pause_in_between(_, Fun, [Element]) ->
    _ = Fun(Element),
    'ok';
foreach_pause_in_between(Time, Fun, [Element|Elements]) ->
    _ = Fun(Element),
    timer:sleep(Time),
    foreach_pause_in_between(Time, Fun, Elements).

-spec get_result_keys(ne_binary(), ne_binary()) -> ne_binaries().
get_result_keys(AccountDb, View) ->
    case kz_datamgr:get_all_results(AccountDb, View) of
        {'ok', JObjs} -> kz_datamgr:get_result_keys(JObjs);
        {'error', _R} ->
            ?LOG("failed to get trunkstore numbers: ~p", [_R]),
            []
    end.

-spec get_phone_numbers(ne_binary()) -> [number_to_fix()].
get_phone_numbers(AccountDb) ->
    CallflowNumbers = get_result_keys(AccountDb, <<"callflows/listing_by_number">>),
    TrunkstoreNumbers = get_result_keys(AccountDb, <<"trunkstore/lookup_did">>),
    case kz_datamgr:open_doc(AccountDb, ?KNM_PHONE_NUMBERS_DOC) of
        {'ok', JObj} ->
            [ #{ key => Key
               , old => phone_number_from_dirty_json(knm_converters:normalize(Key)
                                                    ,kz_json:get_value(Key, JObj))
               , fetched => fetch_number(Key)
               , account_db => AccountDb
               , account_id => kz_util:format_account_id(AccountDb)
               , trunkstore_numbers => TrunkstoreNumbers
               , callflow_numbers => CallflowNumbers
               , fixes => []
               }
              || Key <- kz_json:get_keys(kz_json:public_fields(JObj)),
                 knm_converters:is_reconcilable(Key)
            ];
        {'error', _R} ->
            ?LOG("failed to open ~s doc for account ~s ~p"
                ,[?KNM_PHONE_NUMBERS_DOC, AccountDb, _R]),
            []
    end.

-spec fetch_number(ne_binary()) -> knm_phone_number:knm_phone_number().
fetch_number(Number) ->
    case knm_number:get(Number) of
        {'ok', KNMNumber} -> knm_number:phone_number(KNMNumber);
        {'error', _R} ->
            ?LOG("failed to open ~s doc ~p", [Number, _R]),
            knm_phone_number:new()
    end.

-spec phone_number_from_dirty_json(ne_binary(), kz_json:object()) -> knm_phone_number:knm_phone_number().
phone_number_from_dirty_json(NormalizedNumber, JObj) ->
    State = kz_json:get_first_defined([?PVT_STATE, ?PVT_STATE_LEGACY, <<"state">>], JObj),
    LessDirtyJObj =
        kz_json:from_list(
          props:filter_empty(
            [ {<<"_id">>, NormalizedNumber}
            , {?PVT_DB_NAME, knm_converters:to_db(NormalizedNumber)}
            , {?PVT_ASSIGNED_TO, kz_json:get_first_defined([?PVT_ASSIGNED_TO, <<"assigned_to">>], JObj)}
            , {?PVT_PREVIOUSLY_ASSIGNED_TO, kz_json:get_first_defined([?PVT_PREVIOUSLY_ASSIGNED_TO, <<"previously_assigned_to">>], JObj)}
            , {?PVT_USED_BY, kz_json:get_first_defined([?PVT_USED_BY, <<"used_by">>], JObj)}
            , {?PVT_STATE, State}
            , {?PVT_PORTED_IN, ?NUMBER_STATE_PORT_IN == State}
            , {?PVT_MODULE_NAME, kz_json:get_first_defined([?PVT_MODULE_NAME, <<"module_name">>], JObj, knm_carriers:default_carrier())}
            , {?PVT_CARRIER_DATA, kz_json:get_first_defined([?PVT_CARRIER_DATA, <<"module_data">>], JObj)}
            , {?PVT_AUTH_BY, kz_json:get_first_defined([?PVT_AUTH_BY, <<"authorizing_account">>], JObj)}
            ])
         ),
    knm_phone_number:from_json(
      kz_json:set_value(<<"from_fix">>, kz_json:public_fields(JObj), LessDirtyJObj)
     ).

-spec maybe_fix_number(number_to_fix()) -> 'ok'.
maybe_fix_number(ToFix=#{old := _OldPN}) ->
    ?LOG("##### fixing [~s] #####", [knm_phone_number:number(_OldPN)]),
    Routines = [ fun maybe_fix_assignment/1
               , fun maybe_fix_used_by/1
               ],
    fix_number(lists:foldl(fun(F, Map) -> F(Map) end, ToFix, Routines)).

-spec fix_number(number_to_fix()) -> 'ok'.
fix_number(#{ fixes := Fixes
            , key := Key
            , account_id := AccountId
            , account_db := AccountDb
            , old := OldPN
            , fetched := FetchedPN
            }) ->
    PhoneNumber = case knm_phone_number:module_name(FetchedPN) of
                      'undefined' -> OldPN;
                      _ -> FetchedPN
                  end,
    IsPortIn = is_port_in(PhoneNumber),
    case Fixes of
        ['assign', 'remove_old'] ->
            knm_phone_number:save(
              knm_phone_number:set_assigned_to(PhoneNumber, AccountId)
             ),
            remove_old(IsPortIn, AccountDb, Key);
        _ ->
            ?LOG("unhandled fixes ~p: skipping", [Fixes])
    end.

-spec is_port_in(knm_phone_number:knm_phone_number()) -> boolean().
is_port_in(PhoneNumber) ->
    knm_phone_number:ported_in(PhoneNumber)
        orelse ?NUMBER_STATE_PORT_IN == knm_phone_number:state(PhoneNumber).

-spec remove_old(boolean(), ne_binary(), ne_binary()) -> 'ok'.
remove_old('true', _AccountDb, _Key) ->
    ?LOG("[~s] has state ~s: not removing from ~s/~s"
        ,[_Key, ?NUMBER_STATE_PORT_IN, _AccountDb, ?KNM_PHONE_NUMBERS_DOC]);
remove_old('false', AccountDb, Key) ->
    case kz_datamgr:open_doc(AccountDb, ?KNM_PHONE_NUMBERS_DOC) of
        {'error', _R} ->
            ?LOG("[~s] failed to open phone_numbers doc: ~p", [Key, _R]);
        {'ok', JObj} ->
            NewJObj = kz_json:delete_key(Key, JObj),
            case kz_datamgr:ensure_saved(AccountDb, NewJObj) of
                {'ok', _} -> ?LOG("[~s] phone_numbers doc fixed, number removed", [Key]);
                {'error', _R2} -> ?LOG("[~s] phone_numbers doc failed to fix: ~p", [Key, _R2])
            end
    end.

-spec add_fixes(list(), number_to_fix()) -> number_to_fix().
add_fixes(MoreFixes, ToFix=#{fixes := Fixes}) ->
    ToFix#{fixes => Fixes++MoreFixes}.

-spec maybe_fix_assignment(number_to_fix()) -> number_to_fix().
maybe_fix_assignment(#{ account_id := AccountId
                      , old := OldPN
                      , fetched := FetchedPN
                      } = ToFix) ->
    _Number = knm_phone_number:number(OldPN),
    ?LOG("[~s] maybe fix assignment", [_Number]),
    case {AccountId
         ,knm_phone_number:assigned_to(OldPN)
         ,knm_phone_number:assigned_to(FetchedPN)
         }
    of
        {AccountId, AccountId, AccountId} ->
            ?LOG("[~s] assignment is OK", [_Number]),
            add_fixes(['remove_old'], ToFix);  %% remove OldPN from JObj
        {AccountId, _OldPNAssignment, AccountId} ->
            ?LOG("[~s] phone_numbers assignment is wrong", [_Number]),
            add_fixes(['reassign'], ToFix);  %% reassign OldPN to FetchedPN
        {AccountId, AccountId, _FetchedPNAssignment} ->
            ?LOG("[~s] phone_numbers assignment is wrong", [_Number]),
            add_fixes(['reassign'], ToFix);
        {_, _OldPNAssignment, _FetchedPNAssignment} ->
            ?LOG("[~s] assignment are wrong: number doc (~s) phone_numbers doc (~s)"
                ,[_Number, _OldPNAssignment, _FetchedPNAssignment]),
            add_fixes(['assign'], ToFix)  %% assign OldPN to AccountId
    end.

-spec maybe_fix_used_by(number_to_fix()) -> number_to_fix().
maybe_fix_used_by(#{ old := OldPN
                   , fetched := FetchedPN
                   , callflow_numbers := CallflowNumbers
                   , trunkstore_numbers := TrunkstoreNumbers
                   } = ToFix) ->
    Number = knm_phone_number:number(OldPN),
    ?LOG("[~s] maybe fix used_by", [Number]),
    UsedByApp =
        case {lists:member(Number, TrunkstoreNumbers)
             ,lists:member(Number, CallflowNumbers)
             }
        of
            {'true', 'false'} -> <<"trunkstore">>;
            {'false', 'true'} -> <<"callflow">>;
            {'true', 'true'} -> 'both';
            {_, _} -> 'undefined'
        end,
    case {UsedByApp
         ,knm_phone_number:used_by(OldPN)
         ,knm_phone_number:used_by(FetchedPN)
         }
    of
        {UsedBy, UsedBy, UsedBy} ->
            ?LOG("[~s] used_by is OK", [Number]),
            add_fixes(['remove_old'], ToFix);
        {'both', _, _} ->
            ?LOG("[~s] number is used in callflow/trunkstore ERROR", [Number]),
            add_fixes(['noop'], ToFix);  %% do not touch JObj
        {UsedBy, UsedBy, _FetchedPNUsedBy} ->
            ?LOG("[~s] phone_numbers doc used_by is wrong", [Number]),
            add_fixes([{'used_by', UsedBy}], ToFix);  %% set used_by to UsedBy
        {UsedBy, _OldPNUsedBy, UsedBy} ->
            ?LOG("[~s] number doc used_by is wrong", [Number]),
            add_fixes([{'used_by', UsedBy}], ToFix);
        {_, _OldPNUsedBy, _FetchedPNUsedBy} ->
            ?LOG("[~s] phone_numbers doc & number doc used_by are wrong", [Number]),
            add_fixes(['noop'], ToFix)
    end.

generate_numbers(AccountId, StartingNumber, Quantity) ->
    knm_managed:generate_numbers(AccountId, kz_util:to_integer(StartingNumber), kz_util:to_integer(Quantity)).

%% "End of Module" ~ Captain Obvious
