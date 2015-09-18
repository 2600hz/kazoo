%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(wh_number_fix).

-export([fix_account_numbers/1]).

-include("wnm.hrl").

-define(PHONE_NUMBERS, <<"phone_numbers">>).

-define(TIME_BETWEEN_ACCOUNTS_MS
        ,whapps_config:get_integer(?WNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)
       ).
-define(TIME_BETWEEN_NUMBERS_MS
        ,whapps_config:get_integer(?WNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)
       ).

-type extracted_numbers() :: [{ne_binary(), wh_json:object(), wh_proplist()}].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_account_numbers(ne_binary() | ne_binaries()) -> 'ok'.
fix_account_numbers([]) -> 'ok';
fix_account_numbers([Account|Accounts]) ->
    _ = fix_account_numbers(Account),
    timer:sleep(?TIME_BETWEEN_ACCOUNTS_MS),
    fix_account_numbers(Accounts);
fix_account_numbers(<<_/binary>> = Account) ->
    io:format("########## fixing [~s] ##########~n", [Account]),

    io:format("[~s] getting numbers~n", [Account]),
    PhoneNumbers = get_phone_numbers(Account),

    io:format("[~s] start fixing numbers~n", [Account]),
    fix_numbers(PhoneNumbers),

    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:flush_cache_doc(AccountDb, ?WNM_PHONE_NUMBER_DOC).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_phone_numbers(ne_binary()) -> extracted_numbers().
get_phone_numbers(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'ok', JObj} -> extract_numbers(AccountId, JObj);
        {'error', _E} ->
            io:format("failed to open phone_numbers doc for account ~s ~p~n", [AccountId, _E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec extract_numbers(ne_binary(), wh_json:object()) -> extracted_numbers().
extract_numbers(AccountId, Doc) ->
    TrunkstoreNumbers = get_trunkstore_numbers(AccountId),
    CallflowNumbers = get_callflow_numbers(AccountId),
    Props = [{'account_id', AccountId}
             ,{'trunkstore_numbers', TrunkstoreNumbers}
             ,{'callflow_numbers', CallflowNumbers}
            ],

    wh_json:foldl(
      fun(Number, JObj, Acc) ->
              extract_numbers_fold(Number, JObj, Acc, Props)
      end
      ,[]
      ,Doc
     ).

-spec extract_numbers_fold(wh_json:key(), wh_json:object(), extracted_numbers(), wh_proplist()) ->
                                  extracted_numbers().
extract_numbers_fold(Number, JObj, Acc, Props) ->
    case wnm_util:is_reconcilable(Number) of
        'false' -> Acc;
        'true' ->
            [{Number
              ,get_number_doc(Number)
              ,[{'phone_numbers_doc', JObj} | Props]
             }
             |Acc
            ]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_number_doc(ne_binary()) -> api_object().
get_number_doc(Number) ->
    NumberDb = wnm_util:number_to_db_name(Number),
    case couch_mgr:open_doc(NumberDb, Number) of
        {'ok', JObj} -> JObj;
        {'error', _E} ->
            io:format("failed to open ~s doc ~p~n", [Number, _E]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_trunkstore_numbers(ne_binary()) -> ne_binaries().
get_trunkstore_numbers(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:get_all_results(AccountDb, <<"trunkstore/LookUpDID">>) of
        {'ok', JObjs} -> couch_mgr:get_result_keys(JObjs);
        {'error', _E} ->
            io:format("failed to get trunkstore numbers: ~p~n", [_E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_callflow_numbers(ne_binary()) -> ne_binaries().
get_callflow_numbers(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:get_all_results(AccountDb, <<"callflow/listing_by_number">>) of
        {'ok', JObjs} -> couch_mgr:get_result_keys(JObjs);
        {'error', _E} ->
            io:format("failed to get callflow numbers: ~p~n", [_E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_numbers(extracted_numbers()) -> 'ok'.
fix_numbers([]) -> 'ok';
fix_numbers([{Number, NumberDoc, Props}|Numbers]) ->
    timer:sleep(?TIME_BETWEEN_NUMBERS_MS),
    io:format("##### fixing [~s] #####~n", [Number]),
    Routines = [fun maybe_fix_assignment/3
                ,fun maybe_fix_used_by/3
               ],
    lists:foldl(
      fun(_, 'stop') -> 'stop';
         (F, 'ok') -> F(Number, NumberDoc, Props)
      end
      ,'ok'
      ,Routines
     ),
    fix_numbers(Numbers).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_fix_assignment(ne_binary(), wh_json:object(), wh_proplist()) -> 'ok' | 'stop'.
maybe_fix_assignment(Number, NumberDoc, Props) ->
    io:format("[~s] maybe fix assignment~n", [Number]),

    PhoneNumbersDoc = props:get_value('phone_numbers_doc', Props),
    AccountId = props:get_value('account_id', Props),

    fix_assignment(
      AccountId
      ,get_assignment(PhoneNumbersDoc)
      ,get_assignment(NumberDoc)
      ,Number
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_assignment(api_object()) -> api_binary().
get_assignment('undefined') -> 'undefined';
get_assignment(JObj) ->
    wh_json:get_first_defined([<<"pvt_assigned_to">>, <<"assigned_to">>], JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_assignment(ne_binary(), api_binary(), api_binary(), ne_binary()) -> 'ok'.
fix_assignment(AccountId, AccountId, AccountId, _Number) ->
    io:format("[~s] assignment is OK~n", [_Number]);
fix_assignment(AccountId, _PhoneNumbersAssignment, 'undefined', Number) ->
    io:format("[~s] number doc assignment is undefined or number does not exist~n", [Number]),
    remove_number(AccountId, Number);
fix_assignment(AccountId, _PhoneNumbersAssignment, AccountId, Number) ->
    io:format("[~s] phone_numbers assignment is wrong~n", [Number]),
    fix_phone_numbers_doc_assignment(AccountId, Number);
fix_assignment(AccountId, PhoneNumbersAssignment, NumberAssignment, Number) ->
    D = [Number, PhoneNumbersAssignment, NumberAssignment],
    io:format("[~s] assignment are wrong: number doc (~s) phone_numbers doc (~s)~n", D),
    _ = maybe_add_to_account(Number, AccountId, NumberAssignment),
    remove_number(AccountId, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_phone_numbers_doc_assignment(ne_binary(), ne_binary()) -> 'ok'.
-spec fix_phone_numbers_doc_assignment(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
fix_phone_numbers_doc_assignment(AccountId, Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            fix_phone_numbers_doc_assignment(AccountId, Number, AccountDb, JObj)
    end.

fix_phone_numbers_doc_assignment(AccountId, Number, AccountDb, JObj) ->
    JObj1 = wh_json:set_value([Number, <<"assigned_to">>], AccountId, JObj),
    case couch_mgr:ensure_saved(AccountDb, JObj1) of
        {'ok', _} ->
            io:format("[~s] phone_numbers doc assigned_to (~s) fixed~n", [Number, AccountId]);
        {'error', _E1} ->
            io:format("[~s] phone_numbers doc assigned_to failed to fix: ~p~n", [Number, _E1])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_number(ne_binary(), ne_binary()) -> 'stop' | 'ok'.
-spec remove_number(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'stop'.
remove_number(AccountId, Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),

    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            maybe_remove_non_porting(AccountId, Number, AccountDb, JObj)
    end.

-spec maybe_remove_non_porting(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'stop' | 'ok'.
maybe_remove_non_porting(AccountId, Number, AccountDb, JObj) ->
    case wh_json:get_value([Number, <<"state">>], JObj) of
        ?NUMBER_STATE_PORT_IN ->
            io:format("[~s] will not be removed as it is in 'port_in' state~n", [Number]);
        _Else ->
            remove_number(AccountId, Number, AccountDb, JObj)
    end.

remove_number(_AccountId, Number, AccountDb, JObj) ->
    JObj1 = wh_json:delete_key(Number, JObj),
    case couch_mgr:ensure_saved(AccountDb, JObj1) of
        {'ok', _} ->
            io:format("[~s] phone_numbers doc fixed, number removed~n", [Number]);
        {'error', _E1} ->
            io:format("[~s] phone_numbers doc failed to fix: ~p~n", [Number, _E1])
    end,
    'stop'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_to_account(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
-spec maybe_add_to_account(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_add_to_account(Number, AccountId, NumberAssignment)->
    AccountDb = wh_util:format_account_id(NumberAssignment, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc for account [~s]: ~p~n", [Number, NumberAssignment, _E]);
        {'ok', JObj} ->
            maybe_add_to_account(Number, AccountId, NumberAssignment, JObj)
    end.

maybe_add_to_account(Number, AccountId, NumberAssignment, JObj) ->
    case wh_json:get_value(Number, JObj) of
        'undefined' ->
            add_to_account(Number, AccountId, NumberAssignment, JObj);
        NumJObj ->
            case wh_json:is_empty(NumJObj) of
                'false' ->
                    io:format("[~s] number is already added to right account (~s)~n", [Number, NumberAssignment]);
                'true' ->
                    add_to_account(Number, AccountId, NumberAssignment, JObj)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_to_account(ne_binary(), ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
add_to_account(Number, AccountId, NumberAssignment, Doc)->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc for account [~s]: ~p~n", [Number, AccountId, _E]);
        {'ok', JObj} ->
            NumJObj = wh_json:set_value(<<"assigned_to">>, NumberAssignment, wh_json:get_value(Number, JObj)),
            Doc1 = wh_json:set_value(Number, NumJObj, Doc),
            NumberAssignmentDb = wh_util:format_account_id(NumberAssignment, 'encoded'),
            case couch_mgr:ensure_saved(NumberAssignmentDb, Doc1) of
                {'ok', _} ->
                    couch_mgr:flush_cache_doc(NumberAssignmentDb, ?PHONE_NUMBERS),
                    io:format("[~s] number added to right account (~s)~n", [Number, NumberAssignment]);
                {'error', _E1} ->
                    io:format("[~s] failed to add number to right account (~s) ~p~n", [Number, NumberAssignment, _E1])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_fix_used_by(ne_binary(), wh_json:object(), wh_proplist()) -> 'ok'.
maybe_fix_used_by(Number, NumberDoc, Props) ->
    io:format("[~s] maybe fix used_by~n", [Number]),
    AccountId = props:get_value('account_id', Props),
    TrunkstoreNumbers = props:get_value('trunkstore_numbers', Props),
    CallflowNumbers = props:get_value('callflow_numbers', Props),
    PhoneNumbersDoc = props:get_value('phone_numbers_doc', Props),

    NumberDocUsedBy = wh_json:get_value(<<"used_by">>, NumberDoc),
    PhoneNumbersDocUsedBy = wh_json:get_value(<<"used_by">>, PhoneNumbersDoc),
    UsedBy =
        case {lists:member(Number, TrunkstoreNumbers)
              ,lists:member(Number, CallflowNumbers)
             }
        of
            {'false', 'false'} -> <<>>;
            {'true', 'false'} -> <<"trunkstore">>;
            {'false', 'true'} -> <<"callflow">>;
            {'true', 'true'} -> 'error'
        end,
    fix_used_by(AccountId, UsedBy, NumberDocUsedBy, PhoneNumbersDocUsedBy, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_used_by(ne_binary(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
fix_used_by(_AccountId, UsedBy, UsedBy, UsedBy, _Number) ->
    io:format("[~s] used_by is OK~n", [_Number]);
fix_used_by(_AccountId, 'error', _, _, _Number) ->
    io:format("[~s] number is used in callflow/trunkstore ERROR~n", [_Number]);
fix_used_by(AccountId, UsedBy, UsedBy, _PhoneNumbersDocUsedBy, Number) ->
    io:format("[~s] phone_numbers doc used_by is wrong~n", [Number]),
    fix_phone_numbers_doc_used_by(AccountId, Number, UsedBy);
fix_used_by(AccountId, UsedBy, _NumberDocUsedBy, UsedBy, Number) ->
    io:format("[~s] number doc used_by is wrong~n", [Number]),
    fix_number_doc_used_by(AccountId, Number, UsedBy);
fix_used_by(AccountId, UsedBy, _NumberDocUsedBy, _PhoneNumbersDocUsedBy, Number) ->
    io:format("[~s] phone_numbers doc & number doc used_by are wrong~n", [Number]),
    fix_phone_numbers_doc_used_by(AccountId, Number, UsedBy),
    fix_number_doc_used_by(AccountId, Number, UsedBy).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_number_doc_used_by(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
fix_number_doc_used_by(_AccountId, Number, UsedBy) ->
    NumberDb = wnm_util:number_to_db_name(Number),
    case couch_mgr:open_doc(NumberDb, Number) of
        {'error', _E} ->
            io:format("[~s] failed to open number doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            JObj1 = wh_json:set_value(<<"used_by">>, UsedBy, JObj),
            case couch_mgr:ensure_saved(NumberDb, JObj1) of
                {'ok', _} ->
                    io:format("[~s] number doc used_by (~s) fixed~n", [Number, UsedBy]);
                {'error', _E1} ->
                    io:format("[~s] number doc used_by failed to fix: ~p~n", [Number, _E1])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_phone_numbers_doc_used_by(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
fix_phone_numbers_doc_used_by(AccountId, Number, UsedBy) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            JObj1 = wh_json:set_value([Number, <<"used_by">>], UsedBy, JObj),
            case couch_mgr:ensure_saved(AccountDb, JObj1) of
                {'ok', _} ->
                    io:format("[~s] phone_numbers doc used_by (~s) fixed~n", [Number, UsedBy]);
                {'error', _E1} ->
                    io:format("[~s] phone_numbers doc used_by failed to fix: ~p~n", [Number, _E1])
            end
    end.
