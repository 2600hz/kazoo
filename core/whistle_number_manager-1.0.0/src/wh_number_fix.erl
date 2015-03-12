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

-define(TIME_BETWEEN_ACCOUNTS_MS, whapps_config:get_integer(?WNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)).
-define(TIME_BETWEEN_NUMBERS_MS, whapps_config:get_integer(?WNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)).

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
fix_account_numbers(Account) when is_binary(Account) ->
    io:format("########## fixing [~s] ##########~n", [Account]),

    io:format("[~s] getting numbers~n", [Account]),
    PhoneNumbers = get_phone_numbers(Account),

    io:format("[~s] start fixing numbers~n", [Account]),
    fix_numbers(PhoneNumbers),

    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    couch_mgr:flush_cache_docs(AccountDb).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_phone_numbers(ne_binary()) -> ne_binaries().
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
-spec extract_numbers(ne_binary(), wh_json:object()) -> ne_binaries().
extract_numbers(AccountId, Doc) ->
    wh_json:foldl(
        fun(Number, JObj, Acc) ->
            case wnm_util:is_reconcilable(Number) of
                'false' -> Acc;
                'true' ->
                    Props = [
                        {'account_id', AccountId}
                        ,{'phone_numbers_doc', JObj}
                        ,{'number_doc', get_number_doc(Number)}
                        ,{'trunkstore_numbers', get_trunkstore_numbers(AccountId)}
                        ,{'callflow_numbers', get_callflow_numbers(AccountId)}
                    ],
                    [{Number, Props} |Acc]
            end
        end
        ,[]
        ,Doc
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_number_doc(ne_binary()) -> wh_json:object() | 'undefined'.
get_number_doc(Number) ->
    NumberDb = wnm_util:number_to_db_name(Number),
    case couch_mgr:open_doc(NumberDb, Number) of
        {'error', _E} ->
            io:format("failed to open ~s doc ~p~n", [Number, _E]),
            'undefined';
        {'ok', JObj} -> JObj
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
-spec fix_numbers(wh_proplist()) -> 'ok'.
fix_numbers([]) -> 'ok';
fix_numbers([{Number, Docs}|Numbers]) ->
    timer:sleep(?TIME_BETWEEN_NUMBERS_MS),
    io:format("##### fixing [~s] #####~n", [Number]),
    Routines = [
        fun maybe_fix_assignment/2
        ,fun maybe_fix_used_by/2
    ],
    lists:foreach(
        fun(F) ->
            F(Number, Docs)
        end
        ,Routines
    ),
    fix_numbers(Numbers).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_fix_assignment(ne_binary(), wh_proplist()) -> 'ok'.
maybe_fix_assignment(Number, Docs) ->
    io:format("[~s] maybe fix assignment~n", [Number]),

    NumberDoc = props:get_value('number_doc', Docs),
    PhoneNumbersDoc = props:get_value('phone_numbers_doc', Docs),
    AccountId = props:get_value('account_id', Docs),

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
-spec get_assignment(wh_json:object() | 'undefined') -> wh_json:object() | 'undefined'.
get_assignment('undefined') -> 'undefined';
get_assignment(JObj) ->
    wh_json:get_first_defined([<<"pvt_assigned_to">>, <<"assigned_to">>], JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_assignment(ne_binary(), ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
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
    remove_number(AccountId, Number).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fix_phone_numbers_doc_assignment(ne_binary(), ne_binary()) -> 'ok'.
fix_phone_numbers_doc_assignment(AccountId, Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            JObj1 = wh_json:set_value([Number, <<"assigned_to">>], AccountId, JObj),
            case couch_mgr:ensure_saved(AccountDb, JObj1) of
                {'ok', _} ->
                    io:format("[~s] phone_numbers doc assigned_to (~s) fixed~n", [Number, AccountId]);
                {'error', _E1} ->
                    io:format("[~s] phone_numbers doc assigned_to failed to fix: ~p~n", [Number, _E1])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec remove_number(ne_binary(), ne_binary()) -> 'ok'.
remove_number(AccountId, Number) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_doc(AccountDb, ?PHONE_NUMBERS) of
        {'error', _E} ->
            io:format("[~s] failed to open phone_numbers doc: ~p~n", [Number, _E]);
        {'ok', JObj} ->
            JObj1 = wh_json:delete_key(Number, JObj),
            case couch_mgr:ensure_saved(AccountDb, JObj1) of
                {'ok', _} ->
                    io:format("[~s] phone_numbers doc fixed, number removed~n", [Number]);
                {'error', _E1} ->
                    io:format("[~s] phone_numbers doc failed to fix: ~p~n", [Number, _E1])
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_fix_used_by(ne_binary(), wh_proplist()) -> 'ok'.
maybe_fix_used_by(Number, Docs) ->
    io:format("[~s] maybe fix used_by~n", [Number]),
    AccountId = props:get_value('account_id', Docs),
    TrunkstoreNumbers = props:get_value('trunkstore_numbers', Docs),
    CallflowNumbers = props:get_value('callflow_numbers', Docs),
    NumberDoc = props:get_value('number_doc', Docs),
    PhoneNumbersDoc = props:get_value('phone_numbers_doc', Docs),

    NumberDocUsedBy = wh_json:get_value(<<"used_by">>, NumberDoc),
    PhoneNumbersDocUsedBy = wh_json:get_value(<<"used_by">>, PhoneNumbersDoc),
    UsedBy =
        case {lists:member(Number, TrunkstoreNumbers)
              ,lists:member(Number, CallflowNumbers)}
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
