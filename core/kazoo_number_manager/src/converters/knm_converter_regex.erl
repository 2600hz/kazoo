%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_converter_regex).

-include("knm.hrl").

-export([normalize/1, normalize/2, normalize/3
        ,to_npan/1
        ,to_1npan/1
        ]).
-export([get_e164_converters/0
        ,get_e164_converters/1
        ]).


-define(DEFAULT_E164_CONVERTERS
       ,kz_json:from_list_recursive(
          [{<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>, [{<<"prefix">>, <<"+1">>}]}
          ,{<<"^011(\\d*)$|^00(\\d*)\$">>, [{<<"prefix">>, <<"+">>}]}
          ,{<<"^[2-9]\\d{7,}\$">>, [{<<"prefix">>, <<"+">>}]}
          ])).

-define(SYSTEM_E164_CONVERTERS
       ,kapps_config:get_json(?KNM_CONFIG_CAT, ?KEY_E164_CONVERTERS, ?DEFAULT_E164_CONVERTERS)
       ).

-define(KEY_E164_CONVERTERS, <<"e164_converters">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(kz_term:ne_binary()) ->
                       kz_term:ne_binary().
normalize(?NE_BINARY = Num) ->
    to_e164(Num).

-spec normalize(kz_term:ne_binary(), kz_term:ne_binary()) ->
                       kz_term:ne_binary().
normalize(?NE_BINARY = Num, AccountId) ->
    to_e164(Num, AccountId).

-spec normalize(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
                       kz_term:ne_binary().
normalize(?NE_BINARY = Num, AccountId, DialPlan) ->
    to_e164_from_account_dialplan(Num, AccountId, DialPlan).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_npan(kz_term:ne_binary()) -> kz_term:ne_binary().
to_npan(Num) ->
    case re:run(Num, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>, [{'capture', [1], 'binary'}]) of
        'nomatch' -> Num;
        {'match', [NPAN]} -> NPAN
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_1npan(kz_term:ne_binary()) -> kz_term:ne_binary().
to_1npan(Num) ->
    case re:run(Num, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>, [{'capture', [1], 'binary'}]) of
        'nomatch' -> Num;
        {'match', [NPAN]} -> <<$1, NPAN/binary>>
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_e164(kz_term:ne_binary()) -> kz_term:ne_binary().
to_e164(<<"+",_/binary>> = N) -> N;
to_e164(Number) ->
    Converters = get_e164_converters(),
    Regexes = kz_json:get_keys(Converters),
    maybe_convert_to_e164(Regexes, Converters, Number).

to_e164(<<"+",_/binary>> = N, _AccountId) -> N;
to_e164(Number, Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            to_e164_from_account_dialplan(Number, AccountId, kzd_accounts:dial_plan(JObj));
        {'error', _E} ->
            to_e164_from_account(Number, AccountId)
    end.

-spec to_e164_from_account(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
to_e164_from_account(Number, ?NE_BINARY = AccountId) ->
    Converters = get_e164_converters(AccountId),
    Regexes = kz_json:get_keys(Converters),
    maybe_convert_to_e164(Regexes, Converters, Number).

to_e164_from_account_dialplan(Number, AccountId, 'undefined') ->
    to_e164_from_account(Number, AccountId);
to_e164_from_account_dialplan(Number, AccountId, DialPlan) ->
    to_e164_from_account_dialplan_regexes(Number
                                         ,AccountId
                                         ,DialPlan
                                         ,kz_json:get_keys(DialPlan)
                                         ).

to_e164_from_account_dialplan_regexes(Number, AccountId, _DialPlan, []) ->
    to_e164_from_account(Number, AccountId);
to_e164_from_account_dialplan_regexes(Number, AccountId, DialPlan, Regexes) ->
    to_e164_from_account(apply_dialplan(Number, DialPlan, Regexes), AccountId).

apply_dialplan(Number, _Dialplan, []) -> Number;
apply_dialplan(Number, DialPlan, [Regex|Rs]) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            apply_dialplan(Number, DialPlan, Rs);
        'match' ->
            Number;
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = kz_json:get_binary_value([Regex, <<"prefix">>], DialPlan, <<>>),
            Suffix = kz_json:get_binary_value([Regex, <<"suffix">>], DialPlan, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_e164_converters() -> kz_json:object().
get_e164_converters() ->
    try ?SYSTEM_E164_CONVERTERS
    catch
        _:_ -> ?DEFAULT_E164_CONVERTERS
    end.

-spec get_e164_converters(kz_term:ne_binary()) -> kz_json:object().
get_e164_converters(AccountId) ->
    try kapps_account_config:get_global(AccountId
                                       ,?KNM_CONFIG_CAT
                                       ,?KEY_E164_CONVERTERS
                                       ,?DEFAULT_E164_CONVERTERS
                                       )
    catch
        _:_ -> ?DEFAULT_E164_CONVERTERS
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_convert_to_e164(kz_term:ne_binaries(), kz_json:object(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_convert_to_e164([], _, Number) -> Number;
maybe_convert_to_e164([Regex|Regexs], Converters, Number) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            maybe_convert_to_e164(Regexs, Converters, Number);
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = kz_json:get_binary_value([Regex, <<"prefix">>], Converters, <<>>),
            Suffix = kz_json:get_binary_value([Regex, <<"suffix">>], Converters, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.
