%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_converter_regex).

-include("../knm.hrl").

-export([normalize/1, normalize/2
         ,to_npan/1
         ,to_1npan/1
        ]).

-define(DEFAULT_E164_CONVERTERS, [{<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>
                                   ,wh_json:from_list([{<<"prefix">>, <<"+1">>}])
                                  }
                                  ,{<<"^011(\\d*)$|^00(\\d*)$">>
                                    ,wh_json:from_list([{<<"prefix">>, <<"+">>}])
                                   }
                                  ,{<<"^[2-9]\\d{7,}$">>
                                    ,wh_json:from_list([{<<"prefix">>, <<"+">>}])
                                   }
                                 ]).

-define(KEY_E164_CONVERTERS, <<"e164_converters">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(ne_binary()) -> ne_binary().
normalize(<<_/binary>> = Num) ->
    to_e164(Num).

-spec normalize(ne_binary(), api_binary()) -> ne_binary().
normalize(<<_/binary>> = Num, 'undefined') ->
    to_e164(Num);
normalize(<<_/binary>> = Num, AccountId) ->
    to_e164(Num, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_npan(ne_binary()) -> ne_binary().
to_npan(Num) ->
    case re:run(Num, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>, [{'capture', [1], 'binary'}]) of
        'nomatch' -> Num;
        {'match', [NPAN]} -> NPAN
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_1npan(ne_binary()) -> ne_binary().
to_1npan(Num) ->
    case re:run(Num, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>, [{'capture', [1], 'binary'}]) of
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
-spec to_e164(ne_binary()) -> ne_binary().
to_e164(<<$+, _/binary>> = N) -> N;
to_e164(Number) ->
    Converters = get_e164_converters(),
    Regexes = wh_json:get_keys(Converters),
    maybe_convert_to_e164(Regexes, Converters, Number).

to_e164(<<$+, _/binary>> = N, _AccountId) -> N;
to_e164(Number, Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case kz_account:fetch(AccountId) of
        {'ok', JObj} ->
            to_e164_from_account_dialplan(Number, AccountId, kz_account:dial_plan(JObj));
        {'error', _E} ->
            to_e164_from_account(Number, AccountId)
    end.

-spec to_e164_from_account(ne_binary(), ne_binary()) -> ne_binary().
to_e164_from_account(Number, <<_/binary>> = AccountId) ->
    Converters = get_e164_converters(AccountId),
    Regexes = wh_json:get_keys(Converters),
    maybe_convert_to_e164(Regexes, Converters, Number).

to_e164_from_account_dialplan(Number, AccountId, 'undefined') ->
    to_e164_from_account(Number, AccountId);
to_e164_from_account_dialplan(Number, AccountId, DialPlan) ->
    to_e164_from_account_dialplan_regexes(Number
                                          ,AccountId
                                          ,DialPlan
                                          ,wh_json:get_keys(DialPlan)
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
            Prefix = wh_json:get_binary_value([Regex, <<"prefix">>], DialPlan, <<>>),
            Suffix = wh_json:get_binary_value([Regex, <<"suffix">>], DialPlan, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_e164_converters() -> wh_json:object().
get_e164_converters() ->
    Default = wh_json:from_list(?DEFAULT_E164_CONVERTERS),
    try whapps_config:get(?KNM_CONFIG_CAT
                          ,?KEY_E164_CONVERTERS
                          ,Default
                         )
    of
        Converters -> Converters
    catch
        _:_ -> Default
    end.

-spec get_e164_converters(ne_binary()) -> wh_json:object().
get_e164_converters(AccountId) ->
    Default = wh_json:from_list(?DEFAULT_E164_CONVERTERS),
    try whapps_account_config:get_global(AccountId
                                         ,?KNM_CONFIG_CAT
                                         ,?KEY_E164_CONVERTERS
                                         ,Default
                                        )
    of
        Converters -> Converters
    catch
        _:_ -> Default
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_convert_to_e164(ne_binaries(), wh_json:object(), ne_binary()) -> ne_binary().
maybe_convert_to_e164([], _, Number) -> Number;
maybe_convert_to_e164([Regex|Regexs], Converters, Number) ->
    case re:run(Number, Regex, [{'capture', 'all', 'binary'}]) of
        'nomatch' ->
            maybe_convert_to_e164(Regexs, Converters, Number);
        {'match', Captures} ->
            Root = lists:last(Captures),
            Prefix = wh_json:get_binary_value([Regex, <<"prefix">>], Converters, <<>>),
            Suffix = wh_json:get_binary_value([Regex, <<"suffix">>], Converters, <<>>),
            <<Prefix/binary, Root/binary, Suffix/binary>>
    end.
