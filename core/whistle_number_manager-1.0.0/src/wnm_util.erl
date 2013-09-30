%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_util).

-ifndef(TEST).
-export([pretty_print/1
         ,pretty_print/2
        ]).
-endif.

-export([available_classifiers/0]).
-export([classify_number/1]).
-export([is_reconcilable/1]).
-export([list_carrier_modules/0]).
-export([get_carrier_module/1]).
-export([number_to_db_name/1]).
-export([normalize_number/1]).
-export([to_e164/1, to_npan/1, to_1npan/1]).
-export([is_e164/1, is_npan/1, is_1npan/1]).
-export([find_account_id/1]).
-export([get_all_number_dbs/0]).
-export([are_jobjs_identical/2]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wnm.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CLASSIFIERS, [{<<"tollfree_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+1(800|888|877|866|855)\\d{7}$">>}
                                                                     ,{<<"friendly_name">>, <<"US TollFree">>}
                                                                     ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                    ])}
                              ,{<<"toll_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+1900\\d{7}$">>}
                                                                  ,{<<"friendly_name">>, <<"US Toll">>}
                                                                  ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                 ])}
                              ,{<<"emergency">>, wh_json:from_list([{<<"regex">>, <<"^911$">>}
                                                                    ,{<<"friendly_name">>, <<"Emergency Dispatcher">>}
                                                                   ])}
                              ,{<<"caribbean">>, wh_json:from_list([{<<"regex">>, <<"^\\+?1(684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7}$">>}
                                                                    ,{<<"friendly_name">>, <<"Caribbean">>}
                                                                    ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                   ])}
                              ,{<<"did_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>}
                                                                 ,{<<"friendly_name">>, <<"US DID">>}
                                                                 ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                ])}
                              ,{<<"international">>, wh_json:from_list([{<<"regex">>, <<"^011\\d*$|^00\\d*$">>}
                                                                        ,{<<"friendly_name">>, <<"International">>}
                                                                       ])}
                              ,{<<"unknown">>, wh_json:from_list([{<<"regex">>, <<"^.*$">>}
                                                                  ,{<<"friendly_name">>, <<"Unknown">>}
                                                                 ])}
                             ]).
-define(DEFAULT_E164_CONVERTERS, [{<<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>
                                       ,wh_json:from_list([{<<"prefix">>, <<"+1">>}])
                                  }
                                  ,{<<"^011(\\d*)$|^00(\\d*)$">>
                                        ,wh_json:from_list([{<<"prefix">>, <<"+">>}])
                                   }
                                 ]).
-define(DEFAULT_RECONCILE_REGEX, <<"^\\+?1?\\d{10}$">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-ifndef(TEST).
-spec pretty_print(ne_binary()) -> ne_binary().
pretty_print(Number) ->
    case pretty_print_format(Number) of
        'undefined' -> Number;
        Format ->
            pretty_print(Format, Number)
    end.

-spec pretty_print(ne_binary(), ne_binary()) -> ne_binary().
pretty_print(Format, Number) ->
    Num = wnm_util:normalize_number(Number),
    pretty_print(Format, Num, <<>>).

-spec pretty_print(binary(), binary(), binary()) -> binary().
pretty_print(<<>>, _, Acc) -> Acc;
pretty_print(<<"\\S", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "S">>);
pretty_print(<<"\\#", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "#">>);
pretty_print(<<"\\*", Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, "*">>);
pretty_print(<<"S", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"#", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"*", Format/binary>>, <<>>, Acc) ->
    pretty_print(Format, <<>>, Acc);
pretty_print(<<"S", Format/binary>>, Number, Acc) ->
    pretty_print(Format, binary_tail(Number), Acc);
pretty_print(<<"#", Format/binary>>, Number, Acc) ->
    pretty_print(Format
                 ,binary_tail(Number)
                 ,<<Acc/binary, (binary_head(Number))/binary>>);
pretty_print(<<"*", Format/binary>>, Number, Acc) ->
    pretty_print(Format, <<>>, <<Acc/binary, Number/binary>>);
pretty_print(<<Char, Format/binary>>, Number, Acc) ->
    pretty_print(Format, Number, <<Acc/binary, Char/integer>>).

-spec binary_tail(ne_binary()) -> ne_binary().
binary_tail(Binary) ->
    binary:part(Binary, 1, byte_size(Binary) - 1).

-spec binary_head(ne_binary()) -> ne_binary().
binary_head(Binary) ->
    binary:part(Binary, 0, 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_format(ne_binary()) -> api_binary().
pretty_print_format(Number) ->
    Default = wh_json:from_list(?DEFAULT_CLASSIFIERS),
    Classifiers = whapps_config:get(?WNM_CONFIG_CAT, <<"classifiers">>, Default),
    Num = wnm_util:normalize_number(Number),
    pretty_print_format(Num, wh_json:to_proplist(Classifiers)).

-spec pretty_print_format(ne_binary(), wh_proplist()) -> api_binary().
pretty_print_format(Num, []) ->
    lager:debug("unable to get pretty print format for number ~s", [Num]),
    maybe_use_us_default(Num);
pretty_print_format(Num, [{Classification, Classifier}|Classifiers]) ->
    case re:run(Num, get_classifier_regex(Classifier)) of
        'nomatch' -> pretty_print_format(Num, Classifiers);
        _ when is_binary(Classifier) ->
            lager:debug("number '~s' is classified as ~s but no pretty print format available", [Num, Classification]),
            maybe_use_us_default(Num);
        _ ->
            case wh_json:get_value(<<"pretty_print">>, Classifier) of
                'undefined' -> maybe_use_us_default(Num);
                Format -> Format
            end
    end.

-spec maybe_use_us_default(ne_binary()) -> api_binary().
maybe_use_us_default(<<"+1", _/binary>>) ->
    lager:debug("using US number default pretty print", []),
    <<"SS(###) ### - ####">>;
maybe_use_us_default(_) ->
    'undefined'.

-endif.
%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available_classifiers() -> api_binaries().
available_classifiers() ->
    Default = wh_json:from_list(?DEFAULT_CLASSIFIERS),
    Classifiers = whapps_config:get(?WNM_CONFIG_CAT, <<"classifiers">>, Default),
    correct_depreciated_classifiers(wh_json:to_proplist(Classifiers)).

-spec correct_depreciated_classifiers(wh_proplist()) -> wh_json:object().
correct_depreciated_classifiers(Classifiers) ->
    correct_depreciated_classifiers(Classifiers, wh_json:new()).

-spec correct_depreciated_classifiers(wh_proplist(), wh_json:object()) -> wh_json:object().
correct_depreciated_classifiers([], JObj) ->
    JObj;
correct_depreciated_classifiers([{Classifier, Regex}|Classifiers], JObj) when is_binary(Regex) ->
    J = wh_json:from_list([{<<"regex">>, Regex}
                           ,{<<"friendly_name">>, Classifier}
                          ]),
    correct_depreciated_classifiers(Classifiers, wh_json:set_value(Classifier, J, JObj));
correct_depreciated_classifiers([{Classifier, J}|Classifiers], JObj) ->
    case wh_json:get_value(<<"friendly_name">>, J) of
        'undefined' ->
            Updated = wh_json:set_value(<<"friendly_name">>, Classifier, JObj),
            correct_depreciated_classifiers(Classifiers, wh_json:set_value(Classifier, Updated, JObj));
        _Else ->
            correct_depreciated_classifiers(Classifiers, wh_json:set_value(Classifier, J, JObj))
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Classify a provided number
%% @end
%%--------------------------------------------------------------------
-spec classify_number(ne_binary()) -> api_binary().
-spec classify_number(ne_binary(), wh_proplist()) -> api_binary().

classify_number(Number) ->
    Default = wh_json:from_list(?DEFAULT_CLASSIFIERS),
    Classifiers = whapps_config:get(?WNM_CONFIG_CAT, <<"classifiers">>, Default),
    Num = wnm_util:normalize_number(Number),
    classify_number(Num, wh_json:to_proplist(Classifiers)).

classify_number(Num, []) ->
    lager:debug("unable to classify number ~s", [Num]),
    'undefined';
classify_number(Num, [{Classification, Classifier}|Classifiers]) ->
    case re:run(Num, get_classifier_regex(Classifier)) of
        'nomatch' -> classify_number(Num, Classifiers);
        _ ->
            lager:debug("number '~s' is classified as ~s", [Num, Classification]),
            wh_util:to_binary(Classification)
    end.

-spec get_classifier_regex(ne_binary() | wh_json:object()) -> ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    wh_json:get_value(<<"regex">>, JObj, <<"^$">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Determines if a given number is reconcilable
%% @end
%%--------------------------------------------------------------------
-spec is_reconcilable(ne_binary()) -> boolean().
is_reconcilable(Number) ->
    Regex = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"reconcile_regex">>, ?DEFAULT_RECONCILE_REGEX),
    Num = wnm_util:normalize_number(Number),
    case re:run(Num, Regex) of
        'nomatch' ->
            lager:debug("number '~s' is not reconcilable", [Num]),
            'false';
        _ ->
            lager:debug("number '~s' can be reconciled, proceeding", [Num]),
            'true'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number doc determine if the carrier module is available
%% and if return the name as well as the data
%% @end
%%--------------------------------------------------------------------
-spec get_carrier_module(wh_json:object()) -> atom().
get_carrier_module(JObj) ->
    case wh_json:get_ne_value(<<"pvt_module_name">>, JObj) of
        'undefined' ->
            lager:debug("carrier module not specified on number document"),
            'undefined';
        Module ->
            Carriers = list_carrier_modules(),
            Carrier = wh_util:try_load_module(Module),
            case lists:member(Carrier, Carriers) of
                'true' -> Carrier;
                'false' ->
                    lager:debug("carrier module ~s specified on number document does not exist", [Carrier]),
                    'undefined'
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a list of all available carrier modules
%% @end
%%--------------------------------------------------------------------
-spec list_carrier_modules() -> [] | [atom(),...].
list_carrier_modules() ->
    CarrierModules =
        whapps_config:get(?WNM_CONFIG_CAT, <<"carrier_modules">>, ?WNM_DEAFULT_CARRIER_MODULES),
    [Module || M <- CarrierModules, (Module = wh_util:try_load_module(M)) =/= 'false'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a number determine the database name that it should belong to.
%% @end
%%--------------------------------------------------------------------
-spec number_to_db_name(binary()) -> api_binary().
number_to_db_name(<<NumPrefix:5/binary, _/binary>>) ->
    wh_util:to_binary(
      http_uri:encode(
        wh_util:to_list(
          list_to_binary([?WNM_DB_PREFIX, NumPrefix])
         )
       )
     );
number_to_db_name(_) ->
    'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a provided term into a e164 binary string.
%% @end
%%--------------------------------------------------------------------
-spec normalize_number(string() | binary()) -> binary().
normalize_number(Number) when is_binary(Number) ->
    to_e164(Number);
normalize_number(Number) ->
    normalize_number(wh_util:to_binary(Number)).

-spec is_e164(ne_binary()) -> boolean().
-spec is_npan(ne_binary()) -> boolean().
-spec is_1npan(ne_binary()) -> boolean().

is_e164(DID) ->
    DID =:= to_e164(DID).

is_npan(DID) ->
    re:run(DID, <<"^[2-9][0-9]{2}[2-9][0-9]{6}$">>) =/= 'nomatch'.

is_1npan(DID) ->
    re:run(DID, <<"^\\+1[2-9][0-9]{2}[2-9][0-9]{6}$">>) =/= 'nomatch'.

%% +18001234567 -> +18001234567
-spec to_e164(ne_binary()) -> ne_binary().
to_e164(<<$+, _/binary>> = N) -> N;
to_e164(Number) ->
    Converters = get_e164_converters(),
    Regexs = wh_json:get_keys(Converters),
    maybe_convert_to_e164(Regexs, Converters, Number).

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

%% end up with 8001234567 from 1NPAN and E.164
-spec to_npan(ne_binary()) -> ne_binary().
to_npan(Number) ->
    case re:run(Number, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>, [{'capture', [1], 'binary'}]) of
        'nomatch' -> Number;
        {'match', [NPAN]} -> NPAN
    end.

-spec to_1npan(ne_binary()) -> ne_binary().
to_1npan(Number) ->
    case re:run(Number, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>, [{'capture', [1], 'binary'}]) of
        'nomatch' -> Number;
        {'match', [NPAN]} -> <<$1, NPAN/binary>>
    end.

-spec get_e164_converters() -> wh_json:object().
get_e164_converters() ->
    Default = wh_json:from_list(?DEFAULT_E164_CONVERTERS),
    try whapps_config:get(?WNM_CONFIG_CAT, <<"e164_converters">>, Default) of
        Converters -> Converters
    catch
        _:_ ->
            Default
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check all the fields that might have an account id in hierarchical
%% order
%% @end
%%--------------------------------------------------------------------
-spec find_account_id(wh_json:object()) -> api_binary().
find_account_id(JObj) ->
    SearchFuns = [fun(_) -> wh_json:get_ne_value(<<"pvt_assigned_to">>, JObj) end
                  ,fun('undefined') -> wh_json:get_ne_value(<<"pvt_reserved_for">>, JObj);
                      (Else) -> Else
                   end
                  ,fun('undefined') -> wh_json:get_ne_value(<<"pvt_previously_assigned_to">>, JObj);
                      (Else) -> Else
                   end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, 'undefined', SearchFuns).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will return a list of all number database names
%% @end
%%--------------------------------------------------------------------
-spec get_all_number_dbs() -> ne_binaries().
get_all_number_dbs() ->
    {'ok', Databases} = couch_mgr:db_info(),
    [Db || Db <- Databases, is_number_db(Db)].

is_number_db(<<"numbers/", _/binary>>) -> 'true';
is_number_db(<<"numbers%2f", _/binary>>) -> 'true';
is_number_db(<<"numbers%2F", _/binary>>) -> 'true';
is_number_db(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec are_jobjs_identical(api_object(), api_object()) -> boolean().
are_jobjs_identical('undefined', 'undefined') -> 'true';
are_jobjs_identical('undefined', _) -> 'false';
are_jobjs_identical(_, 'undefined') -> 'false';
are_jobjs_identical(JObj1, JObj2) ->
    [KV || {_, V}=KV <- wh_json:to_proplist(JObj1), (not wh_util:is_empty(V))]
        =:=
    [KV || {_, V}=KV <- wh_json:to_proplist(JObj2), (not wh_util:is_empty(V))].

-ifdef(TEST).
%% PROPER TESTING
%%
%%% 1000000000
%% (AAABBBCCCC, 1AAABBBCCCC) -> AAABBBCCCCCC.
prop_to_npan() ->
    ?FORALL(Number, range(2002000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                NPAN = to_npan(BinNum),
                case byte_size(BinNum) of
                    11 -> BinNum =:= <<"1", NPAN/binary>>;
                    _ -> NPAN =:= BinNum
                end
            end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> 1AAABBBCCCCCC.
prop_to_1npan() ->
    ?FORALL(Number, range(2002000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                OneNPAN = to_1npan(BinNum),
                case byte_size(BinNum) of
                    11 -> OneNPAN =:= BinNum;
                    _ -> OneNPAN =:= <<"1", BinNum/binary>>
                end
            end).

%% (AAABBBCCCC, 1AAABBBCCCC) -> +1AAABBBCCCCCC.
prop_to_e164() ->
    ?FORALL(Number, range(2002000000,19999999999),
            begin
                BinNum = wh_util:to_binary(Number),
                E164 = to_e164(BinNum),
                case byte_size(BinNum) of
                    11 -> E164 =:= <<$+, BinNum/binary>>;
                    10 -> E164 =:= <<$+, $1, BinNum/binary>>;
                    _ -> E164 =:= BinNum
                end
            end).

%% EUNIT TESTING
%%

proper_test_() ->
    {"Runs the module's PropEr tests during eunit testing",
     {'timeout', 15000,
      [
       ?_assertEqual([], proper:module(?MODULE, [{'max_shrinks', 0}]))
      ]}}.

to_e164_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"+12234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_e164(N), Ans) end, Ns).

to_npan_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"2234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_npan(N), Ans) end, Ns).

to_1npan_test() ->
    Ns = [<<"+12234567890">>, <<"12234567890">>, <<"2234567890">>],
    Ans = <<"12234567890">>,
    lists:foreach(fun(N) -> ?assertEqual(to_1npan(N), Ans) end, Ns).

-endif.
