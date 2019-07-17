%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_converters).

-include("knm.hrl").

-export([normalize/1, normalize/2, normalize/3
        ,is_normalized/1
        ,is_npan/1, to_npan/1
        ,is_1npan/1, to_1npan/1
        ,to_db/1
        ,is_reconcilable/1, is_reconcilable/2, are_reconcilable/1
        ,classify/1, available_classifiers/0
        ,available_converters/0
        ,default_converter/0
        ]).

-define(DEFAULT_CONVERTER_B, <<"regex">>).
-define(DEFAULT_CONVERTERS, [?DEFAULT_CONVERTER_B]).

-define(DEFAULT_RECONCILE_REGEX, <<"^(\\+?1)?\\d{10}$|^\\+[2-9]\\d{7,}$|^011\\d*$|^00\\d*$">>).
-define(KEY_RECONCILE_REGEX, <<"reconcile_regex">>).

-define(CLASSIFIER_TOLLFREE_US,
        kz_json:from_list([{<<"regex">>, <<"^\\+1((?:800|88\\d|877|866|855|844|833|822)\\d{7})\$">>}
                          ,{<<"friendly_name">>, <<"US TollFree">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_TOLL_US,
        kz_json:from_list([{<<"regex">>, <<"^\\+1(900\\d{7})\$">>}
                          ,{<<"friendly_name">>, <<"US Toll">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_EMERGENCY,
        kz_json:from_list([{<<"regex">>, <<"^(911|922|933|833|811|711|999)\$">>}
                          ,{<<"emergency">>, 'true'}
                          ,{<<"friendly_name">>, <<"Emergency Dispatcher">>}
                          ])).

-define(CLASSIFIER_CARIBBEAN,
        kz_json:from_list([{<<"regex">>, <<"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})\$">>}
                          ,{<<"friendly_name">>, <<"Caribbean">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_DID_US,
        kz_json:from_list([{<<"regex">>, <<"^(\\+?1)?([2-9][0-9]{2}[2-9][0-9]{6})\$">>}
                          ,{<<"friendly_name">>, <<"US DID">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_INTERNATIONAL,
        kz_json:from_list([{<<"regex">>, <<"^\\+([2-9]\\d{7,})\$">>}
                          ,{<<"friendly_name">>, <<"International">>}
                          ])).

-define(CLASSIFIER_UNKNOWN,
        kz_json:from_list([{<<"regex">>, <<"^(.*)\$">>}
                          ,{<<"friendly_name">>, <<"Unknown">>}
                          ])).

-define(DEFAULT_CLASSIFIERS,
        kz_json:from_list([{<<"tollfree_us">>, ?CLASSIFIER_TOLLFREE_US}
                          ,{<<"toll_us">>, ?CLASSIFIER_TOLL_US}
                          ,{<<"emergency">>, ?CLASSIFIER_EMERGENCY}
                          ,{<<"caribbean">>, ?CLASSIFIER_CARIBBEAN}
                          ,{<<"did_us">>, ?CLASSIFIER_DID_US}
                          ,{<<"international">>, ?CLASSIFIER_INTERNATIONAL}
                          ,{<<"unknown">>, ?CLASSIFIER_UNKNOWN}
                          ])).

-define(DEFAULT_CONVERTER
       ,kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"converter">>, ?DEFAULT_CONVERTER_B)
       ).
-define(RECONCILE_REGEX
       ,kapps_config:get_ne_binary(?KNM_CONFIG_CAT, ?KEY_RECONCILE_REGEX, ?DEFAULT_RECONCILE_REGEX)
       ).

-ifdef(TEST).
%% Orders of classifiers matters, but unfortunately schema file system_config.number_manager.json
%% is being sorted on Kazoo compile so we need to access the macro directly here for testing proposes.
-define(CLASSIFIERS, ?DEFAULT_CLASSIFIERS).
-else.
-define(CLASSIFIERS
       ,kapps_config:get_json(?KNM_CONFIG_CAT, <<"classifiers">>, ?DEFAULT_CLASSIFIERS)
       ).
-endif.

-define(RECONCILE_REGEX(AccountId)
       ,kapps_account_config:get_global(AccountId, ?KNM_CONFIG_CAT, ?KEY_RECONCILE_REGEX, ?DEFAULT_RECONCILE_REGEX)
       ).

-define(CONVERTER_MOD, kz_term:to_atom(<<"knm_converter_", (?DEFAULT_CONVERTER)/binary>>, 'true')).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec normalize(kz_term:ne_binary()) -> kz_term:ne_binary();
               (kz_term:ne_binaries()) -> kz_term:ne_binaries().
normalize(Num=?NE_BINARY) ->
    (?CONVERTER_MOD):normalize(Num);
normalize(Nums)
  when is_list(Nums) ->
    [normalize(Num) || Num <- Nums].

-spec normalize(kz_term:ne_binary(), kz_term:api_ne_binary()) -> kz_term:ne_binary();
               (kz_term:ne_binaries(), kz_term:api_ne_binary()) -> kz_term:ne_binaries().
normalize(Num=?NE_BINARY, 'undefined') ->
    normalize(Num);
normalize(Num=?NE_BINARY, AccountId) ->
    (?CONVERTER_MOD):normalize(Num, AccountId);
normalize(Nums, 'undefined')
  when is_list(Nums) ->
    normalize(Nums);
normalize(Nums, AccountId)
  when is_list(Nums) ->
    [normalize(Num, AccountId) || Num <- Nums].

-spec normalize(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binary();
               (kz_term:ne_binaries(), kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binaries().
normalize(Num=?NE_BINARY, ?MATCH_ACCOUNT_RAW(AccountId), DialPlan) ->
    (?CONVERTER_MOD):normalize(Num, AccountId, DialPlan);
normalize(Nums, ?MATCH_ACCOUNT_RAW(AccountId), DialPlan)
  when is_list(Nums) ->
    [normalize(Num, AccountId, DialPlan) || Num <- Nums].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_normalized(kz_term:ne_binary()) -> boolean().
is_normalized(Num) ->
    normalize(Num) =:= Num.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_npan(kz_term:ne_binary()) -> boolean().
is_npan(Num) ->
    to_npan(Num) =:= Num.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_npan(kz_term:ne_binary()) -> kz_term:ne_binary().
to_npan(Num) ->
    (?CONVERTER_MOD):to_npan(Num).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_1npan(kz_term:ne_binary()) -> boolean().
is_1npan(Num) ->
    to_1npan(Num) =:= Num.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_1npan(kz_term:ne_binary()) -> kz_term:ne_binary().
to_1npan(Num) ->
    (?CONVERTER_MOD):to_1npan(Num).

%%------------------------------------------------------------------------------
%% @doc Given a number determine the database name it belongs to..
%% @end
%%------------------------------------------------------------------------------
-spec to_db(<<_:40,_:_*8>>) -> kz_term:api_binary().
to_db(<<NumPrefix:5/binary, _/binary>>) ->
    kz_http_util:urlencode(<<?KNM_DB_PREFIX, NumPrefix/binary>>);
to_db(_) -> 'undefined'.

%%------------------------------------------------------------------------------
%% @doc Returns `{Reconcilables, NotReconcilables}'.
%% @end
%%------------------------------------------------------------------------------
-spec are_reconcilable(kz_term:ne_binaries()) -> {kz_term:ne_binaries(), kz_term:ne_binaries()}.
are_reconcilable(Nums) ->
    lists:partition(fun is_reconcilable/1, Nums).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_reconcilable(kz_term:ne_binary()) -> boolean().
is_reconcilable(Number) ->
    Num = normalize(Number),
    is_reconcilable_by_regex(Num, ?RECONCILE_REGEX).

-spec is_reconcilable(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_reconcilable(Number, AccountId) ->
    Num = normalize(Number, AccountId),
    is_reconcilable_by_regex(Num, ?RECONCILE_REGEX(AccountId)).

is_reconcilable_by_regex(Num, Regex) ->
    case re:run(Num, Regex) of
        'nomatch' ->
            lager:debug("number '~s' is not reconcilable", [Num]),
            'false';
        _ -> 'true'
    end.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec classify(kz_term:ne_binary()) -> kz_term:api_binary().
classify(Number) ->
    Num = normalize(Number),
    classify_number(Num, kz_json:to_proplist(?CLASSIFIERS)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_classifiers() -> kz_json:object().
available_classifiers() ->
    kz_json:foldl(fun correct_depreciated_classifiers/3, kz_json:new(), ?CLASSIFIERS).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_converters() -> kz_term:ne_binaries().
available_converters() ->
    kapps_config:get_ne_binaries(?KNM_CONFIG_CAT, <<"converters">>, ?DEFAULT_CONVERTERS).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_converter() -> atom().
default_converter() -> ?CONVERTER_MOD.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec classify_number(kz_term:ne_binary(), kz_term:proplist() | kz_term:ne_binary()) -> kz_term:api_binary().
classify_number(Num, []) ->
    lager:debug("unable to classify number ~s", [Num]),
    'undefined';
classify_number(Num, [{Classification, Classifier}|Classifiers]) ->
    case re:run(Num, get_classifier_regex(Classifier)) of
        'nomatch' -> classify_number(Num, Classifiers);
        _ ->
            lager:debug("number '~s' is classified as ~s", [Num, Classification]),
            kz_term:to_binary(Classification)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_classifier_regex(kz_term:ne_binary() | kz_json:object()) -> kz_term:ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    kz_json:get_value(<<"regex">>, JObj, <<"^$">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec correct_depreciated_classifiers(kz_json:key(), kz_json:json_term(), kz_json:object()) ->
                                                 kz_json:object().
correct_depreciated_classifiers(Classifier, ?NE_BINARY=Regex, JObj) ->
    J = kz_json:from_list([{<<"regex">>, Regex}
                          ,{<<"friendly_name">>, Classifier}
                          ]),
    kz_json:set_value(Classifier, J, JObj);
correct_depreciated_classifiers(Classifier, J, JObj) ->
    case kz_json:get_value(<<"friendly_name">>, J) of
        'undefined' ->
            Updated = kz_json:set_value(<<"friendly_name">>, Classifier, JObj),
            kz_json:set_value(Classifier, Updated, JObj);
        _Else ->
            kz_json:set_value(Classifier, J, JObj)
    end.
