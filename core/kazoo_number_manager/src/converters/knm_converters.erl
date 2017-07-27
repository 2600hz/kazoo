%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
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

-define(DEFAULT_RECONCILE_REGEX, <<"^\\+?1?\\d{10}$|^\\+[2-9]\\d{7,}$|^011\\d*$|^00\\d*\$">>).
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
        kz_json:from_list([{<<"regex">>, <<"^(911)\$">>}
                          ,{<<"emergency">>, 'true'}
                          ,{<<"friendly_name">>, <<"Emergency Dispatcher">>}
                          ])).

-define(CLASSIFIER_CARIBBEAN,
        kz_json:from_list([{<<"regex">>, <<"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})\$">>}
                          ,{<<"friendly_name">>, <<"Caribbean">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_DID_US,
        kz_json:from_list([{<<"regex">>, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})\$">>}
                          ,{<<"friendly_name">>, <<"US DID">>}
                          ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                          ])).

-define(CLASSIFIER_INTERNATIONAL,
        kz_json:from_list([{<<"regex">>, <<"^(011\\d*)$|^(00\\d*)\$">>}
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

-ifdef(TEST).
-define(DEFAULT_CONVERTER, ?DEFAULT_CONVERTER_B).
-define(RECONCILE_REGEX, ?DEFAULT_RECONCILE_REGEX).
-define(CLASSIFIERS, ?DEFAULT_CLASSIFIERS).

-else.

-define(DEFAULT_CONVERTER
       ,kapps_config:get_ne_binary(?KNM_CONFIG_CAT, <<"converter">>, ?DEFAULT_CONVERTER_B)
       ).
-define(RECONCILE_REGEX
       ,kapps_config:get_ne_binary(?KNM_CONFIG_CAT, ?KEY_RECONCILE_REGEX, ?DEFAULT_RECONCILE_REGEX)
       ).

-define(CLASSIFIERS
       ,kapps_config:get_json(?KNM_CONFIG_CAT, <<"classifiers">>, ?DEFAULT_CLASSIFIERS)
       ).
-endif.

-define(RECONCILE_REGEX(AccountId)
       ,kapps_account_config:get_global(AccountId, ?KNM_CONFIG_CAT, ?KEY_RECONCILE_REGEX, ?DEFAULT_RECONCILE_REGEX)
       ).

-define(CONVERTER_MOD, kz_term:to_atom(<<"knm_converter_", (?DEFAULT_CONVERTER)/binary>>, 'true')).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(ne_binary()) -> ne_binary();
               (ne_binaries()) -> ne_binaries().
normalize(Num=?NE_BINARY) ->
    (?CONVERTER_MOD):normalize(Num);
normalize(Nums)
  when is_list(Nums) ->
    [normalize(Num) || Num <- Nums].

-spec normalize(ne_binary(), api_ne_binary()) -> ne_binary();
               (ne_binaries(), api_ne_binary()) -> ne_binaries().
normalize(Num=?NE_BINARY, undefined) ->
    normalize(Num);
normalize(Num=?NE_BINARY, AccountId) ->
    (?CONVERTER_MOD):normalize(Num, AccountId);
normalize(Nums, undefined)
  when is_list(Nums) ->
    normalize(Nums);
normalize(Nums, AccountId)
  when is_list(Nums) ->
    [normalize(Num, AccountId) || Num <- Nums].

-spec normalize(ne_binary(), ne_binary(), kz_json:object()) -> ne_binary();
               (ne_binaries(), ne_binary(), kz_json:object()) -> ne_binaries().
normalize(Num=?NE_BINARY, ?MATCH_ACCOUNT_RAW(AccountId), DialPlan) ->
    (?CONVERTER_MOD):normalize(Num, AccountId, DialPlan);
normalize(Nums, ?MATCH_ACCOUNT_RAW(AccountId), DialPlan)
  when is_list(Nums) ->
    [normalize(Num, AccountId, DialPlan) || Num <- Nums].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_normalized(ne_binary()) -> boolean().
is_normalized(Num) ->
    normalize(Num) =:= Num.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_npan(ne_binary()) -> boolean().
is_npan(Num) ->
    to_npan(Num) =:= Num.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_npan(ne_binary()) -> ne_binary().
to_npan(Num) ->
    (?CONVERTER_MOD):to_npan(Num).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_1npan(ne_binary()) -> boolean().
is_1npan(Num) ->
    to_1npan(Num) =:= Num.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_1npan(ne_binary()) -> ne_binary().
to_1npan(Num) ->
    (?CONVERTER_MOD):to_1npan(Num).

%%--------------------------------------------------------------------
%% @public
%% @doc Given a number determine the database name it belongs to.
%%--------------------------------------------------------------------
-spec to_db(<<_:40,_:_*8>>) -> api_binary().
to_db(<<"+", NumPrefix:4/binary, _/binary>>) ->
    kz_http_util:urlencode(<<?KNM_DB_PREFIX, NumPrefix/binary>>);
to_db(_) ->
    'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns `{Reconcilables, NotReconcilables}'.
%% @end
%%--------------------------------------------------------------------
-spec are_reconcilable(ne_binaries()) -> {ne_binaries(), ne_binaries()}.
are_reconcilable(Nums) ->
    lists:partition(fun is_reconcilable/1, Nums).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_reconcilable(ne_binary()) -> boolean().
is_reconcilable(Number) ->
    Num = normalize(Number),
    is_reconcilable_by_regex(Num, ?RECONCILE_REGEX).

-spec is_reconcilable(ne_binary(), ne_binary()) -> boolean().
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


%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec classify(ne_binary()) -> api_binary().
classify(Number) ->
    Num = normalize(Number),
    classify_number(Num, kz_json:to_proplist(?CLASSIFIERS)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available_classifiers() -> kz_json:object().
available_classifiers() ->
    kz_json:foldl(fun correct_depreciated_classifiers/3, kz_json:new(), ?CLASSIFIERS).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available_converters() -> ne_binaries().
available_converters() ->
    kapps_config:get_ne_binaries(?KNM_CONFIG_CAT, <<"converters">>, ?DEFAULT_CONVERTERS).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec default_converter() -> atom().
default_converter() -> ?CONVERTER_MOD.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec classify_number(ne_binary(), kz_proplist() | ne_binary()) -> api_binary().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_classifier_regex(ne_binary() | kz_json:object()) -> ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    kz_json:get_value(<<"regex">>, JObj, <<"^$">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec correct_depreciated_classifiers(kz_json:path(), kz_json:json_term(), kz_json:object()) ->
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
