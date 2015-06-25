%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_converters).

-include("../knm.hrl").

-export([
    normalize/1, is_normalized/1
    ,is_npan/1 ,is_1npan/1
    ,to_db/1
    ,is_reconcilable/1
    ,classify/1, available_classifiers/0
    ,available_converters/0, default/0
]).

-define(DEFAULT_CONVERTERS, [<<"regex">>]).
-define(DEFAULT_CONVERTER, whapps_config:get(?KNM_CONFIG_CAT, <<"converter">>, <<"regex">>)).
-define(CONVERTER_MOD, wh_util:to_atom(<<"knm_converter_", (?DEFAULT_CONVERTER)/binary>>, 'true')).

-define(DEFAULT_RECONCILE_REGEX, <<"^\\+?1?\\d{10}$|^\\+[2-9]\\d{7,}$|^011\\d*$|^00\\d*$">>).
-define(DEFAULT_CLASSIFIERS, [{<<"tollfree_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+1((?:800|888|877|866|855)\\d{7})$">>}
                                                                     ,{<<"friendly_name">>, <<"US TollFree">>}
                                                                     ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                    ])}
                              ,{<<"toll_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+1(900\\d{7})$">>}
                                                                  ,{<<"friendly_name">>, <<"US Toll">>}
                                                                  ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                 ])}
                              ,{<<"emergency">>, wh_json:from_list([{<<"regex">>, <<"^(911)$">>}
                                                                    ,{<<"friendly_name">>, <<"Emergency Dispatcher">>}
                                                                   ])}
                              ,{<<"caribbean">>, wh_json:from_list([{<<"regex">>, <<"^\\+?1((?:684|264|268|242|246|441|284|345|767|809|829|849|473|671|876|664|670|787|939|869|758|784|721|868|649|340)\\d{7})$">>}
                                                                    ,{<<"friendly_name">>, <<"Caribbean">>}
                                                                    ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                   ])}
                              ,{<<"did_us">>, wh_json:from_list([{<<"regex">>, <<"^\\+?1?([2-9][0-9]{2}[2-9][0-9]{6})$">>}
                                                                 ,{<<"friendly_name">>, <<"US DID">>}
                                                                 ,{<<"pretty_print">>, <<"SS(###) ### - ####">>}
                                                                ])}
                              ,{<<"international">>, wh_json:from_list([{<<"regex">>, <<"^(011\\d*)$|^(00\\d*)$">>}
                                                                        ,{<<"friendly_name">>, <<"International">>}
                                                                       ])}
                              ,{<<"unknown">>, wh_json:from_list([{<<"regex">>, <<"^(.*)$">>}
                                                                  ,{<<"friendly_name">>, <<"Unknown">>}
                                                                 ])}
                             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec normalize(ne_binary()) -> ne_binary().
normalize(Num) ->
    (?CONVERTER_MOD):normalize(Num).

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
    (?CONVERTER_MOD):to_npan(Num) =:= Num.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_1npan(ne_binary()) -> boolean().
is_1npan(Num) ->
    (?CONVERTER_MOD):to_1npan(Num) =:= Num.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec to_db(ne_binary()) -> api_binary().
to_db(<<NumPrefix:5/binary, _/binary>>) ->
    cow_qs:urlencode(<<?KNM_DB_PREFIX/binary, NumPrefix/binary>>);
to_db(_) ->
    'undefined'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_reconcilable(ne_binary()) -> boolean().
is_reconcilable(Number) ->
    Regex = whapps_config:get_binary(?KNM_CONFIG_CAT, <<"reconcile_regex">>, ?DEFAULT_RECONCILE_REGEX),
    Num = normalize(Number),
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
%% @end
%%--------------------------------------------------------------------
-spec classify(ne_binary()) -> api_binary().
classify(Number) ->
    Default = wh_json:from_list(?DEFAULT_CLASSIFIERS),
    Classifiers = whapps_config:get(?KNM_CONFIG_CAT, <<"classifiers">>, Default),
    Num = normalize(Number),
    classify_number(Num, wh_json:to_proplist(Classifiers)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available_classifiers() -> wh_json:object().
available_classifiers() ->
    Default = wh_json:from_list(?DEFAULT_CLASSIFIERS),
    Classifiers = whapps_config:get(?KNM_CONFIG_CAT, <<"classifiers">>, Default),
    correct_depreciated_classifiers(wh_json:to_proplist(Classifiers)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec available_converters() -> ne_binaries().
available_converters() ->
    Default = wh_json:from_list(?DEFAULT_CONVERTERS),
    whapps_config:get(?KNM_CONFIG_CAT, <<"converters">>, Default).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec default() -> atom().
default() ->
    ?CONVERTER_MOD.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec classify_number(ne_binary(), wh_proplist() | ne_binary()) -> api_binary().
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_classifier_regex(ne_binary() | wh_json:object()) -> ne_binary().
get_classifier_regex(Classifier) when is_binary(Classifier) ->
    Classifier;
get_classifier_regex(JObj) ->
    wh_json:get_value(<<"regex">>, JObj, <<"^$">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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