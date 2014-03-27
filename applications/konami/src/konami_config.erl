%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handles Konami code configs
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_config).

-export([numbers/0, numbers/1
         ,patterns/0, patterns/1
         ,binding_key/0, binding_key/1
         ,timeout/0, timeout/1
        ]).

-include("konami.hrl").

-define(DEFAULT_BINDING_KEY, <<"*">>).
-define(DEFAULT_DIGIT_TIMEOUT, 3000).

-define(META_SAY_HI, wh_json:from_list([{<<"module">>, <<"say">>}
                                        ,{<<"data">>, wh_json:from_list([{<<"text">>, <<"hi">>}])}
                                       ])).

-define(DEFAULT_NUMBERS, wh_json:from_list([{<<"2">>, ?META_SAY_HI}
                                           ])).
-define(DEFAULT_PATTERNS, wh_json:from_list([{<<"^2$">>, ?META_SAY_HI}
                                            ])).

-type default_fun() :: fun(() -> term()).
-type formatter_fun() :: fun((term()) -> term()).

-spec numbers() -> wh_json:object().
-spec numbers(ne_binary()) -> wh_json:object().
numbers() ->
    whapps_config:get(<<"konami_codes">>, <<"numbers">>, ?DEFAULT_NUMBERS).

numbers(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case konami_doc(AccountDb) of
        'undefined' -> numbers();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"numbers">>, fun numbers/0)
    end.

-spec patterns() -> wh_json:object().
-spec patterns(ne_binary()) -> wh_json:object().
patterns() ->
    whapps_config:get(<<"konami_codes">>, <<"patterns">>, ?DEFAULT_PATTERNS).

patterns(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case konami_doc(AccountDb) of
        'undefined' -> patterns();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"patterns">>, fun patterns/0)
    end.

-spec binding_key() -> <<_:8>>.
-spec binding_key(ne_binary()) -> <<_:8>>.
binding_key() ->
    BindingKey = whapps_config:get(<<"konami_codes">>, <<"binding_key">>, ?DEFAULT_BINDING_KEY),
    constrain_binding_key(BindingKey).

binding_key(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case konami_doc(AccountDb) of
        'undefined' -> binding_key();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"binding_key">>, fun binding_key/0, fun constrain_binding_key/1)
    end.

-spec constrain_binding_key(ne_binary()) -> <<_:8>>.
constrain_binding_key(BindingKey) ->
    case lists:member(BindingKey, ?ANY_DIGIT) of
        'true' -> BindingKey;
        'false' -> ?DEFAULT_BINDING_KEY
    end.

-spec timeout() -> non_neg_integer().
-spec timeout(ne_binary()) -> non_neg_integer().
timeout() ->
    whapps_config:get_integer(<<"konami_codes">>, <<"digit_timeout_ms">>, ?DEFAULT_DIGIT_TIMEOUT).

timeout(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case konami_doc(AccountDb) of
        'undefined' -> timeout();
        KonamiDoc ->
            get_attribute(KonamiDoc, <<"digit_timeout_ms">>, fun timeout/0, fun wh_util:to_integer/1)
    end.

-spec identity(X) -> X.
identity(X) -> X.

-spec get_attribute(wh_json:object(), ne_binary(), default_fun()) -> term().
-spec get_attribute(wh_json:object(), ne_binary(), default_fun(), formatter_fun()) -> term().
get_attribute(JObj, K, DefaultFun) ->
    get_attribute(JObj, K, DefaultFun, fun identity/1).

get_attribute(JObj, K, DefaultFun, FormatterFun) ->
    case wh_json:get_value(K, JObj) of
        'undefined' -> DefaultFun;
        V -> FormatterFun(V)
    end.

-spec konami_doc(ne_binary()) -> api_object().
konami_doc(AccountDb) ->
    case couch_mgr:open_cache_doc(AccountDb, <<"konami_codes">>) of
        {'ok', JObj} -> JObj;
        {'error', _E} ->
            lager:debug("failed to open account(~s)'s konami doc: ~p", [wh_util:format_account_id(AccountDb, 'raw')
                                                                        ,_E
                                                                       ]),
            'undefined'
    end.
