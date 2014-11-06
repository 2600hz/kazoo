%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(stepswitch_formatters).

-export([apply/3]).

-include("stepswitch.hrl").

-spec apply(wh_json:object(), wh_json:object(), direction()) ->
                   wh_json:object().
apply(JObj, MetaFormatters, Direction) ->
    Fs = [fun format_request/3
          ,fun maybe_format_ccvs/3
          ,fun maybe_format_sip_headers/3
         ],

    lists:foldl(fun(F, J) -> F(J, MetaFormatters, Direction) end
                ,JObj
                ,Fs
               ).

-spec maybe_format_ccvs(wh_json:object(), wh_json:object(), direction()) ->
                               wh_json:object().
maybe_format_ccvs(JObj, MetaFormatters, Direction) ->
    maybe_format_sub_object(JObj, MetaFormatters, Direction, <<"Custom-Channel-Vars">>).

-spec maybe_format_sip_headers(wh_json:object(), wh_json:object(), direction()) ->
                                      wh_json:object().
maybe_format_sip_headers(JObj, MetaFormatters, Direction) ->
    maybe_format_sub_object(JObj, MetaFormatters, Direction, <<"SIP-Headers">>).

-spec maybe_format_sub_object(wh_json:object(), wh_json:object(), direction(), wh_json:key()) ->
                                     wh_json:object().
maybe_format_sub_object(JObj, MetaFormatters, Direction, SubKey) ->
    case wh_json:get_value(SubKey, JObj) of
        'undefined' -> JObj;
        SubJObj ->
            Formatted = format_request(SubJObj, MetaFormatters, Direction),
            wh_json:set_value(SubKey, Formatted, JObj)
    end.

-spec format_request(wh_json:object(), wh_json:object(), direction()) ->
                            wh_json:object().
format_request(JObj, MetaFormatters, Direction) ->
    JObjKeys = request_keys(JObj),
    wh_json:foldl(fun(MetaKey, Formatters, AccJObj) ->
                          maybe_apply_formatters_fold(AccJObj
                                                      ,JObjKeys
                                                      ,MetaKey
                                                      ,filter_formatters_by_direction(Direction, Formatters)
                                                     )
                  end, JObj, MetaFormatters).

-spec request_keys(wh_json:object()) -> wh_proplist().
request_keys(JObj) ->
    [{wh_util:to_lower_binary(K), K}
     || K <- wh_json:get_keys(wh_api:remove_defaults(JObj))
    ].

-spec filter_formatters_by_direction(direction(), wh_json:object() | wh_json:objects()) -> wh_json:objects().
filter_formatters_by_direction(Direction, Formatters) when is_list(Formatters) ->
    [Formatter || Formatter <- Formatters,
                  is_formatter_applicable(Formatter, Direction)
    ];
filter_formatters_by_direction(Direction, Formatter) ->
    'true' = wh_json:is_json_object(Formatter),
    filter_formatters_by_direction(Direction, [Formatter]).

-spec is_formatter_applicable(wh_json:object(), direction()) -> boolean().
is_formatter_applicable(Formatter, Direction) ->
    case wh_json:get_atom_value(<<"direction">>, Formatter) of
        'undefined' -> 'true';
        Direction -> 'true';
        _Direction -> 'false'
    end.

-spec maybe_apply_formatters_fold(wh_json:object(), wh_json:keys(), wh_json:key(), wh_json:objects()) ->
                                         wh_json:object().
maybe_apply_formatters_fold(JObj, _JObjKeys, _MetaKey, []) -> JObj;
maybe_apply_formatters_fold(JObj, JObjKeys, MetaKey, [_|_]=Formatters) ->
    case props:get_value(wh_util:to_lower_binary(MetaKey), JObjKeys) of
        'undefined' -> JObj;
        JObjKey -> maybe_apply_formatters(JObj, JObjKey, Formatters)
    end.

-spec maybe_apply_formatters(wh_json:object(), ne_binary(), wh_json:objects()) ->
                                    wh_json:object().
-spec maybe_apply_formatters(wh_json:object(), ne_binary(), ne_binary(), wh_json:objects()) ->
                                    wh_json:object().
-spec maybe_apply_formatters(wh_json:object(), ne_binary(), ne_binary(), ne_binary(), wh_json:objects()) ->
                                    wh_json:object().
maybe_apply_formatters(JObj, <<"Request">> = RequestKey, Formatters) ->
    [RequestUser, RequestRealm] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, RequestKey, RequestUser, RequestRealm, Formatters);
maybe_apply_formatters(JObj, <<"To">> = ToKey, Formatters) ->
    [ToUser, ToRealm] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, ToKey, ToUser, ToRealm, Formatters);
maybe_apply_formatters(JObj, <<"From">> = FromKey, Formatters) ->
    [FromUser, FromRealm] = binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, FromKey, FromUser, FromRealm, Formatters);
maybe_apply_formatters(JObj, Key, Formatters) ->
    maybe_apply_formatters(JObj, Key, wh_json:get_value(Key, JObj), Formatters).

maybe_apply_formatters(JObj, _Key, _Value, []) -> JObj;
maybe_apply_formatters(JObj, Key, Value, [Formatter|Formatters]) ->
    case maybe_strip_key(Formatter) of
        'false' -> maybe_match(JObj, Key, Value, Formatters);
        'true' -> wh_json:delete_key(Key, JObj)
    end.

-spec maybe_strip_key(wh_json:object()) -> boolean().
maybe_strip_key(Formatter) ->
    wh_json:is_true(<<"strip">>, Formatter, 'false').

-spec maybe_match(wh_json:object(), wh_json:key(), wh_json:json_term(), wh_json:objects()) ->
                         wh_json:object().
maybe_match(JObj, Key, Value, [Formatter|Formatters]) ->
    case maybe_match(wh_json:get_value(<<"regex">>, Formatter), Value) of
        {'match', Captured} -> apply_formatter(JObj, Key, Captured, Formatter);
        'nomatch' -> maybe_apply_formatters(JObj, Key, Value, Formatters)
    end.

maybe_apply_formatters(JObj, _Key, _User, _Realm, []) -> JObj;
maybe_apply_formatters(JObj, Key, User, Realm, [Formatter|Formatters]) ->
    case maybe_match_invite_format(JObj, Formatter) of
        'false' -> maybe_match(JObj, Key, User, Realm, Formatters);
        FormatFun -> match_invite_format(JObj, Key, User, Realm, FormatFun)
    end.

-type invite_fun() :: fun((ne_binary()) -> ne_binary()).
-spec maybe_match_invite_format(wh_json:object(), wh_json:object()) ->
                                       'false' | invite_fun().
maybe_match_invite_format(JObj, Formatter) ->
    case wh_json:get_is_true(<<"match_invite_format">>, Formatter, 'false')
        andalso wh_json:get_value(<<"Invite-Format">>, JObj)
    of
        'false' -> 'false';
        'undefined' -> 'false';
        <<"loopback">> -> 'false';
        <<"route">> -> 'false';
        <<"username">> -> 'false';
        <<"e164">> -> fun wnm_util:to_e164/1;
        <<"npan">> -> fun wnm_util:to_npan/1;
        <<"1npan">> -> fun wnm_util:to_1npan/1
    end.

-spec match_invite_format(wh_json:object(), wh_json:key(), ne_binary(), ne_binary(), invite_fun()) ->
                                 wh_json:object().
match_invite_format(JObj, Key, User, Realm, FormatFun) ->
    wh_json:set_value(Key
                      ,list_to_binary([FormatFun(User)
                                       ,"@"
                                       ,Realm
                                      ])
                      ,JObj
                     ).

-spec maybe_match(wh_json:object(), wh_json:key(), ne_binary(), ne_binary(), wh_json:objects()) ->
                         wh_json:object().
maybe_match(JObj, Key, User, Realm, [Formatter|Formatters]) ->
    case maybe_match(wh_json:get_value(<<"regex">>, Formatter), User) of
        {'match', Captured} -> apply_formatter(JObj, Key, Captured, Realm, Formatter);
        'nomatch' -> maybe_apply_formatters(JObj, Key, User, Realm, Formatters)
    end.

-spec maybe_match(api_binary(), ne_binary()) ->
                         {'match', binary()} |
                         'nomatch'.
maybe_match('undefined', _Value) -> 'nomatch';
maybe_match(Regex, Value) ->
    case re:run(Value, Regex, [{'capture', 'all', 'binary'}]) of
        {'match', [_All, Captured |_]} ->
            lager:debug("formatter ~s captured ~s", [Regex, Captured]),
            {'match', Captured};
        {'match', [All]} ->
            lager:debug("formatter ~s didn't capture, but did match ~s", [Regex, All]),
            {'match', All};
        {'match', []} -> 'nomatch';
        'nomatch' -> 'nomatch'
    end.

-spec apply_formatter(wh_json:object(), ne_binary(), ne_binary(), wh_json:object()) ->
                             wh_json:object().
-spec apply_formatter(wh_json:object(), ne_binary(), ne_binary(), ne_binary(), wh_json:object()) ->
                             wh_json:object().
apply_formatter(JObj, Key, Captured, Formatter) ->
    Value = list_to_binary([wh_json:get_value(<<"prefix">>, Formatter, <<>>)
                            ,Captured
                            ,wh_json:get_value(<<"suffix">>, Formatter, <<>>)
                           ]),
    lager:debug("updating ~s to '~s'", [Key, Value]),
    wh_json:set_value(Key, Value, JObj).

apply_formatter(JObj, Key, Captured, Realm, Formatter) ->
    User = list_to_binary([wh_json:get_value(<<"prefix">>, Formatter, <<>>)
                           ,Captured
                           ,wh_json:get_value(<<"suffix">>, Formatter, <<>>)
                          ]),
    lager:debug("updating ~s user to '~s'@~s", [Key, User, Realm]),
    wh_json:set_value(Key, list_to_binary([User, "@", Realm]), JObj).
