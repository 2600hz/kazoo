%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(stepswitch_formatters).

-export([apply/3]).

-include("stepswitch.hrl").

-spec apply(kz_json:object(), kz_json:object(), direction()) ->
                   kz_json:object().
apply(JObj, MetaFormatters, Direction) ->
    Fs = [fun format_request/3
	 ,fun maybe_format_sip_headers/3
         ],

    lists:foldl(fun(F, J) -> F(J, MetaFormatters, Direction) end
	       ,JObj
	       ,Fs
               ).

-spec maybe_format_sip_headers(kz_json:object(), kz_json:object(), direction()) ->
                                      kz_json:object().
maybe_format_sip_headers(JObj, MetaFormatters, Direction) ->
    maybe_format_sub_object(
      JObj
			   ,MetaFormatters
			   ,Direction
			   ,<<"Custom-SIP-Headers">>
     ).

-spec maybe_format_sub_object(kz_json:object(), kz_json:object(), direction(), ne_binary()) ->
                                     kz_json:object().
maybe_format_sub_object(JObj, MetaFormatters, Direction, SubKey) ->
    case kz_json:get_value(SubKey, JObj) of
        'undefined' -> JObj;
        SubJObj ->
            Formatted = format_request(maybe_set_invite_format(JObj, SubJObj), MetaFormatters, Direction),
            kz_json:set_value(SubKey, kz_json:delete_key(<<"Invite-Format">>, Formatted), JObj)
    end.

-spec maybe_set_invite_format(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_set_invite_format(JObj, SubJObj) ->
    case kz_json:get_value(<<"Invite-Format">>, JObj) of
        'undefined' -> SubJObj;
        Format -> kz_json:set_value(<<"Invite-Format">>, Format, SubJObj)
    end.

-spec format_request(kz_json:object(), kz_json:object(), direction()) ->
                            kz_json:object().
format_request(JObj, MetaFormatters, Direction) ->
    JObjKeys = request_keys(JObj),
    kz_json:foldl(fun(MetaKey, Formatters, AccJObj) ->
                          lager:debug("processing ~s ~p", [MetaKey, Formatters]),
                          maybe_apply_formatters_fold(AccJObj
						     ,JObjKeys
						     ,MetaKey
						     ,filter_formatters_by_direction(Direction, Formatters)
                                                     )
                  end
		 ,JObj
		 ,MetaFormatters
                 ).

-spec request_keys(kz_json:object()) -> kz_proplist().
request_keys(JObj) ->
    [{kz_json:normalize_key(K), K}
     || K <- kz_json:get_keys(kz_api:remove_defaults(JObj))
    ].

-spec filter_formatters_by_direction(direction(), kz_json:object() | kz_json:objects()) -> kz_json:objects().
filter_formatters_by_direction(Direction, Formatters) when is_list(Formatters) ->
    [Formatter || Formatter <- Formatters,
                  is_formatter_applicable(Formatter, Direction)
    ];
filter_formatters_by_direction(Direction, Formatter) ->
    'true' = kz_json:is_json_object(Formatter),
    filter_formatters_by_direction(Direction, [Formatter]).

-spec is_formatter_applicable(kz_json:object(), direction()) -> boolean().
is_formatter_applicable(Formatter, Direction) ->
    case kz_json:get_atom_value(<<"direction">>, Formatter) of
        'undefined' -> 'true';
        Direction -> 'true';
        _Direction -> 'false'
    end.

-spec maybe_apply_formatters_fold(kz_json:object(), kz_json:keys(), ne_binary(), kz_json:objects()) ->
                                         kz_json:object().
maybe_apply_formatters_fold(JObj, _JObjKeys, _MetaKey, []) -> JObj;
maybe_apply_formatters_fold(JObj, JObjKeys, MetaKey, [_|_]=Formatters) ->
    case props:get_value(kz_util:to_lower_binary(MetaKey), JObjKeys) of
        'undefined' -> JObj;
        JObjKey -> maybe_apply_formatters(JObj, JObjKey, Formatters)
    end.

-spec maybe_apply_formatters(kz_json:object(), ne_binary(), kz_json:objects()) ->
                                    kz_json:object().
-spec maybe_apply_formatters(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:objects()) ->
                                    kz_json:object().
-spec maybe_apply_formatters(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:objects()) ->
                                    kz_json:object().
maybe_apply_formatters(JObj, <<"Request">> = RequestKey, Formatters) ->
    [RequestUser, RequestRealm] = binary:split(kz_json:get_value(<<"Request">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, RequestKey, RequestUser, RequestRealm, Formatters);
maybe_apply_formatters(JObj, <<"To">> = ToKey, Formatters) ->
    [ToUser, ToRealm] = binary:split(kz_json:get_value(<<"To">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, ToKey, ToUser, ToRealm, Formatters);
maybe_apply_formatters(JObj, <<"From">> = FromKey, Formatters) ->
    [FromUser, FromRealm] = binary:split(kz_json:get_value(<<"From">>, JObj), <<"@">>),
    maybe_apply_formatters(JObj, FromKey, FromUser, FromRealm, Formatters);
maybe_apply_formatters(JObj, <<_/binary>> = Key, Formatters) ->
    maybe_apply_formatters(JObj, Key, kz_json:get_value(Key, JObj), Formatters).

maybe_apply_formatters(JObj, _Key, _Value, []) -> JObj;
maybe_apply_formatters(JObj, Key, Value, [_|_]=Formatters) ->
    Funs = [fun maybe_strip/4
	   ,fun maybe_replace/4
	   ,fun maybe_match_invite_format/4
	   ,fun maybe_match/4
           ],
    apply_formatter_funs(JObj, Key, Value, Formatters, Funs).

-type ffun_return() :: kz_json:object() | 'false'.
-type formatter_fun_4() :: fun((kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:object()) -> ffun_return()).
-type formatter_funs_4() :: [formatter_fun_4()].

-spec apply_formatter_funs(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:objects(), formatter_funs_4()) ->
                                  kz_json:object().
apply_formatter_funs(JObj, Key, Value, [_|Formatters], []) ->
    maybe_apply_formatters(JObj, Key, Value, Formatters);
apply_formatter_funs(JObj, Key, Value, [Formatter|_]=Formatters, [F|Fs]) ->
    case F(JObj, Key, Value, Formatter) of
        'false' -> apply_formatter_funs(JObj, Key, Value, Formatters, Fs);
        Modified -> Modified
    end.

-spec maybe_strip(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:object()) ->
                         ffun_return().
maybe_strip(JObj, Key, _Value, Formatter) ->
    case should_strip_key(Formatter) of
        'false' -> 'false';
        'true' ->
            lager:debug("stripping ~s", [Key]),
            kz_json:delete_key(Key, JObj)
    end.

-spec maybe_replace(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:object()) ->
                           ffun_return().
maybe_replace(JObj, Key, _Value, Formatter) ->
    case kz_json:get_value(<<"value">>, Formatter) of
        'undefined' -> 'false';
        Replace -> kz_json:set_value(Key, Replace, JObj)
    end.

-spec maybe_match_invite_format(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:object()) ->
                                       ffun_return().
maybe_match_invite_format(JObj, Key, Value, Formatter) ->
    case maybe_match_invite_format(JObj, Formatter) of
        'false' -> 'false';
        'true' ->
            lager:debug("matching ~s value (~p) to invite format", [Key, Value]),
            match_invite_format(JObj, Key, Value)
    end.

-spec match_invite_format(kz_json:object(), ne_binary(), kz_json:json_term()) ->
                                 kz_json:object().
match_invite_format(JObj, <<"Diversions">> = Key, [<<_/binary>> = Value|_]) ->
    match_invite_format(JObj, Key, kzsip_diversion:from_binary(Value));
match_invite_format(JObj, <<"Diversions">> = Key, Value) ->
    FormatFun = invite_format_fun(JObj),

    Address = kzsip_diversion:address(Value),

    SIP = knm_sip:parse(Address),
    SIP1 = knm_sip:set_user(SIP, FormatFun(knm_sip:user(SIP))),

    Address1 =  knm_sip:encode(SIP1),

    kz_json:set_value(Key, kzsip_diversion:set_address(Value, Address1), JObj);
match_invite_format(JObj, Key, Value) ->
    FormatFun = invite_format_fun(JObj),
    kz_json:set_value(Key, FormatFun(Value), JObj).

-spec should_strip_key(kz_json:object()) -> boolean().
should_strip_key(Formatter) ->
    kz_json:is_true(<<"strip">>, Formatter, 'false').

-spec maybe_match(kz_json:object(), ne_binary(), kz_json:json_term(), kz_json:object()) ->
                         ffun_return().
maybe_match(JObj, <<"Diversions">> = Key, [<<_/binary>> = Value|_], Formatter) ->
    maybe_match(JObj, Key, kzsip_diversion:from_binary(Value), Formatter);
maybe_match(JObj, <<"Diversions">> = Key, Value, Formatter) ->
    case maybe_match(kz_json:get_value(<<"regex">>, Formatter), kzsip_diversion:user(Value)) of
        {'match', Captured} ->
            User = apply_formatter(Captured, Formatter),
            lager:debug("updating ~s user to '~s'", [Key, User]),

            kz_json:set_value(Key, kzsip_diversion:set_user(Value, User), JObj);
        'nomatch' ->
            lager:debug("diversion ~s didn't match ~s"
		       ,[kzsip_diversion:user(Value), kz_json:get_value(<<"regex">>, Formatter)]
                       ),
            'false'
    end;
maybe_match(JObj, Key, Value, Formatter) ->
    case maybe_match(kz_json:get_value(<<"regex">>, Formatter), Value) of
        {'match', Captured} -> apply_formatter(JObj, Key, Captured, Formatter);
        'nomatch' -> 'false'
    end.

maybe_apply_formatters(JObj, _Key, _User, _Realm, []) -> JObj;
maybe_apply_formatters(JObj, Key, User, Realm, Formatters) ->
    Funs = [fun maybe_strip/5
	   ,fun maybe_replace/5
	   ,fun maybe_match_invite_format/5
	   ,fun maybe_match/5
           ],
    apply_formatter_funs(JObj, Key, User, Realm, Formatters, Funs).

-type formatter_fun_5() :: fun((kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) -> ffun_return()).
-type formatter_funs_5() :: [formatter_fun_5()].

-spec apply_formatter_funs(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:objects(), formatter_funs_5()) ->
                                  kz_json:object().
apply_formatter_funs(JObj, Key, User, Realm, [_|Formatters], []) ->
    maybe_apply_formatters(JObj, Key, User, Realm, Formatters);
apply_formatter_funs(JObj, Key, User, Realm, [Formatter|_]=Formatters, [F|Fs]) ->
    case F(JObj, Key, User, Realm, Formatter) of
        'false' -> apply_formatter_funs(JObj, Key, User, Realm, Formatters, Fs);
        Modified -> Modified
    end.

-spec maybe_strip(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                         ffun_return().
maybe_strip(JObj, Key, _User, _Realm, Formatter) ->
    case should_strip_key(Formatter) of
        'false' -> 'false';
        'true' ->
            lager:debug("stripping ~s", [Key]),
            kz_json:delete_key(Key, JObj)
    end.

-spec maybe_replace(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                           ffun_return().
maybe_replace(JObj, Key, _User, Realm, Formatter) ->
    case kz_json:get_value(<<"value">>, Formatter) of
        'undefined' -> 'false';
        Replace -> kz_json:set_value(Key, <<Replace/binary, "@", Realm/binary>>, JObj)
    end.

-spec maybe_match_invite_format(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                                       ffun_return().
maybe_match_invite_format(JObj, Key, User, Realm, Formatter) ->
    case maybe_match_invite_format(JObj, Formatter) of
        'false' -> 'false';
        'true' ->
            lager:debug("matching ~s value (~s) to invite format", [Key, User]),
            match_invite_format(JObj, Key, User, Realm)
    end.

-spec maybe_match_invite_format(kz_json:object(), kz_json:object()) ->
                                       boolean().
maybe_match_invite_format(JObj, Formatter) ->
    case kz_json:is_true(<<"match_invite_format">>, Formatter, 'false')
        andalso kz_json:get_value(<<"Invite-Format">>, JObj)
    of
        'false' -> 'false';
        'undefined' -> 'false';
        <<"loopback">> -> 'false';
        <<"route">> -> 'false';
        <<"username">> -> 'false';
        <<"e164">> -> 'true';
        <<"npan">> -> 'true';
        <<"1npan">> -> 'true'
    end.

-spec match_invite_format(kz_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                                 kz_json:object().
match_invite_format(JObj, Key, User, Realm) ->
    FormatFun = invite_format_fun(JObj),
    kz_json:set_value(Key
		     ,list_to_binary([FormatFun(User)
				     ,"@"
				     ,Realm
				     ])
		     ,JObj
                     ).

-spec invite_format_fun(kz_json:object()) ->
                               fun((ne_binary()) -> ne_binary()).
invite_format_fun(JObj) ->
    case kz_json:get_value(<<"Invite-Format">>, JObj) of
        <<"e164">> -> fun knm_converters:normalize/1;
        <<"1npan">> -> fun knm_converters:to_1npan/1;
        <<"npan">> -> fun knm_converters:to_npan/1
    end.

-spec maybe_match(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                         ffun_return().
maybe_match(JObj, Key, User, Realm, Formatter) ->
    case maybe_match(kz_json:get_value(<<"regex">>, Formatter), User) of
        {'match', Captured} -> apply_formatter(JObj, Key, Captured, Realm, Formatter);
        'nomatch' -> 'false'
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

-spec apply_formatter(kz_json:object(), ne_binary(), ne_binary(), kz_json:object()) ->
                             kz_json:object().
-spec apply_formatter(kz_json:object(), ne_binary(), ne_binary(), ne_binary(), kz_json:object()) ->
                             kz_json:object().
apply_formatter(Captured, Formatter) ->
    list_to_binary([kz_json:get_value(<<"prefix">>, Formatter, <<>>)
		   ,Captured
		   ,kz_json:get_value(<<"suffix">>, Formatter, <<>>)
                   ]).

apply_formatter(JObj, Key, Captured, Formatter) ->
    Value = apply_formatter(Captured, Formatter),
    lager:debug("updating ~s to '~s'", [Key, Value]),
    kz_json:set_value(Key, Value, JObj).

apply_formatter(JObj, Key, Captured, Realm, Formatter) ->
    User = apply_formatter(Captured, Formatter),
    lager:debug("updating ~s user to '~s'@~s", [Key, User, Realm]),
    kz_json:set_value(Key, list_to_binary([User, "@", Realm]), JObj).
