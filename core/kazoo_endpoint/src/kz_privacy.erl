%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_privacy).

-export([get_method/1
        ,get_method/2
        ]).
-export([should_hide_name/1
        ,should_hide_name/2
        ,should_hide_number/1
        ,should_hide_number/2
        ]).
-export([enforce/1
        ,enforce/2
        ]).
-export([get_mode/1
        ,flags_by_mode/1
        ]).
-export([flags/1
        ,has_flags/1
        ]).
-export([is_anonymous/1
        ,should_block_anonymous/1
        ]).
-export([anonymous_caller_id_name/0, anonymous_caller_id_name/1
        ,anonymous_caller_id_number/0, anonymous_caller_id_number/1
        ]).

-include("kazoo_endpoint.hrl").
-include_lib("kazoo_amqp/include/kapi_offnet_resource.hrl").

-define(PRIVACY_CAT, <<"privacy">>).

-define(KEY_ANONYMOUS_NAME, <<"default_privacy_name">>).
-define(DEFAULT_ANONYMOUS_NAME, <<"Anonymous">>).

-define(KEY_ANONYMOUS_NUMBER, <<"default_privacy_number">>).
-define(DEFAULT_ANONYMOUS_NUMBER, <<"anonymous">>).

-define(KEY_ANONYMOUS_NAMES, <<"anonymous_cid_names">>).
-define(KEY_ANONYMOUS_NUMBERS, <<"anonymous_cid_numbers">>).
-define(DEFAULT_ANONYMOUS_VALUES, [<<"anonymous">>
                                  ,<<"restricted">>
                                  ,<<"0000000000">>
                                  ]).

-define(CCV(Key), [<<"Custom-Channel-Vars">>, Key]).

-define(CALLER_PRIVACY_NAME, <<"Caller-Privacy-Hide-Name">>).
-define(CALLER_PRIVACY_NUMBER, <<"Caller-Privacy-Hide-Number">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_method(kz_json:object()) -> kz_term:ne_binary().
get_method(JObj) ->
    get_method(JObj, get_default_privacy_mode()).

-spec get_method(kz_json:object(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_method(JObj, Default) ->
    %% NOTE: privacy_mode is deperecated, and could be set
    %%   to hide_name or hide_number.  If that is the case
    %%   just assume it should be hidden by Kazoo
    Keys = [[<<"caller_id_options">>, <<"privacy_method">>]
           ,[<<"privacy">>, <<"method">>]
           ,?KEY_PRIVACY_METHOD
           ,<<"privacy_mode">>
           ],
    case kz_json:get_first_defined(Keys, JObj, Default) of
        <<"sip">> -> <<"sip">>;
        _Else -> <<"kazoo">>
    end.

-spec get_default_privacy_mode() -> kz_term:ne_binary().
get_default_privacy_mode() ->
    kapps_config:get_ne_binary(?PRIVACY_CAT
                              ,<<"method">>
                              ,<<"kazoo">>
                              ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_hide_name(kz_json:object()) -> boolean().
should_hide_name(JObj) ->
    Keys = [[<<"caller_id_options">>, <<"outbound_privacy">>]
           ,[<<"privacy">>, <<"hide_name">>]
           ,?KEY_PRIVACY_HIDE_NAME
           ,?CCV(?KEY_PRIVACY_HIDE_NAME)
           ,?CALLER_PRIVACY_NAME
           ,?CCV(?CALLER_PRIVACY_NAME)
           ,<<"privacy_mode">>
           ],
    case kz_json:get_first_defined(Keys, JObj) of
        <<"hide_name">> -> 'true';
        <<"name">> -> 'true';
        <<"yes">> -> 'true';
        <<"full">> -> 'true';
        <<"kazoo">> -> 'true';
        Else -> kz_term:is_true(Else)
    end.

-spec should_hide_name(kz_json:object(), boolean()) -> boolean().
should_hide_name(JObj, Default) ->
    should_hide_name(JObj)
        orelse kz_term:is_true(Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_hide_number(kz_json:object()) -> boolean().
should_hide_number(JObj) ->
    Keys = [[<<"caller_id_options">>, <<"outbound_privacy">>]
           ,[<<"privacy">>, <<"hide_number">>]
           ,?KEY_PRIVACY_HIDE_NUMBER
           ,?CCV(?KEY_PRIVACY_HIDE_NAME)
           ,?CALLER_PRIVACY_NUMBER
           ,?CCV(?CALLER_PRIVACY_NUMBER)
           ,<<"privacy_mode">>
           ],
    case kz_json:get_first_defined(Keys, JObj) of
        <<"hide_number">> -> 'true';
        <<"number">> -> 'true';
        <<"yes">> -> 'true';
        <<"full">> -> 'true';
        <<"kazoo">> -> 'true';
        Else -> kz_term:is_true(Else)
    end.

-spec should_hide_number(kz_json:object(), boolean()) -> boolean().
should_hide_number(JObj, Default) ->
    should_hide_number(JObj)
        orelse kz_term:is_true(Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec enforce(kz_json:object()) -> kz_json:object().
enforce(JObj) ->
    enforce(JObj, get_method(JObj)).

-spec enforce(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
enforce(JObj, DefaultMethod) ->
    Method = get_method(JObj, DefaultMethod),
    Routines = [fun maybe_anonymize_name/2
               ,fun maybe_anonymize_number/2
               ,fun maybe_endpoints/2
               ,fun maybe_custom_channel_vars/2
               ,fun maybe_custom_call_vars/2
               ],
    lists:foldl(fun(F, J) -> F(J, Method) end
               ,JObj
               ,Routines
               ).

-spec maybe_anonymize_name(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_anonymize_name(JObj, Method) ->
    case should_hide_name(JObj) of
        'false' -> JObj;
        'true' -> hide_name(JObj, Method)
    end.

-spec hide_name(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
hide_name(JObj, <<"sip">>) ->
    JObj;
hide_name(JObj, _Method) ->
    Keys = [<<"Outbound-Caller-ID-Name">>
           ,<<"Caller-ID-Name">>
           ],
    AnonymousName = anonymous_caller_id_name(),
    lists:foldl(fun(Key, J) ->
                        kz_json:set_value(Key, AnonymousName, J)
                end
               ,JObj
               ,Keys
               ).

-spec maybe_anonymize_number(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_anonymize_number(JObj, Method) ->
    case should_hide_number(JObj) of
        'false' -> JObj;
        'true' -> hide_number(JObj, Method)
    end.

-spec hide_number(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
hide_number(JObj, <<"sip">>) ->
    JObj;
hide_number(JObj, _Method) ->
    Keys = [<<"Outbound-Caller-ID-Number">>
           ,<<"Caller-ID-Number">>
           ],
    AnonymousNumber = anonymous_caller_id_number(),
    lists:foldl(fun(Key, J) ->
                        kz_json:set_value(Key, AnonymousNumber, J)
                end
               ,JObj
               ,Keys
               ).

-spec maybe_endpoints(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_endpoints(JObj, Method) ->
    case kz_json:get_list_value(<<"Endpoints">>, JObj, []) of
        [] -> JObj;
        Endpoints ->
            kz_json:set_value(<<"Endpoints">>
                             ,[enforce(Endpoint, Method)
                               || Endpoint <- Endpoints
                              ]
                             ,JObj
                             )
    end.

-spec maybe_custom_channel_vars(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_custom_channel_vars(JObj, <<"sip">>) ->
    JObj;
maybe_custom_channel_vars(JObj, Method) ->
    case kz_json:get_ne_json_value(<<"Custom-Channel-Vars">>, JObj) of
        'undefined' -> JObj;
        CCVs ->
            kz_json:set_value(<<"Custom-Channel-Vars">>, enforce(CCVs, Method), JObj)
    end.

-spec maybe_custom_call_vars(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_custom_call_vars(JObj, <<"sip">>) ->
    JObj;
maybe_custom_call_vars(JObj, Method) ->
    case kz_json:get_ne_json_value(<<"Custom-Call-Vars">>, JObj) of
        'undefined' -> JObj;
        CCVs ->
            kz_json:set_value(<<"Custom-Call-Vars">>, enforce(CCVs, Method), JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flags(kz_term:proplist() | kz_json:object()) -> kz_term:proplist().
flags(Props) when is_list(Props) ->
    flags(kz_json:from_list(Props));
flags(JObj) ->
    [{?KEY_PRIVACY_HIDE_NAME, should_hide_name(JObj)}
    ,{?KEY_PRIVACY_HIDE_NUMBER, should_hide_number(JObj)}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_mode(kz_json:object()) -> kz_term:api_ne_binary().
get_mode(JObj) ->
    ChannelCCVs = kz_json:get_ne_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new()),
    CallCCVs = kz_json:get_ne_json_value(<<"Custom-Call-Vars">>, JObj, kz_json:new()),
    Endpoints = kz_json:get_list_value(<<"Endpoints">>, JObj, []),
    IsSIP = get_method(JObj) =:= <<"sip">>
        orelse lists:any(fun(Endpoint) ->
                                 get_method(Endpoint) =:= <<"sip">>
                         end
                        ,Endpoints
                        )
        orelse get_method(ChannelCCVs) =:= <<"sip">>
        orelse get_method(CallCCVs) =:= <<"sip">>,
    HideName = (should_hide_name(JObj)
                orelse lists:any(fun should_hide_name/1, Endpoints)
                orelse should_hide_name(ChannelCCVs)
                orelse should_hide_name(CallCCVs)
               )
        andalso IsSIP,
    HideNumber = (should_hide_number(JObj)
                  orelse lists:any(fun should_hide_number/1, Endpoints)
                  orelse should_hide_number(ChannelCCVs)
                  orelse should_hide_number(CallCCVs)
                 )
        andalso IsSIP,
    case {HideName, HideNumber} of
        {'false', 'false'} -> 'undefined';
        {'false', 'true'} -> <<"number">>;
        {'true', 'false'} -> <<"name">>;
        {'true', 'true'} -> <<"full">>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flags_by_mode(kz_term:api_ne_binary()) -> kz_term:proplist().
flags_by_mode('undefined') ->
    flags_by_mode(<<"full">>);
flags_by_mode(<<"full">>) ->
    [{?KEY_PRIVACY_HIDE_NAME, 'true'}
    ,{?KEY_PRIVACY_HIDE_NUMBER, 'true'}
    ];
flags_by_mode(<<"name">>) ->
    [{?KEY_PRIVACY_HIDE_NAME, 'true'}
    ,{?KEY_PRIVACY_HIDE_NUMBER, 'false'}
    ];
flags_by_mode(<<"number">>) ->
    [{?KEY_PRIVACY_HIDE_NAME, 'false'}
    ,{?KEY_PRIVACY_HIDE_NUMBER, 'true'}
    ];
%% returns empty list so that callflow settings override
flags_by_mode(<<"none">>) -> [];
flags_by_mode(<<"hide_name">>) ->
    flags_by_mode(<<"name">>);
flags_by_mode(<<"hide_number">>) ->
    flags_by_mode(<<"number">>);
flags_by_mode(<<"yes">>) ->
    flags_by_mode(<<"full">>);
flags_by_mode(<<"kazoo">>) ->
    flags_by_mode(<<"full">>);
flags_by_mode(_Else) ->
    lager:debug("unsupported privacy mode ~s, forcing full privacy", [_Else]),
    flags_by_mode(<<"full">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_flags(kz_term:proplist() | kz_json:object()) -> boolean().
has_flags(Props) when is_list(Props) ->
    has_flags(kz_json:from_list(Props));
has_flags(JObj) ->
    should_hide_name(JObj)
        orelse should_hide_number(JObj).

%%------------------------------------------------------------------------------
%% @doc Find out if we call should be blocked if it's anonymous and Account
%% or system is configured to block anonymous calls
%% @end
%%------------------------------------------------------------------------------
-spec should_block_anonymous(kz_json:object()) -> boolean().
should_block_anonymous(JObj) ->
    AccountId = get_value(<<"Account-ID">>, JObj),
    ShouldBlock = kapps_account_config:get_global(AccountId
                                                 ,?PRIVACY_CAT
                                                 ,<<"block_anonymous_caller_id">>
                                                 ,'false'
                                                 ),
    case {kz_term:is_true(ShouldBlock), is_anonymous(JObj)} of
        {'true', 'true'} ->
            lager:info("block anonymous call, account_id: ~s", [AccountId]),
            'true';
        {'true', 'false'} ->
            lager:info("passing non-anonymous call, account_id: ~s", [AccountId]),
            'false';
        _Check ->
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Checks all possible variables to see if the incoming call is anonymous
%% @end
%%------------------------------------------------------------------------------
-spec is_anonymous(kz_json:object()) -> boolean().
is_anonymous(JObj) ->
    IsCallerNumberZero = is_zero(kz_json:get_value(<<"Caller-ID-Number">>, JObj)),
    IsPrivacyName = kz_term:is_true(get_value(<<"Caller-Privacy-Name">>, JObj, 'false')),
    IsPrivacyName2 = kz_json:is_true([<<"Privacy">>, <<"Hide-Name">>], JObj),
    IsPrivacyNumber = kz_term:is_true(get_value(<<"Caller-Privacy-Number">>, JObj, 'false')),
    IsPrivacyNumber2 = kz_json:is_true([<<"Privacy">>, <<"Hide-Number">>], JObj),
    HasPrivacyFlags = has_flags(JObj),
    MatchesNumberRule = maybe_anonymous_cid_number(JObj),
    MatchesNameRule = maybe_anonymous_cid_name(JObj),
    IsCallerNumberZero
        orelse IsPrivacyName
        orelse IsPrivacyName2
        orelse IsPrivacyNumber
        orelse IsPrivacyNumber2
        orelse HasPrivacyFlags
        orelse MatchesNumberRule
        orelse MatchesNameRule.

-spec is_zero(kz_term:api_ne_binary()) -> boolean().
is_zero(?NE_BINARY=Number) ->
    case re:run(erlang:binary_to_list(Number), "^\\+?00+\$", ['global']) of
        {'match', _} -> 'true';
        _ -> 'false'
    end;
is_zero(_) -> 'false'.

-spec maybe_anonymous_cid_number(kz_json:object()) -> boolean().
maybe_anonymous_cid_number(JObj) ->
    case kapps_config:get_is_true(?PRIVACY_CAT, <<"check_additional_anonymous_cid_numbers">>, 'false') of
        'true' -> is_anonymous_cid_number(JObj);
        'false' -> 'false'
    end.

-spec maybe_anonymous_cid_name(kz_json:object()) -> boolean().
maybe_anonymous_cid_name(JObj) ->
    case kapps_config:get_is_true(?PRIVACY_CAT, <<"check_additional_anonymous_cid_names">>, 'false') of
        'true' -> is_anonymous_cid_name(JObj);
        'false' -> 'false'
    end.

-spec is_anonymous_cid_number(kz_json:object()) -> boolean().
is_anonymous_cid_number(JObj) ->
    is_anonymous_rule_member(kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj)
                            ,kapps_config:get_ne_binaries(?PRIVACY_CAT, ?KEY_ANONYMOUS_NUMBERS, ?DEFAULT_ANONYMOUS_VALUES)
                            ).

-spec is_anonymous_cid_name(kz_json:object()) -> boolean().
is_anonymous_cid_name(JObj) ->
    is_anonymous_rule_member(kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj)
                            ,kapps_config:get_ne_binaries(?PRIVACY_CAT, ?KEY_ANONYMOUS_NAMES, ?DEFAULT_ANONYMOUS_VALUES)
                            ).

-spec is_anonymous_rule_member(kz_term:ne_binary(), kz_term:ne_binaries()) -> boolean().
is_anonymous_rule_member(_, []) -> 'false';
is_anonymous_rule_member(Value, [Match|Matches]) ->
    LowerMatch = kz_term:to_lower_binary(Match),
    LowerValue = kz_term:to_lower_binary(Value),
    case LowerMatch =:= LowerValue of
        'true' -> 'true';
        'false' -> is_anonymous_rule_member(Value, Matches)
    end.

%%------------------------------------------------------------------------------
%% @doc Default anonymous Caller IDs from System wide config, or Account
%% @end
%%------------------------------------------------------------------------------
-spec anonymous_caller_id_name() -> kz_term:ne_binary().
anonymous_caller_id_name() ->
    kapps_config:get_binary(?PRIVACY_CAT, ?KEY_ANONYMOUS_NAME, ?DEFAULT_ANONYMOUS_NAME).

-spec anonymous_caller_id_name(kz_term:api_binary()) -> kz_term:ne_binary().
anonymous_caller_id_name('undefined') ->
    anonymous_caller_id_name();
anonymous_caller_id_name(AccountId) ->
    kapps_account_config:get_global(AccountId, ?PRIVACY_CAT, ?KEY_ANONYMOUS_NAME, ?DEFAULT_ANONYMOUS_NAME).

-spec anonymous_caller_id_number() -> kz_term:ne_binary().
anonymous_caller_id_number() ->
    kapps_config:get_binary(?PRIVACY_CAT, ?KEY_ANONYMOUS_NUMBER, ?DEFAULT_ANONYMOUS_NUMBER).

-spec anonymous_caller_id_number(kz_term:api_binary()) -> kz_term:ne_binary().
anonymous_caller_id_number('undefined') ->
    anonymous_caller_id_number();
anonymous_caller_id_number(AccountId) ->
    kapps_account_config:get_global(AccountId, ?PRIVACY_CAT, ?KEY_ANONYMOUS_NUMBER, ?DEFAULT_ANONYMOUS_NUMBER).

%%------------------------------------------------------------------------------
%% @doc Get Key from JObj, if not found look into CCV
%% @end
%%------------------------------------------------------------------------------
-spec get_value(kz_term:ne_binary(), kz_json:object()) -> any().
get_value(Key, JObj) ->
    get_value(Key, JObj, 'undefined').

-spec get_value(kz_term:ne_binary(), kz_json:object(), any()) -> any().
get_value(Key, JObj, Default) ->
    case kz_json:get_first_defined([Key, ?CCV(Key)], JObj) of
        'undefined' -> Default;
        Value ->
            case kz_term:is_empty(Value) of
                'true' -> Default;
                'false' -> Value
            end
    end.
