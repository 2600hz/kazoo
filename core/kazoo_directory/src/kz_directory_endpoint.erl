%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_directory_endpoint).

-export([profile/2, profile/3]).

-include("kazoo_directory.hrl").

-spec get_endpoint_type(kz_json:object()) -> kz_term:ne_binary().
get_endpoint_type(Endpoint) ->
    Type = kz_json:get_first_defined([<<"endpoint_type">>
                                     ,<<"device_type">>
                                     ]
                                    ,Endpoint
                                    ),
    case convert_endpoint_type(Type) of
        'undefined' -> maybe_guess_endpoint_type(Endpoint);
        Else -> Else
    end.

-spec convert_endpoint_type(kz_term:ne_binary()) -> kz_term:api_ne_binary().
convert_endpoint_type(<<"sip_", _/binary>>) -> <<"sip">>;
convert_endpoint_type(<<"smartphone">>) -> <<"sip">>;
convert_endpoint_type(<<"softphone">>) -> <<"sip">>;
convert_endpoint_type(<<"cellphone">>) -> <<"sip">>;
convert_endpoint_type(<<"landline">>) -> <<"sip">>;
convert_endpoint_type(<<"fax">>) -> <<"sip">>;
convert_endpoint_type(<<"skype">>) -> <<"skype">>;
convert_endpoint_type(<<"mobile">>) -> <<"mobile">>;
convert_endpoint_type(_Else) -> 'undefined'.

-spec maybe_guess_endpoint_type(kz_json:object()) -> kz_term:ne_binary().
maybe_guess_endpoint_type(Endpoint) ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"restrict_to_known_types">>, 'false') of
        'false' -> guess_endpoint_type(Endpoint);
        'true' ->
            lager:info("unknown endpoint type and callflows restricted to known types", []),
            <<"unknown">>
    end.

-spec guess_endpoint_type(kz_json:object()) -> kz_term:ne_binary().
guess_endpoint_type(Endpoint) ->
    guess_endpoint_type(Endpoint
                       ,[<<"mobile">>
                        ,<<"sip">>
                        ,<<"skype">>
                        ]
                       ).

-spec guess_endpoint_type(kz_json:object(), kz_term:ne_binaries()) -> kz_term:ne_binary().
guess_endpoint_type(Endpoint, [Type|Types]) ->
    case kz_json:get_ne_value(Type, Endpoint) of
        'undefined' -> guess_endpoint_type(Endpoint, Types);
        _ -> Type
    end;
guess_endpoint_type(Endpoint, []) ->
    case kz_json:get_ne_value(<<"sip">>, Endpoint) of
        'undefined' -> <<"unknown">>;
        _Else -> <<"sip">>
    end.

-spec endpoint_id(kz_json:object(), kz_json:object()) -> kz_json:object().
endpoint_id(Endpoint, CCVs) ->
    case kz_doc:id(Endpoint) of
        'undefined' -> CCVs;
        EndpointId ->
            AuthType = case get_endpoint_type(Endpoint) of
                           <<"mobile">> -> <<"mobile">>;
                           _ -> kz_doc:type(Endpoint)
                       end,
            kz_json:set_values([{<<"Authorizing-ID">>, EndpointId}
                               ,{<<"Authorizing-Type">>, AuthType}
                               ]
                              ,CCVs
                              )
    end.


-spec maybe_fix_presence_id_realm(kz_term:api_ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_fix_presence_id_realm('undefined', _Endpoint, CCVs) -> CCVs;
maybe_fix_presence_id_realm(PresenceId, Endpoint, CCVs) ->
    case binary:match(PresenceId, <<"@">>) of
        'nomatch' ->
            Realm = get_realm(Endpoint),
            kz_json:set_value(<<"Presence-ID">>, <<PresenceId/binary, $@, Realm/binary>>, CCVs);
        _Else -> kz_json:set_value(<<"Presence-ID">>, PresenceId, CCVs)
    end.

-spec presence_id(kz_json:object(), kz_json:object()) -> kz_json:object().
presence_id(Endpoint, CCVs) ->
    Default = kzd_devices:sip_username(Endpoint),
    PresenceId = kzd_devices:presence_id(Endpoint, Default),
    case kz_term:is_empty(PresenceId) of
        'true' -> maybe_fix_presence_id_realm(Default, Endpoint, CCVs);
        'false' -> maybe_fix_presence_id_realm(PresenceId, Endpoint, CCVs)
    end.

-spec owner_id(kz_json:object(), kz_json:object()) -> kz_json:object().
owner_id(Endpoint, CCVs) ->
    case kz_json:get_ne_binary_value(<<"owner_id">>, Endpoint) of
        'undefined' -> CCVs;
        OwnerId -> kz_json:set_value(<<"Owner-ID">>, OwnerId, CCVs)
    end.

-spec hold_music(kz_json:object(), kz_json:object()) -> kz_json:object().
hold_music(Endpoint, CCVs) ->
    case kz_json:get_value([<<"music_on_hold">>, <<"media_id">>], Endpoint) of
        'undefined' -> CCVs;
        MediaId ->
            MOH = kz_media_util:media_path(MediaId, kz_doc:account_id(Endpoint)),
            kz_json:set_value(<<"Hold-Media">>, MOH, CCVs)
    end.

-spec set_invite_uri(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_invite_uri(URI, CCVs) ->
    kz_json:set_values([{<<"SIP-Invite-To-URI">>, URI}
                       ,{<<"SIP-Invite-Request-URI">>, URI}
                       ], CCVs).

-spec set_invite_uri(kz_term:api_ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_invite_uri('undefined', _Realm, CCVs) -> CCVs;
set_invite_uri(URIUser, Realm, CCVs) ->
    URI = <<"sip:", URIUser/binary, "@", Realm/binary>>,
    set_invite_uri(URI, CCVs).

-type invite_uri_fun() :: fun((kz_term:ne_binary()) -> kz_term:ne_binary()).

-spec set_invite_uri(invite_uri_fun(), kz_term:api_ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_invite_uri(_Fun, 'undefined', _Realm, CCVs) -> CCVs;
set_invite_uri(Fun, URIUser, Realm, CCVs) ->
    set_invite_uri(Fun(URIUser), Realm, CCVs).

-spec invite_uri(kz_json:object(), kz_json:object()) -> kz_json:object().
invite_uri(Endpoint, CCVs) ->
    Realm = get_realm(Endpoint),
    case kzd_devices:sip_invite_format(Endpoint) of
        <<"route">> -> set_invite_uri(kzd_devices:sip_route(Endpoint), CCVs);
        <<"e164">> -> set_invite_uri(fun knm_converters:normalize/1, kzd_devices:sip_number(Endpoint), Realm, CCVs);
        <<"1npan">> -> set_invite_uri(fun knm_converters:to_1npan/1, kzd_devices:sip_number(Endpoint), Realm, CCVs);
        <<"npan">> -> set_invite_uri(fun knm_converters:to_npan/1, kzd_devices:sip_number(Endpoint), Realm, CCVs);
        <<"contact">> -> set_invite_uri(kzd_devices:sip_username(Endpoint), Realm, CCVs);
        <<"username">> -> set_invite_uri(kzd_devices:sip_username(Endpoint), Realm, CCVs)
    end.

-spec rtcp_mux(kz_json:object(), kz_json:object()) -> kz_json:object().
rtcp_mux(Endpoint, CCVs) ->
    case kz_json:get_boolean_value([<<"media">>, <<"rtcp_mux">>], Endpoint) of
        'undefined' -> CCVs;
        RTCP_MUX -> kz_json:set_value(<<"RTCP-MUX">>, RTCP_MUX, CCVs)
    end.

-spec webrtc(kz_json:object(), kz_json:object()) -> kz_json:object().
webrtc(Endpoint, CCVs) ->
    case kz_json:is_true([<<"media">>, <<"webrtc">>], Endpoint) of
        'false' -> CCVs;
        'true' ->
            Props = [{<<"RTCP-MUX">>, 'true'}
                    ,{<<"Media-Webrtc">>, 'true'}
                    ],
            kz_json:set_values(Props, CCVs)
    end.

-spec call_waiting(kz_json:object(), kz_json:object()) -> kz_json:object().
call_waiting(Endpoint, CCVs) ->
    case kz_json:is_true([<<"call_waiting">>, <<"enabled">>], Endpoint, 'true') of
        'true' -> CCVs;
        'false' -> kz_json:set_value(<<"Call-Waiting-Disabled">>, 'true', CCVs)
    end.

-spec enable_fax(kz_json:object(), kz_json:object()) -> kz_json:object().
enable_fax(Endpoint, CCVs) ->
    case kz_json:get_value([<<"media">>, <<"fax_option">>], Endpoint) of
        <<"auto">> -> kz_json:set_value(<<"Fax-Enabled">>, <<"true">>, CCVs);
        _Else -> CCVs
    end.

-spec progress_timeout(kz_json:object(), kz_json:object()) -> kz_json:object().
progress_timeout(Endpoint, CCVs) ->
    case kz_json:get_integer_value([<<"media">>, <<"progress_timeout">>], Endpoint, 0) of
        Timeout when Timeout > 0 -> kz_json:set_value(<<"Endpoint-Progress-Timeout">>, Timeout, CCVs);
        _Else -> CCVs
    end.

-spec ignore_early_media(kz_json:object(), kz_json:object()) -> kz_json:object().
ignore_early_media(Endpoint, CCVs) ->
    case kz_json:is_true([<<"media">>, <<"ignore_early_media">>], Endpoint) of
        'true' -> kz_json:set_value(<<"Ignore-Early-Media">>, 'true', CCVs);
        'false' -> CCVs
    end.

-spec bypass_media(kz_json:object(), kz_json:object()) -> kz_json:object().
bypass_media(Endpoint, CCVs) ->
    case kz_json:is_true([<<"media">>, <<"bypass_media">>], Endpoint) of
        'true' -> kz_json:set_value(<<"Bypass-Media">>, 'true', CCVs);
        'false' -> CCVs
    end.

-spec ignore_completed_elsewhere(kz_json:object(), kz_json:object()) -> kz_json:object().
ignore_completed_elsewhere(Endpoint, CCVs) ->
    Default = kapps_config:get_is_true(?CONFIG_CAT, <<"default_ignore_completed_elsewhere">>, 'true'),
    Value = kz_json:get_first_defined([[<<"caller_id_options">>, <<"ignore_completed_elsewhere">>]
                                      ,[<<"sip">>, <<"ignore_completed_elsewhere">>]
                                      ,<<"ignore_completed_elsewhere">>
                                      ], Endpoint, Default),
    kz_json:set_value(<<"Ignore-Completed-Elsewhere">>, kz_term:is_true(Value), CCVs).

-spec enforce_security(kz_json:object(), kz_json:object()) -> kz_json:object().
enforce_security(Endpoint, CCVs) ->
    case kz_json:get_ne_binary_value([<<"media">>, <<"encryption">>, <<"security">>], Endpoint) of
        'undefined' ->
            case kz_json:is_true([<<"media">>, <<"encryption">>, <<"enforce_security">>], Endpoint) of
                'true' ->
                    Security = <<"mandatory">>,
                    kz_json:set_value(<<"Media-Encryption">>, Security, CCVs);
                'false' -> CCVs
            end;
        Security -> kz_json:set_value(<<"Media-Encryption">>, Security, CCVs)
    end.

-spec encryption_flags(kz_json:object(), kz_json:object()) -> kz_json:object().
encryption_flags(Endpoint, CCVs) ->
    kz_endpoint:encryption_method_map(CCVs, Endpoint).

-spec add_alert_info(kz_json:object(), kz_json:object()) -> kz_json:object().
add_alert_info(Endpoint, CCVs) ->
    AlertInfo = [{<<"Internal-Alert-Info">>, kz_json:get_value([<<"ringtones">>, <<"internal">>], Endpoint)}
                ,{<<"External-Alert-Info">>, kz_json:get_value([<<"ringtones">>, <<"external">>], Endpoint)}
                ],
    kz_json:set_values(AlertInfo, CCVs).

-spec get_codecs(kz_json:object()) -> 'undefined' | kz_term:ne_binaries().
get_codecs(JObj) ->
    case kz_json:get_value([<<"media">>, <<"audio">>, <<"codecs">>], JObj, [])
        ++ kz_json:get_value([<<"media">>, <<"video">>, <<"codecs">>], JObj, [])
    of
        [] -> 'undefined';
        Codecs -> Codecs
    end.

-spec profile(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId) ->
    profile(EndpointId, AccountId, []).

-spec profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> {'ok', kz_json:object()} | {'error', any()}.
profile(EndpointId, AccountId, Options) ->
    case kz_endpoint:get(EndpointId, AccountId) of
        {'ok', Endpoint} -> maybe_generate_profile(EndpointId, AccountId, Endpoint, Options);
        Error -> Error
    end.

-spec generate_ccvs(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
generate_ccvs(_EndpointId, AccountId, Endpoint) ->
    Realm = kzd_devices:sip_realm(Endpoint,  kz_json:get_value(<<"realm">>, Endpoint)),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Reseller-ID">>, kzd_accounts:reseller_id(AccountId)}
            ,{<<"Realm">>, Realm}
            ,{<<"Username">>, kzd_devices:sip_username(Endpoint)}
            ,{<<"SIP-Invite-Domain">>, Realm}
            ],
    CCVs = kz_json:from_list(Props),

    CCVFuns = [fun endpoint_id/2
              ,fun owner_id/2
              ,fun rtcp_mux/2
              ,fun webrtc/2
              ,fun enable_fax/2
              ,fun enforce_security/2
              ,fun encryption_flags/2
              ,fun presence_id/2
              ,fun add_alert_info/2
              ,fun hold_music/2
              ,fun invite_uri/2
              ,fun call_waiting/2
              ,fun progress_timeout/2
              ,fun ignore_early_media/2
              ,fun bypass_media/2
              ,fun ignore_completed_elsewhere/2
              ],
    lists:foldl(fun(Fun, Acc) -> Fun(Endpoint, Acc) end, CCVs, CCVFuns).

-spec generate_sip_headers(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
generate_sip_headers(_EndpointId, _AccountId, Endpoint) ->
    Headers = kz_json:new(),
    HeaderFuns = [fun add_sip_headers/2
                 ,fun maybe_add_aor/2
                 ],
    lists:foldl(fun(Fun, Acc) -> Fun(Endpoint, Acc) end, Headers, HeaderFuns).

-spec add_sip_headers(kz_json:object(), kz_json:object()) -> kz_json:object().
add_sip_headers(Endpoint, Headers) ->
    case kzd_devices:custom_sip_headers_inbound(Endpoint) of
        'undefined' -> Headers;
        CustomHeaders -> kz_json:merge_jobjs(CustomHeaders, Headers)
    end.

-spec maybe_add_aor(kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_add_aor(Endpoint, Headers) ->
    Username = kzd_devices:sip_username(Endpoint),
    maybe_add_aor(Username, Endpoint, Headers).

-spec maybe_add_aor(kz_term:api_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_add_aor('undefined', _Endpoint, Headers) -> Headers;
maybe_add_aor(Username, Endpoint, Headers) ->
    Realm = kzd_devices:sip_realm(Endpoint,  kz_json:get_value(<<"realm">>, Endpoint)),
    Format = kzd_devices:sip_invite_format(Endpoint),
    AOR = [{<<"X-KAZOO-AOR">>, <<"sip:", Username/binary, "@", Realm/binary>>}
          ,{<<"X-KAZOO-INVITE-FORMAT">>, Format}
          ],
    kz_json:set_values(AOR, Headers).

-spec maybe_generate_profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> {'ok', kz_json:object()}.
maybe_generate_profile(EndpointId, AccountId, Endpoint, Options) ->
    case kz_json:get_ne_binary_value(<<"Endpoint-Type">>, Endpoint) of
        <<"device">> -> generate_profile(EndpointId, AccountId, Endpoint, Options);
        _ -> {'error', 'not_device'}
    end.

-spec generate_profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> {'ok', kz_json:object()}.
generate_profile(EndpointId, AccountId, Endpoint, Options) ->
    CCVs = generate_ccvs(EndpointId, AccountId, Endpoint),
    SIPHeaders = generate_sip_headers(EndpointId, AccountId, Endpoint),

    CID = {CIDNumber, CIDName} = kz_attributes:get_endpoint_cid(<<"internal">>, Endpoint),
    CED = {CEDNumber, CEDName} = kz_attributes:get_endpoint_cid(<<"external">>, Endpoint),

    {Number, Name} = case props:get_ne_binary_value('kcid_type', Options, <<"Internal">>) of
                         <<"Internal">> -> CID;
                         _ -> CED
                     end,

    CIDType = case props:get_ne_binary_value('fetch_type', Options, <<"sip_auth">>) of
                  <<"sip_auth">> -> <<"Caller">>;
                  _ -> <<"Callee">>
              end,

    CPVs = [{<<CIDType/binary,"-ID-Number">>, Number}
           ,{<<CIDType/binary,"-ID-Name">>, Name}
           ,{<<"Internal-Caller-ID-Number">>, CIDNumber}
           ,{<<"Internal-Caller-ID-Name">>, CIDName}
           ,{<<"External-Caller-ID-Number">>, CEDNumber}
           ,{<<"External-Caller-ID-Name">>, CEDName}
           ,{<<"Account-ID">>, AccountId}
           ,{<<"Endpoint-ID">>, EndpointId}
           ,{<<"Endpoint-Caller-ID-Number">>, CIDNumber}
           ,{<<"Endpoint-Caller-ID-Name">>, CIDName}
           ,{<<"Endpoint-Presence-ID">>, kz_json:get_ne_binary_value(<<"Presence-ID">>, CCVs)}
           ],
    Profile = [{<<"Domain-Name">>, AccountId}
              ,{<<"User-ID">>, EndpointId}
              ,{<<"Account-ID">>, AccountId}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Custom-Profile-Vars">>, kz_json:from_list(CPVs)}
              ,{<<"Custom-SIP-Headers">>, SIPHeaders}
              ,{<<"Codecs">>, get_codecs(Endpoint)}
              ,{<<"CallForward">> , maybe_build_call_forward(EndpointId, AccountId, Endpoint)}
              ,{<<"Expires">>, 360}
              ],
    {'ok', kz_json:from_list(Profile)}.


-spec is_call_forward_enabled(kz_json:object()) -> boolean().
is_call_forward_enabled(Endpoint) ->
    kzd_devices:call_forward_number(Endpoint) =/= 'undefined'
        andalso (kzd_devices:call_forward_enabled(Endpoint)
                 orelse kzd_devices:call_forward_failover(Endpoint)
                ).

-spec maybe_build_call_forward(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_term:api_object().
maybe_build_call_forward(EndpointId, AccountId, Endpoint) ->
    case is_call_forward_enabled(Endpoint) of
        'false' -> 'undefined';
        'true' -> build_call_forward(EndpointId, AccountId, Endpoint)
    end.

-spec build_call_forward(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
build_call_forward(EndpointId, AccountId, Endpoint) ->
    Number = kzd_devices:call_forward_number(Endpoint),
    Realm = get_realm(Endpoint),
    CallFwdURI = list_to_binary([Number, "@", Realm]),
    CCVs = call_forward_properties(EndpointId, AccountId, Endpoint),
    kz_json:set_values([{<<"Call-Forward-Request-URI">>, CallFwdURI}
                       ,{<<"Custom-Channel-Vars">>, CCVs}
                       ]
                      ,kz_json:new()
                      ).

-spec call_forward_properties(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
call_forward_properties(EndpointId, AccountId, Endpoint) ->
    Routines = [fun call_forward_base_properties/3
               ,fun call_forward_confirm_properties/3
               ,fun call_forward_maybe_retain_cid/3
               ],
    Props = lists:foldl(fun(Fun, Acc) -> Acc ++ Fun(EndpointId, AccountId, Endpoint) end, [], Routines),
    kz_json:from_list(Props).

call_forward_base_properties(_EndpointId, _AccountId, Endpoint) ->
    [{<<"Call-Forward">>, 'true'}
    ,{<<"Is-Failover">>, kzd_devices:call_forward_failover(Endpoint)}
    ,{<<"Is-Substitute">>, kzd_devices:call_forward_substitute(Endpoint)}
    ,{<<"Direct-Calls-Only">>, kzd_devices:call_forward_direct_calls_only(Endpoint)}
    ,{<<"Require-Ignore-Early-Media">>, 'true'}
    ,{<<"Ignore-Early-Media">>, 'true'}
    ,{<<"Loopback-Bowout">>, 'true'}
    ,{<<"Call-Forward-From">>, <<"${kz_ctx_Inception}">>}
    ,{<<"Call-Forward-For-UUID">>, <<"${Unique-ID}">>}
    ].

cfw_single_reject() ->
    [<<"USER_BUSY">>
    ,<<"CALL_REJECTED">>
    ,<<"NO_ANSWER">>
    ,<<"NORMAL_CLEARING">>
    ,<<"PROGRESS_TIMEOUT">>
    ,<<"ALLOTTED_TIMEOUT">>
    ].

call_forward_confirm_properties(_EndpointId, AccountId, Endpoint) ->
    Lang = kz_media_util:prompt_language(AccountId),
    case kzd_devices:call_forward_require_keypress(Endpoint) of
        'false' -> [];
        'true' -> [{<<"Confirm-Key">>, <<"1">>}
                  ,{<<"Confirm-Cancel-Timeout">>, 'true'}
                  ,{<<"Confirm-Read-Timeout">>, 3 * ?MILLISECONDS_IN_SECOND}
                  ,{<<"Confirm-File">>, kz_media_util:get_prompt(<<"ivr-group_confirm">>, Lang, AccountId)}
                  ,{<<"Require-Ignore-Early-Media">>, 'true'}
                  ,{<<"Require-Fail-On-Single-Reject">>, cfw_single_reject()}
                  ]
    end.

call_forward_maybe_retain_cid(_EndpointId, _AccountId, Endpoint) ->
    case kzd_devices:call_forward_keep_caller_id(Endpoint) of
        'true' -> [{<<"Retain-CID">>, 'true'}];
        'false' -> []
    end.

-spec get_realm(kz_json:object()) -> kz_term:ne_binary().
get_realm(Endpoint) ->
    kz_json:get_value(<<"realm">>, Endpoint, <<"norealm">>).
