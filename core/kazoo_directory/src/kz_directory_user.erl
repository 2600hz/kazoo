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
-module(kz_directory_user).

-export([profile/2, profile/3]).

-include("kazoo_directory.hrl").


-spec maybe_fix_presence_id_realm(kz_term:api_ne_binary(), kz_json:object(), kz_json:object()) -> kz_json:object().
maybe_fix_presence_id_realm('undefined', _Endpoint, CCVs) -> CCVs;
maybe_fix_presence_id_realm(PresenceId, Endpoint, CCVs) ->
    case binary:match(PresenceId, <<"@">>) of
        'nomatch' ->
            Realm = kz_json:get_ne_binary_value(<<"realm">>, Endpoint),
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
    Realm = kz_json:get_ne_binary_value(<<"realm">>, Endpoint),
    Props = [{<<"Account-ID">>, AccountId}
            ,{<<"Reseller-ID">>, kzd_accounts:reseller_id(AccountId)}
            ,{<<"Realm">>, Realm}
            ,{<<"Username">>, kzd_devices:sip_username(Endpoint)}
            ,{<<"SIP-Invite-Domain">>, Realm}
            ],
    CCVs = kz_json:from_list(Props),

    CCVFuns = [fun owner_id/2
              ,fun presence_id/2
              ,fun hold_music/2
              ],
    lists:foldl(fun(Fun, Acc) -> Fun(Endpoint, Acc) end, CCVs, CCVFuns).

-spec generate_sip_headers(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
generate_sip_headers(_EndpointId, _AccountId, Endpoint) ->
    Headers = kz_json:new(),
    HeaderFuns = [fun add_sip_headers/2
                 ],
    lists:foldl(fun(Fun, Acc) -> Fun(Endpoint, Acc) end, Headers, HeaderFuns).

-spec add_sip_headers(kz_json:object(), kz_json:object()) -> kz_json:object().
add_sip_headers(Endpoint, Headers) ->
    case kzd_devices:custom_sip_headers_inbound(Endpoint) of
        'undefined' -> Headers;
        CustomHeaders -> kz_json:merge_jobjs(CustomHeaders, Headers)
    end.

-spec maybe_generate_profile(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> {'ok', kz_json:object()}.
maybe_generate_profile(EndpointId, AccountId, Endpoint, Options) ->
    %% lager:debug_unsafe("ENDPOINT => ~s", [kz_json:encode(Endpoint, [pretty])]),
    case kz_json:get_ne_binary_value(<<"Endpoint-Type">>, Endpoint) of
        <<"user">> -> generate_profile(EndpointId, AccountId, Endpoint, Options);
        _ -> {'error', 'not_user'}
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
           ],
    Profile = [{<<"Endpoint-Type">>, <<"user">>}
              ,{<<"Members">>, owned_by_query(EndpointId, AccountId)}
              ,{<<"Domain-Name">>, AccountId}
              ,{<<"User-ID">>, EndpointId}
              ,{<<"Custom-Channel-Vars">>, CCVs}
              ,{<<"Custom-Profile-Vars">>, kz_json:from_list(CPVs)}
              ,{<<"Custom-SIP-Headers">>, SIPHeaders}
              ,{<<"Expires">>, 360}
              ],
    {'ok', kz_json:from_list(Profile)}.




-spec owned_by_query(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binaries().
owned_by_query(OwnerId, AccountId) ->
    ViewOptions = [{'key', [OwnerId, <<"device">>]}],
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"attributes/owned">>, ViewOptions) of
        {'ok', JObjs} -> [kz_json:get_value(<<"id">>, JObj) || JObj <- JObjs];
        {'error', _R} -> []
    end.
