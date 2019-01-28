%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz
%%% @doc
%%% Track the FreeSWITCH channel information, and provide accessors
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_channels).

-export([channel_req/1]).
-export([init/0]).

-include("ecallmgr.hrl").
-include_lib("kazoo_sip/include/kzsip_uri.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.channels.*.channel_req">>, ?MODULE, 'channel_req'),
    'ok'.


-spec channel_req(map()) -> 'ok'.
channel_req(#{node := Node, fetch_id := FetchId, payload := JObj}) ->
    ToUser = kz_json:get_value(<<"refer-to-user">>, JObj),
    TargetUUID = kz_json:get_ne_binary_value(<<"replaces-call-id">>, JObj),
    UUID = kz_json:get_ne_binary_value(<<"refer-from-channel-id">>, JObj),
    ForUUID = kz_json:get_ne_binary_value(<<"refer-for-channel-id">>, JObj),
    {'ok', ForChannel} = ecallmgr_fs_channel:fetch(ForUUID, 'proplist'),
    TargetChannel = ecallmgr_fs_channel:fetch_channel(TargetUUID),
    Channel = ecallmgr_fs_channel:fetch_channel(UUID),
    case Channel =/= 'undefined'
        andalso TargetChannel =/= 'undefined'
    of
        'false' -> channel_not_found(Node, FetchId);
        'true' ->
            [Uri] = kzsip_uri:uris(props:get_value(<<"Switch-URL">>, TargetChannel)),
            URL = kzsip_uri:ruri(
                    #uri{user=ToUser
                        ,domain=props:get_value(<<"realm">>, Channel)
                        ,opts=[{<<"fs_path">>, kzsip_uri:ruri(Uri#uri{user= <<>>})}]
                        }),
            CCVs = ecallmgr_fs_channel:channel_ccvs(Channel),
            ForChannelCCVs = ecallmgr_fs_channel:channel_ccvs(ForChannel),
            build_channel_resp(FetchId, JObj, Node, URL, TargetChannel, CCVs, ForChannelCCVs)
    end.

-spec build_channel_resp(kz_term:ne_binary(), kz_json:object(), atom(), kz_term:ne_binary(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
build_channel_resp(FetchId, JObj, Node, URL, Channel, ChannelVars, ForChannelCCVs) ->
    %% NOTE
    %% valid properties to return are
    %% sip-url , dial-prefix, absolute-dial-string, sip-profile (defaulted to current channel profile)
    %% freeswitch formats the dial string with the following logic
    %% if absolute-dial-string => %s%s [dial-prefix, absolute-dial-string]
    %% else => %ssofia/%s/%s [dial-prefix, sip-profile, sip-url]
    Resp = props:filter_undefined(
             [{<<"sip-url">>, URL}
             ,{<<"dial-prefix">>, channel_resp_dialprefix(JObj, Channel, ChannelVars, ForChannelCCVs)}
             ]),
    try_channel_resp(FetchId, Node, Resp).

-spec channel_resp_dialprefix(kz_json:object(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> kz_term:ne_binary().
channel_resp_dialprefix(JObj, Channel, ChannelVars, ForChannelCCVs) ->
    props:to_log(Channel, <<"TARGET CHANNEL">>),
    CallId = kz_binary:rand_hex(16),
    Props = props:filter_undefined(
              [{<<"sip_invite_domain">>, props:get_value(<<"Realm">>, ChannelVars)}
              ,{<<"sip_origination_call_id">>, CallId}

              ,{<<"ecallmgr_", ?CALL_INTERACTION_ID>>, props:get_value(<<"Call-Interaction-ID">>, ChannelVars)}
              ,{<<"ecallmgr_Account-ID">>, props:get_value(<<"Account-ID">>, ChannelVars)}
              ,{<<"ecallmgr_Realm">>, props:get_value(<<"Realm">>, ChannelVars)}
              ,{<<"ecallmgr_Authorizing-Type">>, props:get_value(<<"Authorizing-Type">>, ChannelVars)}
              ,{<<"ecallmgr_Authorizing-ID">>, props:get_value(<<"Authorizing-ID">>, ChannelVars)}
              ,{<<"ecallmgr_Owner-ID">>, props:get_value(<<"Owner-ID">>, ChannelVars)}
              ,{<<"presence_id">>, props:get_value(<<"Presence-ID">>, ChannelVars)}

              ,{<<"sip_h_X-FS-Auth-Token">>, nighmare_auth_token(ForChannelCCVs)}
              ,{<<"sip_h_X-FS-", ?CALL_INTERACTION_ID>>, props:get_value(<<"Call-Interaction-ID">>, ChannelVars)}
              ,{<<"sip_h_X-FS-From-Core-UUID">>, kz_json:get_value(<<"Core-UUID">>, JObj)}
              ,{<<"sip_h_X-FS-Refer-Partner-UUID">>, props:get_value(<<"Other-Leg-Call-ID">>, Channel)}

              ]),
    fs_props_to_binary(Props).

nighmare_auth_token(ChannelVars) ->
    case props:get_value(<<"Authorizing-ID">>, ChannelVars) of
        'undefined' -> 'undefined';
        AuthorizingID -> list_to_binary([AuthorizingID
                                        ,"@"
                                        ,props:get_value(<<"Account-ID">>, ChannelVars)
                                        ])
    end.

-spec fs_props_to_binary(kz_term:proplist()) -> kz_term:ne_binary().
fs_props_to_binary([{Hk,Hv}|T]) ->
    Rest = << <<",", K/binary, "='", (kz_term:to_binary(V))/binary, "'">> || {K,V} <- T >>,
    <<"[", Hk/binary, "='", (kz_term:to_binary(Hv))/binary, "'", Rest/binary, "]">>.

-spec try_channel_resp(kz_term:ne_binary(), atom(), kz_term:proplist()) -> 'ok'.
try_channel_resp(FetchId, Node, Props) ->
    try ecallmgr_fs_xml:sip_channel_xml(Props) of
        {'ok', ConfigXml} ->
            lager:debug("sending sofia XML to ~s: ~s", [Node, ConfigXml]),
            freeswitch:fetch_reply(Node, FetchId, 'channels', erlang:iolist_to_binary(ConfigXml))
    catch
        _E:_R ->
            lager:info("sofia profile resp failed to convert to XML (~s): ~p", [_E, _R]),
            channel_not_found(Node, FetchId)
    end.

-spec channel_not_found(atom(), kz_term:ne_binary()) -> 'ok'.
channel_not_found(Node, FetchId) ->
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, FetchId, 'channels', iolist_to_binary(Resp)).
