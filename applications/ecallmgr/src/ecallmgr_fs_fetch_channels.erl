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
    kazoo_bindings:bind(<<"fetch.channels.*">>, ?MODULE, 'channel_req'),
    'ok'.


-spec channel_req(map()) -> 'ok'.
channel_req(#{node := Node, fetch_id := FetchId, payload := JObj}) ->
    UUID = kzd_fetch:call_id(JObj),
    ForUUID = kz_json:get_value(<<"refer-for-channel-id">>, JObj),
    {'ok', ForChannel} = ecallmgr_fs_channel:fetch(ForUUID, 'proplist'),
    case ecallmgr_fs_channel:fetch_channel(UUID) of
        'undefined' -> channel_not_found(Node, FetchId);
        Channel ->
            [Uri] = kzsip_uri:uris(props:get_value(<<"switch_url">>, Channel)),
            URL = kzsip_uri:ruri(
                    #uri{user= kz_json:get_value(<<"refer-to-user">>, JObj)
                        ,domain= props:get_value(<<"realm">>, Channel)
                        ,opts=[{<<"fs_path">>, kzsip_uri:ruri(Uri#uri{user= <<>>})}]
                        }),
            build_channel_resp(FetchId, JObj, Node, URL, ForChannel, ecallmgr_fs_channel:channel_ccvs(Channel))
    end.

-spec build_channel_resp(ne_binary(), kz_json:object(), atom(), ne_binary(), kz_proplist(), kz_proplist()) -> 'ok'.
build_channel_resp(FetchId, JObj, Node, URL, Channel, ChannelVars) ->
    %% NOTE
    %% valid properties to return are
    %% sip-url , dial-prefix, absolute-dial-string, sip-profile (defaulted to current channel profile)
    %% freeswitch formats the dial string with the following logic
    %% if absolute-dial-string => %s%s [dial-prefix, absolute-dial-string]
    %% else => %ssofia/%s/%s [dial-prefix, sip-profile, sip-url]
    Resp = props:filter_undefined(
             [{<<"sip-url">>, URL}
             ,{<<"dial-prefix">>, channel_resp_dialprefix(JObj, Channel, ChannelVars)}
             ]),
    try_channel_resp(FetchId, Node, Resp).

-spec channel_resp_dialprefix(kz_json:object(), kz_proplist(), kz_proplist()) -> ne_binary().
channel_resp_dialprefix(JObj, Channel, ChannelVars) ->
    Props = props:filter_undefined(
              [{<<"sip_invite_domain">>, props:get_value(<<"Realm">>, ChannelVars)}
              ,{<<"sip_h_X-Core-UUID">>, kz_json:get_value(<<"Core-UUID">>, JObj)}
              ,{<<"sip_h_X-ecallmgr_", ?CALL_INTERACTION_ID>>, props:get_value(<<"interaction_id">>, Channel)}
              ,{<<"sip_h_X-ecallmgr_replaces-call-id">>, kz_json:get_value(<<"replaces-call-id">>, JObj)}
              ,{<<"sip_h_X-ecallmgr_refer-from-channel-id">>, kz_json:get_value(<<"refer-from-channel-id">>, JObj)}
              ,{<<"sip_h_X-ecallmgr_refer-for-channel-id">>, kz_json:get_value(<<"refer-for-channel-id">>, JObj)}
              ,{<<"sip_h_X-ecallmgr_Account-ID">>, props:get_value(<<"Account-ID">>, ChannelVars)}
              ,{<<"sip_h_X-ecallmgr_Realm">>, props:get_value(<<"Realm">>, ChannelVars)}
              ]),
    fs_props_to_binary(Props).

-spec fs_props_to_binary(kz_proplist()) -> ne_binary().
fs_props_to_binary([{Hk,Hv}|T]) ->
    Rest = << <<",", K/binary, "='", (kz_term:to_binary(V))/binary, "'">> || {K,V} <- T >>,
    <<"[", Hk/binary, "='", (kz_term:to_binary(Hv))/binary, "'", Rest/binary, "]">>.

-spec try_channel_resp(ne_binary(), atom(), kz_proplist()) -> 'ok'.
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

-spec channel_not_found(atom(), ne_binary()) -> 'ok'.
channel_not_found(Node, FetchId) ->
    {'ok', Resp} = ecallmgr_fs_xml:not_found(),
    freeswitch:fetch_reply(Node, FetchId, 'channels', iolist_to_binary(Resp)).
