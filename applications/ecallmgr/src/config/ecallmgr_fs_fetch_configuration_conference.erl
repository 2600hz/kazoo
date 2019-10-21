%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Send conference config commands to FS
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_conference).

%% API
-export([init/0]).

-export([conference/1]).

-include("ecallmgr.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Initializes the bindings
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"fetch.configuration.*.*.conference.conf">>, ?MODULE, 'conference'),
    'ok'.

-spec conference(map()) -> fs_sendmsg_ret().
conference(#{node := Node, fetch_id := Id, payload := JObj}=Ctx) ->
    kz_log:put_callid(Id),
    fetch_conference_config(Node, Id, kz_api:event_name(JObj), JObj, Ctx).

-spec fix_conference_profile(kz_json:object()) -> kz_json:object().
fix_conference_profile(Resp) ->
    Ps = kz_json:get_value(<<"Profiles">>, Resp),
    JObj = kz_json:map(fun fix_conference_profile/2, Ps),
    kz_json:set_value(<<"Profiles">>, JObj, Resp).

-spec fix_conference_profile(kz_json:path(), kz_json:object()) -> {kz_json:path(), kz_json:object()}.
fix_conference_profile(Name, Profile) ->
    lager:debug("fixing up conference profile ~s", [Name]),
    Routines = [fun maybe_fix_profile_tts/1
               ,fun conference_sounds/1
               ,fun set_verbose_events/1
               ],
    {Name, kz_json:exec(Routines, Profile)}.

-spec set_verbose_events(kz_json:object()) -> kz_json:object().
set_verbose_events(Profile) ->
    kz_json:set_value(<<"verbose-events">>, <<"true">>, Profile).

-spec maybe_fix_profile_tts(kz_json:object()) -> kz_json:object().
maybe_fix_profile_tts(Profile) ->
    case kz_json:get_value(<<"tts-engine">>, Profile) of
        'undefined' -> Profile;
        <<"flite">> -> fix_flite_tts(Profile);
        _ -> Profile
    end.

-spec fix_flite_tts(kz_json:object()) -> kz_json:object().
fix_flite_tts(Profile) ->
    Voice = kz_json:get_value(<<"tts-voice">>, Profile),
    kz_json:set_value(<<"tts-voice">>, ecallmgr_fs_flite:voice(Voice), Profile).

-spec conference_sounds(kz_json:object()) -> kz_json:object().
conference_sounds(Profile) ->
    kz_json:foldl(fun conference_sound/3, Profile, Profile).

conference_sound(Key, Value, Profile) ->
    maybe_convert_sound(kz_binary:reverse(Key), Key, Value, Profile).

maybe_convert_sound(<<"dnuos-", _/binary>>, Key, Value, Profile) ->
    MediaName = ecallmgr_util:media_path(Value, 'new', kz_log:get_callid(), kz_json:new()),
    lager:debug("fixed up ~s from ~s to ~s", [Key, Value, MediaName]),
    kz_json:set_value(Key, MediaName, Profile);
maybe_convert_sound(_, _Key, _Value, Profile) ->
    Profile.

-spec fetch_conference_config(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), map()) -> fs_sendmsg_ret().
fetch_conference_config(Node, Id, <<"COMMAND">>, JObj, Ctx) ->
    Profile = kz_json:get_value(<<"profile_name">>, JObj),
    Conference = kz_json:get_value(<<"conference_name">>, JObj),
    AccountId = kzd_fetch:account_id(JObj),
    maybe_fetch_conference_profile(Node, Id, Profile, Conference, AccountId, Ctx);
fetch_conference_config(Node, Id, <<"REQUEST_PARAMS">>, JObj, Ctx) ->
    Action = kz_json:get_value(<<"Action">>, JObj),
    ConfName = kz_json:get_value(<<"Conf-Name">>, JObj),
    lager:debug("request conference:~p params:~p", [ConfName, Action]),
    fetch_conference_params(Node, Id, Action, ConfName, JObj, Ctx).

fetch_conference_params(Node, _Id, <<"request-controls">>, ConfName, JObj, Ctx) ->
    Controls = kz_json:get_value(<<"Controls">>, JObj),
    Profile = kz_json:get_value(<<"Conf-Profile">>, JObj),
    lager:debug("request controls:~p for profile: ~p", [Controls, Profile]),

    Cmd = [{<<"Request">>, <<"Controls">>}
          ,{<<"Profile">>, Profile}
          ,{<<"Conference-ID">>, ConfName}
          ,{<<"Controls">>, Controls}
          ,{<<"Call-ID">>, kzd_fetch:call_id(JObj)}
          ,{<<"Account-ID">>, kzd_fetch:account_id(JObj)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Resp = kz_amqp_worker:call(Cmd
                              ,fun kapi_conference:publish_config_req/1
                              ,fun kapi_conference:config_resp_v/1
                              ,ecallmgr_fs_node:fetch_timeout(Node)
                              ),
    {'ok', Xml} = handle_conference_params_response(Resp),
    send_conference_profile_xml(Xml, Ctx);
fetch_conference_params(_Node, _Id, Action, ConfName, _Data, Ctx) ->
    lager:debug("undefined request_params action:~p conference:~p", [Action, ConfName]),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(XmlResp, Ctx).

handle_conference_params_response({'ok', Resp}) ->
    lager:debug("replying with xml response for conference params request"),
    ecallmgr_fs_xml:conference_resp_xml(Resp);
handle_conference_params_response({'error', 'timeout'}) ->
    lager:debug("timed out waiting for conference params"),
    ecallmgr_fs_xml:not_found();
handle_conference_params_response(_Error) ->
    lager:debug("failed to lookup conference params, error:~p", [_Error]),
    ecallmgr_fs_xml:not_found().

-spec maybe_fetch_conference_profile(atom(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), map()) -> fs_sendmsg_ret().
maybe_fetch_conference_profile(_Node, _Id, _, _, 'undefined', Ctx) ->
    lager:debug("failed to lookup conference profile for undefined account-id"),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(XmlResp, Ctx);

maybe_fetch_conference_profile(_Node, _Id, 'undefined', _Conference, _AccountId, Ctx) ->
    lager:debug("failed to lookup undefined profile conference"),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(XmlResp, Ctx);

maybe_fetch_conference_profile(Node, Id, Profile, Conference, AccountId, Ctx) ->
    fetch_conference_profile(Node, Id, Profile, Conference, AccountId, Ctx).

-spec fetch_conference_profile(atom(), kz_term:ne_binary(), kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary(), map()) -> fs_sendmsg_ret().
fetch_conference_profile(Node, _Id, Profile, Conference, AccountId, Ctx) ->
    Cmd = [{<<"Request">>, <<"Conference">>}
          ,{<<"Profile">>, Profile}
          ,{<<"Conference-ID">>, Conference}
          ,{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    Variables = [{<<"Conference-Account-ID">>, AccountId}
                ,{<<"Conference-Node">>, kz_term:to_binary(node())}
                ,{<<"Conference-Profile">>, Profile}
                ],
    lager:debug("fetching profile '~s' for conference '~s' in account '~s'", [Profile, Conference, AccountId]),
    XmlResp = case kz_amqp_worker:call(Cmd
                                      ,fun kapi_conference:publish_config_req/1
                                      ,fun kapi_conference:config_resp_v/1
                                      ,ecallmgr_fs_node:fetch_timeout(Node)
                                      )
              of
                  {'ok', Resp} ->
                      FixedTTS = fix_conference_profile(Resp),
                      {'ok', Xml} = ecallmgr_fs_xml:conference_resp_xml(FixedTTS, Variables),
                      lager:debug("replying with conference profile ~s", [Profile]),
                      Xml;
                  {'error', 'timeout'} ->
                      lager:debug("timed out waiting for conference profile for ~s", [Profile]),
                      {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                      Resp;
                  _Other ->
                      lager:debug("failed to lookup conference profile for ~s: ~p", [Profile, _Other]),
                      {'ok', Resp} = ecallmgr_fs_xml:not_found(),
                      Resp
              end,
    send_conference_profile_xml(XmlResp, Ctx).

-spec send_conference_profile_xml(iolist(), map()) -> fs_sendmsg_ret().
send_conference_profile_xml(XmlResp, #{node := Node} = Ctx) ->
    lager:debug("sending conference profile XML to ~s: ~s", [Node, XmlResp]),
    freeswitch:fetch_reply(Ctx#{reply => iolist_to_binary(XmlResp)}).
