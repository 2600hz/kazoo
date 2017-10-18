%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
%%% @doc
%%% Send conference config commands to FS
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_fetch_configuration_conference).

%% API
-export([init/0]).

-export([conference/1]).

-include("ecallmgr.hrl").


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the bindings
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"fetch.configuration.configuration.name.conference.conf">>, ?MODULE, 'conference'),
    'ok'.

-spec conference(map()) -> fs_sendmsg_ret().
conference(#{node := Node, fetch_id := Id, payload := JObj}) ->
    kz_util:put_callid(Id),
    fetch_conference_config(Node, Id, kz_api:event_name(JObj), JObj).

-spec fix_conference_profile(kz_json:object()) -> kz_json:object().
fix_conference_profile(Resp) ->
    Ps = kz_json:get_value(<<"Profiles">>, Resp),
    JObj = kz_json:map(fun fix_conference_profile/2, Ps),
    kz_json:set_value(<<"Profiles">>, JObj, Resp).

-spec fix_conference_profile(kz_json:path(), kz_json:object()) -> {kz_json:path(), kz_json:object()}.
fix_conference_profile(Name, Profile) ->
    [AccountId | _] = binary:split(Name, <<"_">>),
    Routines = [fun maybe_fix_profile_tts/1
               ,fun conference_sounds/1
               ,fun set_verbose_events/1
               ,{fun kz_json:set_value/3, <<"caller-controls">>, <<AccountId/binary, "_caller-controls">>}
               ,{fun kz_json:set_value/3, <<"moderator-controls">>, <<AccountId/binary, "_moderator-controls">>}
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
    MediaName = ecallmgr_util:media_path(Value),
    kz_json:set_value(Key, MediaName, Profile);
maybe_convert_sound(_, _, _, Profile) -> Profile.


-spec fetch_conference_config(atom(), ne_binary(), ne_binary(), kz_json:object()) -> fs_sendmsg_ret().
fetch_conference_config(Node, Id, <<"COMMAND">>, JObj) ->
    Profile = kz_json:get_value(<<"profile_name">>, JObj),
    Conference = kz_json:get_value(<<"conference_name">>, JObj),
    maybe_fetch_conference_profile(Node, Id, Profile, Conference);
fetch_conference_config(Node, Id, <<"REQUEST_PARAMS">>, JObj) ->
    Action = kz_json:get_value(<<"Action">>, JObj),
    ConfName = kz_json:get_value(<<"Conf-Name">>, JObj),
    lager:debug("request conference:~p params:~p", [ConfName, Action]),
    fetch_conference_params(Node, Id, Action, ConfName, JObj).

fetch_conference_params(Node, Id, <<"request-controls">>, ConfName, JObj) ->
    Controls = kz_json:get_value(<<"Controls">>, JObj),
    lager:debug("request controls:~p for conference:~p", [Controls, ConfName]),
    Cmd = [{<<"Request">>, <<"Controls">>}
          ,{<<"Profile">>, ConfName}
          ,{<<"Controls">>, Controls} | kz_api:default_headers(?APP_NAME, ?APP_VERSION)],
    Resp = kz_amqp_worker:call(Cmd
                              ,fun kapi_conference:publish_config_req/1
                              ,fun kapi_conference:config_resp_v/1
                              ,ecallmgr_fs_node:fetch_timeout(Node)
                              ),
    {'ok', Xml} = handle_conference_params_response(Resp),
    send_conference_profile_xml(Node, Id, Xml);
fetch_conference_params(Node, Id, Action, ConfName, _JObj) ->
    lager:debug("undefined request_params action:~p conference:~p", [Action, ConfName]),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(Node, Id, XmlResp).

handle_conference_params_response({'ok', Resp}) ->
    lager:debug("replying with xml response for conference params request"),
    ecallmgr_fs_xml:conference_resp_xml(Resp);
handle_conference_params_response({'error', 'timeout'}) ->
    lager:debug("timed out waiting for conference params"),
    ecallmgr_fs_xml:not_found();
handle_conference_params_response(_Error) ->
    lager:debug("failed to lookup conference params, error:~p", [_Error]),
    ecallmgr_fs_xml:not_found().

-spec maybe_fetch_conference_profile(atom(), ne_binary(), api_binary(), api_binary()) -> fs_sendmsg_ret().
maybe_fetch_conference_profile(Node, Id, 'undefined', _) ->
    lager:debug("failed to lookup undefined conference profile"),
    {'ok', XmlResp} = ecallmgr_fs_xml:not_found(),
    send_conference_profile_xml(Node, Id, XmlResp);

maybe_fetch_conference_profile(Node, Id, <<"page">> = Profile, Conference) ->
    [_ , AccountId | _] = binary:split(Conference, <<"_">>, ['global']),
    fetch_conference_profile(Node, Id, Profile, AccountId);
maybe_fetch_conference_profile(Node, Id, Profile, _Conference) ->
    [AccountId | _] = binary:split(Profile, <<"_">>),
    fetch_conference_profile(Node, Id, Profile, AccountId).

-spec fetch_conference_profile(atom(), ne_binary(), api_binary(), api_binary()) -> fs_sendmsg_ret().
fetch_conference_profile(Node, Id, Profile, AccountId) ->
    Cmd = [{<<"Request">>, <<"Conference">>}
          ,{<<"Profile">>, Profile}
          ,{<<"Account-ID">>, AccountId}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    lager:debug("fetching profile '~s'", [Profile]),
    XmlResp = case kz_amqp_worker:call(Cmd
                                      ,fun kapi_conference:publish_config_req/1
                                      ,fun kapi_conference:config_resp_v/1
                                      ,ecallmgr_fs_node:fetch_timeout(Node)
                                      )
              of
                  {'ok', Resp} ->
                      FixedTTS = fix_conference_profile(Resp),
                      {'ok', Xml} = ecallmgr_fs_xml:conference_resp_xml(FixedTTS, [{<<"Account-ID">>, AccountId}]),
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
    send_conference_profile_xml(Node, Id, XmlResp).

-spec send_conference_profile_xml(atom(), ne_binary(), iolist()) -> fs_sendmsg_ret().
send_conference_profile_xml(Node, Id, XmlResp) ->
    lager:debug("sending conference profile XML to ~s: ~s", [Node, XmlResp]),
    freeswitch:fetch_reply(Node, Id, 'configuration', iolist_to_binary(XmlResp)).
