%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_config_req).

-include("conference.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Options) ->
    'true' = wapi_conference:config_req_v(JObj),
    ConfigName = wh_json:get_value(<<"Profile">>, JObj),
    fetch_config(JObj, ConfigName).

fetch_config(JObj, <<"default">> = ConfigName) ->
    Config =  whapps_config:get(<<"conferences">>, [<<"profiles">>, ConfigName], wh_json:from_list(?DEFAULT_PROFILE_CONFIG)),
    fetch_config(JObj, ConfigName, Config);
fetch_config(JObj, ConfigName) ->
    Config =  whapps_config:get(<<"conferences">>, [<<"profiles">>, ConfigName]),
    fetch_config(JObj, ConfigName, Config).

fetch_config(_JObj, _ConfigName, 'undefined') ->
    lager:debug("no profile defined for ~s", [_ConfigName]);
fetch_config(JObj, ConfigName, Profile) ->
    lager:debug("profile ~s found", [ConfigName]),
    Resp = [{<<"Profiles">>, wh_json:from_list([{ConfigName, Profile}])}
            ,{<<"Caller-Controls">>, caller_controls(ConfigName)}
            ,{<<"Advertise">>, advertise(ConfigName)}
            ,{<<"Chat-Permissions">>, chat_permissions(ConfigName)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    try
        wapi_conference:publish_config_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                            ,props:filter_undefined(Resp)
                                           )
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

caller_controls(<<"default">> = ConfigName) ->
    caller_controls(ConfigName, whapps_config:get(<<"conferences">>, [<<"caller-controls">>, ConfigName], ?DEFAULT_CALLER_CONTROLS_CONFIG));
caller_controls(ConfigName) ->
    caller_controls(ConfigName, whapps_config:get(<<"conferences">>, [<<"caller-controls">>, ConfigName])).

caller_controls(_ConfigName, 'undefined') -> 'undefined';
caller_controls(ConfigName, Controls) ->
    wh_json:from_list([{ConfigName, Controls}]).

advertise(<<"default">> = ConfigName) ->
    advertise(ConfigName, whapps_config:get(<<"conferences">>, [<<"advertise">>, ConfigName], ?DEFAULT_ADVERTISE_CONFIG));
advertise(ConfigName) ->
    advertise(ConfigName, whapps_config:get(<<"conferences">>, [<<"advertise">>, ConfigName])).

advertise(_ConfigName, 'undefined') -> 'undefined';
advertise(ConfigName, Advertise) -> wh_json:from_list([{ConfigName, Advertise}]).

chat_permissions(<<"default">> = ConfigName) ->
    chat_permissions(ConfigName, whapps_config:get(<<"conferences">>, [<<"chat-permissions">>, ConfigName], ?DEFAULT_CHAT_CONFIG));
chat_permissions(ConfigName) ->
    chat_permissions(ConfigName, whapps_config:get(<<"conferences">>, [<<"chat-permissions">>, ConfigName])).
chat_permissions(_ConfigName, 'undefined') -> 'undefined';
chat_permissions(ConfigName, Chat) -> wh_json:from_list([{ConfigName, Chat}]).
