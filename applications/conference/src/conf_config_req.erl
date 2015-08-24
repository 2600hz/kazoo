%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_config_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Options) ->
    'true' = wapi_conference:config_req_v(JObj),
    ConfigName = wh_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME),
    fetch_config(JObj, ConfigName).

-spec fetch_config(wh_json:object(), ne_binary()) -> 'ok'.
fetch_config(JObj, ?DEFAULT_PROFILE_NAME = ConfigName) ->
    fetch_config(JObj, ConfigName, default_profile());
fetch_config(JObj, ConfigName) ->
    Config = whapps_config:get(?CONFIG_CAT, [<<"profiles">>, ConfigName]),
    fetch_config(JObj, ConfigName, Config).

-spec fetch_config(wh_json:object(), ne_binary(), api_object()) -> 'ok'.
fetch_config(JObj, ConfigName, 'undefined') ->
    lager:debug("no profile defined for '~s', using default", [ConfigName]),
    fetch_config(JObj, ConfigName, default_profile());
fetch_config(JObj, ConfigName, Profile) ->
    lager:debug("profile '~s' found", [ConfigName]),
    Resp = [{<<"Profiles">>, profiles(ConfigName, Profile)}
            ,{<<"Caller-Controls">>, caller_controls(ConfigName)}
            ,{<<"Advertise">>, advertise(ConfigName)}
            ,{<<"Chat-Permissions">>, chat_permissions(ConfigName)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    try wapi_conference:publish_config_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                            ,props:filter_undefined(Resp)
                                           )
    of
        'ok' -> 'ok'
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("failed: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST)
    end.

-spec default_profile() -> wh_json:object().
default_profile() ->
    whapps_config:get(?CONFIG_CAT
                      ,[<<"profiles">>, ?DEFAULT_PROFILE_NAME]
                      ,wh_json:from_list(?DEFAULT_PROFILE_CONFIG)
                     ).

-spec profiles(ne_binary(), wh_json:object()) -> wh_json:object().
profiles(ConfigName, Profile) ->
    NewContent = add_conference_params(ConfigName, Profile),
    wh_json:from_list([{ConfigName, NewContent}]).

-spec add_conference_params(ne_binary(), wh_json:object()) -> wh_json:object().
add_conference_params(ConfigName, Profile) ->
    Conference = get_conference(ConfigName),
    Props = props:filter_undefined(
              [{<<"max-members">>, max_participants(Conference)}
               ,{<<"max-members-sound">>, max_members_sound(Conference)}
              ]),
    wh_json:set_values(Props, Profile).

-spec max_participants(whapps_conference:conference()) -> api_binary().
max_participants(Conference) ->
    case whapps_conference:max_participants(Conference) of
        N when is_integer(N), N > 1 -> N;
        _Else -> 'undefined'
    end.

-spec max_members_sound(whapps_conference:conference()) -> api_binary().
max_members_sound(Conference) ->
    case whapps_conference:play_exit_tone(Conference) of
        'false' -> 'undefined';
        'true' -> ?EXIT_TONE;
        Media -> Media
    end.

-spec get_conference(ne_binary()) -> whapps_conference:conference().
get_conference(?DEFAULT_PROFILE_NAME) -> whapps_conference:new();
get_conference(ConferenceID) ->
    Participants =
        [Pid ||
            {_,Pid,'worker',['conf_participant']} <- supervisor:which_children('conf_participant_sup')],
    Conferences = [conf_participant:conference(Pid) || Pid <- Participants],
    case [Conference || {'ok',Conference} <- Conferences,
                        whapps_conference:id(Conference) =:= ConferenceID]
    of
        [Conference|_] -> Conference;
        _Else ->
            lager:debug("no participant worker found for conference '~s'", [ConferenceID]),
            whapps_conference:new()
    end.

-spec caller_controls(ne_binary()) -> api_object().
-spec caller_controls(ne_binary(), api_object()) -> api_object().
caller_controls(?DEFAULT_PROFILE_NAME = ConfigName) ->
    caller_controls(ConfigName, ?CALLER_CONTROLS(ConfigName, ?DEFAULT_CALLER_CONTROLS_CONFIG));
caller_controls(ConfigName) ->
    caller_controls(ConfigName, ?CALLER_CONTROLS(ConfigName)).

caller_controls(_ConfigName, 'undefined') -> 'undefined';
caller_controls(ConfigName, Controls) ->
    wh_json:from_list([{ConfigName, Controls}]).

-spec advertise(ne_binary()) -> api_object().
-spec advertise(ne_binary(), api_object()) -> api_object().
advertise(?DEFAULT_PROFILE_NAME = ConfigName) ->
    advertise(ConfigName, ?ADVERTISE(ConfigName, ?DEFAULT_ADVERTISE_CONFIG));
advertise(ConfigName) ->
    advertise(ConfigName, ?ADVERTISE(ConfigName)).

advertise(_ConfigName, 'undefined') -> 'undefined';
advertise(ConfigName, Advertise) -> wh_json:from_list([{ConfigName, Advertise}]).

-spec chat_permissions(ne_binary()) -> api_object().
-spec chat_permissions(ne_binary(), api_object()) -> api_object().
chat_permissions(?DEFAULT_PROFILE_NAME = ConfigName) ->
    chat_permissions(ConfigName, ?CHAT_PERMISSIONS(ConfigName, ?DEFAULT_CHAT_CONFIG));
chat_permissions(ConfigName) ->
    chat_permissions(ConfigName, ?CHAT_PERMISSIONS(ConfigName)).

chat_permissions(_ConfigName, 'undefined') -> 'undefined';
chat_permissions(ConfigName, Chat) -> wh_json:from_list([{ConfigName, Chat}]).
