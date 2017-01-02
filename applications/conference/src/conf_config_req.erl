%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_config_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Options) ->
    'true' = kapi_conference:config_req_v(JObj),
    Request = kz_json:get_ne_value(<<"Request">>, JObj),
    handle_request(Request, JObj).

-spec handle_request(ne_binary(), kz_json:object()) -> 'ok'.
handle_request(<<"Conference">>, JObj) ->
    ConfigName = kz_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME),
    fetch_profile_config(JObj, ConfigName);
handle_request(<<"Controls">>, JObj) ->
    ConferenceName = kz_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME),
    ControlsName = kz_json:get_ne_value(<<"Controls">>, JObj),
    fetch_controls_config(JObj, ConferenceName, ControlsName).

-spec fetch_profile_config(kz_json:object(), ne_binary()) -> 'ok'.
fetch_profile_config(JObj, ?PAGE_PROFILE_NAME = ConfigName) ->
    fetch_profile_config(JObj, 'undefined', ConfigName);
fetch_profile_config(JObj, ConfigName) ->
    [AccountId, ConferenceId] = binary:split(ConfigName, <<"_">>),
    fetch_profile_config(JObj, AccountId, ConferenceId).

-spec fetch_profile_config(kz_json:object(), api_binary(), ne_binary()) -> 'ok'.
fetch_profile_config(JObj, AccountId, ?DEFAULT_PROFILE_NAME = ConfigName) ->
    fetch_profile_config(JObj, AccountId, ConfigName, default_profile());
fetch_profile_config(JObj, AccountId, ?PAGE_PROFILE_NAME = ConfigName) ->
    fetch_profile_config(JObj, AccountId, ConfigName, page_profile());
fetch_profile_config(JObj, AccountId, ConferenceId) ->
    Conference = get_conference(AccountId, ConferenceId),
    Profile = kapps_conference:profile(Conference),
    Config = kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"profiles">>, Profile], default_profile()),
    fetch_profile_config(JObj, Conference, ConferenceId, Config).

-spec fetch_profile_config(kz_json:object(), kapps_conference:conference(), ne_binary(), api_object()) -> 'ok'.
fetch_profile_config(JObj, _Conference, ConfigName, 'undefined') ->
    lager:debug("no profile defined for '~s', using default", [ConfigName]),
    fetch_profile_config(JObj, ConfigName, default_profile());
fetch_profile_config(JObj, _Conference, ?PAGE_PROFILE_NAME = ConfigName, Profile) ->
    ServerId = kz_api:server_id(JObj),
    lager:debug("profile '~s' found", [ConfigName]),
    Resp = [{<<"Profiles">>, kz_json:from_list([{ConfigName, Profile}])}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp));
fetch_profile_config(JObj, Conference, ConfigName, Profile) ->
    ServerId = kz_api:server_id(JObj),
    lager:debug("profile '~s' found", [ConfigName]),
    FullProfile = list_to_binary([kapps_conference:account_id(Conference)
                                 ,"_"
                                 ,kapps_conference:id(Conference)
                                 ]),
    Resp = [{<<"Profiles">>, profiles(Conference, FullProfile, Profile)}
           ,{<<"Advertise">>, advertise(ConfigName)}
           ,{<<"Chat-Permissions">>, chat_permissions(ConfigName)}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp)).

-spec fetch_controls_config(kz_json:object(), ne_binary(), ne_binary()) -> 'ok'.
fetch_controls_config(JObj, <<"page_", _/binary>>, ConfigName) ->
    ServerId = kz_api:server_id(JObj),
    [_, ControlsName] = binary:split(ConfigName, <<"_">>),
    Config = caller_controls(ControlsName),
    CallerControls = kz_json:from_list([{ConfigName, Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, Resp);
fetch_controls_config(JObj, ConferenceId, ConfigName) ->
    [AccountId, ControlsName] = binary:split(ConfigName, <<"_">>),
    ServerId = kz_api:server_id(JObj),
    Conference = get_conference(AccountId, ConferenceId),
    ControlCfg = get_conference_controls(ControlsName, Conference),
    Config = caller_controls(AccountId, ControlCfg),
    CallerControls = kz_json:from_list([{ConfigName, Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, Resp).

-spec get_conference_controls(ne_binary(), kapps_conference:conference()) -> ne_binary().
get_conference_controls(<<"caller-controls">>, Conference) ->
    kapps_conference:caller_controls(Conference);
get_conference_controls(<<"moderator-controls">>, Conference) ->
    kapps_conference:moderator_controls(Conference);
get_conference_controls(Name, Conference) ->
    lager:error("request for ~s controls, returning caller-controls", [Name]),
    kapps_conference:caller_controls(Conference).

-spec default_profile() -> kz_json:object().
default_profile() ->
    kapps_config:get(?CONFIG_CAT
                    ,[<<"profiles">>, ?DEFAULT_PROFILE_NAME]
                    ,kz_json:from_list(?DEFAULT_PROFILE_CONFIG)
                    ).

-spec page_profile() -> kz_json:object().
page_profile() ->
    kapps_config:get_json(?CONFIG_CAT
                         ,[<<"profiles">>, ?PAGE_PROFILE_NAME]
                         ,kz_json:from_list(?PAGE_PROFILE_CONFIG)
                         ).

-spec profiles(kapps_conference:conference(), ne_binary(), kz_json:object()) -> kz_json:object().
profiles(Conference, ConfigName, Profile) ->
    NewContent = add_conference_params(Conference, ConfigName, Profile),
    kz_json:from_list([{ConfigName, NewContent}]).

-spec add_conference_params(kapps_conference:conference(), ne_binary(), kz_json:object()) -> kz_json:object().
add_conference_params(Conference, _ConfigName, Profile) ->
    Props = props:filter_undefined(
              [{<<"max-members">>, max_participants(Conference)}
              ,{<<"max-members-sound">>, max_members_sound(Conference)}
              ]),
    kz_json:set_values(Props, Profile).

-spec max_participants(kapps_conference:conference()) -> api_binary().
max_participants(Conference) ->
    case kapps_conference:max_participants(Conference) of
        N when is_integer(N), N > 1 -> N;
        _Else -> 'undefined'
    end.

-spec max_members_sound(kapps_conference:conference()) -> api_binary().
max_members_sound(Conference) ->
    case kapps_conference:max_members_media(Conference) of
        'undefined' -> kz_media_util:get_prompt(?DEFAULT_MAX_MEMBERS_MEDIA);
        Media -> Media
    end.

-spec get_conference(ne_binary(), ne_binary()) -> kapps_conference:conference().
get_conference(AccountId, ConferenceId) ->
    {'ok', JObj} = kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ConferenceId),
    kapps_conference:from_conference_doc(JObj).

-spec caller_controls(ne_binary()) -> kz_json:object().
caller_controls(ConfigName) ->
    kapps_config:get(?CONFIG_CAT, [<<"caller-controls">>, ConfigName], ?DEFAULT_CONTROLS).

-spec caller_controls(ne_binary(), ne_binary()) -> kz_json:object().
caller_controls(AccountId, ConfigName) ->
    kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"caller-controls">>, ConfigName], ?DEFAULT_CONTROLS).

-spec advertise(ne_binary()) -> api_object().
-spec advertise(ne_binary(), api_object()) -> api_object().
advertise(?DEFAULT_PROFILE_NAME = ConfigName) ->
    advertise(ConfigName, ?ADVERTISE(ConfigName, ?DEFAULT_ADVERTISE_CONFIG));
advertise(?PAGE_PROFILE_NAME = ConfigName) ->
    advertise(ConfigName, ?ADVERTISE(ConfigName, ?PAGE_ADVERTISE_CONFIG));
advertise(ConfigName) ->
    advertise(ConfigName, ?ADVERTISE(ConfigName)).

advertise(_ConfigName, 'undefined') -> 'undefined';
advertise(ConfigName, Advertise) -> kz_json:from_list([{ConfigName, Advertise}]).

-spec chat_permissions(ne_binary()) -> api_object().
-spec chat_permissions(ne_binary(), api_object()) -> api_object().
chat_permissions(?DEFAULT_PROFILE_NAME = ConfigName) ->
    chat_permissions(ConfigName, ?CHAT_PERMISSIONS(ConfigName, ?DEFAULT_CHAT_CONFIG));
chat_permissions(?PAGE_PROFILE_NAME= ConfigName) ->
    chat_permissions(ConfigName, ?CHAT_PERMISSIONS(ConfigName, ?PAGE_CHAT_CONFIG));
chat_permissions(ConfigName) ->
    chat_permissions(ConfigName, ?CHAT_PERMISSIONS(ConfigName)).

chat_permissions(_ConfigName, 'undefined') -> 'undefined';
chat_permissions(ConfigName, Chat) -> kz_json:from_list([{ConfigName, Chat}]).
