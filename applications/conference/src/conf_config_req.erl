%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_config_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = kapi_conference:config_req_v(JObj),
    Request = kz_json:get_ne_value(<<"Request">>, JObj),
    lager:debug("~s profile request received", [Request]),
    handle_request(Request, JObj, Props).

-spec handle_request(kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_request(<<"Conference">>, JObj, Props) ->
    case requested_profile_name(JObj) of
        ?PAGE_PROFILE_NAME -> send_profile(JObj, page_profile());
        ?DEFAULT_PROFILE_NAME -> send_profile(JObj, default_profile());
        ProfileName -> lookup_and_send_profile(JObj, Props, ProfileName)
    end;
handle_request(<<"Controls">>, JObj, Props) ->
    io:format("controls: ~p~n", [JObj]),
    case requested_profile_name(JObj) of
        ?PAGE_PROFILE_NAME -> send_controls(JObj);
        ?DEFAULT_PROFILE_NAME -> send_controls(JObj);
        ProfileName -> lookup_and_send_controls(JObj, Props, ProfileName)
    end.

-spec send_profile(kz_json:object(), kz_json:object()) -> 'ok'.
send_profile(JObj, Profile) ->
    ServerId = kz_api:server_id(JObj),
    ProfileName = requested_profile_name(JObj),
    lager:debug("returning conference profile ~s", [ProfileName]),
    Resp = [{<<"Profiles">>, kz_json:from_list([{ProfileName, Profile}])}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp)).

-spec lookup_and_send_profile(kz_json:object(), kz_term:proplist(), kz_term:ne_binary()) -> 'ok'.
lookup_and_send_profile(JObj, Props, ProfileName) ->
    Server = props:get_value('server', Props),
    {'ok', Conference} = conf_participant:conference(Server),
    Name = kapps_conference:profile(Conference),
    Profile = fix_profile(Conference, get_conference_profile(Name, ProfileName)),
    Profiles = kz_json:from_list([{ProfileName, Profile}]),
    lager:debug("returning conference profile ~s", [ProfileName]),
    ServerId = kz_api:server_id(JObj),
    Resp = [{<<"Profiles">>, Profiles}
           ,{<<"Advertise">>, advertise(ProfileName)}
           ,{<<"Chat-Permissions">>, chat_permissions(ProfileName)}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp)).

-spec get_conference_profile(kz_json:object() | kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
get_conference_profile(Name, ProfileName) when is_binary(Name) ->
    case binary:split(ProfileName, <<"_">>) of
        [_, AccountId] ->
            kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"profiles">>, Name]);
        _Else -> kapps_config:get_json(?CONFIG_CAT, [<<"profiles">>, Name])
    end;
get_conference_profile(Profile, _) -> Profile.

-spec default_profile() -> kz_json:object().
default_profile() ->
    kapps_config:get_json(?CONFIG_CAT
                         ,[<<"profiles">>, ?DEFAULT_PROFILE_NAME]
                         ,kz_json:from_list(?DEFAULT_PROFILE_CONFIG)
                         ).

-spec page_profile() -> kz_json:object().
page_profile() ->
    kapps_config:get_json(?CONFIG_CAT
                         ,[<<"profiles">>, ?PAGE_PROFILE_NAME]
                         ,kz_json:from_list(?PAGE_PROFILE_CONFIG)
                         ).

-spec fix_profile(kapps_conference:conference(), kz_term:api_object()) -> kz_json:object().
fix_profile(Conference, 'undefined') ->
    fix_profile(Conference, default_profile());
fix_profile(Conference, Profile) ->
    Routines = [fun add_conference_params/2
               ,fun fix_entry_tones/2
               ,fun fix_exit_tones/2
               ],
    lists:foldl(fun(F, P) -> F(Conference, P) end, Profile, Routines).

-spec add_conference_params(kapps_conference:conference(), kz_json:object()) -> kz_json:object().
add_conference_params(Conference, Profile) ->
    Props = props:filter_undefined(
              [{<<"max-members">>, max_participants(Conference)}
              ,{<<"max-members-sound">>, max_members_sound(Conference)}
              ]),
    kz_json:set_values(Props, Profile).

-spec fix_entry_tones(kapps_conference:conference(), kz_json:object()) -> kz_json:object().
fix_entry_tones(Conference, Profile) ->
    Key = <<"enter-sound">>,
    case kapps_conference:play_entry_tone(Conference) of
        'true' -> ensure_tone(Key, Profile);
        'false' -> remove_tone(Key, Profile)
    end.

-spec fix_exit_tones(kapps_conference:conference(), kz_json:object()) -> kz_json:object().
fix_exit_tones(Conference, Profile) ->
    Key = <<"exit-sound">>,
    case kapps_conference:play_exit_tone(Conference) of
        'true' -> ensure_tone(Key, Profile);
        'false' -> remove_tone(Key, Profile)
    end.

-spec ensure_tone(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
ensure_tone(Key, Profile) ->
    case kz_json:get_ne_value(Key, Profile) of
        'undefined' ->
            lager:debug("ensure tone adding ~s", [Key]),
            EnterSound = kz_json:get_ne_value(Key, default_profile()),
            kz_json:set_value(Key, EnterSound, Profile);
        _Else ->
            lager:debug("ensure tone already has ~s", [Key]),
            Profile
    end.

-spec remove_tone(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
remove_tone(Key, Profile) ->
    lager:debug("remove tone ~s", [Key]),
    kz_json:delete_key(Key, Profile).

-spec max_participants(kapps_conference:conference()) -> kz_term:api_binary().
max_participants(Conference) ->
    case kapps_conference:max_participants(Conference) of
        N when is_integer(N), N > 1 -> N;
        _Else -> 'undefined'
    end.

-spec max_members_sound(kapps_conference:conference()) -> kz_term:api_binary().
max_members_sound(Conference) ->
    case kapps_conference:max_members_media(Conference) of
        'undefined' ->
            kz_media_util:get_account_prompt(?DEFAULT_MAX_MEMBERS_MEDIA
                                            ,'undefined'
                                            ,kapps_conference:account_id(Conference)
                                            );
        Media -> Media
    end.

send_controls(JObj) ->
    ServerId = kz_api:server_id(JObj),
    ControlsType = requested_controls_name(JObj),
    Config = get_control_profile(ControlsType, 'undefined'),
    CallerControls = kz_json:from_list([{ControlsType, Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    io:format("controls resp 1: ~p~n", [Resp]),
    kapi_conference:publish_config_resp(ServerId, Resp).

lookup_and_send_controls(JObj, Props, ProfileName) ->
    ServerId = kz_api:server_id(JObj),
    Server = props:get_value('server', Props),
    {'ok', Conference} = conf_participant:conference(Server),
    ControlsType = requested_controls_name(JObj),
    ControlsName = get_conference_controls_name(ControlsType, Conference),
    lager:debug("returning ~s (~s) controls profile for ~s"
               ,[ControlsName, ControlsType, ProfileName]
               ),
    Config = get_control_profile(ControlsName, ProfileName),
    CallerControls = kz_json:from_list([{ControlsType, Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    io:format("controls resp 2: ~p~n", [Resp]),
    kapi_conference:publish_config_resp(ServerId, Resp).

-spec get_conference_controls_name(kz_term:ne_binary(), kapps_conference:conference()) -> kz_term:ne_binary().
get_conference_controls_name(<<"caller-controls">>, Conference) ->
    kapps_conference:caller_controls(Conference);
get_conference_controls_name(<<"moderator-controls">>, Conference) ->
    kapps_conference:moderator_controls(Conference);
get_conference_controls_name(_Name, Conference) ->
    kapps_conference:caller_controls(Conference).

-spec get_control_profile(kz_json:object() | kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
get_control_profile(Name, ProfileName) when is_binary(Name) ->
    case binary:split(ProfileName, <<"_">>) of
        [_, AccountId] ->
            kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"controls">>, Name]);
        _Else -> kapps_config:get_json(?CONFIG_CAT, [<<"controls">>, Name])
    end;
get_control_profile(Profile, _) -> Profile.

-spec advertise(kz_term:ne_binary()) -> kz_term:api_object().
advertise(?DEFAULT_PROFILE_NAME = ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName, ?DEFAULT_ADVERTISE_CONFIG));
advertise(?PAGE_PROFILE_NAME = ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName, ?PAGE_ADVERTISE_CONFIG));
advertise(ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName)).

-spec advertise(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:api_object().
advertise(_ProfileName, 'undefined') -> 'undefined';
advertise(ProfileName, Advertise) -> kz_json:from_list([{ProfileName, Advertise}]).

-spec chat_permissions(kz_term:ne_binary()) -> kz_term:api_object().
chat_permissions(?DEFAULT_PROFILE_NAME = ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName, ?DEFAULT_CHAT_CONFIG));
chat_permissions(?PAGE_PROFILE_NAME= ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName, ?PAGE_CHAT_CONFIG));
chat_permissions(ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName)).

-spec chat_permissions(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:api_object().
chat_permissions(_ProfileName, 'undefined') -> 'undefined';
chat_permissions(ProfileName, Chat) -> kz_json:from_list([{ProfileName, Chat}]).

-spec requested_profile_name(kz_json:object()) -> kz_term:ne_binary().
requested_profile_name(JObj) ->
    kz_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME).

-spec requested_controls_name(kz_json:object()) -> kz_term:ne_binary().
requested_controls_name(JObj) ->
    kz_json:get_ne_value(<<"Controls">>, JObj, ?DEFAULT_PROFILE_NAME).
