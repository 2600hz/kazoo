%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_config_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = kapi_conference:config_req_v(JObj),
    Request = kz_json:get_ne_value(<<"Request">>, JObj),
    lager:debug("~s profile request received", [Request]),
    case props:get_value('server', Props) of
        'undefined' ->
            handle_request(Request, JObj, create_conference(JObj));
        Server ->
            {'ok', Conference} = conf_participant:conference(Server),
            handle_request(Request, JObj, Conference)
    end.

-spec create_conference(kz_json:object()) -> kapps_conference:conference().
create_conference(JObj) ->
    Conference = kapps_conference:new(),
    ProfileName = kz_json:get_ne_value(<<"Profile">>, JObj),
    case binary:split(ProfileName, <<"_">>) of
        [ConferenceId, AccountId] ->
            Routines = [{fun kapps_conference:set_account_id/2, AccountId}
                       ,{fun kapps_conference:set_id/2, ConferenceId}
                       ],
            kapps_conference:update(Routines, Conference);
        _Else ->
            Routines = [{fun kapps_conference:set_profile_name/2, ProfileName}],
            kapps_conference:update(Routines, Conference)
    end.

-spec handle_request(kz_term:ne_binary(), kz_json:object(), kapps_conference:conference()) -> 'ok'.
handle_request(<<"Conference">>, JObj, Conference) ->
    handle_profile_request(JObj, Conference);
handle_request(<<"Controls">>, JObj, Conference) ->
    handle_controls_request(JObj, Conference).

-spec handle_profile_request(kz_json:object(), kapps_conference:conference()) -> 'ok'.
handle_profile_request(JObj, Conference) ->
    ProfileName = kz_json:get_ne_value(<<"Profile">>, JObj),
    {_, Profile} = kapps_conference:profile(Conference),
    ServerId = kz_api:server_id(JObj),
    Resp = [{<<"Profiles">>, profiles(ProfileName, fix_profile(Conference, Profile))}
           ,{<<"Advertise">>, advertise(ProfileName)}
           ,{<<"Chat-Permissions">>, chat_permissions(ProfileName)}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("returning conference profile ~s", [ProfileName]),
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp)).

-spec requested_profile_name(kz_json:object()) -> kz_term:ne_binary().
requested_profile_name(JObj) ->
    kz_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME).

-spec profiles(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
profiles(ProfileName, Profile) ->
    kz_json:from_list([{ProfileName, Profile}]).

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

-spec fix_profile(kapps_conference:conference(), kz_json:object()) -> kz_json:object().
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
        'true' -> ensure_tone(Key, Profile, kapps_conference:entry_tone(Conference));
        'false' -> remove_tone(Key, Profile)
    end.

-spec fix_exit_tones(kapps_conference:conference(), kz_json:object()) -> kz_json:object().
fix_exit_tones(Conference, Profile) ->
    Key = <<"exit-sound">>,
    case kapps_conference:play_exit_tone(Conference) of
        'true' -> ensure_tone(Key, Profile, kapps_conference:exit_tone(Conference));
        'false' -> remove_tone(Key, Profile)
    end.

-spec ensure_tone(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
ensure_tone(Key, Profile, Tone) ->
    case kz_json:get_ne_value(Key, Profile) of
        'undefined' ->
            lager:debug("ensure tone adding ~s ~s", [Key, Tone]),
            kz_json:set_value(Key, Tone, Profile);
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

-spec handle_controls_request(kz_json:object(), kapps_conference:conference()) -> 'ok'.
handle_controls_request(JObj, Conference) ->
    ProfileName = requested_profile_name(JObj),
    ControlsType = requested_controls_name(JObj),
    ControlsName = get_conference_controls_name(ControlsType, Conference),
    Controls = kapps_conference:controls(Conference, ControlsName),
    ServerId = kz_api:server_id(JObj),
    Resp = [{<<"Caller-Controls">>, controls(ControlsType, Controls)}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("returning ~s (~s) controls profile for ~s"
               ,[ControlsName, ControlsType, ProfileName]
               ),
    kapi_conference:publish_config_resp(ServerId, Resp).

-spec requested_controls_name(kz_json:object()) -> kz_term:ne_binary().
requested_controls_name(JObj) ->
    kz_json:get_ne_value(<<"Controls">>, JObj).

-spec controls(kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
controls(ControlsName, Controls) ->
    kz_json:from_list([{ControlsName, Controls}]).

-spec get_conference_controls_name(kz_term:ne_binary(), kapps_conference:conference()) -> kz_term:ne_binary().
get_conference_controls_name(<<"caller-controls">>, Conference) ->
    kapps_conference:caller_controls(Conference);
get_conference_controls_name(<<"moderator-controls">>, Conference) ->
    kapps_conference:moderator_controls(Conference);
get_conference_controls_name(_Name, Conference) ->
    kapps_conference:caller_controls(Conference).
