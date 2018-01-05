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

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, _Options) ->
    'true' = kapi_conference:config_req_v(JObj),
    Request = kz_json:get_ne_value(<<"Request">>, JObj),
    lager:debug("~s profile request received", [Request]),
    handle_request(Request, JObj).

-spec handle_request(ne_binary(), kz_json:object()) -> 'ok'.
handle_request(<<"Conference">>, JObj) ->
    %% NOTE: if this is not an internal page or default
    %%   conf_participant:send_conference_command will ensure
    %%   the profile name of the call command to enter the
    %%   conference is formatted {accountid}_{conferenceid}.
    %%   Therefore, when the profile request comes in this
    %%   for conference established by this app
    %%   request_profile_name will return this format.
    %%   In that case we lookup the profile
    %%   for conferenceid in accountid but we return it as the
    %%   originally requested {accountid}_{conferenceid}
    %%   so it matches what FreeSWITCH expects.
    fetch_profile_config(JObj, requested_profile_name(JObj));
handle_request(<<"Controls">>, JObj) ->
    %% NOTE: if this is not an internal page or default conference
    %%  the requested_profile_name will only be the conference id.
    %%  However, when the profile was returned by this application
    %%  ecallmgr_fs_config 'fixed' set the controls profile names
    %%  to {accountid}_{controlsname}.  Splitting requested_controls_name
    %%  might give you the accountid and controls name internal key,
    %%  and the profile name will be the conference id.
    fetch_controls_config(JObj
                         ,requested_profile_name(JObj)
                         ,requested_controls_name(JObj)
                         ).

-spec fetch_profile_config(kz_json:object(), ne_binary()) -> 'ok'.
fetch_profile_config(JObj, ProfileName) ->
    case binary:split(ProfileName, <<"_">>) of
        [AccountId, ConferenceId] -> fetch_profile_config(JObj, AccountId, ConferenceId);
        _Else -> fetch_profile_config(JObj, 'undefined', ProfileName)
    end.

-spec fetch_profile_config(kz_json:object(), api_ne_binary(), ne_binary()) -> 'ok'.
fetch_profile_config(JObj, _, ?PAGE_PROFILE_NAME = ProfileName) ->
    fetch_profile_config(JObj, 'undefined', ProfileName, page_profile());
fetch_profile_config(JObj, _, ?DEFAULT_PROFILE_NAME = ProfileName) ->
    fetch_profile_config(JObj, 'undefined', ProfileName, default_profile());
fetch_profile_config(JObj, 'undefined', ProfileName) ->
    fetch_profile_config(JObj, 'undefined', ProfileName, default_profile());
fetch_profile_config(JObj, AccountId, ConferenceId) ->
    lager:debug("looking up conference profile for ~s in account ~s"
               ,[ConferenceId, AccountId]
               ),
    Conference = get_conference(AccountId, ConferenceId),
    Profile = kapps_conference:profile(Conference),
    Config = kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"profiles">>, Profile]),
    fetch_profile_config(JObj, Conference, ConferenceId, Config).

-spec fetch_profile_config(kz_json:object(), 'undefined' | kapps_conference:conference(), api_ne_binary(), api_object()) -> 'ok'.
fetch_profile_config(JObj, Conference, ProfileName, 'undefined') ->
    lager:debug("no profile defined for ~s, using default", [ProfileName]),
    fetch_profile_config(JObj, Conference, ProfileName, default_profile());
fetch_profile_config(JObj, 'undefined', ProfileName, Profile) ->
    ServerId = kz_api:server_id(JObj),
    lager:debug("returning conference profile ~s", [ProfileName]),
    %% NOTE: after possibly splitting the profile name make sure
    %%  we send back a profile with the originally requested name
    Resp = [{<<"Profiles">>, kz_json:from_list([{requested_profile_name(JObj), Profile}])}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp));
fetch_profile_config(JObj, Conference, ProfileName, Profile) ->
    ServerId = kz_api:server_id(JObj),
    lager:debug("returning account conference profile ~s", [ProfileName]),
    %% NOTE: after possibly splitting the profile name make sure
    %%  we send back a profile with the originally requested name
    Resp = [{<<"Profiles">>, profiles(Conference, requested_profile_name(JObj), Profile)}
           ,{<<"Advertise">>, advertise(ProfileName)}
           ,{<<"Chat-Permissions">>, chat_permissions(ProfileName)}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, props:filter_undefined(Resp)).

-spec fetch_controls_config(kz_json:object(), ne_binary(), ne_binary()) -> 'ok'.
fetch_controls_config(JObj, ConferenceId, ControlsName) ->
    case binary:split(ControlsName, <<"_">>) of
        [?PAGE_PROFILE_NAME = PageProfile, _] ->
            lager:debug("looking up page conference controls profile for ~s", [ConferenceId]),
            fetch_controls_config(JObj, ConferenceId, 'undefined', PageProfile);
        [?MATCH_ACCOUNT_RAW(AccountId), ControlsType] ->
            %% NOTE: ControlsType can be 'caller-controls' or 'moderator-controls'.
            %%   We expect these to be used internally as keys on the conference document
            %%   that specify the actual controls profile name to fetch (failing back to defaults).
            lager:debug("looking up account ~s conference controls profile for ~s", [AccountId, ConferenceId]),
            fetch_controls_config(JObj, ConferenceId, AccountId, ControlsType);
        _Else ->
            lager:debug("looking up interal conference controls profile for ~s", [ConferenceId]),
            fetch_controls_config(JObj, ConferenceId, 'undefined', ControlsName)
    end.

-spec fetch_controls_config(kz_json:object(), ne_binary(), api_ne_binary(), ne_binary()) -> 'ok'.
fetch_controls_config(JObj, ConferenceId, 'undefined', ControlsName) ->
    lager:debug("returning ~s controls profile for ~s", [ControlsName, ConferenceId]),
    ServerId = kz_api:server_id(JObj),
    Config = get_control_profile(ControlsName),
    %% NOTE: after possibly splitting the controls name make sure
    %%  we send back a profile with the originally requested name
    CallerControls = kz_json:from_list([{requested_controls_name(JObj), Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, Resp);
fetch_controls_config(JObj, ConferenceId, AccountId, ControlsType) ->
    ServerId = kz_api:server_id(JObj),
    Conference = get_conference(AccountId, ConferenceId),
    %% NOTE: this is where 'caller-controls' or 'moderator-controls' is
    %%  translated by the conferece configuration to the actuall
    %%  control profile defined on the conference document.
    ControlsName = get_conference_controls_name(ControlsType, Conference),
    lager:debug("returning ~s (~s) controls profile for ~s"
               ,[ControlsName, ControlsType, ConferenceId]
               ),
    Config = get_control_profile(AccountId, ControlsName),
    %% NOTE: after possibly splitting the controls name make sure
    %%  we send back a profile with the originally requested name
    CallerControls = kz_json:from_list([{requested_controls_name(JObj), Config}]),
    Resp = [{<<"Caller-Controls">>, CallerControls}
           ,{<<"Msg-ID">>, kz_api:msg_id(JObj)}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_conference:publish_config_resp(ServerId, Resp).

-spec get_conference_controls_name(ne_binary(), kapps_conference:conference()) -> ne_binary().
get_conference_controls_name(<<"caller-controls">>, Conference) ->
    kapps_conference:caller_controls(Conference);
get_conference_controls_name(<<"moderator-controls">>, Conference) ->
    kapps_conference:moderator_controls(Conference);
get_conference_controls_name(_Name, Conference) ->
    kapps_conference:caller_controls(Conference).

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

-spec profiles(kapps_conference:conference(), ne_binary(), kz_json:object()) -> kz_json:object().
profiles(Conference, ProfileName, Profile) ->
    Routines = [fun add_conference_params/2
               ,fun fix_entry_tones/2
               ,fun fix_exit_tones/2
               ],
    NewContent = lists:foldl(fun(F, P) -> F(Conference, P) end, Profile, Routines),
    kz_json:from_list([{ProfileName, NewContent}]).

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

-spec ensure_tone(ne_binary(), kz_json:object()) -> kz_json:object().
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

-spec remove_tone(ne_binary(), kz_json:object()) -> kz_json:object().
remove_tone(Key, Profile) ->
    lager:debug("remove tone ~s", [Key]),
    kz_json:delete_key(Key, Profile).

-spec max_participants(kapps_conference:conference()) -> api_binary().
max_participants(Conference) ->
    case kapps_conference:max_participants(Conference) of
        N when is_integer(N), N > 1 -> N;
        _Else -> 'undefined'
    end.

-spec max_members_sound(kapps_conference:conference()) -> api_binary().
max_members_sound(Conference) ->
    case kapps_conference:max_members_media(Conference) of
        'undefined' ->
            kz_media_util:get_account_prompt(?DEFAULT_MAX_MEMBERS_MEDIA
                                            ,'undefined'
                                            ,kapps_conference:account_id(Conference)
                                            );
        Media -> Media
    end.

-spec get_conference(ne_binary(), ne_binary()) -> kapps_conference:conference().
get_conference(?MATCH_ACCOUNT_RAW(AccountId), ConferenceId) ->
    case kz_datamgr:open_cache_doc(kz_util:format_account_db(AccountId), ConferenceId) of
        {'ok', JObj} -> kapps_conference:from_conference_doc(JObj);
        {'error', _} ->
            Routines = [{fun kapps_conference:set_account_id/2, AccountId}
                       ,{fun kapps_conference:set_id/2, ConferenceId}
                       ],
            kapps_conference:update(Routines, kapps_conference:new())
    end;
get_conference(AccountId, ConferenceId) ->
    lager:info("account id '~s' not valid, using new conference", [AccountId]),
    Routines = [{fun kapps_conference:set_account_id/2, AccountId}
               ,{fun kapps_conference:set_id/2, ConferenceId}
               ],
    kapps_conference:update(Routines, kapps_conference:new()).

-spec get_control_profile(ne_binary()) -> kz_json:objects().
get_control_profile(ProfileName) ->
    kapps_config:get(?CONFIG_CAT, [<<"caller-controls">>, ProfileName], ?DEFAULT_CONTROLS).

-spec get_control_profile(ne_binary(), ne_binary()) -> kz_json:objects().
get_control_profile(?MATCH_ACCOUNT_RAW(AccountId), ProfileName) ->
    kapps_account_config:get_global(AccountId, ?CONFIG_CAT, [<<"caller-controls">>, ProfileName], ?DEFAULT_CONTROLS);
get_control_profile(_AccountId, ProfileName) ->
    lager:info("account id '~s' not valid, getting system config instead"),
    get_control_profile(ProfileName).

-spec advertise(ne_binary()) -> api_object().
-spec advertise(ne_binary(), api_object()) -> api_object().
advertise(?DEFAULT_PROFILE_NAME = ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName, ?DEFAULT_ADVERTISE_CONFIG));
advertise(?PAGE_PROFILE_NAME = ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName, ?PAGE_ADVERTISE_CONFIG));
advertise(ProfileName) ->
    advertise(ProfileName, ?ADVERTISE(ProfileName)).

advertise(_ProfileName, 'undefined') -> 'undefined';
advertise(ProfileName, Advertise) -> kz_json:from_list([{ProfileName, Advertise}]).

-spec chat_permissions(ne_binary()) -> api_object().
-spec chat_permissions(ne_binary(), api_object()) -> api_object().
chat_permissions(?DEFAULT_PROFILE_NAME = ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName, ?DEFAULT_CHAT_CONFIG));
chat_permissions(?PAGE_PROFILE_NAME= ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName, ?PAGE_CHAT_CONFIG));
chat_permissions(ProfileName) ->
    chat_permissions(ProfileName, ?CHAT_PERMISSIONS(ProfileName)).

chat_permissions(_ProfileName, 'undefined') -> 'undefined';
chat_permissions(ProfileName, Chat) -> kz_json:from_list([{ProfileName, Chat}]).

-spec requested_profile_name(kz_json:object()) -> ne_binary().
requested_profile_name(JObj) ->
    kz_json:get_ne_value(<<"Profile">>, JObj, ?DEFAULT_PROFILE_NAME).

-spec requested_controls_name(kz_json:object()) -> ne_binary().
requested_controls_name(JObj) ->
    kz_json:get_ne_value(<<"Controls">>, JObj, ?DEFAULT_PROFILE_NAME).
