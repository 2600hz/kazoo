%%%============================================================================
%%% @copyright (C) 2012-2018 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%============================================================================
-module(kapps_conference).

-include("kapps_call_command.hrl").

-export([new/0]).
-export([from_conference_doc/1, from_conference_doc/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([to_proplist/1]).
-export([is_conference/1]).
-export([update/2]).

-export([id/1, set_id/2]).
-export([name/1, set_name/2]).
-export([account_id/1, set_account_id/2]).
-export([moderator_controls/1, set_moderator_controls/2]).
-export([caller_controls/1, set_caller_controls/2]).
-export([profile/1, set_profile/2]).
-export([focus/1, set_focus/2]).
-export([application_name/1, set_application_name/2]).
-export([application_version/1, set_application_version/2]).
-export([controller_queue/1, set_controller_queue/2]).
-export([bridge_username/1, set_bridge_username/2]).
-export([bridge_password/1, set_bridge_password/2]).
-export([member_pins/1, set_member_pins/2]).
-export([moderator_pins/1, set_moderator_pins/2]).
-export([moderator/1, set_moderator/2]).
-export([member_join_muted/1, set_member_join_muted/2]).
-export([member_join_deaf/1, set_member_join_deaf/2]).
-export([moderator_join_muted/1, set_moderator_join_muted/2]).
-export([moderator_join_deaf/1, set_moderator_join_deaf/2]).
-export([max_participants/1, set_max_participants/2]).
-export([max_members_media/1, set_max_members_media/2]).
-export([require_moderator/1, set_require_moderator/2]).
-export([wait_for_moderator/1, set_wait_for_moderator/2]).
-export([play_name_on_join/1, set_play_name_on_join/2]).
-export([play_entry_prompt/1, set_play_entry_prompt/2]).
-export([play_exit_tone/1, set_play_exit_tone/2]).
-export([play_entry_tone/1, set_play_entry_tone/2]).
-export([play_welcome/1, set_play_welcome/2]).
-export([conference_doc/1, set_conference_doc/2]).
-export([call/1, set_call/2]).

-export([kvs_append/3
        ,kvs_append_list/3
        ,kvs_erase/2
        ,kvs_fetch/2
        ,kvs_fetch_keys/1
        ,kvs_filter/2
        ,kvs_find/2
        ,kvs_fold/3
        ,kvs_from_proplist/2
        ,kvs_is_key/2
        ,kvs_map/2
        ,kvs_store/3
        ,kvs_store_proplist/2
        ,kvs_to_proplist/1
        ,kvs_update/3
        ,kvs_update/4
        ,kvs_update_counter/3
        ]).

-export([flush/0, cache/1, cache/2, retrieve/1]).

-define(BRIDGE_USER, kapps_config:get_ne_binary(<<"conferences">>, <<"bridge_username">>, kz_binary:rand_hex(12))).
-define(BRIDGE_PWD, kapps_config:get_ne_binary(<<"conferences">>, <<"bridge_password">>, kz_binary:rand_hex(12))).


%%%% conference record %%%%%%%%
%%
%%  id                       the conference id
%%  focus                    the conference focus
%%  profile                  conference profile (config settings)
%%  controller_q             the controller queue, for responses
%%  bridge_username          the username used for a conference bridge
%%  bridge_password          the password used for a conference bridge
%%  member_pins              a list of pins for use by members
%%  moderator_pins           a list of pins for use by the moderators
%%  moderator                tri-state true/false if the caller is known to be a moderator, otherwise 'undefined'
%%  member_join_muted        should the member join muted
%%  member_join_deaf         should the member join deaf
%%  moderator_join_muted     should the moderator join muted
%%  moderator_join_deaf      should the moderator join deaf
%%  max_participants         max number of participants
%%  max_members_media        media to play instead of allowing new users to conference
%%  require_moderator        does the conference require a moderator
%%  wait_for_moderator       can members wait for a moderator
%%  play_name_on_join        should participants have their name played on join
%%  play_entry_prompt        Play prompt telling caller they're entering the conference
%%  play_exit_tone           Play tone telling caller they have left the conference
%%  play_entry_tone          Play tone telling caller they have entered the conference
%%  play_welcome             Play prompt welcoming caller to the conference
%%  conference_doc           the complete conference doc used to create the record (when and if)
%%  app_name                 The application name used during kapps_conference_command
%%  app_version              The application version used during kapps_conference_command
%%  kvs                      allows conferences to set values that propogate to children
%%  call                     kapps_call object
%%  account_id               %% account id
%%  moderator_controls       moderator controls (config settings)
%%  caller_controls          caller controls (config settings)
%%

-record(kapps_conference, {id :: kz_term:api_ne_binary()
                          ,name :: kz_term:api_ne_binary()
                          ,focus :: kz_term:api_ne_binary()
                          ,profile = 'undefined' :: kz_term:api_binary()
                          ,controller_q :: kz_term:api_ne_binary()
                          ,bridge_username = ?BRIDGE_USER :: kz_term:ne_binary()
                          ,bridge_password = ?BRIDGE_PWD :: kz_term:ne_binary()
                          ,member_pins = [] :: kz_term:ne_binaries()
                          ,moderator_pins = [] :: kz_term:ne_binaries()
                          ,moderator :: kz_term:api_boolean()
                          ,member_join_muted = 'false' :: boolean()
                          ,member_join_deaf = 'false' :: boolean()
                          ,moderator_join_muted = 'false' :: boolean()
                          ,moderator_join_deaf = 'false' :: boolean()
                          ,max_participants = 0 :: non_neg_integer()
                          ,max_members_media :: kz_term:api_ne_binary()
                          ,require_moderator = 'false' :: boolean()
                          ,wait_for_moderator = 'false' :: boolean()
                          ,play_name_on_join = 'false' :: boolean()
                          ,play_entry_prompt = 'true' :: boolean()
                          ,play_exit_tone = 'true' :: tone()
                          ,play_entry_tone = 'true' :: tone()
                          ,play_welcome = 'true' :: boolean()
                          ,conference_doc :: kz_term:api_object()
                          ,app_name = <<"kapps_conference">> :: kz_term:ne_binary()
                          ,app_version = <<"1.0.0">> :: kz_term:ne_binary()
                          ,kvs = orddict:new() :: orddict:orddict()
                          ,call :: kapps_call:call() | 'undefined'
                          ,account_id :: kz_term:api_ne_binary()
                          ,moderator_controls = <<"moderator-default">> :: kz_term:ne_binary()
                          ,caller_controls = <<"caller-default">> :: kz_term:ne_binary()
                          }).

-type tone() :: boolean() | kz_term:ne_binary().
-export_type([tone/0]).

-opaque conference() :: #kapps_conference{}.
-export_type([conference/0]).

-spec new() -> conference().
new() -> #kapps_conference{}.

-spec from_json(kz_json:object()) -> conference().
from_json(JObj) -> from_json(JObj, #kapps_conference{}).

-spec from_json(kz_json:object(), conference()) -> conference().
from_json(JObj, Conference) ->
    KVS = orddict:from_list(kz_json:to_proplist(kz_json:get_value(<<"Key-Value-Store">>, JObj, kz_json:new()))),
    Conference#kapps_conference{id = kz_json:get_ne_binary_value(<<"Conference-ID">>, JObj, id(Conference))
                               ,name = kz_json:get_ne_binary_value(<<"Conference-Name">>, JObj, profile(Conference))
                               ,profile = kz_json:get_ne_binary_value(<<"Profile">>, JObj, profile(Conference))
                               ,focus = kz_json:get_ne_binary_value(<<"Conference-Focus">>, JObj, focus(Conference))
                               ,controller_q = kz_json:get_ne_binary_value(<<"Controller-Queue">>, JObj, controller_queue(Conference))
                               ,bridge_username = kz_json:get_ne_binary_value(<<"Bridge-Username">>, JObj, bridge_username(Conference))
                               ,bridge_password = kz_json:get_ne_binary_value(<<"Bridge-Password">>, JObj, bridge_password(Conference))
                               ,member_pins = kz_json:get_ne_value(<<"Member-Pins">>, JObj, member_pins(Conference))
                               ,moderator_pins = kz_json:get_ne_value(<<"Moderator-Pins">>, JObj, moderator_pins(Conference))
                               ,moderator = kz_json:is_true(<<"Moderator">>, JObj, moderator(Conference))
                               ,member_join_muted = kz_json:is_true(<<"Member-Join-Muted">>, JObj, member_join_muted(Conference))
                               ,member_join_deaf = kz_json:is_true(<<"Member-Join-Deaf">>, JObj, member_join_deaf(Conference))
                               ,moderator_join_muted = kz_json:is_true(<<"Moderator-Join-Muted">>, JObj, moderator_join_muted(Conference))
                               ,moderator_join_deaf = kz_json:is_true(<<"Moderator-Join-Deaf">>, JObj, moderator_join_deaf(Conference))
                               ,max_participants = kz_json:get_integer_value(<<"Max-Participants">>, JObj, max_participants(Conference))
                               ,max_members_media = kz_json:get_ne_binary_value(<<"Max-Members-Media">>, JObj, max_members_media(Conference))
                               ,require_moderator = kz_json:is_true(<<"Require-Moderator">>, JObj, require_moderator(Conference))
                               ,wait_for_moderator = kz_json:is_true(<<"Wait-For-Moderator">>, JObj, wait_for_moderator(Conference))
                               ,play_name_on_join = kz_json:is_true(<<"Play-Name-On-Join">>, JObj, play_name_on_join(Conference))
                               ,play_entry_prompt = kz_json:is_true(<<"Play-Entry-Prompt">>, JObj, play_entry_prompt(Conference))
                               ,play_exit_tone = get_tone(kz_json:get_value(<<"Play-Exit-Tone">>, JObj, play_exit_tone(Conference)))
                               ,play_entry_tone = get_tone(kz_json:get_value(<<"Play-Entry-Tone">>, JObj, play_entry_tone(Conference)))
                               ,play_welcome = kz_json:is_true(<<"Play-Welcome">>, JObj, play_welcome(Conference))
                               ,conference_doc = kz_json:is_true(<<"Conference-Doc">>, JObj, conference_doc(Conference))
                               ,kvs = orddict:merge(fun(_, _, V2) -> V2 end, Conference#kapps_conference.kvs, KVS)
                               ,call = load_call(JObj, call(Conference))
                               ,account_id = kz_json:get_value(<<"Account-ID">>, JObj)
                               ,moderator_controls = kz_json:get_ne_binary_value(<<"Moderator-Controls">>, JObj, moderator_controls(Conference))
                               ,caller_controls = kz_json:get_ne_binary_value(<<"Caller-Controls">>, JObj, caller_controls(Conference))
                               }.

-spec load_call(kz_json:object(), kapps_call:call() | 'undefined') -> kapps_call:call() | 'undefined'.
load_call(JObj, ConfCall) ->
    case kz_json:get_value(<<"Call">>, JObj) of
        'undefined' -> ConfCall;
        CallJObj -> kapps_call:from_json(CallJObj)
    end.

-spec to_json(conference()) -> kz_json:object().
to_json(#kapps_conference{}=Conference) ->
    Props = to_proplist(Conference),
    KVS = [KV
           || {_, V}=KV <- props:get_value(<<"Key-Value-Store">>, Props, []),
              V =/= 'undefined',
              kz_json:is_json_term(V)
          ],
    kz_json:from_list([KV
                       || {_, V}=KV <- [{<<"Key-Value-Store">>, kz_json:from_list(KVS)}
                                        | props:delete(<<"Key-Value-Store">>, Props)
                                       ],
                          V =/= 'undefined',
                          kz_json:is_json_term(V)
                      ]).

-spec to_proplist(conference()) -> kz_term:proplist().
to_proplist(#kapps_conference{}=Conference) ->
    [{<<"Conference-ID">>, id(Conference)}
    ,{<<"Conference-Name">>, name(Conference)}
    ,{<<"Profile">>, profile(Conference)}
    ,{<<"focus">>, focus(Conference)}
    ,{<<"Controller-Queue">>, controller_queue(Conference)}
    ,{<<"Bridge-Username">>, bridge_username(Conference)}
    ,{<<"Bridge-Password">>, bridge_password(Conference)}
    ,{<<"Member-Pins">>, member_pins(Conference)}
    ,{<<"Moderator-Pins">>, moderator_pins(Conference)}
    ,{<<"Moderator">>, moderator(Conference)}
    ,{<<"Member-Join-Muted">>, member_join_muted(Conference)}
    ,{<<"Member-Join-Deaf">>, member_join_deaf(Conference)}
    ,{<<"Moderator-Join-Muted">>, moderator_join_muted(Conference)}
    ,{<<"Moderator-Join-Deaf">>, moderator_join_deaf(Conference)}
    ,{<<"Max-Participants">>, max_participants(Conference)}
    ,{<<"Max-Members-Media">>, max_members_media(Conference)}
    ,{<<"Require-Moderator">>, require_moderator(Conference)}
    ,{<<"Wait-For-Moderator">>, wait_for_moderator(Conference)}
    ,{<<"Play-Name-On-Join">>, play_name_on_join(Conference)}
    ,{<<"Play-Entry-Prompt">>, play_entry_prompt(Conference)}
    ,{<<"Play-Exit-Tone">>, play_exit_tone(Conference)}
    ,{<<"Play-Entry-Tone">>, play_entry_tone(Conference)}
    ,{<<"Play-Welcome">>, play_welcome(Conference)}
    ,{<<"Conference-Doc">>, conference_doc(Conference)}
    ,{<<"Key-Value-Store">>, kvs_to_proplist(Conference)}
    ,{<<"Call">>, kapps_call:to_json(call(Conference))}
    ,{<<"Account-ID">>, account_id(Conference)}
    ,{<<"Moderator-Controls">>, moderator_controls(Conference)}
    ,{<<"Caller-Controls">>, caller_controls(Conference)}
    ].

-spec is_conference(any()) -> boolean().
is_conference(#kapps_conference{}) -> 'true';
is_conference(_) -> 'false'.


-spec from_conference_doc(kz_json:object()) -> conference().
from_conference_doc(JObj) ->
    from_conference_doc(JObj, #kapps_conference{}).

-spec from_conference_doc(kz_json:object(), conference()) -> conference().
from_conference_doc(JObj, Conference) ->
    Member = kz_json:get_json_value(<<"member">>, JObj),
    Moderator = kz_json:get_json_value(<<"moderator">>, JObj),
    Conference#kapps_conference{id = kz_doc:id(JObj, id(Conference))
                               ,name = kz_json:get_ne_binary_value(<<"name">>, JObj, name(Conference))
                               ,account_id = kz_json:get_ne_binary_value(<<"pvt_account_id">>, JObj, account_id(Conference))
                               ,profile = kz_json:get_ne_binary_value(<<"profile">>, JObj, profile(Conference))
                               ,focus = kz_json:get_ne_binary_value(<<"focus">>, JObj, focus(Conference))
                               ,bridge_username = kz_json:get_ne_binary_value(<<"bridge_username">>, JObj, bridge_username(Conference))
                               ,bridge_password = kz_json:get_ne_binary_value(<<"bridge_password">>, JObj, bridge_password(Conference))
                               ,member_pins = kz_json:get_list_value(<<"pins">>, Member, member_pins(Conference))
                               ,moderator_pins = kz_json:get_list_value(<<"pins">>, Moderator, moderator_pins(Conference))
                               ,member_join_muted = kz_json:is_true(<<"join_muted">>, Member, member_join_muted(Conference))
                               ,member_join_deaf = kz_json:is_true(<<"join_deaf">>, Member, member_join_deaf(Conference))
                               ,play_name_on_join = kz_json:is_true(<<"play_name">>, JObj, play_name_on_join(Conference))
                               ,play_entry_prompt = kz_json:is_true(<<"play_entry_prompt">>, Member, play_entry_prompt(Conference))
                               ,play_exit_tone = get_tone(kz_json:get_value(<<"play_exit_tone">>, JObj, play_exit_tone(Conference)))
                               ,play_entry_tone = get_tone(kz_json:get_value(<<"play_entry_tone">>, JObj, play_entry_tone(Conference)))
                               ,play_welcome = kz_json:is_true(<<"play_welcome">>, JObj, play_welcome(Conference))
                               ,moderator_join_muted = kz_json:is_true(<<"join_muted">>, Moderator, moderator_join_muted(Conference))
                               ,moderator_join_deaf = kz_json:is_true(<<"join_deaf">>, Moderator, moderator_join_deaf(Conference))
                               ,max_participants = kz_json:get_integer_value(<<"max_participants">>, JObj, max_participants(Conference))
                               ,max_members_media = kz_json:get_ne_binary_value(<<"max_members_media">>, JObj, max_members_media(Conference))
                               ,require_moderator = kz_json:is_true(<<"require_moderator">>, JObj, require_moderator(Conference))
                               ,wait_for_moderator = kz_json:is_true(<<"wait_for_moderator">>, JObj, wait_for_moderator(Conference))
                               ,conference_doc = JObj
                               ,moderator_controls = kz_json:get_ne_binary_value(<<"moderator_controls">>, JObj, moderator_controls(Conference))
                               ,caller_controls = kz_json:get_ne_binary_value(<<"caller_controls">>, JObj, caller_controls(Conference))
                               }.

-type updater_1() :: fun((conference()) -> conference()).
-type updater_2() :: {fun((_, conference()) -> conference()), _}.
-type updaters() :: [updater_1() | updater_2(),...].
-spec update(updaters(), conference()) -> conference().
update(Updaters, Conference) ->
    lists:foldl(fun update_fold/2, Conference, Updaters).

-spec update_fold(updater_1() | updater_2(), conference()) -> conference().
update_fold({Fun, Value}, Conference) when is_function(Fun, 2) ->
    Fun(Value, Conference);
update_fold(Fun, Conference) when is_function(Fun, 1) ->
    Fun(Conference).

-spec id(conference()) -> kz_term:api_binary().
id(#kapps_conference{id=Id}) -> Id.

-spec set_id(kz_term:api_binary(), conference()) -> conference().
set_id(Id, Conference) when is_binary(Id); Id =:= 'undefined' ->
    Conference#kapps_conference{id=Id}.

-spec set_name(kz_term:ne_binary(), conference()) -> conference().
set_name(Name, Conference) when is_binary(Name) ->
    Conference#kapps_conference{name=Name}.

-spec name(conference()) -> kz_term:ne_binary().
name(#kapps_conference{name=Name}) ->
    Name.

-spec set_account_id(kz_term:ne_binary(), conference()) -> conference().
set_account_id(AccountId, Conference) when is_binary(AccountId) ->
    Conference#kapps_conference{account_id=AccountId}.

-spec account_id(conference()) -> kz_term:ne_binary().
account_id(#kapps_conference{account_id=AccountId}) ->
    AccountId.

-spec set_moderator_controls(kz_term:ne_binary(), conference()) -> conference().
set_moderator_controls(ModeratorCtrls, Conference) when is_binary(ModeratorCtrls) ->
    Conference#kapps_conference{moderator_controls=ModeratorCtrls}.

-spec moderator_controls(conference()) -> kz_term:ne_binary().
moderator_controls(#kapps_conference{moderator_controls=ModeratorCtrls}) ->
    ModeratorCtrls.

-spec set_caller_controls(kz_term:ne_binary(), conference()) -> conference().
set_caller_controls(CallerCtrls, Conference) when is_binary(CallerCtrls) ->
    Conference#kapps_conference{caller_controls=CallerCtrls}.

-spec caller_controls(conference()) -> kz_term:ne_binary().
caller_controls(#kapps_conference{caller_controls=CallerCtrls}) ->
    CallerCtrls.

-spec profile(conference()) -> kz_term:api_binary().
profile(#kapps_conference{profile='undefined'}=Conference) ->
    id(Conference);
profile(#kapps_conference{profile=P}) -> P.

-spec set_profile(kz_term:api_binary(), conference()) -> conference().
set_profile(P, Conference) when is_binary(P); P =:= 'undefined' ->
    Conference#kapps_conference{profile=P}.

-spec application_name(conference()) -> kz_term:ne_binary().
application_name(#kapps_conference{app_name=AppName}) ->
    AppName.

-spec set_application_name(kz_term:ne_binary(), conference()) -> conference().
set_application_name(AppName, #kapps_conference{}=Conference) when is_binary(AppName) ->
    Conference#kapps_conference{app_name=AppName}.

-spec application_version(conference()) -> kz_term:ne_binary().
application_version(#kapps_conference{app_version=AppVersion}) ->
    AppVersion.

-spec set_application_version(kz_term:ne_binary(), conference()) -> conference().
set_application_version(AppVersion, #kapps_conference{}=Conference) when is_binary(AppVersion) ->
    Conference#kapps_conference{app_version=AppVersion}.

-spec focus(conference()) -> kz_term:api_binary().
focus(#kapps_conference{focus=Focus}) ->
    Focus.

-spec set_focus(kz_term:ne_binary(), conference()) -> conference().
set_focus(Focus, Conference) when is_binary(Focus) ->
    Conference#kapps_conference{focus=Focus}.

-spec controller_queue(conference()) -> kz_term:api_binary().
controller_queue(#kapps_conference{controller_q=ControllerQ}) ->
    ControllerQ.

-spec set_controller_queue(kz_term:ne_binary(), conference()) -> conference().
set_controller_queue(ControllerQ, Conference) when is_binary(ControllerQ) ->
    Conference#kapps_conference{controller_q=ControllerQ}.

-spec bridge_username(conference()) -> kz_term:ne_binary().
bridge_username(#kapps_conference{bridge_username=BridgeUsername}) ->
    BridgeUsername.

-spec set_bridge_username(kz_term:ne_binary(), conference()) -> conference().
set_bridge_username(BridgeUsername, Conference) when is_binary(BridgeUsername) ->
    Conference#kapps_conference{bridge_username=BridgeUsername}.

-spec bridge_password(conference()) -> kz_term:ne_binary().
bridge_password(#kapps_conference{bridge_password=BridgePassword}) ->
    BridgePassword.

-spec set_bridge_password(kz_term:ne_binary(), conference()) -> conference().
set_bridge_password(BridgePassword, Conference) when is_binary(BridgePassword) ->
    Conference#kapps_conference{bridge_password=BridgePassword}.

-spec member_pins(conference()) -> kz_term:ne_binaries().
member_pins(#kapps_conference{member_pins=MemberPins}) ->
    MemberPins.

-spec set_member_pins([kz_term:ne_binary()], conference()) -> conference().
set_member_pins(MemberPins, Conference) when is_list(MemberPins) ->
    Conference#kapps_conference{member_pins=MemberPins}.

-spec moderator_pins(conference()) -> [kz_term:ne_binary()].
moderator_pins(#kapps_conference{moderator_pins=ModeratorPins}) ->
    ModeratorPins.

-spec set_moderator_pins([kz_term:ne_binary()], conference()) -> conference().
set_moderator_pins(ModeratorPins, Conference) when is_list(ModeratorPins) ->
    Conference#kapps_conference{moderator_pins=ModeratorPins}.

-spec moderator(conference()) -> kz_term:api_boolean().
moderator(#kapps_conference{moderator=Moderator}) ->
    Moderator.

-spec set_moderator(kz_term:api_boolean(), conference()) -> conference().
set_moderator('undefined', Conference) ->
    Conference#kapps_conference{moderator='undefined'};
set_moderator(Moderator, Conference) when is_boolean(Moderator) ->
    Conference#kapps_conference{moderator=Moderator}.

-spec member_join_muted(conference()) -> boolean().
member_join_muted(#kapps_conference{member_join_muted=MemberJoinMuted}) ->
    MemberJoinMuted.

-spec set_member_join_muted(boolean(), conference()) -> conference().
set_member_join_muted(MemberJoinMuted, Conference) when is_boolean(MemberJoinMuted) ->
    Conference#kapps_conference{member_join_muted=MemberJoinMuted}.

-spec member_join_deaf(conference()) -> boolean().
member_join_deaf(#kapps_conference{member_join_deaf=MemberJoinDeaf}) ->
    MemberJoinDeaf.

-spec set_member_join_deaf(boolean(), conference()) -> conference().
set_member_join_deaf(MemberJoinDeaf, Conference) when is_boolean(MemberJoinDeaf) ->
    Conference#kapps_conference{member_join_deaf=MemberJoinDeaf}.

-spec moderator_join_muted(conference()) -> boolean().
moderator_join_muted(#kapps_conference{moderator_join_muted=ModeratorJoinMuted}) ->
    ModeratorJoinMuted.

-spec set_moderator_join_muted(boolean(), conference()) -> conference().
set_moderator_join_muted(ModeratorJoinMuted, Conference) when is_boolean(ModeratorJoinMuted) ->
    Conference#kapps_conference{moderator_join_muted=ModeratorJoinMuted}.

-spec moderator_join_deaf(conference()) -> boolean().
moderator_join_deaf(#kapps_conference{moderator_join_deaf=ModeratorJoinDeaf}) ->
    ModeratorJoinDeaf.

-spec set_moderator_join_deaf(boolean(), conference()) -> conference().
set_moderator_join_deaf(ModeratorJoinDeaf, Conference) when is_boolean(ModeratorJoinDeaf) ->
    Conference#kapps_conference{moderator_join_deaf=ModeratorJoinDeaf}.

-spec max_participants(conference()) -> pos_integer().
max_participants(#kapps_conference{max_participants=MaxParticipants}) ->
    MaxParticipants.

-spec set_max_participants(integer(), conference()) -> conference().
set_max_participants(MaxParticipants, Conference) when is_integer(MaxParticipants) ->
    Conference#kapps_conference{max_participants=MaxParticipants}.

-spec max_members_media(conference()) -> kz_term:api_binary().
max_members_media(#kapps_conference{max_members_media = Value}) -> Value.

-spec set_max_members_media(kz_term:ne_binary(), conference()) -> conference().
set_max_members_media(Value, Conference) when is_binary(Value) ->
    Conference#kapps_conference{max_members_media = Value}.

-spec require_moderator(conference()) -> boolean().
require_moderator(#kapps_conference{require_moderator=RequireModerator}) ->
    RequireModerator.

-spec set_require_moderator(boolean(), conference()) -> conference().
set_require_moderator(RequireModerator, Conference) when is_boolean(RequireModerator) ->
    Conference#kapps_conference{require_moderator=RequireModerator}.

-spec wait_for_moderator(conference()) -> boolean().
wait_for_moderator(#kapps_conference{wait_for_moderator=WaitForModerator}) ->
    WaitForModerator.

-spec set_wait_for_moderator(boolean(), conference()) -> conference().
set_wait_for_moderator(WaitForModerator, Conference) when is_boolean(WaitForModerator) ->
    Conference#kapps_conference{wait_for_moderator=WaitForModerator}.

-spec play_name_on_join(conference()) -> boolean().
play_name_on_join(#kapps_conference{play_name_on_join=PlayNameOnJoin}) ->
    PlayNameOnJoin.
-spec set_play_name_on_join(boolean(), conference()) -> conference().
set_play_name_on_join(PlayNameOnJoin, Conference) when is_boolean(PlayNameOnJoin) ->
    Conference#kapps_conference{play_name_on_join=PlayNameOnJoin}.

-spec play_entry_prompt(conference()) -> boolean().
play_entry_prompt(#kapps_conference{play_entry_prompt=ShouldPlay}) ->
    ShouldPlay.
-spec set_play_entry_prompt(boolean(), conference()) -> conference().
set_play_entry_prompt(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#kapps_conference{play_entry_prompt=ShouldPlay}.

-spec play_exit_tone(conference()) -> tone().
play_exit_tone(#kapps_conference{play_exit_tone=ShouldPlay}) -> ShouldPlay.
-spec set_play_exit_tone(tone(), conference()) -> conference().
set_play_exit_tone(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#kapps_conference{play_exit_tone=ShouldPlay};
set_play_exit_tone(Media, Conference) when is_binary(Media) ->
    Conference#kapps_conference{play_exit_tone = get_tone(Media)}.

-spec play_entry_tone(conference()) -> tone().
play_entry_tone(#kapps_conference{play_entry_tone=ShouldPlay}) -> ShouldPlay.
-spec set_play_entry_tone(tone(), conference()) -> conference().
set_play_entry_tone(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#kapps_conference{play_entry_tone=ShouldPlay};
set_play_entry_tone(Media, Conference) when is_binary(Media) ->
    Conference#kapps_conference{play_entry_tone = get_tone(Media)}.

-spec play_welcome(conference()) -> boolean().
play_welcome(#kapps_conference{play_welcome=ShouldPlay}) ->
    ShouldPlay.
-spec set_play_welcome(boolean(), conference()) -> conference().
set_play_welcome(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#kapps_conference{play_welcome=ShouldPlay}.

-spec conference_doc(conference()) -> kz_term:api_object().
conference_doc(#kapps_conference{conference_doc=JObj}) -> JObj.

-spec set_conference_doc(kz_json:object(), conference()) -> conference().
set_conference_doc(JObj, Conference) ->
    Conference#kapps_conference{conference_doc=JObj}.

-spec kvs_append(any(), any(), conference()) -> conference().
kvs_append(Key, Value, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:append(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_append_list(any(), [any(),...], conference()) -> conference().
kvs_append_list(Key, ValList, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:append_list(kz_term:to_binary(Key), ValList, Dict)}.

-spec kvs_erase(any(), conference()) -> conference().
kvs_erase(Key, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:erase(kz_term:to_binary(Key), Dict)}.

-spec kvs_fetch(any(), conference()) -> any().
kvs_fetch(Key, #kapps_conference{kvs=Dict}) ->
    try orddict:fetch(kz_term:to_binary(Key), Dict)
    catch
        'error':'function_clause' -> 'undefined'
    end.

-spec kvs_fetch_keys(conference()) -> list().
kvs_fetch_keys( #kapps_conference{kvs=Dict}) ->
    orddict:fetch_keys(Dict).

-spec kvs_filter(fun((any(), any()) -> boolean()), conference()) -> conference().
kvs_filter(Pred, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find(any(), conference()) -> {'ok', any()} | 'error'.
kvs_find(Key, #kapps_conference{kvs=Dict}) ->
    orddict:find(kz_term:to_binary(Key), Dict).

-spec kvs_fold(fun((any(), any(), any()) -> any()), any(), conference()) -> conference().
kvs_fold(Fun, Acc0, #kapps_conference{kvs=Dict}) ->
    orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist(kz_term:proplist(), conference()) -> conference().
kvs_from_proplist(List, #kapps_conference{kvs=Dict}=Conference) ->
    L = orddict:from_list([{kz_term:to_binary(K), V} || {K, V} <- List]),
    Conference#kapps_conference{kvs=orddict:merge(fun(_, V, _) -> V end, L, Dict)}.

-spec kvs_is_key(any(), conference()) -> boolean().
kvs_is_key(Key, #kapps_conference{kvs=Dict}) ->
    orddict:is_key(kz_term:to_binary(Key), Dict).

-spec kvs_map(fun((any(), any()) -> any()), conference()) -> conference().
kvs_map(Pred, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store(any(), any(), conference()) -> conference().
kvs_store(Key, Value, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:store(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_store_proplist(kz_term:proplist(), conference()) -> conference().
kvs_store_proplist(List, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=lists:foldr(fun({K, V}, D) ->
                                             orddict:store(kz_term:to_binary(K), V, D)
                                     end, Dict, List)}.

-spec kvs_to_proplist(conference()) -> kz_term:proplist().
kvs_to_proplist(#kapps_conference{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update(any(), fun((any()) -> any()), conference()) -> conference().
kvs_update(Key, Fun, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:update(kz_term:to_binary(Key), Fun, Dict)}.

-spec kvs_update(any(), fun((any()) -> any()), any(), conference()) -> conference().
kvs_update(Key, Fun, Initial, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:update(kz_term:to_binary(Key), Fun, Initial, Dict)}.

-spec kvs_update_counter(any(), number(), conference()) -> conference().
kvs_update_counter(Key, Number, #kapps_conference{kvs=Dict}=Conference) ->
    Conference#kapps_conference{kvs=orddict:update_counter(kz_term:to_binary(Key), Number, Dict)}.

-spec flush() -> 'ok'.
flush() -> kz_cache:flush_local(?KAPPS_CALL_CACHE).


-spec cache(conference()) -> 'ok'.
cache(#kapps_conference{}=Conference) ->
    cache(Conference, 300 * ?MILLISECONDS_IN_SECOND).

-spec cache(conference(), pos_integer()) -> 'ok'.
cache(#kapps_conference{id=ConferenceId}=Conference, Expires) ->
    CacheProps = [{'expires', Expires}],
    kz_cache:store_local(?KAPPS_CALL_CACHE, {?MODULE, 'conference', ConferenceId}, Conference, CacheProps).

-spec retrieve(kz_term:ne_binary()) -> {'ok', conference()} |
                               {'error', 'not_found'}.
retrieve(ConferenceId) ->
    kz_cache:fetch_local(?KAPPS_CALL_CACHE, {?MODULE, 'conference', ConferenceId}).

-spec call(conference()) -> kapps_call:call() | 'undefined'.
call(#kapps_conference{call=Call}) -> Call.

-spec set_call(kapps_call:call(), conference()) -> conference().
set_call(Call, Conference) ->
    Conference#kapps_conference{call=Call}.


%% @private
-spec get_tone(any()) -> tone().
get_tone(Thing) ->
    case kz_term:is_boolean(Thing) of
        'true' -> kz_term:is_true(Thing);
        'false' -> case is_binary(Thing) of
                       'true' -> Thing;
                       'false' -> 'true'
                   end
    end.
