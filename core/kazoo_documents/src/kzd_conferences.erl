-module(kzd_conferences).

-export([new/0]).
-export([bridge_password/1, bridge_password/2, set_bridge_password/2]).
-export([bridge_username/1, bridge_username/2, set_bridge_username/2]).
-export([caller_controls/1, caller_controls/2, set_caller_controls/2]).
-export([conference_numbers/1, conference_numbers/2, set_conference_numbers/2]).
-export([focus/1, focus/2, set_focus/2]).
-export([max_members_media/1, max_members_media/2, set_max_members_media/2]).
-export([max_participants/1, max_participants/2, set_max_participants/2]).
-export([member/1, member/2, set_member/2]).
-export([member_join_deaf/1, member_join_deaf/2, set_member_join_deaf/2]).
-export([member_join_muted/1, member_join_muted/2, set_member_join_muted/2]).
-export([member_numbers/1, member_numbers/2, set_member_numbers/2]).
-export([member_pins/1, member_pins/2, set_member_pins/2]).
-export([moderator/1, moderator/2, set_moderator/2]).
-export([moderator_join_deaf/1, moderator_join_deaf/2, set_moderator_join_deaf/2]).
-export([moderator_join_muted/1, moderator_join_muted/2, set_moderator_join_muted/2]).
-export([moderator_numbers/1, moderator_numbers/2, set_moderator_numbers/2]).
-export([moderator_pins/1, moderator_pins/2, set_moderator_pins/2]).
-export([moderator_controls/1, moderator_controls/2, set_moderator_controls/2]).
-export([name/1, name/2, set_name/2]).
-export([owner_id/1, owner_id/2, set_owner_id/2]).
-export([play_entry_tone/1, play_entry_tone/2, set_play_entry_tone/2]).
-export([play_exit_tone/1, play_exit_tone/2, set_play_exit_tone/2]).
-export([play_name/1, play_name/2, set_play_name/2]).
-export([play_welcome/1, play_welcome/2, set_play_welcome/2]).
-export([profile/1, profile/2, set_profile/2]).
-export([require_moderator/1, require_moderator/2, set_require_moderator/2]).
-export([wait_for_moderator/1, wait_for_moderator/2, set_wait_for_moderator/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec bridge_password(doc()) -> kz_term:api_binary().
-spec bridge_password(doc(), Default) -> binary() | Default.
bridge_password(Doc) ->
    bridge_password(Doc, 'undefined').
bridge_password(Doc, Default) ->
    kz_json:get_binary_value([<<"bridge_password">>], Doc, Default).

-spec set_bridge_password(doc(), binary()) -> doc().
set_bridge_password(Doc, BridgePassword) ->
    kz_json:set_value([<<"bridge_password">>], BridgePassword, Doc).

-spec bridge_username(doc()) -> kz_term:api_binary().
-spec bridge_username(doc(), Default) -> binary() | Default.
bridge_username(Doc) ->
    bridge_username(Doc, 'undefined').
bridge_username(Doc, Default) ->
    kz_json:get_binary_value([<<"bridge_username">>], Doc, Default).

-spec set_bridge_username(doc(), binary()) -> doc().
set_bridge_username(Doc, BridgeUsername) ->
    kz_json:set_value([<<"bridge_username">>], BridgeUsername, Doc).

-spec caller_controls(doc()) -> kz_term:api_binary().
-spec caller_controls(doc(), Default) -> binary() | Default.
caller_controls(Doc) ->
    caller_controls(Doc, 'undefined').
caller_controls(Doc, Default) ->
    kz_json:get_binary_value([<<"caller_controls">>], Doc, Default).

-spec set_caller_controls(doc(), binary()) -> doc().
set_caller_controls(Doc, CallerControls) ->
    kz_json:set_value([<<"caller_controls">>], CallerControls, Doc).

-spec conference_numbers(doc()) -> kz_term:ne_binaries().
-spec conference_numbers(doc(), Default) -> kz_term:ne_binaries() | Default.
conference_numbers(Doc) ->
    conference_numbers(Doc, []).
conference_numbers(Doc, Default) ->
    kz_json:get_list_value([<<"conference_numbers">>], Doc, Default).

-spec set_conference_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_conference_numbers(Doc, ConferenceNumbers) ->
    kz_json:set_value([<<"conference_numbers">>], ConferenceNumbers, Doc).

-spec focus(doc()) -> kz_term:api_binary().
-spec focus(doc(), Default) -> binary() | Default.
focus(Doc) ->
    focus(Doc, 'undefined').
focus(Doc, Default) ->
    kz_json:get_binary_value([<<"focus">>], Doc, Default).

-spec set_focus(doc(), binary()) -> doc().
set_focus(Doc, Focus) ->
    kz_json:set_value([<<"focus">>], Focus, Doc).

-spec max_members_media(doc()) -> kz_term:api_binary().
-spec max_members_media(doc(), Default) -> binary() | Default.
max_members_media(Doc) ->
    max_members_media(Doc, 'undefined').
max_members_media(Doc, Default) ->
    kz_json:get_binary_value([<<"max_members_media">>], Doc, Default).

-spec set_max_members_media(doc(), binary()) -> doc().
set_max_members_media(Doc, MaxMembersMedia) ->
    kz_json:set_value([<<"max_members_media">>], MaxMembersMedia, Doc).

-spec max_participants(doc()) -> kz_term:api_integer().
-spec max_participants(doc(), Default) -> integer() | Default.
max_participants(Doc) ->
    max_participants(Doc, 'undefined').
max_participants(Doc, Default) ->
    kz_json:get_integer_value([<<"max_participants">>], Doc, Default).

-spec set_max_participants(doc(), integer()) -> doc().
set_max_participants(Doc, MaxParticipants) ->
    kz_json:set_value([<<"max_participants">>], MaxParticipants, Doc).

-spec member(doc()) -> kz_json:object().
-spec member(doc(), Default) -> kz_json:object() | Default.
member(Doc) ->
    member(Doc, kz_json:new()).
member(Doc, Default) ->
    kz_json:get_json_value([<<"member">>], Doc, Default).

-spec set_member(doc(), kz_json:object()) -> doc().
set_member(Doc, Member) ->
    kz_json:set_value([<<"member">>], Member, Doc).

-spec member_join_deaf(doc()) -> boolean().
-spec member_join_deaf(doc(), Default) -> boolean() | Default.
member_join_deaf(Doc) ->
    member_join_deaf(Doc, false).
member_join_deaf(Doc, Default) ->
    kz_json:get_boolean_value([<<"member">>, <<"join_deaf">>], Doc, Default).

-spec set_member_join_deaf(doc(), boolean()) -> doc().
set_member_join_deaf(Doc, MemberJoinDeaf) ->
    kz_json:set_value([<<"member">>, <<"join_deaf">>], MemberJoinDeaf, Doc).

-spec member_join_muted(doc()) -> boolean().
-spec member_join_muted(doc(), Default) -> boolean() | Default.
member_join_muted(Doc) ->
    member_join_muted(Doc, true).
member_join_muted(Doc, Default) ->
    kz_json:get_boolean_value([<<"member">>, <<"join_muted">>], Doc, Default).

-spec set_member_join_muted(doc(), boolean()) -> doc().
set_member_join_muted(Doc, MemberJoinMuted) ->
    kz_json:set_value([<<"member">>, <<"join_muted">>], MemberJoinMuted, Doc).

-spec member_numbers(doc()) -> kz_term:ne_binaries().
-spec member_numbers(doc(), Default) -> kz_term:ne_binaries() | Default.
member_numbers(Doc) ->
    member_numbers(Doc, []).
member_numbers(Doc, Default) ->
    kz_json:get_list_value([<<"member">>, <<"numbers">>], Doc, Default).

-spec set_member_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_member_numbers(Doc, MemberNumbers) ->
    kz_json:set_value([<<"member">>, <<"numbers">>], MemberNumbers, Doc).

-spec member_pins(doc()) -> kz_term:ne_binaries().
-spec member_pins(doc(), Default) -> kz_term:ne_binaries() | Default.
member_pins(Doc) ->
    member_pins(Doc, []).
member_pins(Doc, Default) ->
    kz_json:get_list_value([<<"member">>, <<"pins">>], Doc, Default).

-spec set_member_pins(doc(), kz_term:ne_binaries()) -> doc().
set_member_pins(Doc, MemberPins) ->
    kz_json:set_value([<<"member">>, <<"pins">>], MemberPins, Doc).

-spec moderator(doc()) -> kz_json:object().
-spec moderator(doc(), Default) -> kz_json:object() | Default.
moderator(Doc) ->
    moderator(Doc, kz_json:new()).
moderator(Doc, Default) ->
    kz_json:get_json_value([<<"moderator">>], Doc, Default).

-spec set_moderator(doc(), kz_json:object()) -> doc().
set_moderator(Doc, Moderator) ->
    kz_json:set_value([<<"moderator">>], Moderator, Doc).

-spec moderator_join_deaf(doc()) -> boolean().
-spec moderator_join_deaf(doc(), Default) -> boolean() | Default.
moderator_join_deaf(Doc) ->
    moderator_join_deaf(Doc, false).
moderator_join_deaf(Doc, Default) ->
    kz_json:get_boolean_value([<<"moderator">>, <<"join_deaf">>], Doc, Default).

-spec set_moderator_join_deaf(doc(), boolean()) -> doc().
set_moderator_join_deaf(Doc, ModeratorJoinDeaf) ->
    kz_json:set_value([<<"moderator">>, <<"join_deaf">>], ModeratorJoinDeaf, Doc).

-spec moderator_join_muted(doc()) -> boolean().
-spec moderator_join_muted(doc(), Default) -> boolean() | Default.
moderator_join_muted(Doc) ->
    moderator_join_muted(Doc, false).
moderator_join_muted(Doc, Default) ->
    kz_json:get_boolean_value([<<"moderator">>, <<"join_muted">>], Doc, Default).

-spec set_moderator_join_muted(doc(), boolean()) -> doc().
set_moderator_join_muted(Doc, ModeratorJoinMuted) ->
    kz_json:set_value([<<"moderator">>, <<"join_muted">>], ModeratorJoinMuted, Doc).

-spec moderator_numbers(doc()) -> kz_term:ne_binaries().
-spec moderator_numbers(doc(), Default) -> kz_term:ne_binaries() | Default.
moderator_numbers(Doc) ->
    moderator_numbers(Doc, []).
moderator_numbers(Doc, Default) ->
    kz_json:get_list_value([<<"moderator">>, <<"numbers">>], Doc, Default).

-spec set_moderator_numbers(doc(), kz_term:ne_binaries()) -> doc().
set_moderator_numbers(Doc, ModeratorNumbers) ->
    kz_json:set_value([<<"moderator">>, <<"numbers">>], ModeratorNumbers, Doc).

-spec moderator_pins(doc()) -> kz_term:ne_binaries().
-spec moderator_pins(doc(), Default) -> kz_term:ne_binaries() | Default.
moderator_pins(Doc) ->
    moderator_pins(Doc, []).
moderator_pins(Doc, Default) ->
    kz_json:get_list_value([<<"moderator">>, <<"pins">>], Doc, Default).

-spec set_moderator_pins(doc(), kz_term:ne_binaries()) -> doc().
set_moderator_pins(Doc, ModeratorPins) ->
    kz_json:set_value([<<"moderator">>, <<"pins">>], ModeratorPins, Doc).

-spec moderator_controls(doc()) -> kz_term:api_binary().
-spec moderator_controls(doc(), Default) -> binary() | Default.
moderator_controls(Doc) ->
    moderator_controls(Doc, 'undefined').
moderator_controls(Doc, Default) ->
    kz_json:get_binary_value([<<"moderator_controls">>], Doc, Default).

-spec set_moderator_controls(doc(), binary()) -> doc().
set_moderator_controls(Doc, ModeratorControls) ->
    kz_json:set_value([<<"moderator_controls">>], ModeratorControls, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec owner_id(doc()) -> kz_term:api_ne_binary().
-spec owner_id(doc(), Default) -> kz_term:ne_binary() | Default.
owner_id(Doc) ->
    owner_id(Doc, 'undefined').
owner_id(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"owner_id">>], Doc, Default).

-spec set_owner_id(doc(), kz_term:ne_binary()) -> doc().
set_owner_id(Doc, OwnerId) ->
    kz_json:set_value([<<"owner_id">>], OwnerId, Doc).

-spec play_entry_tone(doc()) -> any().
-spec play_entry_tone(doc(), Default) -> any() | Default.
play_entry_tone(Doc) ->
    play_entry_tone(Doc, 'undefined').
play_entry_tone(Doc, Default) ->
    kz_json:get_value([<<"play_entry_tone">>], Doc, Default).

-spec set_play_entry_tone(doc(), any()) -> doc().
set_play_entry_tone(Doc, PlayEntryTone) ->
    kz_json:set_value([<<"play_entry_tone">>], PlayEntryTone, Doc).

-spec play_exit_tone(doc()) -> any().
-spec play_exit_tone(doc(), Default) -> any() | Default.
play_exit_tone(Doc) ->
    play_exit_tone(Doc, 'undefined').
play_exit_tone(Doc, Default) ->
    kz_json:get_value([<<"play_exit_tone">>], Doc, Default).

-spec set_play_exit_tone(doc(), any()) -> doc().
set_play_exit_tone(Doc, PlayExitTone) ->
    kz_json:set_value([<<"play_exit_tone">>], PlayExitTone, Doc).

-spec play_name(doc()) -> boolean().
-spec play_name(doc(), Default) -> boolean() | Default.
play_name(Doc) ->
    play_name(Doc, false).
play_name(Doc, Default) ->
    kz_json:get_boolean_value([<<"play_name">>], Doc, Default).

-spec set_play_name(doc(), boolean()) -> doc().
set_play_name(Doc, PlayName) ->
    kz_json:set_value([<<"play_name">>], PlayName, Doc).

-spec play_welcome(doc()) -> kz_term:api_boolean().
-spec play_welcome(doc(), Default) -> boolean() | Default.
play_welcome(Doc) ->
    play_welcome(Doc, 'undefined').
play_welcome(Doc, Default) ->
    kz_json:get_boolean_value([<<"play_welcome">>], Doc, Default).

-spec set_play_welcome(doc(), boolean()) -> doc().
set_play_welcome(Doc, PlayWelcome) ->
    kz_json:set_value([<<"play_welcome">>], PlayWelcome, Doc).

-spec profile(doc()) -> kz_term:api_binary().
-spec profile(doc(), Default) -> binary() | Default.
profile(Doc) ->
    profile(Doc, 'undefined').
profile(Doc, Default) ->
    kz_json:get_binary_value([<<"profile">>], Doc, Default).

-spec set_profile(doc(), binary()) -> doc().
set_profile(Doc, Profile) ->
    kz_json:set_value([<<"profile">>], Profile, Doc).

-spec require_moderator(doc()) -> kz_term:api_boolean().
-spec require_moderator(doc(), Default) -> boolean() | Default.
require_moderator(Doc) ->
    require_moderator(Doc, 'undefined').
require_moderator(Doc, Default) ->
    kz_json:get_boolean_value([<<"require_moderator">>], Doc, Default).

-spec set_require_moderator(doc(), boolean()) -> doc().
set_require_moderator(Doc, RequireModerator) ->
    kz_json:set_value([<<"require_moderator">>], RequireModerator, Doc).

-spec wait_for_moderator(doc()) -> kz_term:api_boolean().
-spec wait_for_moderator(doc(), Default) -> boolean() | Default.
wait_for_moderator(Doc) ->
    wait_for_moderator(Doc, 'undefined').
wait_for_moderator(Doc, Default) ->
    kz_json:get_boolean_value([<<"wait_for_moderator">>], Doc, Default).

-spec set_wait_for_moderator(doc(), boolean()) -> doc().
set_wait_for_moderator(Doc, WaitForModerator) ->
    kz_json:set_value([<<"wait_for_moderator">>], WaitForModerator, Doc).
