%%%============================================================================
%%% @copyright (C) 2012 VoIP Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%============================================================================
-module(whapps_conference).

-include("whapps_call_command.hrl").

-export([new/0]).
-export([from_conference_doc/1, from_conference_doc/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([to_proplist/1]).
-export([is_conference/1]).
-export([update/2]).

-export([id/1, set_id/2]).
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
-export([require_moderator/1, set_require_moderator/2]).
-export([wait_for_moderator/1, set_wait_for_moderator/2]).
-export([play_name_on_join/1, set_play_name_on_join/2]).
-export([play_entry_prompt/1, set_play_entry_prompt/2]).
-export([play_entry_tone/1, set_play_entry_tone/2]).
-export([play_welcome/1, set_play_welcome/2]).
-export([conference_doc/1, set_conference_doc/2]).

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

-define(BRIDGE_PWD, <<"\/\/|-|157L3_(0|\|Ph3R3|\|(3">>).

-record(whapps_conference, {
          id :: api_binary()                           %% the conference id
         ,focus :: api_binary()                        %% the conference focus
         ,profile = <<"default">> :: api_binary()      %% conference profile (config settings)
         ,controller_q :: api_binary()                 %% the controller queue, for responses
         ,bridge_username = <<"test">> :: ne_binary()  %% the username used for a conference bridge
         ,bridge_password = ?BRIDGE_PWD :: ne_binary() %% the password used for a conference bridge
         ,member_pins = [] :: ne_binaries()            %% a list of pins for use by members
         ,moderator_pins = [] :: ne_binaries()         %% a list of pins for use by the moderators
         ,moderator :: boolean()                       %% tri-state true/false if the caller is known to be a moderator, otherwise 'undefined'
         ,member_join_muted = 'false' :: boolean()     %% should the member join muted
         ,member_join_deaf = 'false' :: boolean()      %% should the member join deaf
         ,moderator_join_muted = 'false' :: boolean()  %% should the moderator join muted
         ,moderator_join_deaf = 'false' :: boolean()   %% should the moderator join deaf
         ,max_participants = 0 :: non_neg_integer()    %% max number of participants
         ,require_moderator = 'false' :: boolean()     %% does the conference require a moderator
         ,wait_for_moderator = 'false' :: boolean()    %% can members wait for a moderator
         ,play_name_on_join = 'false' :: boolean()     %% should participants have their name played on join
         ,play_entry_prompt = 'true' :: boolean()      %% Play prompt telling caller they're entering the conference
         ,play_entry_tone = 'true' :: boolean()        %% Play tone telling caller they've entered the conference
         ,play_welcome = 'true' :: boolean()           %% Play prompt welcoming caller to the conference
         ,conference_doc :: wh_json:object()           %% the complete conference doc used to create the record (when and if)
         ,app_name = <<"whapps_conference">> :: ne_binary() %% The application name used during whapps_conference_command
         ,app_version = <<"1.0.0">> :: ne_binary()     %% The application version used during whapps_conference_command
         ,kvs = orddict:new() :: orddict:orddict()     %% allows conferences to set values that propogate to children
         }).

-opaque conference() :: #whapps_conference{}.
-export_type([conference/0]).

-spec new() -> conference().
new() -> #whapps_conference{}.

-spec from_json(wh_json:object()) -> conference().
from_json(JObj) -> from_json(JObj, #whapps_conference{}).

-spec from_json(wh_json:object(), conference()) -> conference().
from_json(JObj, Conference) ->
    KVS = orddict:from_list(wh_json:to_proplist(wh_json:get_value(<<"Key-Value-Store">>, JObj, wh_json:new()))),
    Conference#whapps_conference{
      id = wh_json:get_ne_value(<<"Conference-ID">>, JObj, id(Conference))
      ,profile = wh_json:get_ne_value(<<"Profile">>, JObj, profile(Conference))
      ,focus = wh_json:get_ne_value(<<"Conference-Focus">>, JObj, focus(Conference))
      ,controller_q = wh_json:get_ne_value(<<"Controller-Queue">>, JObj, controller_queue(Conference))
      ,bridge_username = wh_json:get_ne_value(<<"Bridge-Username">>, JObj, bridge_username(Conference))
      ,bridge_password = wh_json:get_ne_value(<<"Bridge-Password">>, JObj, bridge_password(Conference))
      ,member_pins = wh_json:get_ne_value(<<"Member-Pins">>, JObj, member_pins(Conference))
      ,moderator_pins = wh_json:get_ne_value(<<"Moderator-Pins">>, JObj, moderator_pins(Conference))
      ,moderator = wh_json:get_value(<<"Moderator">>, JObj, moderator(Conference))
      ,member_join_muted = wh_json:is_true(<<"Member-Join-Muted">>, JObj, member_join_muted(Conference))
      ,member_join_deaf = wh_json:is_true(<<"Member-Join-Deaf">>, JObj, member_join_deaf(Conference))
      ,moderator_join_muted = wh_json:is_true(<<"Moderator-Join-Muted">>, JObj, moderator_join_muted(Conference))
      ,moderator_join_deaf = wh_json:is_true(<<"Moderator-Join-Deaf">>, JObj, moderator_join_deaf(Conference))
      ,max_participants = wh_json:get_integer_value(<<"Max-Participants">>, JObj, max_participants(Conference))
      ,require_moderator = wh_json:is_true(<<"Require-Moderator">>, JObj, require_moderator(Conference))
      ,wait_for_moderator = wh_json:is_true(<<"Wait-For-Moderator">>, JObj, wait_for_moderator(Conference))
      ,play_name_on_join = wh_json:is_true(<<"Play-Name-On-Join">>, JObj, play_name_on_join(Conference))
      ,play_entry_prompt = wh_json:is_true(<<"Play-Entry-Prompt">>, JObj, play_entry_prompt(Conference))
      ,play_entry_tone = wh_json:is_true(<<"Play-Entry-Tone">>, JObj, play_entry_tone(Conference))
      ,play_welcome = wh_json:is_true(<<"Play-Welcome">>, JObj, play_welcome(Conference))
      ,conference_doc = wh_json:is_true(<<"Conference-Doc">>, JObj, conference_doc(Conference))
      ,kvs = orddict:merge(fun(_, _, V2) -> V2 end, Conference#whapps_conference.kvs, KVS)
     }.

-spec to_json(conference()) -> wh_json:object().
to_json(#whapps_conference{}=Conference) ->
    Props = to_proplist(Conference),
    KVS = [KV
           || {_, V}=KV <- props:get_value(<<"Key-Value-Store">>, Props, []),
              V =/= 'undefined',
              wh_json:is_json_term(V)
          ],
    wh_json:from_list([KV
                       || {_, V}=KV <- [{<<"Key-Value-Store">>, wh_json:from_list(KVS)}
                                        | props:delete(<<"Key-Value-Store">>, Props)
                                       ],
                          V =/= 'undefined',
                          wh_json:is_json_term(V)
                      ]).

-spec to_proplist(conference()) -> wh_proplist().
to_proplist(#whapps_conference{}=Conference) ->
    [{<<"Conference-ID">>, id(Conference)}
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
     ,{<<"Require-Moderator">>, require_moderator(Conference)}
     ,{<<"Wait-For-Moderator">>, wait_for_moderator(Conference)}
     ,{<<"Play-Name-On-Join">>, play_name_on_join(Conference)}
     ,{<<"Play-Entry-Prompt">>, play_entry_prompt(Conference)}
     ,{<<"Play-Entry-Tone">>, play_entry_tone(Conference)}
     ,{<<"Play-Welcome">>, play_welcome(Conference)}
     ,{<<"Conference-Doc">>, conference_doc(Conference)}
     ,{<<"Key-Value-Store">>, kvs_to_proplist(Conference)}
    ].

-spec is_conference(term()) -> boolean().
is_conference(#whapps_conference{}) -> 'true';
is_conference(_) -> 'false'.

-spec from_conference_doc(wh_json:object()) -> conference().
-spec from_conference_doc(wh_json:object(), conference()) -> conference().

from_conference_doc(JObj) ->
    from_conference_doc(JObj, #whapps_conference{}).

from_conference_doc(JObj, Conference) ->
    Member = wh_json:get_value(<<"member">>, JObj),
    Moderator = wh_json:get_value(<<"moderator">>, JObj),
    Conference#whapps_conference{
      id = wh_json:get_ne_value(<<"_id">>, JObj, id(Conference))
      ,profile = wh_json:get_ne_value(<<"profile">>, JObj, profile(Conference))
      ,focus = wh_json:get_ne_value(<<"focus">>, JObj, focus(Conference))
      ,bridge_username = wh_json:get_ne_value(<<"bridge_username">>, JObj, bridge_username(Conference))
      ,bridge_password = wh_json:get_ne_value(<<"bridge_password">>, JObj, bridge_password(Conference))
      ,member_pins = wh_json:get_ne_value(<<"pins">>, Member, member_pins(Conference))
      ,moderator_pins = wh_json:get_ne_value(<<"pins">>, Moderator, moderator_pins(Conference))
      ,member_join_muted = wh_json:is_true(<<"join_muted">>, Member, member_join_muted(Conference))
      ,member_join_deaf = wh_json:is_true(<<"join_deaf">>, Member, member_join_deaf(Conference))
      ,play_name_on_join = wh_json:is_true(<<"play_name">>, Member, play_name_on_join(Conference))
      ,play_entry_prompt = wh_json:is_true(<<"play_entry_prompt">>, Member, play_entry_prompt(Conference))
      ,play_entry_tone = wh_json:is_true(<<"play_entry_tone">>, JObj, play_entry_tone(Conference))
      ,play_welcome = wh_json:is_true(<<"play_welcome">>, JObj, play_welcome(Conference))
      ,moderator_join_muted = wh_json:is_true(<<"join_muted">>, Moderator, moderator_join_muted(Conference))
      ,moderator_join_deaf = wh_json:is_true(<<"join_deaf">>, Moderator, moderator_join_deaf(Conference))
      ,max_participants = wh_json:get_integer_value(<<"max_participants">>, JObj, max_participants(Conference))
      ,require_moderator = wh_json:is_true(<<"require_moderator">>, JObj, require_moderator(Conference))
      ,wait_for_moderator = wh_json:is_true(<<"wait_for_moderator">>, JObj, wait_for_moderator(Conference))
      ,conference_doc = JObj
     }.

-spec update([fun(),...], conference()) -> conference().
update(Updaters, Conference) ->
    lists:foldr(fun(F, C) -> F(C) end, Conference, Updaters).

-spec id(conference()) -> api_binary().
id(#whapps_conference{id=Id}) -> Id.

-spec set_id(api_binary(), conference()) -> conference().
set_id(Id, Conference) when is_binary(Id); Id =:= 'undefined' ->
    Conference#whapps_conference{id=Id}.

-spec profile(conference()) -> api_binary().
profile(#whapps_conference{profile=P}) -> P.

-spec set_profile(api_binary(), conference()) -> conference().
set_profile(P, Conference) when is_binary(P); P =:= 'undefined' ->
    Conference#whapps_conference{profile=P}.

-spec application_name(conference()) -> ne_binary().
application_name(#whapps_conference{app_name=AppName}) ->
    AppName.

-spec set_application_name(ne_binary(), conference()) -> conference().
set_application_name(AppName, #whapps_conference{}=Conference) when is_binary(AppName) ->
    Conference#whapps_conference{app_name=AppName}.

-spec application_version(conference()) -> ne_binary().
application_version(#whapps_conference{app_version=AppVersion}) ->
    AppVersion.

-spec set_application_version(ne_binary(), conference()) -> conference().
set_application_version(AppVersion, #whapps_conference{}=Conference) when is_binary(AppVersion) ->
    Conference#whapps_conference{app_version=AppVersion}.

-spec focus(conference()) -> api_binary().
focus(#whapps_conference{focus=Focus}) ->
    Focus.

-spec set_focus(ne_binary(), conference()) -> conference().
set_focus(Focus, Conference) when is_binary(Focus) ->
    Conference#whapps_conference{focus=Focus}.

-spec controller_queue(conference()) -> api_binary().
controller_queue(#whapps_conference{controller_q=ControllerQ}) ->
    ControllerQ.

-spec set_controller_queue(ne_binary(), conference()) -> conference().
set_controller_queue(ControllerQ, Conference) when is_binary(ControllerQ) ->
    Conference#whapps_conference{controller_q=ControllerQ}.

-spec bridge_username(conference()) -> ne_binary().
bridge_username(#whapps_conference{bridge_username=BridgeUsername}) ->
    BridgeUsername.

-spec set_bridge_username(ne_binary(), conference()) -> conference().
set_bridge_username(BridgeUsername, Conference) when is_binary(BridgeUsername) ->
    Conference#whapps_conference{bridge_username=BridgeUsername}.

-spec bridge_password(conference()) -> ne_binary().
bridge_password(#whapps_conference{bridge_password=BridgePassword}) ->
    BridgePassword.

-spec set_bridge_password(ne_binary(), conference()) -> conference().
set_bridge_password(BridgePassword, Conference) when is_binary(BridgePassword) ->
    Conference#whapps_conference{bridge_password=BridgePassword}.

-spec member_pins(conference()) -> [ne_binary(),...] | [].
member_pins(#whapps_conference{member_pins=MemberPins}) ->
    MemberPins.

-spec set_member_pins([ne_binary(),...] | [], conference()) -> conference().
set_member_pins(MemberPins, Conference) when is_list(MemberPins) ->
    Conference#whapps_conference{member_pins=MemberPins}.

-spec moderator_pins(conference()) -> [ne_binary(),...] | [].
moderator_pins(#whapps_conference{moderator_pins=ModeratorPins}) ->
    ModeratorPins.

-spec set_moderator_pins([ne_binary(),...] | [], conference()) -> conference().
set_moderator_pins(ModeratorPins, Conference) when is_list(ModeratorPins) ->
    Conference#whapps_conference{moderator_pins=ModeratorPins}.

-spec moderator(conference()) -> 'undefined' | boolean().
moderator(#whapps_conference{moderator=Moderator}) ->
    Moderator.

-spec set_moderator('undefined' | boolean(), conference()) -> conference().
set_moderator(undefined, Conference) ->
    Conference#whapps_conference{moderator=undefined};
set_moderator(Moderator, Conference) when is_boolean(Moderator) ->
    Conference#whapps_conference{moderator=Moderator}.

-spec member_join_muted(conference()) -> boolean().
member_join_muted(#whapps_conference{member_join_muted=MemberJoinMuted}) ->
    MemberJoinMuted.

-spec set_member_join_muted(boolean(), conference()) -> conference().
set_member_join_muted(MemberJoinMuted, Conference) when is_boolean(MemberJoinMuted) ->
    Conference#whapps_conference{member_join_muted=MemberJoinMuted}.

-spec member_join_deaf(conference()) -> boolean().
member_join_deaf(#whapps_conference{member_join_deaf=MemberJoinDeaf}) ->
    MemberJoinDeaf.

-spec set_member_join_deaf(boolean(), conference()) -> conference().
set_member_join_deaf(MemberJoinDeaf, Conference) when is_boolean(MemberJoinDeaf) ->
    Conference#whapps_conference{member_join_deaf=MemberJoinDeaf}.

-spec moderator_join_muted(conference()) -> boolean().
moderator_join_muted(#whapps_conference{moderator_join_muted=ModeratorJoinMuted}) ->
    ModeratorJoinMuted.

-spec set_moderator_join_muted(boolean(), conference()) -> conference().
set_moderator_join_muted(ModeratorJoinMuted, Conference) when is_boolean(ModeratorJoinMuted) ->
    Conference#whapps_conference{moderator_join_muted=ModeratorJoinMuted}.

-spec moderator_join_deaf(conference()) -> boolean().
moderator_join_deaf(#whapps_conference{moderator_join_deaf=ModeratorJoinDeaf}) ->
    ModeratorJoinDeaf.

-spec set_moderator_join_deaf(boolean(), conference()) -> conference().
set_moderator_join_deaf(ModeratorJoinDeaf, Conference) when is_boolean(ModeratorJoinDeaf) ->
    Conference#whapps_conference{moderator_join_deaf=ModeratorJoinDeaf}.

-spec max_participants(conference()) -> pos_integer().
max_participants(#whapps_conference{max_participants=MaxParticipants}) ->
    MaxParticipants.

-spec set_max_participants(integer(), conference()) -> conference().
set_max_participants(MaxParticipants, Conference) when is_integer(MaxParticipants) ->
    Conference#whapps_conference{max_participants=MaxParticipants}.

-spec require_moderator(conference()) -> boolean().
require_moderator(#whapps_conference{require_moderator=RequireModerator}) ->
    RequireModerator.

-spec set_require_moderator(boolean(), conference()) -> conference().
set_require_moderator(RequireModerator, Conference) when is_boolean(RequireModerator) ->
    Conference#whapps_conference{require_moderator=RequireModerator}.

-spec wait_for_moderator(conference()) -> boolean().
wait_for_moderator(#whapps_conference{wait_for_moderator=WaitForModerator}) ->
    WaitForModerator.

-spec set_wait_for_moderator(boolean(), conference()) -> conference().
set_wait_for_moderator(WaitForModerator, Conference) when is_boolean(WaitForModerator) ->
    Conference#whapps_conference{wait_for_moderator=WaitForModerator}.

-spec play_name_on_join(conference()) -> boolean().
play_name_on_join(#whapps_conference{play_name_on_join=PlayNameOnJoin}) ->
    PlayNameOnJoin.
-spec set_play_name_on_join(boolean(), conference()) -> conference().
set_play_name_on_join(PlayNameOnJoin, Conference) when is_boolean(PlayNameOnJoin) ->
    Conference#whapps_conference{play_name_on_join=PlayNameOnJoin}.

-spec play_entry_prompt(conference()) -> boolean().
play_entry_prompt(#whapps_conference{play_entry_prompt=ShouldPlay}) ->
    ShouldPlay.
-spec set_play_entry_prompt(boolean(), conference()) -> conference().
set_play_entry_prompt(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#whapps_conference{play_entry_prompt=ShouldPlay}.

-spec play_entry_tone(conference()) -> boolean().
play_entry_tone(#whapps_conference{play_entry_tone=ShouldPlay}) ->
    ShouldPlay.
-spec set_play_entry_tone(boolean(), conference()) -> conference().
set_play_entry_tone(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#whapps_conference{play_entry_tone=ShouldPlay}.

-spec play_welcome(conference()) -> boolean().
play_welcome(#whapps_conference{play_welcome=ShouldPlay}) ->
    ShouldPlay.
-spec set_play_welcome(boolean(), conference()) -> conference().
set_play_welcome(ShouldPlay, Conference) when is_boolean(ShouldPlay) ->
    Conference#whapps_conference{play_welcome=ShouldPlay}.

-spec conference_doc(conference()) -> 'undefined' | wh_json:object().
conference_doc(#whapps_conference{conference_doc=JObj}) -> JObj.

-spec set_conference_doc(wh_json:object(), conference()) -> conference().
set_conference_doc(JObj, Conference) ->
    Conference#whapps_conference{conference_doc=JObj}.

-spec kvs_append(term(), term(), conference()) -> conference().
kvs_append(Key, Value, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:append(wh_util:to_binary(Key), Value, Dict)}.

-spec kvs_append_list(term(), [term(),...], conference()) -> conference().
kvs_append_list(Key, ValList, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:append_list(wh_util:to_binary(Key), ValList, Dict)}.

-spec kvs_erase(term(), conference()) -> conference().
kvs_erase(Key, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:erase(wh_util:to_binary(Key), Dict)}.

-spec kvs_fetch(term(), conference()) -> term().
kvs_fetch(Key, #whapps_conference{kvs=Dict}) ->
    try orddict:fetch(wh_util:to_binary(Key), Dict) of
        Ok -> Ok
    catch
        error:function_clause -> 'undefined'
    end.

-spec kvs_fetch_keys(conference()) -> [term(),...] | [].
kvs_fetch_keys( #whapps_conference{kvs=Dict}) ->
    orddict:fetch_keys(Dict).

-spec kvs_filter(fun((term(), term()) -> boolean()), conference()) -> conference().
kvs_filter(Pred, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find(term(), conference()) -> {'ok', term()} | 'error'.
kvs_find(Key, #whapps_conference{kvs=Dict}) ->
    orddict:find(wh_util:to_binary(Key), Dict).

-spec kvs_fold(fun((term(), term(), term()) -> term()), term(), conference()) -> conference().
kvs_fold(Fun, Acc0, #whapps_conference{kvs=Dict}) ->
    orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist(wh_proplist(), conference()) -> conference().
kvs_from_proplist(List, #whapps_conference{kvs=Dict}=Conference) ->
    L = orddict:from_list([{wh_util:to_binary(K), V} || {K, V} <- List]),
    Conference#whapps_conference{kvs=orddict:merge(fun(_, V, _) -> V end, L, Dict)}.

-spec kvs_is_key(term(), conference()) -> boolean().
kvs_is_key(Key, #whapps_conference{kvs=Dict}) ->
    orddict:is_key(wh_util:to_binary(Key), Dict).

-spec kvs_map(fun((term(), term()) -> term()), conference()) -> conference().
kvs_map(Pred, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store(term(), term(), conference()) -> conference().
kvs_store(Key, Value, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:store(wh_util:to_binary(Key), Value, Dict)}.

-spec kvs_store_proplist(wh_proplist(), conference()) -> conference().
kvs_store_proplist(List, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=lists:foldr(fun({K, V}, D) ->
                                             orddict:store(wh_util:to_binary(K), V, D)
                                     end, Dict, List)}.

-spec kvs_to_proplist(conference()) -> wh_proplist().
kvs_to_proplist(#whapps_conference{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update(term(), fun((term()) -> term()), conference()) -> conference().
kvs_update(Key, Fun, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:update(wh_util:to_binary(Key), Fun, Dict)}.

-spec kvs_update(term(), fun((term()) -> term()), term(), conference()) -> conference().
kvs_update(Key, Fun, Initial, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:update(wh_util:to_binary(Key), Fun, Initial, Dict)}.

-spec kvs_update_counter(term(), number(), conference()) -> conference().
kvs_update_counter(Key, Number, #whapps_conference{kvs=Dict}=Conference) ->
    Conference#whapps_conference{kvs=orddict:update_counter(wh_util:to_binary(Key), Number, Dict)}.

-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?WHAPPS_CALL_CACHE).

-spec cache(conference()) -> 'ok'.
-spec cache(conference(), pos_integer()) -> 'ok'.

cache(#whapps_conference{}=Conference) ->
    cache(Conference, 300000).

cache(#whapps_conference{id=ConferenceId}=Conference, Expires) ->
    CacheProps = [{'expires', Expires}],
    wh_cache:store_local(?WHAPPS_CALL_CACHE, {?MODULE, 'conference', ConferenceId}, Conference, CacheProps).

-spec retrieve(ne_binary()) -> {'ok', conference()} |
                               {'error', 'not_found'}.
retrieve(ConferenceId) ->
    wh_cache:fetch_local(?WHAPPS_CALL_CACHE, {?MODULE, 'conference', ConferenceId}).
