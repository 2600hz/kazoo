%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_conference).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [
                          answer/1, b_conference/5, b_play/2, hangup/1,
                          b_play_and_collect_digits/6, wait_for_dtmf/1
                         ]).

-import(cf_conference_command, [
                               b_members/2
                              ]).

-record(prompts, {
           greeting = <<"/system_media/conf-welcome">>
          ,request_pin = <<"/system_media/conf-pin">>
          ,incorrect_pin = <<"/system_media/conf-bad-pin">>
          ,max_pin_tries = <<"shout://translate.google.com/translate_tts?tl=en&q=You+have+reached+the+maximum+number+of+entry+attempts!+Goodbye.">>
          ,alone_enter = <<"/system_media/conf-alone">>
          ,single_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+is+only+one+other+participant.">>
          ,multiple_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+are+~s+other+participants.">>
          ,announce_join = <<"tone_stream://%(200,0,500,600,700)">>
          ,announce_leave = <<"tone_stream://%(500,0,300,200,100,50,25)">>
          ,muted = <<"/system_media/conf-muted">>
          ,unmuted = <<"/system_media/conf-unmuted">>
          ,deaf = <<"shout://translate.google.com/translate_tts?tl=en&q=Silenced.">>
          ,undeaf = <<"shout://translate.google.com/translate_tts?tl=en&q=Audiable.">>
         }).

-record(control, {
           mute = <<>>
          ,unmute = <<>>
          ,deaf = <<>>
          ,undeaf = <<>>
          ,toggle_mute = <<"0">>
          ,toggle_deaf = <<"*">>
          ,hangup = <<"#">>
         }).

-record(conf, {
           id = <<"fe306820-51c4-11e0-b8af-0800200c9a66">>
          ,members = []
          ,member = undefined
          ,member_pins = [<<"1234">>]
          ,moderator_pins = [<<"9876">>]
          ,max_pin_tries = 3
          ,member_join_muted = <<"true">>
          ,member_join_deaf = <<"false">>
          ,moderator_join_muted = <<"false">>
          ,moderator_join_deaf = <<"false">>
          ,max_members = 0
          ,require_moderator = <<"false">>
          ,wait_for_moderator = <<"false">>
          ,prompts = #prompts{}
          ,control = #control{}
         }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> tuple(stop | continue)).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    Conf = update_members(get_conference_profile(Data, Call#cf_call.account_db), Call),
    answer(Call),
    _ = play_conference_name(Conf, Call),
    case check_pin(Conf, Call, 1) of
        member ->
            join_conference(Conf, Call, member),
            caller_controls(Conf, Call),
            announce_leave(Conf, Call),
            CFPid ! {continue};
        moderator ->
            join_conference(Conf, Call, moderator),
            caller_controls(Conf, Call),
            announce_leave(Conf, Call),
            CFPid ! {continue};
        stop ->
            CFPid ! {stop}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Bridges the caller in to the conference with the intial flags
%% defined by this conference profile
%% @end
%%--------------------------------------------------------------------
-spec(join_conference/3 :: (Conf :: #conf{}, Call :: #cf_call{}, Type :: member|moderator) -> ok).
join_conference(Conf, Call, Type) ->
    _ = play_conference_count(Conf, Call),
    _ = case Type of
	    member ->
		b_conference(Conf#conf.id, Conf#conf.member_join_muted, Conf#conf.member_join_deaf, <<"false">>, Call);
	    moderator ->
		b_conference(Conf#conf.id, Conf#conf.moderator_join_muted, Conf#conf.moderator_join_deaf, <<"true">>, Call)
	end,
    announce_join(Conf, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays the conference greeting to the caller
%% @end
%%--------------------------------------------------------------------
-spec(play_conference_name/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
play_conference_name(#conf{prompts=Prompts}, Call) ->
    b_play(Prompts#prompts.greeting, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Request the conference pin number from the caller, and determines
%% if the pin is for a member or moderator.  If the caller fails to
%% provide a valid pin within max_pin_tries the call is hungup
%% @end
%%--------------------------------------------------------------------
-spec(check_pin/3 :: (Conf :: #conf{}, Call :: #cf_call{}, LoopCount :: integer()) -> member | moderator | stop).
check_pin(#conf{prompts=Prompts} = Conf, Call, LoopCount) when LoopCount > Conf#conf.max_pin_tries ->
    ?LOG("maxium number of invalid pin entries"),
    _ = b_play(Prompts#prompts.max_pin_tries, Call),
    stop;
check_pin(#conf{prompts=Prompts} = Conf, Call, LoopCount) ->
    ?LOG("requesting pin number"),
    case b_play_and_collect_digits(<<"4">>, <<"6">>, Prompts#prompts.request_pin, <<"1">>, <<"5000">>, Call) of
        {ok, Pin} ->
            case lists:member(Pin, Conf#conf.member_pins) of
                true ->
                    ?LOG("pin number is valid for a member"),
                    member;
                false ->
                    case lists:member(Pin, Conf#conf.moderator_pins) of
                        true ->
                            ?LOG("pin number is valid for a moderator"),
                            moderator;
                        false ->
                            _ = b_play(Prompts#prompts.incorrect_pin, Call),
                            check_pin(Conf, Call, LoopCount+1)
                    end
            end;
        {error, _} ->
            check_pin(Conf, Call, LoopCount+1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays the current conference member count prompt to the caller
%% @end
%%-------------------------------------------------------------------
-spec(play_conference_count/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> tuple(ok, json_object()) | tuple(error, atom())).
play_conference_count(#conf{prompts=Prompts, members=Members}, Call) ->
    case length(Members) of
        0 ->
            b_play(Prompts#prompts.alone_enter, Call);
        1 ->
            b_play(Prompts#prompts.single_enter, Call);
        Count ->
            Prompt = io_lib:format(whistle_util:to_list(Prompts#prompts.multiple_enter), [Count]),
            b_play(whistle_util:to_binary(Prompt), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays the conference announce_join media to the conference
%% @end
%%--------------------------------------------------------------------
-spec(announce_join/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
announce_join(#conf{prompts=Prompts, id=ConfId}, Call) ->
    ?LOG("caller has joined the conference"),
    cf_conference_command:play(Prompts#prompts.announce_join, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Plays the conference announce_leave media to the conference
%% @end
%%--------------------------------------------------------------------
-spec(announce_leave/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
announce_leave(#conf{prompts=Prompts, id=ConfId}, Call) ->
    ?LOG("caller has left the conference"),
    cf_conference_command:play(Prompts#prompts.announce_leave, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches the conference parameters from the datastore and loads the
%% conference record
%% @end
%%--------------------------------------------------------------------
-spec(get_conference_profile/2 :: (Data :: json_object(), Db :: binary()) -> #conf{}).
get_conference_profile(Data, Db) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded conference ~s", [Id]),
            Default=#conf{},
            #conf{
                 id = Id
                ,member_pins = wh_json:get_value(<<"member_pins">>, JObj, [])
                ,moderator_pins = wh_json:get_value(<<"moderator_pins">>, JObj, [])
                ,member_join_muted = wh_json:get_value(<<"member_join_muted">>, JObj, Default#conf.member_join_muted)
                ,member_join_deaf = wh_json:get_value(<<"member_join_deaf">>, JObj, Default#conf.member_join_deaf)
                ,moderator_join_muted = wh_json:get_value(<<"moderator_join_muted">>, JObj, Default#conf.moderator_join_muted)
                ,moderator_join_deaf = wh_json:get_value(<<"moderator_join_deaf">>, JObj, Default#conf.moderator_join_deaf)
                ,max_members = wh_json:get_value(<<"max_members">>, JObj, Default#conf.max_members)
                ,require_moderator = wh_json:get_value(<<"require_moderator">>, JObj, Default#conf.require_moderator)
                ,wait_for_moderator = wh_json:get_value(<<"wait_for_moderator">>, JObj, Default#conf.wait_for_moderator)
         };
        {error, R} ->
            ?LOG("failed to load conference ~s, ~w", [Id, R]),            
            #conf{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loops waiting for DTMF events and responds appropraitely to provide
%% an individual member with conference controls
%% @end
%%--------------------------------------------------------------------
-spec(caller_controls/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
caller_controls(#conf{control=Control} = Conf, Call) ->
    case wait_for_dtmf(200000) of
        {ok, <<>>} ->
            caller_controls(Conf, Call);
        {ok, Digit} ->
            if
                Digit == Control#control.mute ->
                    mute_caller(Conf, Call);
                Digit == Control#control.unmute ->
                    unmute_caller(Conf, Call);
                Digit == Control#control.deaf ->
                    deaf_caller(Conf, Call);
                Digit == Control#control.undeaf ->
                    undeaf_caller(Conf, Call);
                Digit == Control#control.toggle_mute ->
                    toggle_mute(Conf, Call);
                Digit == Control#control.toggle_deaf ->
                    toggle_deaf(Conf, Call);
                Digit == Control#control.hangup ->
                    cf_call_command:hangup(Call);
                true ->
                    ok
            end,
            caller_controls(Conf, Call);
        {error, channel_hungup} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Mutes or unmutes the conference member belonging to Call,
%% depending on the current 'speak' state
%% @end
%%--------------------------------------------------------------------
-spec(toggle_mute/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
toggle_mute(Conf, Call) ->
    C1 = update_members(Conf, Call),
    case binary:match(wh_json:get_value(<<"Status">>, C1#conf.member), <<"speak">>) of
        nomatch ->
            unmute_caller(C1, Call);
        _ ->
            mute_caller(C1, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Deaf or undeaf the conference member belonging to Call, depending
%% on the current 'hear' state
%% @end
%%--------------------------------------------------------------------
-spec(toggle_deaf/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
toggle_deaf(Conf, Call) ->
    C1 = update_members(Conf, Call),
    case binary:match(wh_json:get_value(<<"Status">>, C1#conf.member), <<"hear">>) of
        nomatch ->
            undeaf_caller(C1, Call);
        _ ->
            deaf_caller(C1, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Mutes the conference member belonging to Call
%% @end
%%--------------------------------------------------------------------
-spec(mute_caller/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
mute_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = wh_json:get_value(<<"Member-ID">>, Member),
    ?LOG("mute member ~s", [MemberId]),
    cf_conference_command:mute(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.muted, MemberId, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unmutes the conference member belonging to Call
%% @end
%%--------------------------------------------------------------------
-spec(unmute_caller/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
unmute_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = wh_json:get_value(<<"Member-ID">>, Member),
    ?LOG("unmute member ~s", [MemberId]),
    cf_conference_command:unmute(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.unmuted, MemberId, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes the conference member belonging to Call deaf
%% @end
%%--------------------------------------------------------------------
-spec(deaf_caller/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
deaf_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = wh_json:get_value(<<"Member-ID">>, Member),
    ?LOG("deaf member ~s", [MemberId]),
    cf_conference_command:deaf(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.deaf, MemberId, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes the conference member belonging to Call undeaf
%% @end
%%--------------------------------------------------------------------
-spec(undeaf_caller/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> ok).
undeaf_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = wh_json:get_value(<<"Member-ID">>, Member),
    ?LOG("undeaf member ~s", [MemberId]),
    cf_conference_command:undeaf(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.undeaf, MemberId, ConfId, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetches a list of members in the conference and updates the record
%% @end
%%--------------------------------------------------------------------
-spec(update_members/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> #conf{}).
update_members(#conf{id=ConfId} = Conf, Call) ->
    C1 = Conf#conf{members = b_members(ConfId, Call)},
    find_call_member(C1, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines which conference member belongs to Call and updates
%% the record
%% @end
%%-------------------------------------------------------------------
-spec(find_call_member/2 :: (Conf :: #conf{}, Call :: #cf_call{}) -> #conf{}).
find_call_member(Conf, #cf_call{call_id=CallId}) ->
    Member = lists:foldr(fun (Member, Acc) ->
                                 case wh_json:get_value(<<"Call-ID">>, Member) of
                                     CallId ->
                                         Member;
                                     _ ->
                                         Acc
                                 end
                         end, undefined, Conf#conf.members),
    Conf#conf{member = Member}.
