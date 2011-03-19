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

-define(APP_NAME, <<"cf_conference">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-import(cf_call_command, [
                          answer/1, b_conference/2, b_play/2, hangup/1, 
                          b_play_and_collect_digits/6, wait_for_dtmf/1
                         ]).

-import(cf_conference_command, [
                               b_members/2
                              ]).

-record(prompts, {
           greeting = <<"shout://translate.google.com/translate_tts?tl=en&q=Welcome+to+the+conference!">> 
          ,request_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Please+enter+the+conference+pin+number+followed+by+the+pound+key.">>
          ,incorrect_pin = <<"shout://translate.google.com/translate_tts?tl=en&q=Invalid+pin+number.">>
          ,max_pin_tries = <<"shout://translate.google.com/translate_tts?tl=en&q=You+have+reached+the+maximum+number+of+entry+attempts!+Goodbye.">>
          ,alone_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=You+are+currently+the+only+participant.">>
          ,single_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+is+only+one+other+participant.">>
          ,multiple_enter = <<"shout://translate.google.com/translate_tts?tl=en&q=There+are+~p+other+participants.">>
          ,announce_join = <<"tone_stream://%(200,0,500,600,700)">>
          ,announce_leave = <<"tone_stream://%(500,0,300,200,100,50,25)">>
          ,muted = <<"shout://translate.google.com/translate_tts?tl=en&q=Muted.">>
          ,unmuted = <<"shout://translate.google.com/translate_tts?tl=en&q=Unmuted.">>          
          ,deaf = <<"shout://translate.google.com/translate_tts?tl=en&q=Silenced.">>
          ,undeaf = <<"shout://translate.google.com/translate_tts?tl=en&q=Hear.">>          
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
          ,moderator_pins = []
          ,max_pin_tries = 3
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
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->
    Conf = update_members(#conf{}, Call),
    answer(Call),
    play_conference_name(Conf, Call),
    grant_access(Conf, Call, 1),
    play_conference_count(Conf, Call),
    b_conference(Conf#conf.id, Call),
    announce_join(Conf, Call),
    caller_controls(Conf, Call),
    announce_leave(Conf, Call),
    stop.

play_conference_name(#conf{prompts=Prompts}, Call) ->
    b_play(Prompts#prompts.greeting, Call).

grant_access(#conf{prompts=Prompts} = Conf, Call, LoopCount) when LoopCount > Conf#conf.max_pin_tries ->
    b_play(Prompts#prompts.max_pin_tries, Call),
    hangup(Call),
    {stop};
grant_access(#conf{prompts=Prompts} = Conf, Call, LoopCount) ->
    case b_play_and_collect_digits(<<"4">>, <<"6">>, Prompts#prompts.request_pin, <<"1">>, <<"5000">>, Call) of
        {ok, Pin} ->
            case lists:member(Pin, Conf#conf.member_pins) of
                true ->
                    ok;
                false -> 
                    b_play(Prompts#prompts.incorrect_pin, Call),
                    grant_access(Conf, Call, LoopCount+1)
            end;
        {error, _} ->
            grant_access(Conf, Call, LoopCount+1)
    end.

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

announce_join(#conf{prompts=Prompts, id=ConfId}, Call) ->
    cf_conference_command:play(Prompts#prompts.announce_join, ConfId, Call).

announce_leave(#conf{prompts=Prompts, id=ConfId}, Call) ->
    cf_conference_command:play(Prompts#prompts.announce_leave, ConfId, Call).

caller_controls(#conf{control=Control} = Conf, Call) ->
    case wait_for_dtmf(200000) of
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
        {error, timeout} ->
            caller_controls(Conf, Call);
        {error, channel_hungup} ->
            ok
    end.

toggle_mute(Conf, Call) ->
    C1 = update_members(Conf, Call),
    case binary:match(whapps_json:get_value(<<"Status">>, C1#conf.member), <<"speak">>) of
        nomatch ->                     
            unmute_caller(C1, Call);
        _ ->
            mute_caller(C1, Call)
    end;    

toggle_deaf(Conf, Call) ->
    C1 = update_members(Conf, Call),
    case binary:match(whapps_json:get_value(<<"Status">>, C1#conf.member), <<"hear">>) of
        nomatch ->                     
            unhear_caller(C1, Call);
        _ ->
            hear_caller(C1, Call)
    end;    

mute_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = whapps_json:get_value(<<"Member-ID">>, Member),
    cf_conference_command:mute(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.muted, MemberId, ConfId, Call).

unmute_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = whapps_json:get_value(<<"Member-ID">>, Member),
    cf_conference_command:unmute(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.unmuted, MemberId, ConfId, Call).

deaf_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = whapps_json:get_value(<<"Member-ID">>, Member),
    cf_conference_command:deaf(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.deaf, MemberId, ConfId, Call).

undeaf_caller(#conf{member=Member, id=ConfId, prompts=Prompts}, Call) ->
    MemberId = whapps_json:get_value(<<"Member-ID">>, Member),
    cf_conference_command:undeaf(MemberId, ConfId, Call),
    cf_conference_command:play(Prompts#prompts.undeaf, MemberId, ConfId, Call).

update_members(#conf{id=ConfId} = Conf, Call) ->
    C1 = Conf#conf{members = b_members(ConfId, Call)},
    find_call_member(C1, Call).

find_call_member(Conf, #cf_call{route_request=RR}) ->
    CallId = get_value(<<"Call-ID">>, RR),
    Member = lists:foldr(fun (Member, Acc) ->
                                 case whapps_json:get_value(<<"Call-ID">>, Member) of
                                     CallId ->
                                         Member;
                                     _ ->
                                         Acc
                                 end
                         end, undefined, Conf#conf.members),
    Conf#conf{member = Member}.
