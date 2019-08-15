%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Takes a text and uses TTS to play it to the caller.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`text'</dt>
%%%   <dd>This is what should be said.</dd>
%%%
%%%   <dt>`voice'</dt>
%%%   <dd>One of: `male' or `female'.</dd>
%%%
%%%   <dt>`engine'</dt>
%%%   <dd>`flite' or `ispeech'.</dd>
%%% </dl>
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_tts).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    kapps_call_command:answer(Call),

    Command = kz_json:from_list(
                [{<<"Application-Name">>, <<"tts">>}
                ,{<<"Text">>, to_say(Data)}
                ,{<<"Terminators">>, kapps_call_command:tts_terminators(terminators(Data))}
                ,{<<"Voice">>, kapps_call_command:tts_voice(voice(Data))}
                ,{<<"Language">>, kapps_call_command:tts_language(language(Data), Call)}
                ,{<<"Engine">>, kapps_call_command:tts_engine(engine(Data), Call)}
                ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                ,{<<"Endless-Playback">>, endless_playback(Data)}
                ]),

    send_command(Command, Call).

send_command(TTSCommand, Call) ->
    NoopId = kz_datamgr:get_uuid(),

    Commands = [kz_json:from_list([{<<"Application-Name">>, <<"noop">>}
                                  ,{<<"Call-ID">>, kapps_call:call_id(Call)}
                                  ,{<<"Msg-ID">>, NoopId}
                                  ])
               ,TTSCommand
               ],
    Command = [{<<"Application-Name">>, <<"queue">>}
              ,{<<"Commands">>, Commands}
              ],
    kapps_call_command:send_command(Command, Call),

    lager:debug("tts is waiting for noop ~s", [NoopId]),
    case cf_util:wait_for_noop(Call, NoopId) of
        {'ok', Call1} ->
            %% Give control back to cf_exe process
            cf_exe:set_call(Call1),
            cf_exe:continue(Call1);
        {'error', _} ->
            cf_exe:stop(Call)
    end.

terminators(Data) ->
    kz_json:get_list_value(<<"terminators">>, Data, ?ANY_DIGIT).

engine(Data) ->
    kz_json:get_binary_value(<<"engine">>, Data).

language(Data) ->
    kz_json:get_binary_value(<<"language">>, Data).

voice(Data) ->
    kz_json:get_binary_value(<<"voice">>, Data).

to_say(Data) ->
    kz_json:get_binary_value(<<"text">>, Data).

endless_playback(Data) ->
    kz_json:get_boolean_value(<<"endless_playback">>, Data).
