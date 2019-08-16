%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Execute the 'Say' verb
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_twiml_say).

-export([exec/3]).

-include("kzt.hrl").

-spec exec(kapps_call:call(), kz_types:xml_els() | kz_types:xml_texts(), kz_types:xml_attribs()) ->
                  {'ok', kapps_call:call()} |
                  {'error', _, kapps_call:call()}.
exec(Call, XmlText, Attrs) ->
    kapps_call_command:answer(Call),
    SayMe = kz_xml:texts_to_binary(XmlText, kapps_config:get_integer(<<"pivot">>, <<"tts_texts_size">>, ?TTS_SIZE_LIMIT)),

    Props = kz_xml:attributes_to_proplist(Attrs),

    Voice = kzt_twiml_util:get_voice(Props),
    Lang = kzt_twiml_util:get_lang(Props),
    Engine = kzt_twiml_util:get_engine(Props),

    Terminators = kzt_twiml_util:get_terminators(Props),

    lager:info("SAY: '~ts' using voice ~s, in lang ~s, and engine ~s", [SayMe, Voice, Lang, Engine]),

    case kzt_twiml_util:loop_count(Props) of
        0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, 'infinity');
        N when N > 0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N)
    end.
