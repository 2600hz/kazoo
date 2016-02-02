%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Execute the 'Say' verb
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml_say).

-export([exec/3]).

-include("kzt.hrl").

-spec exec(whapps_call:call(), xml_els() | xml_texts(), xml_attribs()) ->
                  {'ok', whapps_call:call()} |
                  {'error', _, whapps_call:call()}.
exec(Call, XmlText, Attrs) ->
    whapps_call_command:answer(Call),
    SayMe = kz_xml:texts_to_binary(XmlText, whapps_config:get_integer(<<"pivot">>, <<"tts_texts_size">>, ?TTS_SIZE_LIMIT)),

    Props = kz_xml:attributes_to_proplist(Attrs),

    Voice = kzt_twiml_util:get_voice(Props),
    Lang = kzt_twiml_util:get_lang(Props),
    Engine = kzt_twiml_util:get_engine(Props),

    Terminators = kzt_twiml_util:get_terminators(Props),

    lager:info("SAY: '~s' using voice ~s, in lang ~s, and engine ~s", [SayMe, Voice, Lang, Engine]),

    case kzt_twiml_util:loop_count(Props) of
        0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, 'infinity');
        N when N > 0 -> kzt_receiver:say_loop(Call, SayMe, Voice, Lang, Terminators, Engine, N)
    end.
