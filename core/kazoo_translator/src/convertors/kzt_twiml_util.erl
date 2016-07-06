%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_twiml_util).

-export([get_terminators/1
        ,loop_count/1
        ,get_voice/1
        ,get_engine/1
        ,get_lang/1
        ,finish_dtmf/1, finish_dtmf/2
        ,get_finish_key/1
        ,get_max_length/1
        ,pause_for/1
        ,action_url/1
        ,timeout_s/1, timeout_s/2
        ,num_digits/1
        ,reject_prompt/1
        ,reject_reason/1
        ,reject_code/1
        ,reject_status/1
        ]).

-include("kzt.hrl").

-spec get_terminators(kz_proplist()) -> ne_binaries().
get_terminators(Props) ->
    kapi_dialplan:terminators(props:get_binary_value('terminators', Props)).

-spec get_lang(kz_proplist()) -> ne_binary().
get_lang(Props) ->
    case props:get_binary_value('language', Props) of
        'undefined' -> ?DEFAULT_TTS_LANG;
        <<"en">> -> <<"en-US">>;
        <<"en-gb">> -> <<"en-GB">>;
        <<"es">> -> <<"es">>;
        <<"fr">> -> <<"fr">>;
        <<"de">> -> <<"de">>;
        <<"it">> -> <<"it">>
    end.

-spec get_voice(kz_proplist()) -> ne_binary().
get_voice(Props) ->
    case props:get_binary_value('voice', Props) of
        <<"man">> -> <<"male">>;
        <<"male">> -> <<"male">>;
        <<"woman">> -> <<"female">>;
        <<"female">> -> <<"female">>;
        'undefined' -> ?DEFAULT_TTS_VOICE
    end.

-spec get_engine(kz_proplist()) -> ne_binary().
get_engine(Props) ->
    case props:get_binary_value('engine', Props) of
        'undefined' -> ?DEFAULT_TTS_ENGINE;
        Engine -> Engine
    end.

-spec loop_count(kz_proplist()) -> integer().
loop_count(Props) -> props:get_integer_value('loop', Props, 1).

-spec finish_dtmf(kz_proplist()) -> ne_binary().
-spec finish_dtmf(kz_proplist(), ne_binary()) -> ne_binary().
finish_dtmf(Props) -> finish_dtmf(Props, <<"#">>).
finish_dtmf(Props, Default) when is_list(Props) ->
    case props:get_binary_value('finishOnKey', Props) of
        'undefined' -> Default;
        DTMF ->
            'true' = lists:member(DTMF, ?ANY_DIGIT),
            DTMF
    end.

-spec get_finish_key(kz_proplist()) -> ne_binaries().
get_finish_key(Props) ->
    kapi_dialplan:terminators(props:get_binary_value('finishOnKey', Props)).

-spec get_max_length(kz_proplist()) -> pos_integer().
get_max_length(Props) ->
    Max = kapps_config:get_integer(?MODULE, <<"max_length">>, 3600),
    case props:get_integer_value('maxLength', Props) of
        'undefined' -> Max;
        N when N > 0, N =< Max -> N
    end.

%% limit pause to 1 hour (3600000 ms)
-spec pause_for(kz_proplist()) -> 1000..3600000.
pause_for(Props) ->
    case props:get_integer_value('length', Props) of
        'undefined' -> ?MILLISECONDS_IN_SECOND;
        N when is_integer(N), N > 0, N =< 3600 -> N * ?MILLISECONDS_IN_SECOND;
        N when is_integer(N), N > 3600 -> 3600000
    end.

-spec action_url(kz_proplist()) -> api_binary().
action_url(Props) -> props:get_binary_value('action', Props).

-spec reject_prompt(kz_proplist()) -> api_binary().
reject_prompt(Props) -> props:get_binary_value('prompt', Props).

-spec timeout_s(kz_proplist()) -> pos_integer().
-spec timeout_s(kz_proplist(), pos_integer()) -> pos_integer().
timeout_s(Props) -> timeout_s(Props, 30).
timeout_s(Props, Default) ->
    case props:get_integer_value('timeout', Props, Default) of
        N when is_integer(N), N > 3600 -> 3600;
        N when is_integer(N), N > 0 -> N
    end.

-spec num_digits(kz_proplist()) -> kz_timeout().
num_digits(Props) ->
    case props:get_integer_value('numDigits', Props) of
        'undefined' -> 'infinity';
        N when is_integer(N), N > 0 -> N
    end.

-spec reject_reason(kz_proplist()) -> ne_binary().
reject_reason(Props) ->
    case props:get_binary_value('reason', Props) of
        'undefined' -> <<"rejected">>;
        <<"rejected">> -> <<"rejected">>;
        <<"busy">> -> <<"busy">>
    end.

-spec reject_code(ne_binary()) -> ne_binary().
reject_code(<<"busy">>) -> <<"486">>;
reject_code(<<"rejected">>) -> <<"503">>.

-spec reject_status(ne_binary()) -> ne_binary().
reject_status(<<"486">>) -> ?STATUS_BUSY;
reject_status(<<"503">>) -> ?STATUS_NOANSWER.
