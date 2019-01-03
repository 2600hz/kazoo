%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_util).

-export([convert_content/3]).

-include("kazoo_speech.hrl").

-spec convert_content(binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> binary() | 'error'.
convert_content(Content, <<"audio/mpeg">>=_ConvertFrom, <<"application/wav">> = _ConvertTo) ->
    Mp3File = kazoo_speech_util:tmp_file_name(<<"mp3">>),
    WavFile = kazoo_speech_util:tmp_file_name(<<"wav">>),
    kz_util:write_file(Mp3File, Content),
    Cmd = io_lib:format("lame --decode ~s ~s &> /dev/null && echo -n \"success\"", [Mp3File, WavFile]),
    _ = os:cmd(Cmd),
    kz_util:delete_file(Mp3File),
    case file:read_file(WavFile) of
        {'ok', WavContent} ->
            kz_util:delete_file(WavFile),
            WavContent;
        {'error', _R} ->
            lager:info("unable to convert mpeg to wav: ~p", [_R]),
            'error'
    end;
convert_content(_, ContentType, ConvertTo) ->
    lager:info("unsupported conversion from ~s to ~s", [ContentType, ConvertTo]),
    'error'.
