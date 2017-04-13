-module(kazoo_asr_util).

-export([maybe_convert_content/2]).

-include("kazoo_speech.hrl").

-spec maybe_convert_content(binary(), ne_binary()) -> provider_return().
maybe_convert_content(Content, ContentType) ->
    case lists:member(ContentType, default_content_types()) of
        'true' -> Content;
        'false' ->
            ConvertTo = default_preferred_content_type(),
            case convert_content(Content, ContentType, ConvertTo) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted -> Converted
            end
    end.

-spec convert_content(binary(), ne_binary(), ne_binary()) -> binary() | 'error'.
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
    lager:info("unsupported conversion from %s to %s", [ContentType, ConvertTo]),
    'error'.

-spec default_content_types() -> ne_binaries().
default_content_types() ->
    kapps_config:get(?MOD_CONFIG_CAT
                    ,<<"asr_content_types">>
                    ,[<<"application/mpeg">>
                     ,<<"application/wav">>
                     ]).

-spec default_preferred_content_type() -> ne_binary().
default_preferred_content_type() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT
                              ,<<"asr_prefered_content_type">>
                              ,<<"application/mpeg">>
                              ).
