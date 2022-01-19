%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_util).

-export([maybe_convert_content/4]).

-include("kazoo_speech.hrl").

%%------------------------------------------------------------------------------
%% @doc Convert audio file/content-type if initial format not supported
%% @end
%%------------------------------------------------------------------------------
-spec maybe_convert_content(binary(), kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary()) -> conversion_return().
maybe_convert_content(Content, ContentType, SupportedContentTypes, PreferredContentType) ->
    case lists:member(ContentType, SupportedContentTypes) of
        'true' -> {Content, ContentType};
        'false' ->
            lager:debug("content-type ~s is not supported by asr provider", [ContentType]),
            case convert_content(Content, ContentType, PreferredContentType) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted -> {Converted, PreferredContentType}
            end
    end.

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
