%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(wh_media_url).

-export([playback/2]).
-export([store/3]).

-include("whistle_media.hrl").

playback(undefined, _) ->
    {error, invalid_media_name};
playback(<<"tts://", _/binary>> = TTS, Options) ->
    lager:debug("lookup tts media url for ~s", [TTS]),
    wh_media_tts:get_uri(TTS, Options);
playback(Media, Options) ->
    lager:debug("lookup media url for ~s", [Media]),
    case wh_media_file:get_uri(Media, Options) of
        {error, _}=E -> E;
        {ok, URI}=Ok ->
            _ = wh_media_file:maybe_prepare_proxy(URI),
            Ok
    end.
            
store(Db, Id, Attachment) ->
    Options = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    wh_media_file:get_uri([Db, Id, Attachment], Options).
