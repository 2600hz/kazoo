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

-spec playback/2 :: (api_binary(), wh_json:object()) ->
                            {'ok', ne_binary()} |
                            {'error', _}.
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
            
-spec store/3 :: (ne_binary(), ne_binary(), ne_binary()) ->
                         {'ok', ne_binary()} |
                         {'error', _}.
store(Db, Id, Attachment) ->
    Options = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    Rev = case couch_mgr:lookup_doc_rev(Db, Id) of
              {ok, R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    case wh_media_file:get_uri([Db, Id, Attachment], Options) of
        {error, _}=E -> E;
        {ok, URI} ->
            {ok, <<URI/binary, Rev/binary>>}
    end.
