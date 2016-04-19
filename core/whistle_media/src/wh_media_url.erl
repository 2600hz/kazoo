%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_media_url).

-export([playback/2]).
-export([store/3, store/4]).

-include("whistle_media.hrl").

-spec playback(api_binary(), wh_json:object()) ->
                      {'ok', ne_binary()} |
                      {'error', any()}.
playback('undefined', _) ->
    {'error', 'invalid_media_name'};
playback(<<"tts://", _/binary>> = TTS, Options) ->
    lager:debug("lookup tts media url for ~s", [TTS]),
    wh_media_tts:get_uri(TTS, Options);
playback(<<"prompt://", PromptPath/binary>>, Options) ->
    lager:debug("looking up prompt path ~s", [PromptPath]),
    case binary:split(PromptPath, <<"/">>, ['global']) of
        [AccountId, PromptId, Language] ->
            Media = wh_media_map:prompt_path(AccountId, PromptId, Language),
            playback(Media, Options);
        _Path ->
            lager:warning("invalid prompt path: ~p", [_Path]),
            {'error', 'invalid_media_name'}
    end;
playback(Media, JObj) ->
    lager:debug("lookup media url for ~s", [Media]),
    wh_media_file:get_uri(Media, JObj).

-spec store(ne_binary(), kazoo_data:docid(), ne_binary()) ->
                   {'ok', ne_binary()} |
                   {'error', any()}.
store(Db, {Type, Id}, Attachment) ->
    JObj = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    wh_media_file:get_uri([Db, Id, Type, Attachment], JObj);
store(Db, Id, Attachment) ->
    JObj = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    wh_media_file:get_uri([Db, Id, Attachment], JObj).

-spec store(ne_binary(), kazoo_data:docid(), ne_binary(), wh_proplist()) ->
                   {'ok', ne_binary()} |
                   {'error', any()}.
store(Db, Id, Attachment, Options) ->
    JObj = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    Rev = props:get_value('rev', Options),
    Type = props:get_value('doc_type', Options),
    wh_media_file:get_uri([Db, Id, Type, Rev, Attachment], JObj).

