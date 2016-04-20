%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_media_url).

-export([playback/1, playback/2]).
-export([store/2, store/3, store/4]).

-include("whistle_media.hrl").

-type build_media_url() :: api_binary() | binaries() | wh_json:object().
-type build_media_url_ret() :: ne_binary() | {'error', any()}.

-spec playback(build_media_url()) -> build_media_url_ret().
-spec playback(build_media_url(), wh_json:object()) -> build_media_url_ret().

playback('undefined') ->
    {'error', 'invalid_media_name'};
playback(Arg) ->
    playback(Arg, wh_json:new()).

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
playback(<<_/binary>> = Media, JObj) ->
    lager:debug("lookup media url for ~s", [Media]),
    wh_media_file:get_uri(Media, JObj);
playback(Path, JObj)
  when is_list(Path) ->
    wh_media_file:get_uri(Path, JObj);
playback(Doc, JObj) ->
    lager:debug("building media url from doc"),
    case wh_media_util:store_path_from_doc(Doc) of
        {'error', _} = Error -> Error;
        Media -> wh_media_file:get_uri(Media, JObj)
    end.

-spec store(wh_json:object(), ne_binary()) -> build_media_url_ret().
store(JObj, AName) ->
    Opts = [{'doc_type', wh_doc:type(JObj)}
            ,{'doc_owner', wh_json:get_value(<<"owner_id">>, JObj)}
           ],
    store(wh_doc:account_db(JObj), wh_doc:id(JObj), AName, props:filter_undefined(Opts)).

-spec store(ne_binary(), kazoo_data:docid(), ne_binary()) -> build_media_url_ret().
store(Db, Id, Attachment) ->
    store(Db, Id, Attachment, []).

-spec store(ne_binary(), kazoo_data:docid(), ne_binary(), wh_proplist()) -> build_media_url_ret().
store(Db, {Type, Id}, Attachment, Options) ->
    store(Db, Id, Attachment, [{'doc_type', Type} | Options]);
store(Db, Id, Attachment, Options) ->
    JObj = wh_json:from_list([{<<"Stream-Type">>, <<"store">>}]),
    wh_media_file:get_uri([Db, Id, Attachment, Options], JObj).
