%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_url).

-export([playback/1, playback/2]).
-export([store/2, store/3, store/4]).

-include("kazoo_media.hrl").

-define(STREAM_TYPE_STORE, kz_json:from_list([{<<"Stream-Type">>, <<"store">>}])).

-type build_media_url() :: kz_term:api_binary() | kz_term:binaries() | kz_json:object().
-type build_media_url_ret() :: kz_term:ne_binary() | {'error', atom()}.


-spec playback(build_media_url()) -> build_media_url_ret().
playback('undefined') ->
    {'error', 'invalid_media_name'};
playback(Arg) ->
    playback(Arg, kz_json:new()).

-spec playback(build_media_url(), kz_json:object()) -> build_media_url_ret().
playback('undefined', _) ->
    {'error', 'invalid_media_name'};
playback(<<"tts://", Id/binary>>, Options) ->
    lager:debug("lookup tts media url for ~s", [Id]),
    kz_media_tts:get_uri(Id, Options);
playback(<<"prompt://", PromptPath/binary>>, Options) ->
    lager:debug("looking up prompt path ~s", [PromptPath]),
    case binary:split(PromptPath, <<"/">>, ['global']) of
        [AccountId, PromptId, Language] ->
            Media = kz_media_map:prompt_path(AccountId, PromptId, Language),
            playback(Media, Options);
        [AccountId, PromptId] ->
            lager:info("got req for prompt ~s without language, checking account ~s", [PromptId, AccountId]),
            Language = kz_media_util:prompt_language(AccountId),
            Media = kz_media_map:prompt_path(AccountId, PromptId, Language),
            playback(Media, Options);
        _Path ->
            lager:warning("invalid prompt path: ~p", [_Path]),
            {'error', 'invalid_media_name'}
    end;
playback(<<_/binary>> = Media, JObj) ->
    lager:debug("lookup media url for ~s", [Media]),
    kz_media_file:get_uri(Media, JObj);
playback(Path, JObj)
  when is_list(Path) ->
    kz_media_file:get_uri(Path, JObj);
playback(Doc, JObj) ->
    lager:debug("building media url from doc"),
    case kz_media_util:store_path_from_doc(Doc) of
        #media_store_path{}=Media -> kz_media_file:get_uri(Media, JObj);
        Error -> Error
    end.

-spec store(kz_json:object(), kz_term:ne_binary()) ->
          build_media_url_ret().
store(JObj, AName) ->
    Media = kz_media_util:store_path_from_doc(JObj, AName),
    kz_media_file:get_uri(Media, ?STREAM_TYPE_STORE).

-spec store(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary()) ->
          build_media_url_ret().
store(Db, Id, Attachment) ->
    store(Db, Id, Attachment, []).

-spec store(kz_term:ne_binary(), kazoo_data:docid(), kz_term:ne_binary(), kz_term:proplist()) ->
          build_media_url_ret().
store(Db, {Type, Id}, Attachment, Options) ->
    store(Db, Id, Attachment, [{'doc_type', Type} | Options]);
store(Db, ?NE_BINARY = Id, Attachment, Options) ->
    kz_media_file:get_uri([Db, Id, Attachment, Options], ?STREAM_TYPE_STORE).
