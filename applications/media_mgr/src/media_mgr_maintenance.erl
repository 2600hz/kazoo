%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_mgr_maintenance).

-export([prompt_url/1, prompt_url/2, prompt_url/3]).

-include("media.hrl").

-spec prompt_url(ne_binary()) -> 'ok'.
-spec prompt_url(ne_binary(), ne_binary()) -> 'ok'.
-spec prompt_url(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.

prompt_url(PromptId) ->
    AccountId = ?KZ_MEDIA_DB,
    Language = kz_media_util:default_prompt_language(),
    prompt_url(PromptId, AccountId, Language).

prompt_url(PromptId, AccountId) ->
    Language = kz_media_util:prompt_language(AccountId),
    prompt_url(PromptId, AccountId, Language).

prompt_url(PromptId, AccountId, Language) ->
    case kz_media_url:playback(<<"prompt://", AccountId/binary, "/", PromptId/binary, "/", Language/binary>>, kz_json:new()) of
        {'error', _E} ->
            io:format("failed to find URL for ~s/~s/~s: ~p~n", [AccountId, PromptId, Language, _E]);
        URL ->
            io:format(" URL for ~s/~s/~s: ~s~n", [AccountId, PromptId, Language, URL])
    end.
