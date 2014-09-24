%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_mgr_maintenance).

-export([prompt_url/1, prompt_url/2, prompt_url/3]).

-include("media.hrl").

prompt_url(PromptId) ->
    AccountId = ?WH_MEDIA_DB,
    Language = wh_media_util:default_prompt_language(),
    prompt_url(PromptId, AccountId, Language).

prompt_url(PromptId, AccountId) ->
    Language = wh_media_util:prompt_language(AccountId),
    prompt_url(PromptId, AccountId, Language).

prompt_url(PromptId, AccountId, Language) ->
    {'ok', URL} = wh_media_url:playback(<<"prompt://", AccountId/binary, "/", PromptId/binary, "/", Language/binary>>, wh_json:new()),
    io:format(" URL for ~s/~s/~s: ~s~n", [AccountId, PromptId, Language, URL]).
