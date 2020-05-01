%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2020, 2600Hz
%%% @doc
%%% @author Sergey Safarov <s.safarov@gmail.com>, Sponsored by Audian
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_google).
-behaviour(gen_asr_provider).

-export([available/0]).
-export([preferred_content_type/0]).
-export([accepted_content_types/0]).
-export([freeform/4]).
-export([commands/5]).
-export([set_api_key/1]).

-include("kazoo_speech.hrl").

-define(GOOGLE_CONFIG_CAT, <<(?MOD_CONFIG_CAT)/binary, ".google">>).
-define(GOOGLE_ASR_URL, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_url">>, <<"https://speech.googleapis.com/v1/speech:recognize">>)).
-define(GOOGLE_ASR_KEY, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_api_key">>, <<>>)).
-define(GOOGLE_ASR_PROFANITY_FILTER, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_profanity_filter">>)).
-define(GOOGLE_ASR_ENABLE_WORD_TIME_OFFSETS, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_enable_word_time_offsets">>)).
-define(GOOGLE_ASR_ENABLE_AUTOMATIC_PUNCTUATION, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_enable_automatic_punctuation">>, 'true')).
-define(GOOGLE_ASR_MODEL, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_model">>, <<"phone_call">>)).
-define(GOOGLE_ASR_USE_ENHANCED, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_use_enhanced">>, 'true')).
-define(GOOGLE_ASR_PREFERRED_CONTENT_TYPE, <<"application/wav">>).
-define(GOOGLE_ASR_ACCEPTED_CONTENT_TYPES, [<<"audio/wav">>, <<"application/wav">>]).

%%%------------------------------------------------------------------------------
%%% @doc Return true if Google ASR is configured / available otherwise false.
%%% @end
%%%------------------------------------------------------------------------------
-spec available() -> boolean().
available() ->
    kz_term:is_not_empty(?GOOGLE_ASR_KEY).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Return or set the preferred asr content type for the ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec preferred_content_type() -> kz_term:ne_binary().
preferred_content_type() ->
    ?GOOGLE_ASR_PREFERRED_CONTENT_TYPE.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Set the asr key
%%% @end
%%%-----------------------------------------------------------------------------
-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?GOOGLE_CONFIG_CAT, <<"asr_api_key">>, Key),
    'ok'.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Return list of supported Content Types by ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec accepted_content_types() -> kz_term:ne_binaries().
accepted_content_types() ->
    ?GOOGLE_ASR_ACCEPTED_CONTENT_TYPES.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
commands(_Bin, _Commands, _ContentType, _Locale, _Opts) ->
    {'error', 'asr_provider_failure', <<"Not implemented">>}.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Callback for API request to ASR Provider and handle transcription response.
%%% @end
%%%-----------------------------------------------------------------------------
-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
freeform(Content, ContentType, Locale, Options) ->
    case kazoo_asr_util:maybe_convert_content(Content, ContentType, accepted_content_types(), preferred_content_type()) of
        {'error', _}=E -> E;
        {Content1, ContentType1} -> exec_freeform(Content1, ContentType1, Locale, Options)
    end.

-spec exec_freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          asr_resp().
exec_freeform(Content, _ContentType, Locale, Options) ->
    BaseUrl = ?GOOGLE_ASR_URL,
    Headers = req_headers(),
    lager:debug("sending request to ~s", [BaseUrl]),

    AudioConfig = [{<<"languageCode">>, Locale}
                  ,{<<"profanityFilter">>, ?GOOGLE_ASR_PROFANITY_FILTER}
                  ,{<<"enableWordTimeOffsets">>, ?GOOGLE_ASR_ENABLE_WORD_TIME_OFFSETS}
                  ,{<<"enableAutomaticPunctuation">>, ?GOOGLE_ASR_ENABLE_AUTOMATIC_PUNCTUATION}
                  ,{<<"model">>, ?GOOGLE_ASR_MODEL}
                  ,{<<"useEnhanced">>, ?GOOGLE_ASR_USE_ENHANCED}
                  ],
    AudioContent = [{<<"content">>, base64:encode(Content)}],
    Req = kz_json:from_list([{<<"config">>,kz_json:from_list(AudioConfig)}
                            ,{<<"audio">>,kz_json:from_list(AudioContent)}
                            ]),
    Body = kz_json:encode(Req),
    lager:debug("asr req body: ~s", [Body]),

    handle_response(make_request(BaseUrl, Headers, Body, Options)).

-spec req_headers() -> kz_http:headers().
req_headers() ->
    [{"Content-Type", "application/json; charset=UTF-8"}
    ,{"X-Goog-Api-Key", ?GOOGLE_ASR_KEY}
    ,{"User-Agent", kz_term:to_list(node())}
    ].

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Execute API request to ASR Provider and handle transcription response.
%%% @end
%%%-----------------------------------------------------------------------------
-spec make_request(kz_term:ne_binary(), kz_term:proplist(), iolist(), kz_term:proplist()) -> kz_http:ret().
make_request(BaseUrl, Headers, Body, Opts) ->
    case props:get_value('receiver', Opts) of
        Pid when is_pid(Pid) ->
            HTTPOptions = props:delete('receiver', Opts),
            lager:debug("streaming response to ~p", [Pid]),
            kz_http:async_req(Pid, 'post', kz_term:to_list(BaseUrl), Headers, Body, HTTPOptions);
        _ ->
            HTTPOptions = props:delete('receiver', Opts),
            kz_http:post(kz_term:to_list(BaseUrl), Headers, Body, HTTPOptions)
    end.

-spec handle_response(kz_http:ret()) -> asr_resp().
handle_response({'error', _R}=E) ->
    lager:debug("asr failed with error ~p", [_R]),
    E;
handle_response({'http_req_id', ReqID}) ->
    lager:debug("streaming response ~p to provided receiver", [ReqID]),
    {'ok', ReqID};
handle_response({'ok', 200, _Headers, Content2}) ->
    lager:debug("ASR of media succeeded: ~s", [Content2]),
    Results = kz_json:get_list_value(<<"results">>, kz_json:decode(Content2), []),
    Alternatives = lists:map(fun(Alternative) -> [Value|_] = kz_json:get_list_value(<<"alternatives">>, Alternative)
                                                     ,Value
                             end, Results),
    Sentences = lists:map(fun(Sentence) -> kz_json:get_value(<<"transcript">>, Sentence) end, Alternatives),
    Props = [{<<"result">>, <<"success">>}
            ,{<<"text">>, list_to_binary(Sentences)}
            ],
    {'ok', kz_json:from_list(Props)};
handle_response({'ok', _Code, _Hdrs, Content2}) ->
    lager:debug("asr of media failed with code ~p", [_Code]),
    lager:debug("resp: ~s", [Content2]),
    {'error', 'asr_provider_failure', kz_json:decode(Content2)}.
