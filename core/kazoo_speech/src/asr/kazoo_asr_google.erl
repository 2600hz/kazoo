%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
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

-export([freeform/4
        ,commands/5
        ]).

-include("kazoo_speech.hrl").

-define(GOOGLE_CONFIG_CAT, <<(?MOD_CONFIG_CAT)/binary, ".google">>).
-define(GOOGLE_ASR_URL, kapps_config:get_string(?GOOGLE_CONFIG_CAT, <<"asr_url">>, <<"https://speech.googleapis.com/v1/speech:recognize">>)).
-define(GOOGLE_ASR_KEY, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_api_key">>, <<>>)).
-define(GOOGLE_ASR_PROFANITY_FILTER, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_profanity_filter">>)).
-define(GOOGLE_ASR_ENABLE_WORD_TIME_OFFSETS, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_enable_word_time_offsets">>)).
-define(GOOGLE_ASR_ENABLE_AUTOMATIC_PUNCTUATION, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_enable_automatic_punctuation">>, 'true')).
-define(GOOGLE_ASR_MODEL, kapps_config:get_binary(?GOOGLE_CONFIG_CAT, <<"asr_model">>, <<"phone_call">>)).
-define(GOOGLE_ASR_USE_ENHANCED, kapps_config:get_is_true(?GOOGLE_CONFIG_CAT, <<"asr_use_enhanced">>, 'true')).

-define(DEFAULT_ASR_CONTENT_TYPE, <<"application/wav">>).
-define(SUPPORTED_CONTENT_TYPES, [<<"application/wav">>]).

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
commands(_Bin, _Commands, _ContentType, _Locale, _Opts) ->
    {'error', 'asr_provider_failure', <<"Not implemented">>}.

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
freeform(Content, ContentType, Locale, Options) ->
    case kazoo_asr_util:maybe_convert_content(Content, ContentType, ?SUPPORTED_CONTENT_TYPES, ?DEFAULT_ASR_CONTENT_TYPE) of
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

-spec make_request(kz_term:text(), kz_term:proplist(), iolist(), kz_term:proplist()) -> kz_http:ret().
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
