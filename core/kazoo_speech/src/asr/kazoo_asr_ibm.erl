%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2022, 2600Hz
%%% @doc This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_ibm).
-behaviour(gen_asr_provider).

-export([available/0]).
-export([preferred_content_type/0]).
-export([accepted_content_types/0]).
-export([freeform/4]).
-export([commands/5]).
-export([set_api_key/1]).

-include("kazoo_speech.hrl").

-define(IBM_CONFIG_CAT, <<(?MOD_CONFIG_CAT)/binary, ".ibm">>).
-define(IBM_ASR_URL, kapps_config:get_binary(?IBM_CONFIG_CAT, <<"asr_url">>)).
-define(IBM_ASR_KEY, kapps_config:get_binary(?IBM_CONFIG_CAT, <<"asr_api_key">>)).
-define(IBM_ASR_PROFANITY_FILTER, kapps_config:get_is_true(?IBM_CONFIG_CAT, <<"asr_profanity_filter">>, 'true')).
-define(IBM_ASR_MODEL, kapps_config:get_binary(?IBM_CONFIG_CAT, <<"asr_model">>, <<"en-US_NarrowbandModel">>)).
-define(IBM_ASR_SMART_FORMATTING, kapps_config:get_is_true(?IBM_CONFIG_CAT, <<"asr_smart_formatting">>, 'true')).
-define(IBM_ASR_PREFERRED_CONTENT_TYPE, <<"audio/mpeg">>).
-define(IBM_ASR_ACCEPTED_CONTENT_TYPES, [<<"audio/mpeg">>, <<"audio/wav">>]).

%%%------------------------------------------------------------------------------
%%% @doc Return true if IBM ASR is configured / available otherwise false.
%%% @end
%%%------------------------------------------------------------------------------
-spec available() -> boolean().
available() ->
    kz_term:is_not_empty(?IBM_ASR_KEY).

%%%-----------------------------------------------------------------------------
%%% @doc Return or set the preferred asr content type for the ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec preferred_content_type() -> kz_term:ne_binary().
preferred_content_type() ->
    ?IBM_ASR_PREFERRED_CONTENT_TYPE.

%%%-----------------------------------------------------------------------------
%%% @doc Return list of supported Content Types by ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec accepted_content_types() -> kz_term:ne_binaries().
accepted_content_types() ->
    ?IBM_ASR_ACCEPTED_CONTENT_TYPES.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> provider_return().
commands(_Bin, _Commands, _ContentType, _Locale, _Opts) ->
    {'error', 'asr_provider_failure', <<"not implemented">>}.

%%%-----------------------------------------------------------------------------
%%% @doc Callback for API request to ASR Provider and handle transcription response.
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
exec_freeform(Content, ContentType, _Locale, Options) ->
    URL = build_url(),
    Headers = req_headers(ContentType),
    OptionsWithAuth = [{'basic_auth', {<<"apikey">>, ?IBM_ASR_KEY}} | Options],
    lager:debug("sending request to ~s with headers ~p", [URL, Headers]),
    handle_response(make_request(URL, Headers, Content, OptionsWithAuth)).

-spec build_url() -> kz_term:binary().
build_url() ->
    URL = <<(?IBM_ASR_URL)/binary, "/v1/recognize">>,
    case kz_http_util:props_to_querystring(
           props:filter_undefined(
             [{<<"profanity_filter">>, ?IBM_ASR_PROFANITY_FILTER}
             ,{<<"model">>, ?IBM_ASR_MODEL}
             ,{<<"smart_formatting">>, ?IBM_ASR_SMART_FORMATTING}
             ])
          )
    of
        [] -> URL;
        QueryString ->
            <<URL/binary, "?", (kz_term:to_binary(QueryString))/binary>>
    end.

-spec req_headers(kz_term:ne_binary()) -> kz_http:headers().
req_headers(ContentType) ->
    [{"Content-Type", ContentType}
    ,{"User-Agent", kz_term:to_list(node())}
    ].

%%%-----------------------------------------------------------------------------
%%% @doc Execute API request to ASR Provider and handle transcription response.
%%% @end
%%%-----------------------------------------------------------------------------
-spec make_request(kz_term:ne_binary(), kz_term:proplist(), iodata(), kz_term:proplist()) ->
          kz_http:ret().
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
    Sentences = [get_sentence(Alternative) || Alternative <- Results],

    Props = [{<<"result">>, <<"success">>}
            ,{<<"text">>, list_to_binary(Sentences)}
            ],
    {'ok', kz_json:from_list(Props)};
handle_response({'ok', _Code, _Hdrs, Content2}) ->
    lager:debug("asr of media failed with code ~p", [_Code]),
    lager:debug("resp: ~s", [Content2]),
    {'error', 'asr_provider_failure', Content2}.

-spec get_sentence(kz_json:object()) -> kz_term:ne_binary().
get_sentence(Alternative) ->
    [Sentence|_] = kz_json:get_list_value(<<"alternatives">>, Alternative),
    kz_json:get_value(<<"transcript">>, Sentence).

%%%-----------------------------------------------------------------------------
%%% @doc Set the asr key
%%% @end
%%%-----------------------------------------------------------------------------
-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?IBM_CONFIG_CAT, <<"asr_api_key">>, Key),
    'ok'.
