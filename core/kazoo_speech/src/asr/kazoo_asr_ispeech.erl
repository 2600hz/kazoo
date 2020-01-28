%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_ispeech).
-behaviour(gen_asr_provider).

-export([preferred_content_type/0
        ,accepted_content_types/0
        ,freeform/4
        ,commands/5
        ,set_api_key/1
        ]).

-include("kazoo_speech.hrl").

-define(DEFAULT_ASR_CONTENT_TYPE, <<"application/wav">>).
-define(SUPPORTED_CONTENT_TYPES, [<<"application/wav">>]).

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Set the asr key
%%% @end
%%%-----------------------------------------------------------------------------
-spec set_api_key(kz_term:ne_binary()) -> 'ok'.
set_api_key(Key) ->
    {'ok', _} = kapps_config:set_default(?MOD_CONFIG_CAT, <<"asr_api_key">>, Key),
    'ok'.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Return or set the preferred asr content type for the ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec preferred_content_type() -> kz_term:ne_binary().
preferred_content_type() ->
    ?DEFAULT_ASR_CONTENT_TYPE.

%%%-----------------------------------------------------------------------------
%%% @doc
%%% Return list of supported Content Types by ASR provider
%%% @end
%%%-----------------------------------------------------------------------------
-spec accepted_content_types() -> kz_term:ne_binaries().
accepted_content_types() ->
    ?SUPPORTED_CONTENT_TYPES.

-spec default_url() -> kz_term:ne_binary().
default_url() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>).

-spec default_api_key() -> binary().
default_api_key() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>).

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
freeform(Content, ContentType, Locale, Options) ->
    case kazoo_asr_util:maybe_convert_content(Content, ContentType, accepted_content_types(), preferred_content_type()) of
        {'error', _}=E -> E;
        {Content1, ContentType1} -> exec_freeform(Content1, ContentType1, Locale, Options)
    end.

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          provider_return().
commands(Content, Commands, ContentType, Locale, Options) ->
    case kazoo_asr_util:maybe_convert_content(Content, ContentType, accepted_content_types(), preferred_content_type()) of
        {'error', _}=E -> E;
        {Content1, ContentType1} -> exec_commands(Content1, Commands, ContentType1, Locale, Options)
    end.

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

-spec handle_response(kz_http:ret()) -> asr_resp() | provider_return().
handle_response({'error', _R}=E) ->
    lager:debug("asr failed with error ~p", [_R]),
    E;
handle_response({'http_req_id', ReqID}) ->
    lager:debug("streaming response ~p to provided receiver", [ReqID]),
    {'ok', ReqID};
handle_response({'ok', 200, _Headers, Content2}) ->
    lager:debug("ASR of media succeeded: ~s", [Content2]),
    {'ok', kz_json:decode(Content2)};
handle_response({'ok', _Code, _Hdrs, Content2}) ->
    lager:debug("asr of media failed with code ~p", [_Code]),
    lager:debug("resp: ~s", [Content2]),
    {'error', 'asr_provider_failure', kz_json:decode(Content2)}.

%%------------------------------------------------------------------------------
%% @doc Send a freeform ASR request to iSpeech
%% @end
%%------------------------------------------------------------------------------
-spec exec_freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          asr_resp().
exec_freeform(Content, ContentType, Locale, Options) ->
    BaseUrl = default_url(),
    lager:debug("sending request to ~s", [BaseUrl]),
    Props = [{<<"apikey">>, default_api_key()}
            ,{<<"action">>, <<"recognize">>}
            ,{<<"freeform">>, <<"1">>}
            ,{<<"content-type">>, ContentType}
            ,{<<"output">>, <<"json">>}
            ,{<<"locale">>, Locale}
            ,{<<"audio">>, base64:encode(Content)}
            ],
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = kz_http_util:props_to_querystring(Props),
    lager:debug("asr req body: ~s", [Body]),

    handle_response(make_request(BaseUrl, Headers, Body, Options)).

%%------------------------------------------------------------------------------
%% @doc Send a command list ASR request to iSpeech
%% @end
%%------------------------------------------------------------------------------
-spec exec_commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
          provider_return().
exec_commands(Bin, Commands, ContentType, Locale, Opts) ->
    BaseUrl = default_url(),

    Commands1 = kz_binary:join(Commands, <<"|">>),

    lager:debug("sending request to ~s", [BaseUrl]),

    Props = [{<<"apikey">>, default_api_key()}
            ,{<<"action">>, <<"recognize">>}
            ,{<<"alias">>, <<"command1|YESNOMAYBE">>}
            ,{<<"YESNOMAYBE">>, Commands1}
            ,{<<"command1">>, <<"say %YESNOMAYBE%">>}
            ,{<<"content-type">>, ContentType}
            ,{<<"output">>, <<"json">>}
            ,{<<"locale">>, Locale}
            ,{<<"audio">>, base64:encode(Bin)}
            ],
    Headers = [{"Content-Type", "application/json"}],

    Body = kz_json:encode(kz_json:from_list(Props)),
    lager:debug("req body: ~s", [Body]),

    handle_response(make_request(BaseUrl, Headers, Body, Opts)).
