%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_asr_ispeech).
-behaviour(gen_asr_provider).

-export([freeform/4
        ,commands/5
        ]).

-include("kazoo_speech.hrl").

-define(DEFAULT_ASR_CONTENT_TYPE, <<"application/wav">>).
-define(SUPPORTED_CONTENT_TYPES, [<<"application/wav">>]).

-spec default_url() -> kz_term:ne_binary().
default_url() ->
    kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"asr_url">>, <<"http://api.ispeech.org/api/json">>).

-spec default_api_key() -> binary().
default_api_key() ->
    kapps_config:get_binary(?MOD_CONFIG_CAT, <<"asr_api_key">>, <<>>).

-spec default_preferred_content_type() -> kz_term:ne_binary().
default_preferred_content_type() ->
    PreferredContentType = kapps_config:get_binary(?MOD_CONFIG_CAT
                                                  ,<<"asr_preferred_content_type">>
                                                  ,?DEFAULT_ASR_CONTENT_TYPE
                                                  ),
    validate_content_type(PreferredContentType).

-spec validate_content_type(binary()) -> kz_term:ne_binary().
validate_content_type(ContentType) ->
    case lists:member(ContentType, ?SUPPORTED_CONTENT_TYPES) of
        'true' -> ContentType;
        'false' ->
            lager:debug("content-type ~s is not supported by ispeech", [ContentType]),
            ?DEFAULT_ASR_CONTENT_TYPE
    end.

-spec freeform(binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> asr_resp().
freeform(Content, ContentType, Locale, Options) ->
    case maybe_convert_content(Content, ContentType) of
        {'error', _}=E -> E;
        {Content1, ContentType1} -> exec_freeform(Content1, ContentType1, Locale, Options)
    end.

-spec commands(kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) ->
                      provider_return().
commands(Content, Commands, ContentType, Locale, Options) ->
    case maybe_convert_content(Content, ContentType) of
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

%%------------------------------------------------------------------------------
%% @doc Convert audio file/content-type if initial format not supported
%% @end
%%------------------------------------------------------------------------------
-spec maybe_convert_content(binary(), kz_term:ne_binary()) -> conversion_return().
maybe_convert_content(Content, ContentType) ->
    case lists:member(ContentType, ?SUPPORTED_CONTENT_TYPES) of
        'true' -> {Content, ContentType};
        'false' ->
            ConvertTo = default_preferred_content_type(),
            case kazoo_asr_util:convert_content(Content, ContentType, ConvertTo) of
                'error' -> {'error', 'unsupported_content_type'};
                Converted -> {Converted, ConvertTo}
            end
    end.
