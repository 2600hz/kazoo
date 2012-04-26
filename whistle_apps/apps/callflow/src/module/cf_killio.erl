%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Provide a direct replacement for Twilio-based servers
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_killio).

-include("../callflow.hrl").
-include("cf_killio.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%%
%% Expected data payload:
%%   http_method: "get" | "post"
%%   req_timeout: integer(), defaults to 5 seconds
%%   voice_url: string(), url to get/post to
%%
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    BaseParams = wh_json:from_list(req_params(Call)),

    Method = http_method(wh_json:get_value(<<"http_method">>, Data, get)),
    VoiceUri = wh_json:get_value(<<"voice_url">>, Data),
    Uri = uri(VoiceUri, Method, BaseParams),

    ReqTimeout = wh_json:get_integer_value(<<"req_timeout">>, Data, 5),

    send_req(whapps_call:kvs_store(voice_uri, VoiceUri, Call), Uri, Method, BaseParams).

send_req(Call, Uri, Method, BaseParams) ->
    lager:debug("sending req to ~s via ~s", [Uri, Method]),
    case ibrowse:send_req(wh_util:to_list(Uri), [], Method, wh_json:encode(BaseParams), ?DEFAULT_OPTS) of
        {ok, "200", Hdrs, RespBody} ->
            lager:debug("recv 200: ~s", [RespBody]),
            handle_resp(Call, Hdrs, RespBody);
        {ok, _RespCode, _Hdrs, _RespBody} ->
            lager:debug("recv other: ~s: ~s", [_RespCode, _RespBody]),
            cf_exe:continue(Call);
        {error, _Reason} ->
            lager:debug("error with req: ~p", [_Reason]),
            cf_exe:continue(Call)
    end.

handle_resp(Call, Hdrs, RespBody) ->
    case props:get_value("Content-Type", Hdrs) of
        "text/xml" ->
            process_xml(Call, xmerl_scan:string(wh_util:to_list(RespBody)));
        _CT ->
            lager:debug("unknown content type response: ~s", [_CT]),
            cf_exe:continue(Call)
    end.

process_xml(Call, {Resp, _}) ->
    case Resp#xmlElement.name =:= 'Response' of
        true -> process_response(Call, Resp#xmlElement.content);
        false ->
            lager:debug("root element not Response tag, ~p", [Resp#xmlElement.name]),
            cf_exe:continue(Call)
    end.

process_response(Call, [#xmlText{}|T]) ->
    process_response(Call, T);
process_response(Call, [#xmlElement{name=Name, content=Content}=El|T]) ->
    lager:debug("hname: ~p, content: ~p", [Name, Content]),
    process_element(Call, Name, Content, El),
    process_response(Call, T);
process_response(Call, []) ->
    cf_exe:continue(Call).

process_element(Call, 'Gather', [#xmlText{}|T], El) ->
    process_element(Call, 'Gather', T, El);
process_element(Call, 'Gather', [#xmlElement{name=Name, content=Content}=El1|T], El) ->
    process_element(Call, Name, Content, El1),
    process_element(Call, 'Gather', T, El);
process_element(Call, 'Gather', [], #xmlElement{attributes=Attrs}) ->
    Props = attrs_to_proplist(Attrs),

    MaxDigitsBin = wh_util:to_binary(props:get_value(numDigits, Props)),
    MaxDigits = wh_util:to_integer(MaxDigitsBin),
    Timeout = wh_util:to_integer(props:get_value(timeout, Props), 5),

    case whapps_call_command:b_collect_digits(MaxDigitsBin, Timeout, Call) of
        {ok, DTMFs} when byte_size(DTMFs) =:= MaxDigits ->
            lager:debug("recv DTMFs: ~s", [DTMFs]),

            Uri = resolve_uri(whapps_call:kvs_fetch(voice_uri, Call), props:get_value(action, Props)),
            Method = props:get_value(method, Props),

            BaseParams = wh_json:from_list(req_params(Call)),

            send_req(Call, Uri, Method, BaseParams);
        {ok, _DTMFs} ->
            lager:debug("failed to collect ~b digits, got ~s", [MaxDigits, _DTMFs])
    end;

process_element(Call, 'Play', [#xmlText{value=PlayMe, type=text}], #xmlElement{attributes=Attrs}) ->
    lager:debug("PLAY: ~s", [PlayMe]),
    case props:get_value(loop, attrs_to_proplist(Attrs), 1) of
        0 ->
            %% play music in a loop
            whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call);
        1 ->
            whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call);
        N when N > 1 ->
            play_loop(Call, N, wh_util:to_binary(PlayMe))
    end;
process_element(Call, 'Say', [#xmlText{value=SayMe, type=text}], _El) ->
    lager:debug("SAY: ~s", [SayMe]),
    whapps_call_command:b_say(wh_util:to_binary(SayMe), Call).

play_loop(_, _, 0) ->
    ok;
play_loop(Call, PlayMe, N) ->
    whapps_call_command:b_play(wh_util:to_binary(PlayMe), Call),
    play_loop(Call, PlayMe, N-1).

attrs_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].

uri(URI, get, BaseParams) ->
    JSON_QS = wh_json:to_querystring(BaseParams),
    case mochiweb_util:urlsplit(wh_util:to_list(URI)) of
        {Scheme, Host, Path, [], Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, JSON_QS, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, [QS, "&", JSON_QS], Fragment})
    end;
uri(URI, post, _BaseParams) ->
    URI.

http_method(M) when is_atom(M) ->
    true = lists:member(M, ?SUPPORTED_METHODS),
    M;
http_method(M) when is_list(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower(M)));
http_method(M) when is_binary(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower_binary(M))).

req_params(Call) ->
    req_params(Call, <<"ringing">>).
req_params(Call, Status) ->
    [{<<"CallSid">>, whapps_call:call_id(Call)}
     ,{<<"AccountSid">>, whapps_call:account_id(Call)}
     ,{<<"From">>, whapps_call:from_user(Call)}
     ,{<<"To">>, whapps_call:to_user(Call)}
     ,{<<"CallStatus">>, Status}
     ,{<<"ApiVersion">>, <<"2010-04-01">>}
     ,{<<"Direction">>, <<"inbound">>}
     ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
    ].

resolve_uri(_Raw, [$h,$t,$t,$p|_]=Abs) -> Abs;
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(RawPath, Relative) ->
    PathTokensRev = lists:reverse(binary:split(wh_util:to_binary(RawPath), <<"/">>, [global])),
    UrlTokens = binary:split(wh_util:to_binary(Relative), <<"/">>),

    wh_util:join_binary(
      lists:reverse(
        lists:foldl(fun(<<"..">>, []) -> [];
                       (<<"..">>, [_ | PathTokens]) -> PathTokens;
                       (<<".">>, PathTokens) -> PathTokens;
                       (Segment, [LastToken|DirTokens]=PathTokens) ->
                            case filename:extension(LastToken) of
                                <<>> ->
                                    %% no extension, append Segment to Tokens
                                    [Segment | PathTokens];
                                _Ext ->
                                    %% Extension found, append Segment to DirTokens
                                    [Segment|DirTokens]
                            end
                    end, PathTokensRev, UrlTokens)
       ), <<"/">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_path_test() ->
    RawPath = <<"http://killio/script.php">>,
    Relative = <<"script2.php">>,
    RawPath1 = <<"http://killio/script2.php">>,

    ?assertEqual(RawPath1, resolve_uri(RawPath, Relative)),
    ?assertEqual(RawPath1, resolve_uri(RawPath, RawPath1)).
-endif.
