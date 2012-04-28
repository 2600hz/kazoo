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

-export([handle/2
         ,send_req/4
         ,req_params/1, req_params/2
         ,http_method/1, resolve_uri/2
        ]).

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
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    whapps_call_command:answer(Call),

    BaseParams = wh_json:from_list(req_params(Call)),

    Method = http_method(wh_json:get_value(<<"http_method">>, Data, get)),
    VoiceUri = wh_json:get_value(<<"voice_url">>, Data),

    send_req(whapps_call:kvs_store(voice_uri, VoiceUri, Call), VoiceUri, Method, BaseParams).

-spec send_req/4 :: (whapps_call:call(), nonempty_string() | ne_binary(), 'get' | 'post', wh_json:json_object()) -> any().
send_req(Call, Uri, get, BaseParams) ->
    send(Call, uri(Uri, get, wh_json:to_querystring(BaseParams)), get, [], []);
send_req(Call, Uri, post, BaseParams) ->
    send(Call, Uri, post, [{"Content-Type", "application/x-www-form-urlencoded"}], wh_json:to_querystring(BaseParams)).

send(Call, Uri, Method, ReqHdrs, ReqBody) ->
    lager:debug("sending req to ~s via ~s", [iolist_to_binary(Uri), Method]),

    case ibrowse:send_req(wh_util:to_list(Uri), ReqHdrs, Method, ReqBody, ?DEFAULT_OPTS) of
        {ok, "200", RespHdrs, RespBody} ->
            lager:debug("recv 200: ~s", [RespBody]),
            handle_resp(Call, RespHdrs, RespBody);
        {ok, "302", Hdrs, _RespBody} ->
            Redirect = props:get_value("Location", Hdrs),
            lager:debug("recv 302: redirect to ~s", [Redirect]),
            Redirect1 = resolve_uri(Uri, Redirect),
            send(Call, Redirect1, Method, ReqHdrs, ReqBody);
        {ok, _RespCode, _Hdrs, _RespBody} ->
            lager:debug("recv other: ~s: ~s", [_RespCode, _RespBody]),
            lager:debug("other hrds: ~p", [_Hdrs]),
            cf_exe:continue(Call);
        {error, _Reason} ->
            lager:debug("error with req: ~p", [_Reason]),
            cf_exe:continue(Call)
    end.

handle_resp(Call, Hdrs, RespBody) ->
    case props:get_value("Content-Type", Hdrs) of
        "text/xml" ->
            cf_twiml:process_call(Call, xmerl_scan:string(wh_util:to_list(RespBody)));
        _CT ->
            lager:debug("unknown content type response: ~s", [_CT]),
            cf_exe:continue(Call)
    end.

-spec req_params/1 :: (whapps_call:call()) -> proplist().
-spec req_params/2 :: (whapps_call:call(), ne_binary()) -> proplist().
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

http_method(M) when is_atom(M) ->
    true = lists:member(M, ?SUPPORTED_METHODS),
    M;
http_method(M) when is_list(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower_string(M)));
http_method(M) when is_binary(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower_binary(M))).

uri(URI, get, QueryString) ->
    case mochiweb_util:urlsplit(wh_util:to_list(URI)) of
        {Scheme, Host, Path, [], Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, [QS, "&", QueryString], Fragment})
    end.

-spec resolve_uri/2 :: (nonempty_string() | ne_binary(), nonempty_string() | ne_binary() | 'undefined') -> ne_binary().
resolve_uri(Raw, undefined) -> wh_util:to_binary(Raw);
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
