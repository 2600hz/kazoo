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

-export([init/0, handle/2]).

init() ->
    ok = wh_util:ensure_started(public_key),
    ok = wh_util:ensure_started(ssl),
    ok = wh_util:ensure_started(lhttpc).

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
    init(),

    BaseParams = wh_json:from_list([{<<"CallSid">>, whapps_call:call_id(Call)}
                                    ,{<<"AccountSid">>, whapps_call:account_id(Call)}
                                    ,{<<"From">>, whapps_call:from_user(Call)}
                                    ,{<<"To">>, whapps_call:to_user(Call)}
                                    ,{<<"CallStatus">>, <<"ringing">>}
                                    ,{<<"ApiVersion">>, <<"2010-04-01">>}
                                    ,{<<"Direction">>, <<"inbound">>}
                                    ,{<<"CallerName">>, whapps_call:caller_id_name(Call)}
                                   ]),

    Method = wh_util:to_atom(wh_json:get_value(<<"http_method">>, Data, get)),
    Uri = uri(wh_json:get_value(<<"voice_url">>, Data), Method, BaseParams),
    ReqTimeout = wh_json:get_integer_value(<<"req_timeout">>, Data, 5),

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
process_response(Call, [#xmlElement{name=Name, content=Content}|T]) ->
    lager:debug("hname: ~p, content: ~p", [Name, Content]),
    process_element(Call, Name, Content),
    process_response(Call, T);
process_response(Call, []) ->
    cf_exe:continue(Call).

process_element(Call, 'Say', [#xmlText{value=SayMe, type=text}]) ->
    lager:debug("SAY: ~s", [SayMe]),
    whapps_call_command:b_say(SayMe, Call),
    ok.

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
