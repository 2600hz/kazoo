%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Accept third-party dialplan
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_pivot).

-include("../callflow.hrl").

-export([handle/2
         ,send_req/4
        ]).

-define(DEFAULT_OPTS, [{response_format, binary}]).

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
%%   req_format: string(), data format and payload expected for initial
%%     request (defaults to twiml for the moment),
%%     formats: twiml, kazoo
%%
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    BaseParams = wh_json:from_list(init_req_params(Data, Call)),

    Method = wht_util:http_method(wh_json:get_value(<<"http_method">>, Data, get)),
    VoiceUri = wh_json:get_value(<<"voice_url">>, Data),

    send_req(whapps_call:kvs_store(voice_uri, VoiceUri, Call), VoiceUri, Method, BaseParams).

-spec send_req/4 :: (whapps_call:call(), nonempty_string() | ne_binary(), 'get' | 'post', wh_json:json_object()) -> any().
send_req(Call, Uri, get, BaseParams) ->
    send(Call, uri(Uri, get, wh_json:to_querystring(BaseParams)), get, [], []);
send_req(Call, Uri, post, BaseParams) ->
    send(Call, Uri, post, [{"Content-Type", "application/x-www-form-urlencoded"}], wh_json:to_querystring(BaseParams)).

send(Call, Uri, Method, ReqHdrs, ReqBody) ->
    true = is_call_active(),

    lager:debug("sending req to ~s via ~s", [iolist_to_binary(Uri), Method]),

    case ibrowse:send_req(wh_util:to_list(Uri), ReqHdrs, Method, ReqBody, ?DEFAULT_OPTS) of
        {ok, "200", RespHdrs, RespBody} ->
            lager:debug("recv 200: ~s", [RespBody]),
            handle_resp(Call, RespHdrs, RespBody);
        {ok, "302", Hdrs, _RespBody} ->
            Redirect = props:get_value("Location", Hdrs),
            lager:debug("recv 302: redirect to ~s", [Redirect]),
            Redirect1 = wht_util:resolve_uri(Uri, Redirect),
            send(Call, Redirect1, Method, ReqHdrs, ReqBody);
        {ok, _RespCode, _Hdrs, _RespBody} ->
            lager:debug("recv other: ~s: ~s", [_RespCode, _RespBody]),
            lager:debug("other hrds: ~p", [_Hdrs]),
            cf_exe:continue(Call);
        {error, _Reason} ->
            lager:debug("error with req: ~p", [_Reason]),
            cf_exe:continue(Call)
    end.

%% scan our existing message queue for the channel hangup event
%% its possible that we could get stuck in here if there are lots of events
%% to process...a more robust solution is needed, maybe :)
-spec is_call_active/0 :: () -> boolean().
is_call_active() ->
    receive
        {amqp_msg, JObj} ->
            case whapps_util:get_event_type(JObj) of
                { <<"call_event">>, <<"CHANNEL_HANGUP">> } ->
                    lager:debug("hangup received, done here"),
                    false;
                _T -> is_call_active()
            end;
        _M ->
            is_call_active()
    after 0 -> true
    end.

handle_resp(Call, Hdrs, RespBody) ->
    CT = props:get_value("Content-Type", Hdrs),
    try wht_translator:exec(Call, wh_util:to_list(RespBody), CT) of
        {stop, Call1} ->
            lager:debug("translator says stop"),
            cf_exe:stop(Call1);
        {ok, Call1} ->
            lager:debug("translator says ok, continuing"),
            cf_exe:continue(Call1);
        {request, Call1, Uri, Method, Params} ->
            lager:debug("translator says make request to ~s", [Uri]),
            send_req(Call1, Uri, Method, Params)
    catch
        throw:{error, no_translators} ->
            lager:debug("unknown content type ~s, no translators", [CT]),
            cf_exe:continue(Call);
        throw:{error, unrecognized_cmds} ->
            lager:debug("no translators recognize the supplied commands: ~s", [RespBody]),
            cf_exe:continue(Call)
    end.

uri(URI, get, QueryString) ->
    case mochiweb_util:urlsplit(wh_util:to_list(URI)) of
        {Scheme, Host, Path, [], Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, [QS, "&", QueryString], Fragment})
    end.

init_req_params(Data, Call) ->
    FmtBin = <<"wht_", (wh_json:get_value(<<"req_format">>, Data, <<"twiml">>))/binary>>,
    try 
        FmtAtom = wh_util:to_atom(FmtBin),
        FmtAtom:req_params(Call)
    catch
        error:badarg ->
            case code:where_is_file(wh_util:to_list(<<FmtBin/binary, ".beam">>)) of
                non_existing -> [];
                _Path -> wh_util:to_atom(FmtBin, true), init_req_params(Data, Call)
            end;
        error:undef ->
            []
    end.
