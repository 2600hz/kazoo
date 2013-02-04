%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handle an individual webhook request's lifecycle
%%% @end
%%% Created : 23 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_req).

-behaviour(gen_server).

%% API
-export([start_link/2, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          callback_uri = "" :: nonempty_string()
         ,callback_method = 'post' :: http_methods()
         ,callback_attempts = 'undefined' :: 'undefined' | non_neg_integer()
         ,bind_event = 'undefined' :: 'undefined' | hook_types()
         ,acct_id = 'undefined' :: 'undefined' | ne_binary()
         ,realm = 'undefined' :: 'undefined' | ne_binary()
         ,ctl_q = 'undefined' :: 'undefined' | ne_binary()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(JObj, Props) ->
    gen_listener:start_link(?MODULE, [{responders, [{?MODULE, [{<<"*">>, <<"*">>}]}]}
                                      ,{bindings, [{self, []}]}
                                     ], [JObj, Props]).

handle_req(JObj, Props) ->
    lager:debug("received another payload, sending to callback"),
    gen_listener:cast(props:get_value(server, Props), {event, JObj}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([JObj, Props]) ->
    wh_util:put_callid(JObj),

    Hook = props:get_value(hook, Props),

    gen_listener:cast(self(), {start_req, JObj}),

    lager:debug("started req handler for acct ~s: ~s", [props:get_value(acct_id, Props)
                                                 ,wh_json:get_value(<<"bind_event">>, Hook)
                                                ]),

    {ok, #state{callback_uri = wh_json:get_string_value(<<"callback_uri">>, Hook)
                ,callback_method = wh_json:get_atom_value(<<"callback_method">>, Hook)
                ,callback_attempts = attempts(wh_json:get_integer_value(<<"retries">>, Hook, 0))
                ,bind_event = wh_json:get_atom_value(<<"bind_event">>, Hook)
                ,acct_id = props:get_value(acct_id, Props)
                ,realm = props:get_value(realm, Props)
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({event, JObj}, #state{bind_event=route, ctl_q=undefined}=State) ->
    case wh_util:get_event_type(JObj) of
        {<<"dialplan">>, <<"route_win">>} ->
            true = wapi_route:win_v(JObj),
            CtlQ = wh_json:get_value(<<"Server-ID">>, JObj),
            lager:debug("route_win recv, ctl q: ~s", [CtlQ]),

            add_call_bindings(JObj),
            lager:debug("bound to call events"),

            handle_cast({send_event, JObj}, State#state{ctl_q=CtlQ});
        {_Cat, _Name} ->
            lager:debug("recv unexpected route event: ~s:~s", [_Cat, _Name]),
            {noreply, State, 5000}
    end;
handle_cast({event, JObj}, #state{bind_event=route}=State) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, _} ->
            lager:debug("send call event to webhook"),
            handle_cast({send_event, JObj}, State);
        {<<"call_detail">>, _} ->
            lager:debug("send cdr to webhook"),
            _ = handle_cast({send_event, JObj}, State),
            {stop, normal, State};
        {_Cat, _Name} ->
            lager:debug("recv unexpected route event: ~s:~s", [_Cat, _Name]),
            {noreply, State, 5000}
    end;
handle_cast({event, JObj}, #state{bind_event=authn}=State) ->
    true = wapi_registration:success_v(JObj),
    lager:debug("registration was a success"),
    handle_cast({send_event, JObj}, State);
handle_cast({event, JObj}, #state{bind_event=authz}=State) ->
    true = wapi_authz:win_v(JObj),
    lager:debug("authz_win recv"),
    handle_cast({send_event, JObj}, State);

handle_cast({send_event, JObj}, #state{bind_event=route, ctl_q=CtlQ}=State) ->
    {ok, JSON} = encode_event(route, JObj),
    case send_http_req(JSON, State) of
        {ok, RespJObj} ->
            send_amqp_event_resp(CtlQ, RespJObj),
            lager:debug("possibly sent amqp resp"),
            {noreply, State, 5000};
        {error, _E} ->
            lager:debug("failed to send event: ~p", [_E]),
            {noreply, State, 5000};
        ignore ->
            lager:debug("ignoring HTTP response"),
            {noreply, State, 5000}
    end;
handle_cast({send_event, JObj}, #state{bind_event=BindEvent}=State) ->
    {ok, JSON} = encode_event(BindEvent, JObj),
    _ = send_http_req(JSON, State),
    lager:debug("sent followup event to HTTP"),
    {stop, normal, State};

handle_cast({start_req, ReqJObj}, #state{bind_event=BindEvent}=State) ->
    case is_valid_req(ReqJObj, BindEvent) of
        false ->
            lager:debug("json failed validation for ~s", [BindEvent]),
            {stop, invalid_req, State};
        true ->
            {ok, JSON} = encode_req(BindEvent, wh_json:set_value(<<"Server-ID">>, <<>>, ReqJObj)),
            case send_http_req(JSON, State) of
                {ok, RespJObj} ->
                    Self = self(),
                    spawn(fun() ->
                                  wh_util:put_callid(ReqJObj),
                                  Q = gen_listener:queue_name(Self),
                                  add_followup_bindings(Self, BindEvent, ReqJObj, RespJObj),
                                  send_amqp_resp(wh_json:get_value(<<"Server-ID">>, ReqJObj)
                                                 ,wh_json:set_values([{<<"Server-ID">>, Q}
                                                                      ,{<<"App-Name">>, ?APP_NAME}
                                                                      ,{<<"App-Version">>, ?APP_VERSION}
                                                                     ], RespJObj)
                                                 ,BindEvent
                                                )
                          end),
                    {noreply, State, 5000};
                {error, {conn_failed, nxdomain}=E} ->
                    lager:info("failed to find domain of callback"),
                    %% email someone
                    %% disable the webhook
                    {stop, E, State};
                {error, E} ->
                    {stop, E, State};
                ignore ->
                    lager:debug("ignoring response, no answer for the request"),
                    {stop, normal, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    lager:debug("timed out waiting, going down"),
    {stop, normal, State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State, 5000}.

handle_event(_JObj, _State) ->
    {reply, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("terminating handler for reason: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_http_req(iolist() | ne_binary(), #state{}) -> 'ignore' |
                                                               {'ok', wh_json:json_object()} |
                                                               {'error', 'retries_exceeded' | term()}.
-spec send_http_req(iolist() | ne_binary(), #state{}, non_neg_integer()) -> 'ignore' |
                                                                                  {'ok', wh_json:json_object()} |
                                                                                  {'error', 'retries_exceeded' | term()}.
send_http_req(JSON, #state{callback_attempts=Attempts}=State) ->
    send_http_req(JSON, State, Attempts).

send_http_req(_, _, 0) ->
    {error, retries_exceeded};
send_http_req(JSON, State, Attempts) ->
    case send_http_req_once(JSON, State) of
        {ok, _}=OK -> OK;
        {error, retry} -> send_http_req(JSON, State, Attempts-1);
        {error, _E}=E -> E;
        ignore -> ignore
    end.

send_http_req_once(JSON, #state{callback_uri=CB_URI
                                ,callback_method=Method
                               }) ->
    URI = uri(Method, CB_URI, JSON),
    lager:debug("sending req to ~s using http ~s", [URI, Method]),
    lager:debug("req: ~s", [JSON]),

    case ibrowse:send_req(URI, ?DEFAULT_REQ_HEADERS, Method, JSON, ?DEFAULT_OPTS) of
        {ok, Status, RespHeaders, RespBody} ->
            lager:debug("resp: ~s", [RespBody]),
            process_resp(Status, RespHeaders, RespBody);
        {error, _E}=E ->
            lager:debug("failed to send request to ~s", [URI]),
            E
    end.

-spec uri(http_methods(), nonempty_string(), iolist() | ne_binary()) -> nonempty_string().
uri(get, URI, JSON) ->
    JSON_QS = wh_json:to_querystring(wh_json:decode(JSON)),
    case mochiweb_util:urlsplit(URI) of
        {Scheme, Host, Path, [], Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, JSON_QS, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, [QS, "&", JSON_QS], Fragment})
    end;
uri(_, URI, _) ->
    URI.

-spec encode_event(hook_types(), wh_json:json_object()) -> {'ok', iolist() | ne_binary()} | {'error', string() | 'unhandled_event'}.
encode_event(route, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, _} ->
            wapi_call:event(JObj);
        {<<"call_detail">>, _} ->
            wapi_call:cdr(JObj);
        {<<"dialplan">>, <<"route_win">>} ->
            wapi_route:win(JObj);
        {_Cat, _Name} ->
            lager:debug("unhandled route event: ~s:~s", [_Cat, _Name]),
            {error, unhandled_event}
    end;
encode_event(authn, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"directory">>, <<"reg_success">>} ->
            wapi_registration:success(JObj);
        {_Cat, _Name} ->
            lager:debug("unhandled authn event: ~s:~s", [_Cat, _Name]),
            {error, unhandled_event}
    end;
encode_event(authz, JObj) ->
    case wh_util:get_event_type(JObj) of
        {<<"dialplan">>, <<"authz_win">>} ->
            wapi_authz:win(JObj);
        {_Cat, _Name} ->
            lager:debug("unhandled authz event: ~s:~s", [_Cat, _Name]),
            {error, unhandled_event}
    end.

-spec process_resp(nonempty_string(), proplist(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', 'retry' | 'unsupported_content_type'} | 'ignore'.
process_resp(_, _, <<>>) ->
    lager:debug("no response to decode"),
    ignore;
process_resp([$2|_]=_Status, RespHeaders, RespBody) ->
    lager:debug("successful status ~s received", [_Status]),
    decode_to_json(props:get_value("Content-Type", RespHeaders), RespBody);
process_resp(_Status, _RespHeaders, _RespBody) ->
    lager:debug("failed status ~s received", [_Status]),
    {error, retry}.

%% Should convert from ContentType to a JSON binary (like xml -> Erlang XML -> Erlang JSON -> json)
-spec decode_to_json(nonempty_string(), ne_binary()) -> {'ok', wh_json:json_object()} | {'error', 'unsupported_content_type'}.
decode_to_json("application/json" ++ _, JSON) ->
    {ok, wh_json:decode(JSON)};
decode_to_json(_ContentType, _RespBody) ->
    lager:debug("unhandled content type: ~s", [_ContentType]),
    {error, unsupported_content_type}.

-spec encode_req(hook_types(), wh_json:json_object()) -> {'ok', iolist()} | {'error', string()}.
encode_req(BindEvent, JObj) ->
    case webhooks_util:api_call(BindEvent, fun(ApiMod) -> ApiMod:req(JObj) end) of
        {ok, _}=OK -> OK;
        {error, _}=Err -> Err
    end.

-spec is_valid_req(wh_json:json_object(), hook_types()) -> boolean().
is_valid_req(JObj, BindEvent) ->
    case webhooks_util:api_call(BindEvent, fun(ApiMod) -> ApiMod:req_v(JObj) end) of
        true -> true;
        _ -> false
    end.

-spec send_amqp_resp(ne_binary(), wh_json:json_object(), hook_types()) -> 'ok'.
send_amqp_resp(RespQ, RespJObj, route) ->
    {<<"dialplan">>, <<"route_resp">>} = wh_util:get_event_type(RespJObj),
    wapi_route:publish_resp(RespQ, RespJObj);
send_amqp_resp(RespQ, RespJObj, BindEvent) ->
    webhooks_util:api_call(BindEvent, fun(ApiMod) -> ApiMod:publish_resp(RespQ, RespJObj) end).

-spec send_amqp_event_resp(ne_binary(), wh_json:json_object()) -> 'ok'.
send_amqp_event_resp(RespQ, RespJObj) ->
    case wh_util:get_event_type(RespJObj) of
        {<<"dialplan">>, <<"command">>} ->
            lager:debug("publish dialplan command(s)"),
            wapi_dialplan:publish_command(RespQ, RespJObj);
        {_Cat, _Evt} ->
            lager:debug("unhandled route event: ~s: ~s", [_Cat, _Evt]),
            ok
    end.

attempts(N) when N =< 0 -> 1;
attempts(N) when N > 3 -> 3;
attempts(N) -> N.

add_followup_bindings(Srv, authn, ReqJObj, _RespJObj) ->
    lager:debug("listening for reg success"),
    gen_listener:add_binding(Srv, registration, [{user, wh_json:get_value(<<"Auth-User">>, ReqJObj)}
                                                 ,{realm, wh_json:get_value(<<"Auth-Realm">>, ReqJObj)}
                                                 ,{restrict_to, [reg_success]}
                                                ]);
add_followup_bindings(_, authz, _ReqJObj, _RespJObj) ->
    lager:debug("waiting for authz_win to come");
add_followup_bindings(_, route, _ReqJObj, _RespJObj) ->
    lager:debug("waiting for route_win to come").

add_call_bindings(JObj) ->
    CallID = wh_json:get_value(<<"Call-ID">>, JObj),
    lager:debug("listening for call events and cdr on ~s", [CallID]),
    gen_listener:add_binding(self(), call, [{callid, CallID}
                                            ,{restrict_to, [events, cdr]}
                                           ]).
