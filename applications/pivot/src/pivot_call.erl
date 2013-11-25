%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% Handle processing of the pivot call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(pivot_call).

-behaviour(gen_listener).

%% API
-export([start_link/2
         ,handle_resp/3
         ,maybe_relay_event/2
         ,stop_call/2
         ,new_request/3, new_request/4
         ,updated_call/2
         ,usurp_executor/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("pivot.hrl").

-define(DEFAULT_OPTS, [{'response_format', 'binary'}]).

-type http_method() :: 'get' | 'post'.

-record(state, {
          voice_uri :: api_binary()
         ,cdr_uri :: api_binary()
         ,request_format = <<"twiml">> :: api_binary()
         ,method = 'get' :: http_method()
         ,call :: whapps_call:call()
         ,request_id :: ibrowse_req_id()
         ,request_params :: wh_json:object()
         ,response_body :: binary()
         ,response_content_type :: binary()
         ,response_pid :: pid() %% pid of the processing of the response
         ,response_event_handlers = [] :: pids()
         ,response_ref :: reference() %% monitor ref for the pid
         }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {'ok', Pid} | ignore | {'error', Error}
%% @end
%%--------------------------------------------------------------------
start_link(Call, JObj) ->
    CallId = whapps_call:call_id(Call),

    gen_listener:start_link(?MODULE, [{'bindings', [{'call', [{'callid', CallId}
                                                              ,{'restrict_to', ['events', 'cdr']}
                                                             ]}
                                                    ,{'self', []}
                                                   ]}
                                      ,{'responders', [{{?MODULE, 'maybe_relay_event'}
                                                        ,[{<<"conference">>, <<"config_req">>}
                                                          ,{<<"call_event">>, <<"*">>}
                                                          ,{<<"resource">>, <<"offnet_resp">>}
                                                          ,{<<"call_detail">>, <<"cdr">>}
                                                         ]
                                                       }
                                                      ]}
                                     ], [Call, JObj]).

-spec stop_call(pid(), whapps_call:call()) -> 'ok'.
stop_call(Srv, Call) -> gen_listener:cast(Srv, {'stop', Call}).

-spec new_request(pid(), ne_binary(), http_method()) -> 'ok'.
-spec new_request(pid(), ne_binary(), http_method(), wh_json:object()) -> 'ok'.
new_request(Srv, Uri, Method) ->
    gen_listener:cast(Srv, {'request', Uri, Method}).
new_request(Srv, Uri, Method, Params) ->
    gen_listener:cast(Srv, {'request', Uri, Method, Params}).

-spec updated_call(pid(), whapps_call:call()) -> 'ok'.
updated_call(Srv, Call) -> gen_listener:cast(Srv, {'updated_call', Call}).

-spec usurp_executor(pid()) -> 'ok'.
usurp_executor(Srv) -> gen_listener:cast(Srv, 'usurp').

-spec maybe_relay_event(wh_json:object(), wh_proplist()) -> 'ok'.
maybe_relay_event(JObj, Props) ->
    case props:get_value('pid', Props) of
        P when is_pid(P) -> whapps_call_command:relay_event(P, JObj);
        _ -> 'ok'
    end,
    case props:get_value('pids', Props) of
        [_|_]=Pids ->
            [whapps_call_command:relay_event(P, JObj) || P <- Pids];
        _ -> 'ok'
    end,

    relay_cdr_event(JObj, Props).

relay_cdr_event(JObj, Props) ->
    case wh_util:get_event_type(JObj) of
        {<<"call_detail">>, <<"cdr">>} ->
            Pid = proplists:get_value('server', Props),
            gen_listener:cast(Pid, {'cdr', JObj});
        _ -> 'ok'
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {'ok', State} |
%%                     {'ok', State, Timeout} |
%%                     ignore |
%%                     {'stop', Reason}
%% @end
%%--------------------------------------------------------------------
init([Call, JObj]) ->
    put('callid', whapps_call:call_id(Call)),

    Method = kzt_util:http_method(wh_json:get_value(<<"HTTP-Method">>, JObj, 'get')),
    VoiceUri = wh_json:get_value(<<"Voice-URI">>, JObj),

    ReqFormat = wh_json:get_value(<<"Request-Format">>, JObj, <<"twiml">>),
    BaseParams = wh_json:from_list(req_params(ReqFormat, Call)),

    lager:debug("starting pivot req to ~s to ~s", [Method, VoiceUri]),

    ?MODULE:new_request(self(), VoiceUri, Method, BaseParams),

    {'ok', #state{cdr_uri=wh_json:get_value(<<"CDR-URI">>, JObj)
                  ,call=Call
                  ,request_format=ReqFormat
                 }
     ,'hibernate'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {'reply', Reply, State} |
%%                                   {'reply', Reply, State, Timeout} |
%%                                   {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, Reply, State} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', 'ok', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {'noreply', State} |
%%                                  {'noreply', State, Timeout} |
%%                                  {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('usurp', State) ->
    lager:debug("terminating pivot call because of usurp"),
    {'stop', 'normal', State#state{call='undefined'}};
handle_cast({'request', Uri, Method}, #state{call=Call
                                             ,request_format=ReqFormat
                                            }=State) ->
    handle_cast({'request', Uri, Method, req_params(ReqFormat, Call)}, State);
handle_cast({'request', Uri, Method, Params}, #state{call=Call}=State) ->
    Call1 = kzt_util:set_voice_uri(Uri, Call),

    {'ok', ReqId, Call2} = send_req(Call1, Uri, Method, Params),
    lager:debug("sent request ~p to '~s' via '~s'", [ReqId, Uri, Method]),
    {'noreply', State#state{request_id=ReqId
                            ,request_params=Params
                            ,response_content_type = <<>>
                            ,response_body = <<>>
                            ,method=Method
                            ,voice_uri=Uri
                            ,call=Call2
                           }};

handle_cast({'updated_call', Call}, State) ->
    {'noreply', State#state{call=Call}};

handle_cast({'gen_listener', {'created_queue', Q}}, #state{call=Call}=State) ->
    %% TODO: Block on waiting for controller queue
    {'noreply', State#state{call=whapps_call:set_controller_queue(Q, Call)}};

handle_cast({'stop', Call}, #state{cdr_uri='undefined'}=State) ->
    lager:debug("no cdr callback, server going down"),
    _ = whapps_call_command:hangup(Call),
    {'stop', 'normal', State};

handle_cast({'cdr', JObj}, #state{cdr_uri=Url}=State) when Url =/= 'undefined'->
    JObj1 = wh_json:delete_key(<<"Custom-Channel-Vars">>, JObj),
    Body =  wh_json:to_querystring(wh_api:remove_defaults(JObj1)),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    _R = ibrowse:send_req(wh_util:to_list(Url), Headers, 'post', Body),
    lager:debug("cdr callback resp from server: ~p", [_R]),
    {'stop', 'normal', State};

handle_cast({'add_event_handler', {Pid, _Ref}}, #state{response_event_handlers=Pids}=State) ->
    lager:debug("adding event handler ~p", [Pid]),
    {'noreply', State#state{response_event_handlers=[Pid | Pids]}};
handle_cast({'add_event_handler', Pid}, #state{response_event_handlers=Pids}=State) when is_pid(Pid) ->
    lager:debug("adding event handler ~p", [Pid]),
    {'noreply', State#state{response_event_handlers=[Pid | Pids]}};

handle_cast({'wh_amqp_channel',{'new_channel',_IsNew}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Req, State) ->
    lager:debug("unhandled cast: ~p", [_Req]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {'noreply', State} |
%%                                   {'noreply', State, Timeout} |
%%                                   {'stop', Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'ibrowse_async_headers', ReqId, "200", RespHeaders}
            ,#state{request_id=ReqId}=State) ->
    CT = wh_util:to_binary(props:get_value("Content-Type", RespHeaders)),
    lager:debug("recv 200 response, content-type: ~s", [CT]),
    {'noreply', State#state{response_content_type=CT}};

handle_info({'ibrowse_async_headers', ReqId, "302", RespHeaders}
            ,#state{voice_uri=Uri
                    ,method=Method
                    ,request_id=ReqId
                    ,request_params=Params
                   }=State) ->
    Redirect = props:get_value("Location", RespHeaders),
    lager:debug("recv 302: redirect to ~s", [Redirect]),
    Redirect1 = kzt_util:resolve_uri(Uri, Redirect),

    ?MODULE:new_request(self(), Redirect1, Method, Params),
    {'noreply', State};
handle_info({'ibrowse_async_headers', ReqId, "4"++ StatusCode, _RespHeaders}
            ,#state{request_id=ReqId}=State) ->
    lager:debug("recv client failure status code 4~s", [StatusCode]),
    {'noreply', State};
handle_info({'ibrowse_async_headers', ReqId, "5"++ StatusCode, _RespHeaders}
            ,#state{request_id=ReqId}=State) ->
    lager:debug("recv server failure status code 5~s", [StatusCode]),
    {'noreply', State};

handle_info({'ibrowse_async_response', ReqId, {'error', 'connection_closed'}}
            ,#state{request_id=ReqId
                    ,response_body=_RespBody
                   }=State) ->
    lager:debug("connection closed unexpectedly: collected: ~s", [_RespBody]),
    {'noreply', State};
handle_info({'ibrowse_async_response', ReqId, Chunk}
            ,#state{request_id=ReqId
                    ,response_body=RespBody
                   }=State) ->
    lager:debug("adding response chunk: '~s'", [Chunk]),
    {'noreply', State#state{response_body = <<RespBody/binary, Chunk/binary>>}};

handle_info({'ibrowse_async_response_end', ReqId}, #state{request_id=ReqId
                                                          ,response_body=RespBody
                                                          ,response_content_type=CT
                                                          ,call=Call
                                                         }=State) ->
    Self = self(),

    {Pid, Ref} = spawn_monitor(?MODULE, 'handle_resp', [kzt_util:set_amqp_listener(Self, Call)
                                                        ,CT
                                                        ,RespBody
                                                       ]),
    lager:debug("processing resp with ~p(~p)", [Pid, Ref]),
    {'noreply', State#state{request_id = 'undefined'
                            ,request_params = wh_json:new()
                            ,response_body = <<>>
                            ,response_content_type = <<>>
                            ,response_pid = Pid
                            ,response_ref = Ref
                           }
     ,'hibernate'};

handle_info({'DOWN', Ref, 'process', Pid, Reason}, #state{response_pid=Pid
                                                          ,response_ref=Ref
                                                         }=State) ->
    lager:debug("response pid ~p(~p) down: ~p", [Pid, Ref, Reason]),
    {'noreply', State#state{response_pid='undefined'}, 'hibernate'};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling messaging bus events
%%
%% @spec handle_event(JObj, State) -> {'noreply', proplist()} |
%%                                    ignore
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, #state{response_pid=Pid
                           ,response_event_handlers=Pids
                          }) ->
    {'reply', [{'pid', Pid}
               ,{'pids', Pids}
              ]}.

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
terminate(_Reason, #state{call=Call}) ->
    _ = whapps_call_command:hangup(Call),
    lager:debug("pivot call terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec send_req(whapps_call:call(), ne_binary(), http_method(), wh_json:object() | wh_proplist()) ->
                      'ok' |
                      {'ok', ibrowse_req_id()} |
                      {'stop', whapps_call:call()}.
send_req(Call, Uri, 'get', BaseParams) ->
    UserParams = kzt_translator:get_user_vars(Call),
    Params = wh_json:set_values(wh_json:to_proplist(BaseParams), UserParams),
    UpdatedCall = whapps_call:kvs_erase(<<"digits_collected">>, Call),
    send(UpdatedCall, uri(Uri, wh_json:to_querystring(Params)), 'get', [], []);
send_req(Call, Uri, 'post', BaseParams) when is_list(BaseParams) ->
    UserParams = kzt_translator:get_user_vars(Call),
    Params = wh_json:set_values(BaseParams, UserParams),
    UpdatedCall = whapps_call:kvs_erase(<<"digits_collected">>, Call),
    send(UpdatedCall, Uri, 'post', [{"Content-Type", "application/x-www-form-urlencoded"}], wh_json:to_querystring(Params));
 send_req(Call, Uri, 'post', BaseParams) ->
    send_req(Call, Uri, 'post', wh_json:to_proplist(BaseParams)).

-spec send(whapps_call:call(), iolist(), atom(), wh_proplist(), iolist()) ->
                        'ok' |
                        {'ok', ibrowse_req_id()} |
                        {'stop', whapps_call:call()}.
send(Call, Uri, Method, ReqHdrs, ReqBody) ->
    lager:debug("sending req to ~s(~s): ~s", [iolist_to_binary(Uri), Method, iolist_to_binary(ReqBody)]),

    Opts = [{'stream_to', self()}
            | ?DEFAULT_OPTS
           ],

    case ibrowse:send_req(wh_util:to_list(Uri), ReqHdrs, Method, ReqBody, Opts) of
        {'ibrowse_req_id', ReqId} ->
            lager:debug("response coming in asynchronosly to ~p", [ReqId]),
            {'ok', ReqId, Call};
        {'ok', "200", RespHdrs, RespBody} ->
            lager:debug("recv 200: ~s", [RespBody]),
            handle_resp(Call, RespHdrs, RespBody);
        {'ok', "302", Hdrs, _RespBody} ->
            Redirect = props:get_value("Location", Hdrs),
            lager:debug("recv 302: redirect to ~s", [Redirect]),
            Redirect1 = kzt_util:resolve_uri(Uri, Redirect),
            send(Call, Redirect1, Method, ReqHdrs, ReqBody);
        {'ok', _RespCode, _Hdrs, _RespBody} ->
            lager:debug("recv other: ~s: ~s", [_RespCode, _RespBody]),
            lager:debug("other hrds: ~p", [_Hdrs]),
            {'stop', Call};
        {'error', {'conn_failed', {'error', 'econnrefused'}}} ->
            lager:debug("connection to host refused, going down"),
            {'stop', Call};
        {'error', _Reason} ->
            lager:debug("error with req: ~p", [_Reason]),
            {'stop', Call}
    end.

-spec handle_resp(whapps_call:call(), ne_binary(), binary()) -> 'ok'.
handle_resp(Call, CT, RespBody) ->
    put('callid', whapps_call:call_id(Call)),
    Srv = kzt_util:get_amqp_listener(Call),

    case process_resp(Call, CT, RespBody) of
        {'stop', Call1} -> ?MODULE:stop_call(Srv, Call1);
        {'ok', Call1} -> ?MODULE:stop_call(Srv, Call1);
        {'usurp', _Call1} -> ?MODULE:usurp_executor(Srv);
        {'request', Call1} ->
            ?MODULE:updated_call(Srv, Call1),
            ?MODULE:new_request(Srv
                                ,kzt_util:get_voice_uri(Call1)
                                ,kzt_util:get_voice_uri_method(Call1)
                               )
    end.

process_resp(Call, _, <<>>) ->
    lager:debug("no response body, finishing up"),
    {'stop', Call};
process_resp(Call, Hdrs, RespBody) when is_list(Hdrs) ->
    handle_resp(Call, props:get_value("Content-Type", Hdrs), RespBody);
process_resp(Call, CT, RespBody) ->
    try kzt_translator:exec(Call, wh_util:to_list(RespBody), CT) of
        {'stop', _Call1}=Stop ->
            lager:debug("translator says stop"),
            Stop;
        {'ok', _Call1}=OK ->
            lager:debug("translator says ok, continuing"),
            OK;
        {'request', _Call1}=Req ->
            lager:debug("translator says make another request"),
            Req;
        {'usurp', _Call1}=U ->
            lager:debug("translator has been usurped"),
            U;
        {'error', Call1} ->
            lager:debug("error in translator, FAIL"),
            {'stop', Call1}
    catch
        'throw':{'error', 'no_translators', _CT} ->
            lager:debug("unknown content type ~s, no translators", [_CT]),
            {'stop', Call};
        'throw':{'error', 'unrecognized_cmds'} ->
            lager:debug("no translators recognize the supplied commands: ~s", [RespBody]),
            {'stop', Call}
    end.

-spec uri(ne_binary(), iolist()) -> iolist().
uri(URI, QueryString) ->
    case mochiweb_util:urlsplit(wh_util:to_list(URI)) of
        {Scheme, Host, Path, [], Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            mochiweb_util:urlunsplit({Scheme, Host, Path, [QS, "&", QueryString], Fragment})
    end.

-spec req_params(ne_binary(), whapps_call:call()) -> wh_proplist().
req_params(Format, Call) ->
    FmtAtom = wh_util:to_atom(<<"kzt_", Format/binary>>, 'true'),
    try FmtAtom:req_params(Call) of
        Result ->
            lager:debug("get req params from ~s", [FmtAtom]),
            Result
    catch
        'error':'undef' -> []
    end.
