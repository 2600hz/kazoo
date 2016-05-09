%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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

-define(SERVER, ?MODULE).

-type http_method() :: 'get' | 'post'.

-record(state, {voice_uri :: api_binary()
                ,cdr_uri :: api_binary()
                ,request_format = <<"twiml">> :: api_binary()
                ,method = 'get' :: http_method()
                ,call :: kapps_call:call()
                ,request_id :: kz_http:req_id()
                ,request_params :: kz_json:object()
                ,response_code :: ne_binary()
                ,response_headers :: binaries() | ne_binary()
                ,response_body = <<>> :: binary()
                ,response_content_type :: binary()
                ,response_pid :: pid() %% pid of the processing of the response
                ,response_event_handlers = [] :: pids()
                ,response_ref :: reference() %% monitor ref for the pid
                ,debug = 'false' :: boolean()
                ,requester_queue :: api_binary()
               }).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kapps_call:call(), kz_json:object()) -> startlink_ret().
start_link(Call, JObj) ->
    CallId = kapps_call:call_id(Call),

    gen_listener:start_link(?SERVER, [{'bindings', [{'call', [{'callid', CallId}]}
                                                    ,{'self', []}
                                                   ]}
                                      ,{'responders', [{{?MODULE, 'maybe_relay_event'}
                                                        ,[{<<"conference">>, <<"config_req">>}
                                                          ,{<<"resource">>, <<"offnet_resp">>}
                                                          ,{<<"call_event">>, <<"*">>}
                                                         ]
                                                       }
                                                      ]}
                                     ], [Call, JObj]).

-spec stop_call(pid(), kapps_call:call()) -> 'ok'.
stop_call(Srv, Call) -> gen_listener:cast(Srv, {'stop', Call}).

-spec new_request(pid(), ne_binary(), http_method()) -> 'ok'.
-spec new_request(pid(), ne_binary(), http_method(), kz_json:object()) -> 'ok'.
new_request(Srv, Uri, Method) ->
    gen_listener:cast(Srv, {'request', Uri, Method}).
new_request(Srv, Uri, Method, Params) ->
    gen_listener:cast(Srv, {'request', Uri, Method, Params}).

-spec updated_call(pid(), kapps_call:call()) -> 'ok'.
updated_call(Srv, Call) -> gen_listener:cast(Srv, {'updated_call', Call}).

-spec usurp_executor(pid()) -> 'ok'.
usurp_executor(Srv) -> gen_listener:cast(Srv, 'usurp').

-spec maybe_relay_event(kz_json:object(), kz_proplist()) -> 'ok'.
maybe_relay_event(JObj, Props) ->
    _ = case props:get_value('pid', Props) of
            P when is_pid(P) -> kapps_call_command:relay_event(P, JObj);
            _ -> 'ok'
        end,
    _ = case props:get_value('pids', Props) of
            [_|_]=Pids ->
                [kapps_call_command:relay_event(P, JObj) || P <- Pids];
            _ -> 'ok'
        end,
    relay_cdr_event(JObj, Props).

-spec relay_cdr_event(kz_json:object(), kz_proplist()) -> 'ok'.
relay_cdr_event(JObj, Props) ->
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
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
-spec init([kapps_call:call() | kz_json:object()]) -> {'ok', state(), 'hibernate'}.
init([Call, JObj]) ->
    kz_util:put_callid(kapps_call:call_id(Call)),

    Method = kzt_util:http_method(kz_json:get_value(<<"HTTP-Method">>, JObj, 'get')),
    VoiceUri = kz_json:get_value(<<"Voice-URI">>, JObj),

    ReqFormat = kz_json:get_value(<<"Request-Format">>, JObj, <<"twiml">>),
    BaseParams = kz_json:from_list(req_params(ReqFormat, Call)),

    lager:debug("starting pivot req to ~s to ~s", [Method, VoiceUri]),

    ?MODULE:new_request(self(), VoiceUri, Method, BaseParams),

    {'ok'
     ,#state{cdr_uri=kz_json:get_value(<<"CDR-URI">>, JObj)
             ,call=kapps_call:kvs_update_counter('pivot_counter', 1, Call)
             ,request_format=ReqFormat
             ,debug=kz_json:is_true(<<"Debug">>, JObj, 'false')
             ,requester_queue = kapps_call:controller_queue(Call)
            }
     ,'hibernate'
    }.

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
-spec handle_call(any(), pid_ref(), state()) -> {'reply', 'ok', state()}.
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
-spec handle_cast(any(), state()) -> {'noreply', state()} |
                                     {'stop', 'normal', state()}.
handle_cast('usurp', State) ->
    lager:debug("terminating pivot call because of usurp"),
    {'stop', 'normal', State#state{call='undefined'}};
handle_cast({'request', Uri, Method}, #state{call=Call
                                             ,request_format=ReqFormat
                                            }=State) ->
    handle_cast({'request', Uri, Method, req_params(ReqFormat, Call)}, State);
handle_cast({'request', Uri, Method, Params}, #state{call=Call
                                                     ,debug=Debug
                                                     ,requester_queue=Q
                                                    }=State) ->
    Call1 = kzt_util:set_voice_uri(Uri, Call),

    case send_req(Call1, Uri, Method, Params, Debug) of
        {'ok', ReqId, Call2} ->
            lager:debug("sent request ~p to '~s' via '~s'", [ReqId, Uri, Method]),
            {'noreply', State#state{request_id=ReqId
                                    ,request_params=Params
                                    ,response_content_type = <<>>
                                    ,response_body = <<>>
                                    ,method=Method
                                    ,voice_uri=Uri
                                    ,call=Call2
                                   }};
        _ ->
            kapi_pivot:publish_failed(Q, [{<<"Call-ID">>, kapps_call:call_id(Call)}
                                          | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                         ]),
            {'stop', 'normal', State}
    end;

handle_cast({'updated_call', Call}, State) ->
    {'noreply', State#state{call=Call}};

handle_cast({'gen_listener', {'created_queue', Q}}, #state{call=Call}=State) ->
    %% TODO: Block on waiting for controller queue
    {'noreply', State#state{call=kapps_call:set_controller_queue(Q, Call)}};

handle_cast({'stop', Call}, #state{cdr_uri='undefined'}=State) ->
    lager:debug("no cdr callback, terminating call"),
    kapps_call_command:hangup(Call),
    {'stop', 'normal', State};

handle_cast({'cdr', _JObj}, #state{cdr_uri='undefined'
                                   ,call=Call
                                  }=State) ->
    lager:debug("recv cdr for call, no cdr uri though"),
    erlang:send_after(3000, self(), {'stop', Call}),
    {'noreply', State};
handle_cast({'cdr', JObj}, #state{cdr_uri=Url
                                  ,call=Call
                                  ,debug=Debug
                                 }=State) ->
    JObj1 = kz_json:delete_key(<<"Custom-Channel-Vars">>, JObj),
    Body =  kz_json:to_querystring(kz_api:remove_defaults(JObj1)),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],

    maybe_debug_req(Call, Url, 'post', Headers, Body, Debug),

    case kz_http:post(kz_term:to_list(Url), Headers, Body) of
        {'ok', RespCode, RespHeaders, RespBody} ->
            maybe_debug_resp(Debug, Call, integer_to_binary(RespCode), RespHeaders, RespBody),
            lager:debug("recv ~p from cdr url ~s", [RespCode, Url]);
        {'error', _E} ->
            lager:debug("failed to send CDR: ~p", [_E])
    end,

    erlang:send_after(3000, self(), {'stop', Call}),
    {'noreply', State};

handle_cast({'add_event_handler', {Pid, _Ref}}, #state{response_event_handlers=Pids}=State) ->
    lager:debug("adding event handler ~p", [Pid]),
    {'noreply', State#state{response_event_handlers=[Pid | Pids]}};
handle_cast({'add_event_handler', Pid}, #state{response_event_handlers=Pids}=State) when is_pid(Pid) ->
    lager:debug("adding event handler ~p", [Pid]),
    {'noreply', State#state{response_event_handlers=[Pid | Pids]}};

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
-spec handle_info(any(), state()) -> {'noreply', state()} |
                                     {'stop', any(), state()}.
handle_info({'stop', _Call}, State) ->
    {'stop', 'normal', State};
handle_info({'http', {ReqId, 'stream_start', Hdrs}}
            ,#state{request_id=ReqId
                   }=State) ->
    RespHeaders = normalize_resp_headers(Hdrs),
    lager:debug("recv resp headers"),
    {'noreply', State#state{response_headers=RespHeaders}};

handle_info({'http', {ReqId, {'error', Error}}}
            ,#state{request_id=ReqId
                    ,response_body=_RespBody
                   }=State) ->
    lager:info("recv error ~p : collected: ~s", [Error, _RespBody]),
    {'noreply', State};

handle_info({'http', {ReqId, 'stream', Chunk}}
            ,#state{request_id=ReqId
                    ,response_body=RespBody
                   }=State) ->
    lager:info("adding response chunk: '~s'", [Chunk]),
    {'noreply', State#state{response_body = <<RespBody/binary, Chunk/binary>>}};

handle_info({'http', {ReqId, 'stream_end', FinalHeaders}}, #state{request_id=ReqId
                                                                  ,response_body=RespBody
                                                                  ,call=Call
                                                                  ,debug=Debug
                                                                  ,requester_queue=RequesterQ
                                                                 }=State) ->
    RespHeaders = normalize_resp_headers(FinalHeaders),
    maybe_debug_resp(Debug, Call, <<"200">>, RespHeaders, RespBody),
    HandleArgs = [RequesterQ
                  ,kzt_util:set_amqp_listener(self(), Call)
                  ,props:get_value(<<"content-type">>, RespHeaders)
                  ,RespBody
                 ],
    {Pid, Ref} = kz_util:spawn_monitor(fun handle_resp/4, HandleArgs),
    lager:debug("processing resp with ~p(~p)", [Pid, Ref]),
    {'noreply', State#state{request_id = 'undefined'
                            ,request_params = kz_json:new()
                            ,response_body = <<>>
                            ,response_content_type = <<>>
                            ,response_pid = Pid
                            ,response_ref = Ref
                           }
     ,'hibernate'};

handle_info({'http', {ReqId, {{_, StatusCode, _}, RespHeaders, _}}}
            ,#state{request_id=ReqId}=State)
  when (StatusCode - 400) < 100 ->
    lager:info("recv client failure status code ~p", [StatusCode]),
    {'noreply', State#state{
                  response_content_type=props:get_value(<<"content-type">>, RespHeaders)
                  ,response_code = integer_to_binary(StatusCode)
                  ,response_headers=RespHeaders
                 }
    };
handle_info({'http', {ReqId, {{_, StatusCode, _}, RespHeaders, _}}}
            ,#state{request_id=ReqId}=State)
  when (StatusCode - 500) < 100 ->
    lager:info("recv server failure status code ~p", [StatusCode]),
    {'noreply', State#state{
                  response_content_type=props:get_value(<<"content-type">>, RespHeaders)
                  ,response_code = integer_to_binary(StatusCode)
                  ,response_headers=RespHeaders
                 }
    };

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
-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
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
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{response_pid=Pid}) ->
    exit(Pid, 'kill'),
    lager:info("pivot call terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {'ok', NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec send_req(kapps_call:call(), ne_binary(), http_method(), kz_json:object() | kz_proplist(), boolean()) ->
                      {'ok', kz_http:req_id(), kapps_call:call()} |
                      {'stop', kapps_call:call()}.
send_req(Call, Uri, Method, BaseParams, Debug) when not is_list(BaseParams) ->
    send_req(Call, Uri, Method, kz_json:to_proplist(BaseParams), Debug);
send_req(Call, Uri, 'get', BaseParams, Debug) ->
    UserParams = kzt_translator:get_user_vars(Call),
    Params = kz_json:set_values(BaseParams, UserParams),
    UpdatedCall = kapps_call:kvs_erase(<<"digits_collected">>, Call),
    send(UpdatedCall, uri(Uri, kz_json:to_querystring(Params)), 'get', [], [], Debug);
send_req(Call, Uri, 'post', BaseParams, Debug) ->
    UserParams = kzt_translator:get_user_vars(Call),
    Params = kz_json:set_values(BaseParams, UserParams),
    UpdatedCall = kapps_call:kvs_erase(<<"digits_collected">>, Call),
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    send(UpdatedCall, Uri, 'post', Headers, kz_json:to_querystring(Params), Debug).

-spec send(kapps_call:call(), ne_binary(), http_method(), kz_proplist(), iolist(), boolean()) ->
                  {'ok', kz_http:req_id(), kapps_call:call()} |
                  {'stop', kapps_call:call()}.
send(Call, Uri, Method, ReqHdrs, ReqBody, Debug) ->
    lager:info("sending req to ~s(~s): ~s", [Uri, Method, iolist_to_binary(ReqBody)]),

    maybe_debug_req(Call, Uri, Method, ReqHdrs, ReqBody, Debug),

    case kz_http:async_req(self(), Method, kz_term:to_list(Uri), ReqHdrs, ReqBody) of
        {'http_req_id', ReqId} ->
            lager:debug("response coming in asynchronosly to ~p", [ReqId]),
            {'ok', ReqId, Call};
        {'error', _Reason} ->
            lager:debug("error with req: ~p", [_Reason]),
            {'stop', Call}
    end.

-spec normalize_resp_headers(kz_proplist()) -> kz_proplist().
normalize_resp_headers(Headers) ->
    [{kz_term:to_lower_binary(K), kz_term:to_binary(V)} || {K, V} <- Headers].

-spec handle_resp(api_binary(), kapps_call:call(), ne_binary(), binary()) -> 'ok'.
handle_resp(RequesterQ, Call, CT, RespBody) ->
    kz_util:put_callid(kapps_call:call_id(Call)),
    Srv = kzt_util:get_amqp_listener(Call),

    case process_resp(RequesterQ, Call, CT, RespBody) of
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

-spec process_resp(api_binary(), kapps_call:call(), list() | binary(), binary()) ->
                          {'stop', kapps_call:call()} |
                          {'ok', kapps_call:call()} |
                          {'request', kapps_call:call()} |
                          {'usurp', kapps_call:call()}.
process_resp(_, Call, _, <<>>) ->
    lager:debug("no response body, finishing up"),
    {'stop', Call};
process_resp(RequesterQ, Call, Hdrs, RespBody) when is_list(Hdrs) ->
    handle_resp(RequesterQ, Call, props:get_value(<<"content-type">>, Hdrs), RespBody);
process_resp(RequesterQ, Call, CT, RespBody) ->
    lager:info("finding translator for content type ~s", [CT]),
    try kzt_translator:exec(Call, kz_term:to_list(RespBody), CT) of
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
            lager:info("translator has been usurped"),
            U;
        {'error', Call1} ->
            lager:debug("error in translator, FAIL"),
            {'stop', Call1}
    catch
        'throw':{'error', 'no_translators', _CT} ->
            lager:info("unknown content type ~s, no translators", [_CT]),
            {'stop', Call};
        'throw':{'error', 'unrecognized_cmds'} ->
            lager:info("no translators recognize the supplied commands: ~s", [RespBody]),
            kapi_pivot:publish_failed(RequesterQ, [{<<"Call-ID">>, kapps_call:call_id(Call)}
                                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                                  ]),
            {'stop', Call}
    end.

-spec uri(ne_binary(), iolist()) -> ne_binary().
uri(URI, QueryString) ->
    case kz_http_util:urlsplit(URI) of
        {Scheme, Host, Path, <<>>, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, QueryString, Fragment});
        {Scheme, Host, Path, QS, Fragment} ->
            kz_http_util:urlunsplit({Scheme, Host, Path, <<QS/binary, "&", QueryString/binary>>, Fragment})
    end.

-spec req_params(ne_binary(), kapps_call:call()) -> kz_proplist().
req_params(Format, Call) ->
    FmtAtom = kz_term:to_atom(<<"kzt_", Format/binary>>, 'true'),
    try FmtAtom:req_params(Call) of
        Result ->
            lager:debug("get req params from ~s", [FmtAtom]),
            Result
    catch
        'error':'undef' -> []
    end.

-spec maybe_debug_req(kapps_call:call(), binary(), atom(), kz_proplist(), iolist(), boolean()) -> 'ok'.
maybe_debug_req(_Call, _Uri, _Method, _ReqHdrs, _ReqBody, 'false') -> 'ok';
maybe_debug_req(Call, Uri, Method, ReqHdrs, ReqBody, 'true') ->
    Headers = kz_json:from_list([{fix_value(K), fix_value(V)} || {K, V} <- ReqHdrs]),
    store_debug(Call, [{<<"uri">>, iolist_to_binary(Uri)}
                       ,{<<"method">>, kz_term:to_binary(Method)}
                       ,{<<"req_headers">>, Headers}
                       ,{<<"req_body">>, iolist_to_binary(ReqBody)}
                       ,{<<"iteration">>, kapps_call:kvs_fetch('pivot_counter', Call)}
                      ]).

-spec maybe_debug_resp(boolean(), kapps_call:call(), ne_binary(), kz_proplist(), binary()) -> 'ok'.
maybe_debug_resp('false', _Call, _StatusCode, _RespHeaders, _RespBody) -> 'ok';
maybe_debug_resp('true', Call, StatusCode, RespHeaders, RespBody) ->
    Headers = kz_json:from_list([{fix_value(K), fix_value(V)} || {K, V} <- RespHeaders]),
    store_debug(
        Call
        ,[{<<"resp_status_code">>, StatusCode}
          ,{<<"resp_headers">>, Headers}
          ,{<<"resp_body">>, RespBody}
          ,{<<"iteration">>, kapps_call:kvs_fetch('pivot_counter', Call)}
        ]
    ).

-spec store_debug(kapps_call:call(), kz_proplist()) -> 'ok'.
store_debug(Call, Doc) ->
    AccountModDb = kz_util:format_account_mod_id(kapps_call:account_id(Call)),
    JObj =
        kz_doc:update_pvt_parameters(
            kz_json:from_list([{<<"call_id">>, kapps_call:call_id(Call)} | Doc])
            ,AccountModDb
            ,[{'account_id', kapps_call:account_id(Call)}
              ,{'account_db', AccountModDb}
              ,{'type', <<"pivot_debug">>}
              ,{'now', kz_util:current_tstamp()}
            ]
        ),
    case kazoo_modb:save_doc(AccountModDb, JObj) of
        {'ok', _Saved} ->
            lager:debug("saved debug doc: ~p", [_Saved]);
        {'error', _E} ->
            lager:debug("failed to save debug doc: ~p", [_E])
    end.

-spec fix_value(number() | list()) -> number() | ne_binary().
fix_value(N) when is_number(N) -> N;
fix_value(O) -> kz_term:to_lower_binary(O).
