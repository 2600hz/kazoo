%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%% Handles starting/stopping a call recording
%%%
%%% "data":{
%%%   "action":["start","stop"] // one of these
%%%   ,"time_limit":600 // in seconds, how long to record the call
%%%   ,"format":["mp3","wav"] // what format to store the recording in
%%%   ,"url":"http://server.com/path/to/dump/file" // what URL to PUT the file to
%%%   ,"record_on_answer": boolean() // whether to delay the start of the recording
%%% }
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_recording).

-export([start_link/2
         ,start_recording/2
         ,handle_call_event/2

         ,get_timelimit/1
         ,get_format/1
         ,get_url/1
         ,get_media_name/2
         ,should_store_recording/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("whistle_media.hrl").

-record(state, {url                        :: ne_binary()
                ,format                    :: ne_binary()
                ,media_name                :: ne_binary()
                ,call                      :: whapps_call:call()
                ,record_on_answer          :: boolean()
                ,time_limit                :: pos_integer()
                ,store_attempted = 'false' :: boolean()
                ,is_recording = 'false'  :: boolean()
                ,channel_status_ref        :: reference() | 'undefined'
                ,time_limit_ref            :: reference() | 'undefined'
               }).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'call', [{'callid', CallId}
                                     ,{'restrict_to', ['CHANNEL_ANSWER', 'CHANNEL_DESTROY'
                                                       ,'CHANNEL_BRIDGE', 'CHANNEL_EXECUTE_COMPLETE'
                                                       ,'RECORD_START', 'RECORD_STOP'
                                                      ]}
                                    ]}
                           ,{'self', []}
                          ]).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).


-spec start_link(whapps_call:call(), wh_json:object()) -> startlink_ret().
start_link(Call, Data) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call, Data]).

-spec start_recording(whapps_call:call(), wh_json:object()) -> no_return().
start_recording(Call, Data) ->
    {'ok', State} = init([Call, Data]),
    gen_listener:enter_loop(?MODULE, [{'bindings', ?BINDINGS(whapps_call:call_id(Call))}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], State).

handle_call_event(JObj, Props) ->
    wh_util:put_callid(JObj),
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            lager:debug("channel bridge maybe start recording"),
            gen_listener:cast(props:get_value('server', Props), 'maybe_start_recording');
        {<<"call_event">>, <<"RECORD_START">>} ->
            lager:debug("record_start event recv'd"),
            gen_listener:cast(props:get_value('server', Props), 'record_start');
        {<<"call_event">>, <<"RECORD_STOP">>} ->
            lager:debug("record_stop event recv'd"),
            gen_listener:cast(props:get_value('server', Props), 'store_recording');
        {<<"call_event">>, <<"CHANNEL_DESTROY">>} ->
            gen_listener:cast(props:get_value('server', Props), 'stop_call');
        {<<"call_event">>, <<"channel_status_resp">>} ->
            gen_listener:cast(props:get_value('server', Props), {'channel_status', wh_json:get_value(<<"Status">>, JObj)});
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>} ->
            case {wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Application-Response">>, JObj)} of
                {<<"store">>, <<"failure">>} ->
                    gen_listener:cast(props:get_value('server', Props), 'store_failed');
                {<<"store">>, <<"success">>} ->
                    gen_listener:cast(props:get_value('server', Props), 'store_succeeded');
                {_App, _Res} -> lager:debug("ignore exec complete: ~s: ~s", [_App, _Res])
            end;
        {<<"error">>, <<"dialplan">>} ->
            case wh_json:get_value([<<"Request">>, <<"Application-Name">>], JObj) of
                <<"store">> ->
                    gen_listener:cast(props:get_value('server', Props), 'store_failed');
                _App ->
                    lager:debug("ignore dialplan error: ~s", [_App])
            end;
        {_, _Evt} -> lager:debug("ignore event ~p", [_Evt])
    end.

-spec init(list()) -> {'ok', state()}.
init([Call, Data]) ->
    whapps_call:put_callid(Call),
    lager:info("starting event listener for record_call"),

    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    TimeLimit = get_timelimit(wh_json:get_integer_value(<<"time_limit">>, Data)),
    RecordOnAnswer = wh_json:is_true(<<"record_on_answer">>, Data, 'false'),

    {'ok', #state{url=get_url(Data)
                  ,format=Format
                  ,media_name=get_media_name(whapps_call:call_id(Call), Format)
                  ,call=Call
                  ,time_limit=TimeLimit
                  ,record_on_answer=RecordOnAnswer
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
    {'reply', {'error', 'not_implemented'}, State}.

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
handle_cast('record_start', #state{time_limit=TimeLimit}=State) ->
    {'noreply', State#state{is_recording='true'
                            ,channel_status_ref=start_check_call_timer()
                            ,time_limit_ref=start_time_limit_timer(TimeLimit)
                           }};
handle_cast('maybe_start_recording', #state{is_recording='true'}=State) ->
    lager:debug("we've already starting a recording for this call"),
    {'noreply', State};
handle_cast('maybe_start_recording', #state{is_recording='false'
                                            ,call=Call
                                            ,media_name=MediaName
                                            ,time_limit=TimeLimit
                                           }=State) ->
    start_recording(Call, MediaName, TimeLimit),
    {'noreply', State};

handle_cast('stop_call', #state{store_attempted='true'}=State) ->
    lager:debug("we've already sent a store attempt, waiting to hear back"),
    {'noreply', State};
handle_cast('stop_call', #state{media_name=MediaName
                                ,format=Format
                                ,url=Url
                                ,call=Call
                               }=State) ->
    lager:debug("recv stop_call event"),
    save_recording(Call, MediaName, Format, should_store_recording(Url)),
    lager:debug("sent store command"),
    {'noreply', State};
handle_cast('store_recording', #state{media_name=MediaName
                                      ,format=Format
                                      ,url=Url
                                      ,call=Call
                                     }=State) ->
    lager:debug("recv store_recording event"),
    save_recording(Call, MediaName, Format, should_store_recording(Url)),
    {'noreply', State#state{store_attempted='true'
                            ,is_recording='false'
                           }};
handle_cast({'channel_status',<<"active">>}, #state{channel_status_ref='undefined'}=State) ->
    {'noreply', State#state{channel_status_ref=start_check_call_timer()}};
handle_cast({'channel_status', <<"terminated">>}, #state{channel_status_ref='undefined'
                                                         ,store_attempted='false'
                                                        }=State) ->
    lager:debug("channel terminated, we're done here"),
    {'stop', 'normal', State};
handle_cast({'channel_status', <<"terminated">>}, #state{channel_status_ref='undefined'
                                                         ,store_attempted='true'
                                                        }=State) ->
    lager:debug("channel terminated, but we've sent a store attempt, so hold on"),
    {'noreply', State};
handle_cast({'channel_status', _S}, #state{channel_status_ref='undefined'}=State) ->
    Ref = start_check_call_timer(),
    lager:debug("unknown channel status respoonse: ~s, starting timer back up: ~p", [_S, Ref]),
    {'noreply', State#state{channel_status_ref=Ref}};
handle_cast({'channel_status', _S}, State) ->
    {'noreply', State};

handle_cast('store_succeeded', State) ->
    lager:debug("store succeeded"),
    {'stop', 'normal', State};
handle_cast('store_failed', State) ->
    lager:debug("store failed"),
    {'stop', 'normal', State};

handle_cast({'gen_listener',{'created_queue',Queue}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(Queue, Call)}};

handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='true'}=State) ->
    lager:debug("waiting for answer to start recording"),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='false'
                                                              ,is_recording='false'
                                                              ,call=Call
                                                              ,media_name=MediaName
                                                              ,time_limit=TimeLimit
                                                             }=State) ->
    start_recording(Call, MediaName, TimeLimit),
    lager:debug("started the recording"),
    {'noreply', State};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

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
handle_info('stop_recording', #state{media_name=MediaName
                                     ,call=Call
                                     ,time_limit_ref=TLRef
                                    }=State) ->
    lager:debug("recv stop_recording event"),
    maybe_stop_timer(TLRef),
    whapps_call_command:record_call(MediaName, <<"stop">>, Call),
    {'noreply', State};
handle_info({'timeout', TLRef, 'stop_recording'}, #state{media_name=MediaName
                                                         ,call=Call
                                                         ,time_limit_ref=TLRef
                                                        }=State) ->
    lager:debug("recv stop_recording timer, forcing recording to stop"),
    whapps_call_command:record_call(MediaName, <<"stop">>, Call),
    {'noreply', State};
handle_info({'check_call', Ref}, #state{call=Call
                                        ,channel_status_ref=Ref
                                       }=State) ->
    whapps_call_command:channel_status(Call),
    {'noreply', State#state{channel_status_ref='undefined'}};
handle_info(_Info, #state{channel_status_ref=_Ref}=State) ->
    lager:debug("unhandled message(~p): ~p", [_Ref, _Info]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec start_check_call_timer() -> reference().
start_check_call_timer() ->
    CheckRef = erlang:make_ref(),
    {'ok', _} = timer:send_after(5000, self(), {'check_call', CheckRef}),
    CheckRef.

-spec start_time_limit_timer(pos_integer()) -> reference().
start_time_limit_timer(TimeLimit) ->
    erlang:start_timer((TimeLimit+10) * 1000, self(), 'stop_recording').

-spec maybe_stop_timer(term()) -> 'ok'.
maybe_stop_timer(Timer) when is_reference(Timer) ->
    catch erlang:cancel_timer(Timer),
    'ok';
maybe_stop_timer(_) -> 'ok'.

-spec get_timelimit('undefined' | integer()) -> pos_integer().
get_timelimit('undefined') ->
    whapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600);
get_timelimit(TL) ->
    case (Max = whapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600)) > TL of
        'true' -> TL;
        'false' when Max > 0 -> Max;
        'false' -> Max
    end.

get_format('undefined') -> whapps_config:get(?CONFIG_CAT, [<<"call_recording">>, <<"extension">>], <<"mp3">>);
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec store_recording_meta(whapps_call:call(), ne_binary(), api_binary()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', any()}.
store_recording_meta(Call, MediaName, Ext) ->
    AcctDb = whapps_call:account_db(Call),
    CallId = whapps_call:call_id(Call),

    MediaDoc = wh_doc:update_pvt_parameters(
                 wh_json:from_list(
                   [{<<"name">>, MediaName}
                    ,{<<"description">>, <<"recording ", MediaName/binary>>}
                    ,{<<"content_type">>, ext_to_mime(Ext)}
                    ,{<<"media_type">>, Ext}
                    ,{<<"media_source">>, <<"recorded">>}
                    ,{<<"source_type">>, wh_util:to_binary(?MODULE)}
                    ,{<<"pvt_type">>, <<"private_media">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, CallId}
                    ,{<<"_id">>, get_recording_doc_id(CallId)}
                   ])
                 ,AcctDb
                ),
    couch_mgr:save_doc(AcctDb, MediaDoc).

ext_to_mime(<<"wav">>) -> <<"audio/x-wav">>;
ext_to_mime(_) -> <<"audio/mp3">>.

get_recording_doc_id(CallId) -> <<"call_recording_", CallId/binary>>.

-spec get_media_name(ne_binary(), api_binary()) -> ne_binary().
get_media_name(CallId, Ext) ->
    <<(get_recording_doc_id(CallId))/binary, ".", Ext/binary>>.

-spec get_url(wh_json:object()) -> api_binary().
get_url(Data) ->
    wh_json:get_value(<<"url">>, Data).

-spec store_url(whapps_call:call(), wh_json:object()) -> ne_binary().
store_url(Call, JObj) ->
    AccountDb = whapps_call:account_db(Call),
    MediaId = wh_json:get_value(<<"_id">>, JObj),
    MediaName = wh_json:get_value(<<"name">>, JObj),
    {'ok', URL} = wh_media_url:store(AccountDb, MediaId, MediaName),
    URL.

-type store_url() :: 'false' | {'true', 'local' | ne_binary()}.

-spec should_store_recording(api_binary()) -> store_url().
should_store_recording('undefined') ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end;
should_store_recording(Url) -> {'true', Url}.

-spec save_recording(whapps_call:call(), ne_binary(), ne_binary(), store_url()) -> 'ok'.
save_recording(_Call, _MediaName, _Format, 'false') ->
    lager:debug("not configured to store recording ~s", [_MediaName]);
save_recording(Call, MediaName, Format, {'true', 'local'}) ->
    {'ok', MediaJObj} = store_recording_meta(Call, MediaName, Format),
    lager:info("stored meta: ~p", [MediaJObj]),

    StoreUrl = store_url(Call, MediaJObj),
    lager:info("store local url: ~s", [StoreUrl]),

    store_recording(MediaName, StoreUrl, Call);
save_recording(Call, MediaName, _Format, {'true', Url}) ->
    lager:info("store remote url: ~s", [Url]),
    store_recording(MediaName, Url, Call).

-spec store_recording(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
store_recording(MediaName, Url, Call) ->
    StoreUrl = append_path(Url, MediaName),
    lager:debug("appending filename to url: ~s", [StoreUrl]),
    'ok' = whapps_call_command:store(MediaName, StoreUrl, Call).

-spec append_path(ne_binary(), ne_binary()) -> ne_binary().
append_path(Url, MediaName) ->
    S = byte_size(Url)-1,

    Encoded = cowboy_http:urlencode(MediaName),

    case Url of
        <<_:S/binary, "/">> -> <<Url/binary, Encoded/binary>>;
        _ -> <<Url/binary, "/", Encoded/binary>>
    end.

-spec start_recording(whapps_call:call(), ne_binary(), pos_integer()) -> 'ok'.
start_recording(Call, MediaName, TimeLimit) ->
  lager:debug("starting recording of ~s", [MediaName]),
  whapps_call_command:record_call(MediaName, <<"start">>, TimeLimit, Call).
