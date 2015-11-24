%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz
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
%%%
%%% Fix KAZOO-3406: Sponsored by Velvetech LLC, implemented by SIPLABS LLC
%%%-------------------------------------------------------------------
-module(wh_media_recording).

-behaviour(gen_listener).

-export([start_link/2
         ,start_recording/2
         ,handle_call_event/2

         ,get_timelimit/1
         ,get_format/1
         ,get_url/1
         ,get_media_name/2
         ,get_response_media/1
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

-type couch_connection() :: wh_couch_connections:couch_connection().
-type couch_connections() :: wh_couch_connections:couch_connections().

-type media() :: {api_binary(), ne_binary()}.

-type store_url() :: 'false' |
                     {'true', 'local'} |
                     {'true', 'third_party'} |
                     {'true', atom()} |
                     {'true', 'other', ne_binary()}.

-record(state, {url                        :: api_binary()
                ,format                    :: ne_binary()
                ,sample_rate               :: integer() | 'undefined'
                ,media                     :: media()
                ,doc_db                    :: ne_binary()
                ,doc_id                    :: ne_binary()
                ,call                      :: whapps_call:call()
                ,record_on_answer          :: boolean()
                ,record_on_bridge          :: boolean()
                ,should_store              :: store_url()
                ,time_limit                :: pos_integer()
                ,record_min_sec            :: pos_integer()
                ,store_attempted = 'false' :: boolean()
                ,is_recording = 'false'    :: boolean()
                ,retries = 0               :: integer()
               }).
-type state() :: #state{}.

-define(STORAGE_TIMEOUT, whapps_config:get(?WHM_CONFIG_CAT, <<"storage_timeout_ms">>, 5 * ?MILLISECONDS_IN_MINUTE)).

-define(STORAGE_RETRY_TIMES(AccountId)
        ,whapps_account_config:get_global(AccountId, ?WHM_CONFIG_CAT
                                          ,[<<"call_recording">>, <<"storage_retry_times">>]
                                          ,5
                                         )).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS(CallId), [{'call', [{'callid', CallId}
                                     ,{'restrict_to', ['CHANNEL_ANSWER'
                                                       ,'CHANNEL_BRIDGE'
                                                       ,'RECORD_START'
                                                       ,'RECORD_STOP'
                                                       ,'CHANNEL_REPLACED'
                                                       ,'CHANNEL_TRANSFEROR'
                                                      ]}
                                    ]}
                           ,{'self', []}
                          ]).
-define(CALL_BINDING(CallId), {'call', [{'callid', CallId}
                                         ,{'restrict_to', ['RECORD_STOP'
                                                           ,'CHANNEL_REPLACED'
                                                           ,'CHANNEL_TRANSFEROR'
                                                          ]
                                          }
                                        ]
                               }
                          ).
-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).
-define(MAX_RECORDING_LIMIT, wh_media_util:max_recording_time_limit()).
-define(CHECK_CHANNEL_STATUS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(RECORDING_ID_KEY, <<"Recording-ID">>).

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

-spec get_response_media(wh_json:object()) -> media().
get_response_media(JObj) ->
    Filename = wh_json:get_value(<<"Application-Response">>, JObj),
    {filename:dirname(Filename), filename:basename(Filename)}.

-spec handle_call_event(wh_json:object(), wh_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    wh_util:put_callid(JObj),
    Pid = props:get_value('server', Props),
    case wh_util:get_event_type(JObj) of
        {<<"call_event">>, <<"CHANNEL_TRANSFEROR">>} ->
            gen_listener:add_binding(Pid, ?CALL_BINDING(kz_call_event:other_leg_call_id(JObj)));
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>} ->
            gen_listener:cast(Pid, 'maybe_start_recording_on_bridge');
        {<<"call_event">>, <<"CHANNEL_ANSWER">>} ->
            gen_listener:cast(Pid, 'maybe_start_recording_on_answer');
         {<<"call_event">>, <<"CHANNEL_REPLACED">>} ->
            gen_listener:add_binding(Pid, ?CALL_BINDING(kz_call_event:replaced_by(JObj)));
        {<<"call_event">>, <<"RECORD_START">>} ->
            gen_listener:cast(Pid, {'record_start', get_response_media(JObj)});
        {<<"call_event">>, <<"RECORD_STOP">>} ->
            Media = get_response_media(JObj),
            ChannelState = wh_json:get_value(<<"Channel-State">>, JObj),
            FreeSWITCHNode = wh_json:get_value(<<"Switch-Node">>, JObj),
            gen_listener:cast(Pid, {'record_stop', Media, ChannelState, FreeSWITCHNode});
        {_Cat, _Evt} -> lager:debug("ignore event ~p/~p", [_Cat, _Evt])
    end.

-spec init(list()) -> {'ok', state()}.
init([Call, Data]) ->
    whapps_call:put_callid(Call),
    lager:info("starting event listener for record_call"),

    Format = get_format(wh_json:get_value(<<"format">>, Data)),
    TimeLimit = get_timelimit(wh_json:get_integer_value(<<"time_limit">>, Data)),
    RecordOnAnswer = wh_json:is_true(<<"record_on_answer">>, Data, 'false'),
    RecordOnBridge = wh_json:is_true(<<"record_on_bridge">>, Data, 'false'),
    SampleRate = wh_json:get_integer_value(<<"record_sample_rate">>, Data),
    DefaultRecordMinSec = whapps_config:get_integer(?WHM_CONFIG_CAT, <<"record_min_sec">>, 0),
    RecordMinSec = wh_json:get_integer_value(<<"record_min_sec">>, Data, DefaultRecordMinSec),
    AccountId = whapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    AccountDb = wh_util:format_account_modb(kazoo_modb:get_modb(AccountId, Year, Month),'encoded'),
    CallId = whapps_call:call_id(Call),
    DocId = ?MATCH_MODB_PREFIX(wh_util:to_binary(Year), wh_util:pad_month(Month), CallId),
    MediaId = wh_json:get_value(?RECORDING_ID_KEY, Data, wh_util:rand_hex_binary(16)),
    MediaName = get_media_name(MediaId, Format),
    Url = get_url(Data),
    ShouldStore = should_store_recording(Url),

    {'ok', #state{url=Url
                  ,format=Format
                  ,media={'undefined',MediaName}
                  ,doc_id=DocId
                  ,doc_db=AccountDb
                  ,call=Call
                  ,time_limit=TimeLimit
                  ,record_on_answer=RecordOnAnswer
                  ,record_on_bridge=RecordOnBridge
                  ,should_store=ShouldStore
                  ,sample_rate = SampleRate
                  ,record_min_sec = RecordMinSec
                  ,retries = ?STORAGE_RETRY_TIMES(AccountId)
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
handle_cast({'record_start', {_, Media}}, #state{media={_, Media}
                                                 ,is_recording='true'
                                                }=State) ->
    lager:debug("record start received but we're already recording"),
    {'noreply', State};
handle_cast({'record_start', {_, Media}}, #state{media={_, Media}}=State) ->
    {'noreply', State#state{is_recording='true'}};
handle_cast({'record_stop', {_, MediaName}=Media, <<"HANGUP">>, FS},
            #state{media={_, MediaName}
                   ,is_recording='true'
                   ,call=Call
                  }=State) ->
    Call1 = whapps_call:kvs_store(<<"FreeSwitch-Node">>, FS, Call),
    gen_server:cast(self(), 'store_recording'),
    {'noreply', State#state{media=Media, call=Call1}};
handle_cast({'record_start', _}, State) ->
    {'noreply', State};
handle_cast({'record_stop', _Media, _ChannelState, _FS}, State) ->
    {'noreply', State};
handle_cast('maybe_start_recording_on_bridge', #state{is_recording='true'}=State) ->
    {'noreply', State};
handle_cast('maybe_start_recording_on_bridge', #state{is_recording='false'
                                                      ,record_on_bridge='true'
                                                      ,call=Call
                                                      ,media={_, MediaName}
                                                      ,time_limit=TimeLimit
                                                      ,sample_rate = SampleRate
                                                      ,record_min_sec = RecordMinSec
                                                     }=State) ->
    start_recording(Call, MediaName, TimeLimit, <<"wh_media_recording">>, SampleRate, RecordMinSec),
    {'noreply', State};
handle_cast('maybe_start_recording_on_answer', #state{is_recording='true'}=State) ->
    {'noreply', State};
handle_cast('maybe_start_recording_on_answer', #state{is_recording='false'
                                                      ,record_on_answer='true'
                                                      ,call=Call
                                                      ,media={_, MediaName}
                                                      ,time_limit=TimeLimit
                                                      ,sample_rate = SampleRate
                                                      ,record_min_sec = RecordMinSec
                                                     }=State) ->
    start_recording(Call, MediaName, TimeLimit, <<"wh_media_recording">>, SampleRate, RecordMinSec),
    {'noreply', State};
handle_cast('recording_started', #state{should_store='false'}=State) ->
    lager:debug("recording started and we are not storing, exiting"),
    {'stop', 'normal', State};
handle_cast('recording_started', State) ->
    {'noreply', State};
handle_cast('store_recording', #state{should_store=Store
                                      ,is_recording='true'
                                      ,store_attempted='false'
                                     }=State) ->
    save_recording(State, Store),
    {'noreply', State#state{store_attempted='true'
                            ,is_recording='false'
                           }};
handle_cast('store_recording', #state{is_recording='false'}=State) ->
    lager:debug("store_recording event but we're not recording, exiting"),
    {'stop', 'normal', State};
handle_cast('store_succeeded', State) ->
    lager:debug("store succeeded"),
    {'stop', 'normal', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast('store_failed', #state{retries=0}=State) ->
    lager:debug("store failed, no more retries."),
    {'stop', 'normal', State};
handle_cast('store_failed', #state{retries=Retries
                                   ,should_store=Store
                                  }=State) ->
    Sleep = ?MILLISECONDS_IN_SECOND * random:uniform(10),
    lager:debug("store failed, retrying ~p more times, next in ~p seconds", [Retries, Sleep]),
    timer:sleep(Sleep),
    save_recording(State, Store),
    {'noreply', State#state{retries=Retries - 1}};
handle_cast({'gen_listener',{'created_queue', Queue}}, #state{call=Call}=State) ->
    Funs = [{fun whapps_call:kvs_store/3, 'consumer_pid', self()}
            ,{fun whapps_call:kvs_store/3, 'consumer_queue', Queue}
            ,fun whapps_call:clear_helpers/1
            ,{fun whapps_call:set_controller_queue/2, Queue}
           ],
    {'noreply', State#state{call=whapps_call:exec(Funs, Call)}};

handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='true'}=State) ->
    lager:debug("waiting for bridge or answer to start recording"),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='false'
                                                              ,record_on_bridge='false'
                                                              ,is_recording='false'
                                                              ,call=Call
                                                              ,media={_, MediaName}
                                                              ,time_limit=TimeLimit
                                                              ,sample_rate = SampleRate
                                                              ,record_min_sec = RecordMinSec
                                                             }=State) ->
    start_recording(Call, MediaName, TimeLimit, <<"wh_media_recording">>, SampleRate, RecordMinSec),
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
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
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

-spec get_timelimit(api_integer()) -> pos_integer().
get_timelimit('undefined') ->
    whapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600);
get_timelimit(TL) ->
    case (Max = whapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600)) > TL of
        'true' -> TL;
        'false' when Max > 0 -> Max;
        'false' -> Max
    end.

-spec get_format(api_binary()) -> ne_binary().
get_format('undefined') -> whapps_config:get(?WHM_CONFIG_CAT, [<<"call_recording">>, <<"extension">>], <<"mp3">>);
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec store_recording_meta(state()) -> ne_binary() | {'error', any()}.
store_recording_meta(#state{call=Call
                            ,format=Ext
                            ,media={_, MediaName}
                            ,doc_db=Db
                            ,doc_id=DocId
                           }) ->
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
                    ,{<<"pvt_type">>, <<"call_recording">>}
                    ,{<<"from">>, whapps_call:from(Call)}
                    ,{<<"to">>, whapps_call:to(Call)}
                    ,{<<"caller_id_number">>, whapps_call:caller_id_number(Call)}
                    ,{<<"caller_id_name">>, whapps_call:caller_id_name(Call)}
                    ,{<<"call_id">>, CallId}
                    ,{<<"_id">>, DocId}
                   ])
                 ,Db
                ),
    kazoo_modb:create(Db),
    case couch_mgr:ensure_saved(Db, MediaDoc) of
        {'ok', JObj} -> wh_doc:revision(JObj);
        {'error', _}= Err -> Err
    end.

-spec maybe_store_recording_meta(state()) -> ne_binary() | {'error', any()}.
maybe_store_recording_meta(#state{doc_db=Db, doc_id=DocId}=State) ->
    case couch_mgr:lookup_doc_rev(Db, DocId) of
        {'ok', Rev} -> Rev;
        _ -> store_recording_meta(State)
    end.


-spec ext_to_mime(ne_binary()) -> ne_binary().
ext_to_mime(<<"wav">>) -> <<"audio/x-wav">>;
ext_to_mime(_) -> <<"audio/mp3">>.

-spec get_media_name(ne_binary(), api_binary()) -> ne_binary().
get_media_name(CallId, Ext) ->
    <<CallId/binary, ".", Ext/binary>>.

-spec get_url(wh_json:object()) -> api_binary().
get_url(Data) ->
    wh_json:get_value(<<"url">>, Data).

-spec store_url(state(), ne_binary()) -> ne_binary().
store_url(#state{doc_db=Db, doc_id=MediaId, media={_,MediaName}, should_store={'true', Tag}}, Rev) ->
    Url = wh_couch_connections:get_url(Tag),
    <<Url/binary, Db/binary, "/", MediaId/binary, "/", MediaName/binary, "?rev=", Rev/binary>>.

-spec should_store_recording() -> store_url().
-spec should_store_recording(api_binary()) -> store_url().
should_store_recording(Url) ->
    case wh_util:is_empty(Url) of
        'true' -> should_store_recording();
        'false' -> {'true', 'other', Url}
    end.

should_store_recording() ->
    BCHost = whapps_config:get_ne_binary(?CONFIG_CAT, <<"third_party_bigcouch_host">>),
    case whapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' when BCHost =/= 'undefined' -> {'true', 'third_party'};
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end.

-spec save_recording(state(), store_url()) -> 'ok'.
save_recording(#state{media={_, MediaName}}, 'false') ->
    lager:info("not configured to store recording ~s", [MediaName]),
    gen_server:cast(self(), 'stop');
save_recording(#state{call=Call, media=Media}=State, {'true', Tag}) ->
    maybe_add_connection(Tag),
    case maybe_store_recording_meta(State) of
        {'error', Err} ->
            lager:warning("error storing metadata for ~p : ", [Tag, Err]),
            gen_server:cast(self(), 'store_failed');
        Rev ->
            StoreUrl = store_url(State, Rev),
            lager:info("store ~p url: ~s", [Tag, StoreUrl]),
            store_recording(Media, StoreUrl, Call, Tag)
    end;
save_recording(#state{call=Call, media=Media}, {'true', 'other', Url}) ->
    lager:info("store remote url: ~s", [Url]),
    store_recording(Media, Url, Call, 'other').

-spec maybe_add_connection(atom()) -> 'ok'.
maybe_add_connection('local') -> 'ok';
maybe_add_connection(Tag) ->
    Connections = wh_couch_connections:get_by_tag(Tag),
    maybe_add_connection(Tag, Connections).

-spec maybe_add_connection(atom(), couch_connections()) -> 'ok'.
maybe_add_connection(Tag, []) ->
    Conn = couch_connection(Tag),
    wh_couch_connections:add_unique(Conn, Tag),
    wh_couch_connections:wait_for_connection(Tag, ?MILLISECONDS_IN_MINUTE),
    couch_mgr:server_tag(Tag);
maybe_add_connection(Tag, [_|_]) ->
    couch_mgr:server_tag(Tag).

-spec couch_connection(atom()) -> couch_connection().
couch_connection('third_party') ->
  BCHost = whapps_config:get_ne_binary(?CONFIG_CAT, <<"third_party_bigcouch_host">>),
  BCPort = whapps_config:get_integer(?CONFIG_CAT, <<"third_party_bigcouch_port">>, 5984),
  wh_couch_connection:config(BCHost, BCPort);
couch_connection(_Tag) -> wh_couch_connection:config().

-spec store_recording({ne_binary(), ne_binary()}, ne_binary(), whapps_call:call(), 'local' | 'other') -> 'ok'.
store_recording(Media, Url, Call, 'other') ->
    StoreUrl = append_path(Url, Media),
    lager:debug("appending filename to url: ~s", [StoreUrl]),
    store(Media, StoreUrl, Call);
store_recording(Media, StoreUrl, Call, _Tag) ->
    store(Media, StoreUrl, Call).

-spec append_path(ne_binary(), {ne_binary(), ne_binary()}) -> ne_binary().
append_path(Url, {_, MediaName}) ->
    S = byte_size(Url)-1,

    Encoded = wh_util:uri_encode(MediaName),

    case Url of
        <<_:S/binary, "/">> -> <<Url/binary, Encoded/binary>>;
        _ -> <<Url/binary, "/", Encoded/binary>>
    end.

-spec start_recording(whapps_call:call(), ne_binary(), pos_integer(), ne_binary(), api_integer(), api_integer()) -> 'ok'.
start_recording(Call, MediaName, TimeLimit, MediaRecorder, SampleRate, RecordMinSec) ->
    lager:debug("starting recording of ~s", [MediaName]),
    Call1 = whapps_call:set_custom_channel_var(<<"Media-Recorder">>, MediaRecorder, Call),
    Props = [{<<"Media-Name">>, MediaName}
             ,{<<"Record-Sample-Rate">>, SampleRate}
             ,{<<"Record-Min-Sec">>, wh_util:to_binary(RecordMinSec)}
            ],
    whapps_call_command:start_record_call(Props, TimeLimit, Call1),
    gen_server:cast(self(), 'recording_started').

-spec store({ne_binary(), ne_binary()}, ne_binary(), whapps_call:call()) -> 'ok'.
store({DirName, MediaName}, StoreUrl, Call) ->
    Args = [{<<"File-Name">>, filename:join(DirName, MediaName)}
            ,{<<"Url">>, StoreUrl}
            ,{<<"Http-Method">>, <<"put">>}
           ],
    API = [{<<"Command">>, <<"send_http">>}
           ,{<<"Args">>, wh_json:from_list(Args)}
           ,{<<"FreeSWITCH-Node">>, whapps_call:kvs_fetch(<<"FreeSwitch-Node">>, Call)}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case wh_amqp_worker:call(API, fun wapi_switch:publish_command/1, fun wapi_switch:fs_reply_v/1, ?STORAGE_TIMEOUT) of
        {'error', 'timeout'} -> gen_server:cast(self(), 'store_failed');
        {'ok', JObj} -> check_store_result(wh_json:get_value(<<"Result">>, JObj), JObj)
    end,
    'ok'.

-spec check_store_result(ne_binary(), wh_json:object()) -> 'ok'.
check_store_result(<<"success">>, _JObj) ->
    gen_server:cast(self(), 'store_succeeded');
check_store_result(<<"error">>, JObj) ->
    lager:debug("error ~s received for store", [wh_json:get_value(<<"Error">>, JObj)]),
    gen_server:cast(self(), 'store_failed').
