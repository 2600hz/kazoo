%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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
-module(kz_media_recording).

-behaviour(gen_listener).

-export([start_link/2
        ,handle_call_event/2

        ,get_timelimit/1
        ,get_format/1
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

-include("kazoo_media.hrl").

-define(SERVER, ?MODULE).

-type media() :: {api_binary(), ne_binary()}.

-type store_url() :: 'false' |
                     {'true', 'local'} |
                     {'true', 'other', ne_binary()}.

-record(state, {url                        :: api_binary()
               ,format                    :: ne_binary()
               ,sample_rate               :: integer() | 'undefined'
               ,media                     :: media()
               ,doc_db                    :: ne_binary()
               ,doc_id                    :: ne_binary()
               ,cdr_id                    :: ne_binary()
               ,interaction_id            :: ne_binary()
               ,call                      :: kapps_call:call()
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

-define(STORAGE_RETRY_TIMES(AccountId)
       ,kapps_account_config:get_global(AccountId, ?CONFIG_CAT
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
-define(MAX_RECORDING_LIMIT, kz_media_util:max_recording_time_limit()).
-define(CHECK_CHANNEL_STATUS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(RECORDING_ID_KEY, <<"media_name">>).

-spec start_link(kapps_call:call(), kz_json:object()) -> startlink_ret().
start_link(Call, Data) ->
    gen_listener:start_link(?SERVER, [{'bindings', ?BINDINGS(kapps_call:call_id(Call))}
                                     ,{'responders', ?RESPONDERS}
                                     ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                     ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                     ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call, Data]).

-spec get_response_media(kz_json:object()) -> media().
get_response_media(JObj) ->
    Filename = kz_json:get_value(<<"Application-Response">>, JObj),
    {filename:dirname(Filename), filename:basename(Filename)}.

-spec handle_call_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    kz_util:put_callid(JObj),
    Pid = props:get_value('server', Props),
    case kz_util:get_event_type(JObj) of
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
            FreeSWITCHNode = kz_json:get_value(<<"Switch-Nodename">>, JObj),
            gen_listener:cast(Pid, {'record_stop', Media, FreeSWITCHNode});
        {_Cat, _Evt} -> 'ok'
    end.

-spec init(list()) -> {'ok', state()}.
init([Call, Data]) ->
    kapps_call:put_callid(Call),
    lager:info("starting event listener for record_call"),

    Format = get_format(kz_json:get_value(<<"format">>, Data)),
    TimeLimit = get_timelimit(kz_json:get_integer_value(<<"time_limit">>, Data)),
    RecordOnAnswer = kz_json:is_true(<<"record_on_answer">>, Data, 'false'),
    RecordOnBridge = kz_json:is_true(<<"record_on_bridge">>, Data, 'false'),
    SampleRate = kz_json:get_integer_value(<<"record_sample_rate">>, Data),
    DefaultRecordMinSec = kapps_config:get_integer(?CONFIG_CAT, <<"record_min_sec">>, 0),
    RecordMinSec = kz_json:get_integer_value(<<"record_min_sec">>, Data, DefaultRecordMinSec),
    AccountId = kapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    AccountDb = kz_util:format_account_modb(kazoo_modb:get_modb(AccountId, Year, Month),'encoded'),
    CallId = kapps_call:call_id(Call),
    CdrId = ?MATCH_MODB_PREFIX(kz_util:to_binary(Year), kz_util:pad_month(Month), CallId),
    RecordingId = kz_util:rand_hex_binary(16),
    DocId = ?MATCH_MODB_PREFIX(kz_util:to_binary(Year), kz_util:pad_month(Month), RecordingId),
    InteractionId = kapps_call:custom_channel_var(?CALL_INTERACTION_ID, Call),
    DefaultMediaName = get_media_name(kz_util:rand_hex_binary(16), Format),
    MediaName = kz_json:get_value(?RECORDING_ID_KEY, Data, DefaultMediaName),
    Url = kz_json:get_value(<<"url">>, Data),
    ShouldStore = should_store_recording(Url),

    {'ok', #state{url=Url
                 ,format=Format
                 ,media={'undefined',MediaName}
                 ,doc_id=DocId
                 ,doc_db=AccountDb
                 ,cdr_id=CdrId
                 ,interaction_id=InteractionId
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
-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
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
-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast({'record_start', {_, Media}}, #state{media={_, Media}
                                                ,is_recording='true'
                                                }=State) ->
    lager:debug("record start received but we're already recording"),
    {'noreply', State};
handle_cast({'record_start', {_, Media}}, #state{media={_, Media}}=State) ->
    lager:debug("record start received for ~s", [Media]),
    {'noreply', State#state{is_recording='true'}};
handle_cast({'record_start', _}, State) ->
    {'noreply', State};
handle_cast('stop_recording', #state{media={_, MediaName}
                                    ,is_recording='true'
                                    ,call=Call
                                    }=State) ->
    _ = kapps_call_command:record_call([{<<"Media-Name">>, MediaName}], <<"stop">>, Call),
    lager:debug("sent command to stop recording, waiting for record stop"),
    {'noreply', State};
handle_cast('stop_recording', #state{is_recording='false'}=State) ->
    lager:debug("received stop recording and we're not recording, exiting"),
    {'stop', 'normal', State};
handle_cast({'record_stop', {_, MediaName}=Media, FS},
            #state{media={_, MediaName}
                  ,is_recording='true'
                  ,call=Call
                  }=State) ->
    Call1 = kapps_call:kvs_store(<<"FreeSwitch-Node">>, FS, Call),
    gen_server:cast(self(), 'store_recording'),
    {'noreply', State#state{media=Media, call=Call1}};
handle_cast({'record_stop', {_, MediaName}, _FS}, #state{media={_, MediaName}
                                                        ,is_recording='false'
                                                        }=State) ->
    lager:debug("received record_stop but we're not recording, exiting"),
    {'stop', 'normal', State};
handle_cast({'record_stop', _Media, _FS}, State) ->
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
                                                     ,doc_id=Id
                                                     }=State) ->
    start_recording(Call, MediaName, TimeLimit, Id, SampleRate, RecordMinSec),
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
                                                     ,doc_id=Id
                                                     }=State) ->
    start_recording(Call, MediaName, TimeLimit, Id, SampleRate, RecordMinSec),
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
    Sleep = ?MILLISECONDS_IN_SECOND * rand:uniform(10),
    lager:debug("store failed, retrying ~p more times, next in ~p seconds", [Retries, Sleep]),
    timer:sleep(Sleep),
    save_recording(State, Store),
    {'noreply', State#state{retries=Retries - 1}};
handle_cast({'gen_listener',{'created_queue', Queue}}, #state{call=Call}=State) ->
    Funs = [{fun kapps_call:kvs_store/3, 'consumer_pid', self()}
           ,{fun kapps_call:kvs_store/3, 'consumer_queue', Queue}
           ,fun kapps_call:clear_helpers/1
           ,{fun kapps_call:set_controller_queue/2, Queue}
           ],
    {'noreply', State#state{call=kapps_call:exec(Funs, Call)}};

handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='true'}=State) ->
    lager:debug("waiting for answer to start recording"),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_bridge='true'}=State) ->
    lager:debug("waiting for bridge to start recording"),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{record_on_answer='false'
                                                             ,record_on_bridge='false'
                                                             ,is_recording='false'
                                                             ,call=Call
                                                             ,media={_, MediaName}
                                                             ,time_limit=TimeLimit
                                                             ,sample_rate = SampleRate
                                                             ,record_min_sec = RecordMinSec
                                                             ,doc_id=Id
                                                             }=State) ->
    start_recording(Call, MediaName, TimeLimit, Id, SampleRate, RecordMinSec),
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
-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
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
-spec handle_event(kz_json:object(), kz_proplist()) -> handle_event_ret().
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
-spec terminate(any(), state()) -> 'ok'.
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
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_timelimit(api_object() | integer()) -> pos_integer().
get_timelimit('undefined') ->
    kapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600);
get_timelimit(TL) when is_integer(TL) ->
    case (Max = kapps_config:get(?CONFIG_CAT, <<"max_recording_time_limit">>, 600)) > TL of
        'true' -> TL;
        'false' when Max > 0 -> Max;
        'false' -> Max
    end;
get_timelimit(Data) ->
    get_timelimit(kz_json:get_integer_value(<<"time_limit">>, Data)).

-spec get_format(api_binary()) -> ne_binary().
get_format('undefined') -> kapps_config:get(?CONFIG_CAT, [<<"call_recording">>, <<"extension">>], <<"mp3">>);
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"mp4">> = MP4) -> MP4;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec store_recording_meta(state()) -> ne_binary() | {'error', any()}.
store_recording_meta(#state{call=Call
                           ,format=Ext
                           ,media={_, MediaName}
                           ,doc_db=Db
                           ,doc_id=DocId
                           ,cdr_id=CdrId
                           ,interaction_id=InteractionId
                           ,url=Url
                           }) ->
    CallId = kapps_call:call_id(Call),
    MediaDoc = kz_doc:update_pvt_parameters(
                 kz_json:from_list(
                   props:filter_empty(
                     [{<<"name">>, MediaName}
                     ,{<<"description">>, <<"recording ", MediaName/binary>>}
                     ,{<<"content_type">>, kz_mime:from_extension(Ext)}
                     ,{<<"media_type">>, Ext}
                     ,{<<"media_source">>, <<"recorded">>}
                     ,{<<"source_type">>, kz_util:to_binary(?MODULE)}
                     ,{<<"pvt_type">>, <<"call_recording">>}
                     ,{<<"from">>, kapps_call:from(Call)}
                     ,{<<"to">>, kapps_call:to(Call)}
                     ,{<<"caller_id_number">>, kapps_call:caller_id_number(Call)}
                     ,{<<"caller_id_name">>, kapps_call:caller_id_name(Call)}
                     ,{<<"call_id">>, CallId}
                     ,{<<"owner_id">>, kapps_call:owner_id(Call)}
                     ,{<<"url">>, Url}
                     ,{<<"cdr_id">>, CdrId}
                     ,{<<"interaction_id">>, InteractionId}
                     ,{<<"_id">>, DocId}
                     ]))
                                           ,Db
                ),
    kazoo_modb:create(Db),
    case kz_datamgr:ensure_saved(Db, MediaDoc) of
        {'ok', JObj} -> kz_doc:revision(JObj);
        {'error', _}= Err -> Err
    end.

-spec maybe_store_recording_meta(state()) -> ne_binary() | {'error', any()}.
maybe_store_recording_meta(#state{doc_db=Db, doc_id=DocId}=State) ->
    case kz_datamgr:lookup_doc_rev(Db, {<<"call_recording">>, DocId}) of
        {'ok', Rev} -> Rev;
        _ -> store_recording_meta(State)
    end.


-spec get_media_name(ne_binary(), api_binary()) -> ne_binary().
get_media_name(Name, Ext) ->
    case filename:extension(Name) of
        Ext -> Name;
        _ -> <<Name/binary, ".", Ext/binary>>
    end.

-spec store_url(state(), ne_binary()) -> ne_binary().
store_url(#state{doc_db=Db
                ,doc_id=MediaId
                ,media={_,MediaName}
                ,format=_Ext
                ,should_store={'true', 'local'}
                }, _Rev) ->
    kz_media_url:store(Db, {<<"call_recording">>, MediaId}, MediaName, []);
store_url(#state{doc_db=Db
                ,doc_id=MediaId
                ,media={_,MediaName}
                ,format=Ext
                ,should_store={'true', 'other', Url}
                }, _Rev) ->
    HttpOptions = #{url => Url
                   ,verb => 'put'
                   ,field_separator => <<>>
                   ,field_list => [<<"call_recording_">>
                                  ,{field, <<"call_id">>}
                                  ,<<".", Ext/binary>>
                                  ]
                   },
    Handler = #{att_proxy => 'true'
               ,att_post_handler => 'external'
               ,att_handler => {'kz_att_http', HttpOptions}
               },
    Options = [{'plan_override', Handler}],
    kz_media_url:store(Db, {<<"call_recording">>, MediaId}, MediaName, Options).

-spec should_store_recording() -> store_url().
-spec should_store_recording(api_binary()) -> store_url().
should_store_recording(Url) ->
    case kz_util:is_empty(Url) of
        'true' -> should_store_recording();
        'false' -> {'true', 'other', Url}
    end.

should_store_recording() ->
    case kapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false') of
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end.

-spec save_recording(state(), store_url()) -> 'ok'.
save_recording(#state{media={_, MediaName}}, 'false') ->
    lager:info("not configured to store recording ~s", [MediaName]),
    gen_server:cast(self(), 'stop');
save_recording(#state{call=Call, media=Media}=State, _) ->
    case maybe_store_recording_meta(State) of
        {'error', Err} ->
            lager:warning("error storing metadata : ~p", [Err]),
            gen_server:cast(self(), 'store_failed');
        Rev ->
            StoreUrl = store_url(State, Rev),
            lager:info("store url: ~s", [StoreUrl]),
            store_recording(Media, StoreUrl, Call)
    end.

-spec start_recording(kapps_call:call(), ne_binary(), pos_integer(), ne_binary(), api_integer(), api_integer()) -> 'ok'.
start_recording(Call, MediaName, TimeLimit, MediaDocId, SampleRate, RecordMinSec) ->
    lager:debug("starting recording of ~s", [MediaName]),
    Props = [{<<"Media-Name">>, MediaName}
            ,{<<"Media-Recording-ID">>, MediaDocId}
            ,{<<"Record-Sample-Rate">>, SampleRate}
            ,{<<"Record-Min-Sec">>, kz_util:to_binary(RecordMinSec)}
            ,{<<"Media-Recorder">>, <<"kz_media_recording">>}
            ],
    kapps_call_command:start_record_call(Props, TimeLimit, Call),
    gen_server:cast(self(), 'recording_started').

-spec store_recording({ne_binary(), ne_binary()}, ne_binary(), kapps_call:call()) -> 'ok'.
store_recording({DirName, MediaName}, StoreUrl, Call) ->
    Filename = filename:join(DirName, MediaName),
    case kapps_call_command:store_file(Filename, StoreUrl, Call) of
        {'error', 'timeout'} -> gen_server:cast(self(), 'store_failed');
        {'error', Error} ->
            lager:error("error storing recording : ~p", [Error]),
            gen_server:cast(self(), 'store_failed');
        'ok' -> gen_server:cast(self(), 'store_succeeded')
    end.
