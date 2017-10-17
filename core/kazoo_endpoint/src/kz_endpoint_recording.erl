%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%% Handles endpoint inbound recording
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(kz_endpoint_recording).

-behaviour(gen_listener).

-export([start_link/0
        ,record_call_command/4
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,handle_call_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kazoo_endpoint.hrl").

-define(SERVER, ?MODULE).

-define(MEDIA_RECORDING_ENDPOINT_ID, <<"Media-Recording-Endpoint-ID">>).

-type media_directory() :: file:filename_all().
-type media_name() :: file:filename_all().
-type media() :: {media_directory() | 'undefined', media_name()}.

-type store_url() :: 'false' |
                     {'true', 'local'} |
                     {'true', 'other', ne_binary()}.

-type state() :: map().

-define(STORAGE_RETRY_TIMES(AccountId)
       ,kz_media_config:storage_retry_times(AccountId)
       ).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'call', [{'restrict_to', ['RECORD_START'
                                             ,'RECORD_STOP'
                                             ]}
                            ]}
                  ,{'self', []}
                  ]
       ).

-define(RESPONDERS, [{{?MODULE, 'handle_call_event'}
                     ,[{<<"*">>, <<"*">>}]
                     }
                    ]
       ).
-define(QUEUE_NAME, <<"endpoint_recordings">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(MAX_RECORDING_LIMIT, kz_media_util:max_recording_time_limit()).
-define(CHECK_CHANNEL_STATUS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(RECORDING_ID_KEY, <<"media_name">>).

-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}       % optional to include
                            ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                            ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                            ]
                           ,[]
                           ).

-spec get_response_media(kz_json:object()) -> media().
get_response_media(JObj) ->
    Filename = kz_call_event:application_response(JObj),
    {filename:dirname(Filename), filename:basename(Filename)}.

-spec handle_call_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    kz_util:put_callid(JObj),
    Pid = props:get_value('server', Props),
    case kz_util:get_event_type(JObj) of
        {<<"call_event">>, <<"RECORD_START">>} ->
            maybe_log_start_recording(Pid, JObj);
        {<<"call_event">>, <<"RECORD_STOP">>} ->
            maybe_save_recording(Pid, JObj);
        {_Cat, _Evt} -> 'ok'
    end.

-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:info("starting event listener for inbound endpoint record_call"),
    {'ok', #{}}.

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
%% handle_cast({'record_start', JObj}, State) ->
%%     kz_util:put_callid(JObj),
%%     lager:debug("RECORD START : ~p", [JObj]),
%%     {'noreply', State};

%% handle_cast({'record_stop', JObj}, State) ->
%%     lager:debug("received record_stop, storing recording : ~p", [JObj]),
%%     gen_server:cast(self(), {'store_recording', init_from_jobj(JObj)}),
%%     {'noreply', State};

%% handle_cast({'store_recording', #{store_attempted := 'false'}=Store}, State) ->
%%     lager:debug("attempting to save recording"),
%%     save_recording(Store),
%%     {'noreply', State};
%% handle_cast({'store_recording', #{store_attempted := 'true'}}, State) ->
%%     {'noreply', State};

handle_cast({'store_succeeded', #{}}, State) ->
    lager:debug("store succeeded"),
    {'noreply', State};

handle_cast({'store_failed', #{retries := 0}}, State) ->
    lager:debug("store failed, no more retries."),
    {'noreply', State};
handle_cast({'store_failed', #{retries := Retries} = Store}, State) ->
    Sleep = ?MILLISECONDS_IN_MINUTE * rand:uniform(10),
    lager:debug("store failed, retrying ~p more times, next in ~p minute(s)"
               ,[Retries, Sleep / ?MILLISECONDS_IN_MINUTE]
               ),
    timer:sleep(Sleep),
    save_recording(Store#{retries => Retries - 1}),
    {'noreply', State};
handle_cast({'gen_listener',{'created_queue', Queue}}, State) ->
    {'noreply', State#{queue => Queue}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, State) ->
    lager:debug("we're ready to accept recording events"),
    {'noreply', State#{is_consuming => 'true'}};
handle_cast({'gen_listener',{'is_consuming', 'false'}}, State) ->
    lager:warning("we're not consuming any events"),
    {'noreply', State#{is_consuming => 'false'}};

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
-spec handle_event(kz_json:object(), kz_proplist()) -> gen_listener:handle_event_return().
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

-spec get_timelimit(api_integer()) -> pos_integer().
get_timelimit('undefined') ->
    kz_media_util:max_recording_time_limit();
get_timelimit(TL) when is_integer(TL) ->
    Max = kz_media_util:max_recording_time_limit(),
    case Max > TL of
        'true' -> TL;
        'false' when Max > 0 -> Max;
        'false' -> Max
    end.

-spec get_format(api_ne_binary()) -> ne_binary().
get_format('undefined') -> kz_media_config:call_recording_extension();
get_format(<<"mp3">> = MP3) -> MP3;
get_format(<<"mp4">> = MP4) -> MP4;
get_format(<<"wav">> = WAV) -> WAV;
get_format(_) -> get_format('undefined').

-spec store_recording_meta(map()) -> ne_binary() | {'error', any()}.
store_recording_meta(#{media := {_, MediaName}
                      ,doc_db := Db
                      ,doc_id := DocId
                      ,cdr_id := CdrId
                      ,interaction_id := InteractionId
                      ,url := Url
                      ,call_id := CallId
                      ,event := JObj
                      ,account_id := AccountId
                      ,origin := Origin
                      }) ->
    Ext = filename:extension(MediaName),
    Timestamp = kz_call_event:timestamp(JObj),
    Length = kz_call_event:recording_length(JObj),
    Seconds = Length div ?MILLISECONDS_IN_SECOND,
    Start = Timestamp - Seconds,
    BaseMediaDoc = kz_json:from_list(
                     [{<<"name">>, MediaName}
                     ,{<<"description">>, <<"recording ", MediaName/binary>>}
                     ,{<<"content_type">>, kz_mime:from_extension(Ext)}
                     ,{<<"media_type">>, Ext}
                     ,{<<"media_source">>, <<"recorded">>}
                     ,{<<"source_type">>, kz_term:to_binary(?MODULE)}
                     ,{<<"from">>, kz_json:get_ne_binary_value(<<"From">>, JObj)}
                     ,{<<"to">>, kz_json:get_ne_binary_value(<<"To">>, JObj)}
                     ,{<<"request">>, kz_json:get_ne_binary_value(<<"Request">>, JObj)}
                     ,{<<"direction">>, kz_call_event:call_direction(JObj)}
                     ,{<<"start">>, Start}
                     ,{<<"duration">>, Seconds}
                     ,{<<"duration_ms">>, Length}
                     ,{<<"caller_id_number">>, kz_call_event:caller_id_number(JObj)}
                     ,{<<"caller_id_name">>, kz_call_event:caller_id_name(JObj)}
                     ,{<<"callee_id_number">>, kz_call_event:callee_id_number(JObj)}
                     ,{<<"callee_id_name">>, kz_call_event:callee_id_name(JObj)}
                     ,{<<"call_id">>, CallId}
                     ,{<<"owner_id">>, kz_call_event:custom_channel_var(JObj, <<"Owner-ID">>)}
                     ,{<<"url">>, Url}
                     ,{<<"cdr_id">>, CdrId}
                     ,{<<"interaction_id">>, InteractionId}
                     ,{<<"_id">>, DocId}
                     ,{<<"origin">>, Origin}
                     ,{<<"custom_channel_vars">>, kz_call_event:custom_channel_vars(JObj)}
                     ]
                    ),

    MediaDoc = kz_doc:update_pvt_parameters(BaseMediaDoc, Db, [{'type', <<"call_recording">>}]),
    case kazoo_modb:save_doc(Db, MediaDoc, [{ensure_saved, true}]) of
        {'ok', Doc} ->
            lager:debug("recording meta ~s saved for ~s", [DocId, AccountId]),
            kz_doc:revision(Doc);
        {'error', _}= Err -> Err
    end.

-spec maybe_store_recording_meta(map()) -> ne_binary() | {'error', any()}.
maybe_store_recording_meta(#{doc_db := Db
                            ,doc_id := DocId
                            }=State) ->
    case kz_datamgr:lookup_doc_rev(Db, {<<"call_recording">>, DocId}) of
        {'ok', Rev} -> Rev;
        _ -> store_recording_meta(State)
    end.

-spec get_media_name(ne_binary(), api_ne_binary()) -> ne_binary().
get_media_name(Name, Ext) ->
    case filename:extension(Name) of
        Ext -> Name;
        _ -> <<Name/binary, ".", Ext/binary>>
    end.

-spec store_url(map(), ne_binary()) -> ne_binary().
store_url(#{doc_db := Db
           ,doc_id := MediaId
           ,media := {_,MediaName}
           ,should_store := {'true', 'local'}
           }, _Rev) ->
    kz_media_url:store(Db, {<<"call_recording">>, MediaId}, MediaName, []);
store_url(#{doc_db := Db
           ,doc_id := MediaId
           ,media := {_,MediaName}
           ,should_store := {'true', 'other', Url}
           ,verb := Verb
           } = State, _Rev) ->
    HandlerOpts = #{url => Url
                   ,verb => Verb
                   ,field_separator => <<>>
                   ,field_list => handler_fields(Url, State)
                   },
    AttHandler = handler_from_url(Url),
    Handler = #{att_proxy => 'true'
               ,att_post_handler => 'external'
               ,att_handler => {AttHandler, HandlerOpts}
               },
    Options = [{'plan_override', Handler}],
    kz_media_url:store(Db, {<<"call_recording">>, MediaId}, MediaName, Options).

-spec handler_fields(ne_binary(), state()) -> list().
handler_fields(Url, State) ->
    {Protocol, _, _, _, _} = kz_http_util:urlsplit(Url),
    handler_fields_for_protocol(Protocol, Url, State).

-spec handler_fields_for_protocol(ne_binary(), ne_binary(), state()) -> list().
handler_fields_for_protocol(<<"ftp", _/binary>>, _Url, #{format:=Ext}) ->
    [<<"call_recording_">>
    ,{field, <<"call_id">>}
    ,<<".", Ext/binary>>
    ];
handler_fields_for_protocol(<<"http", _/binary>>, Url, #{account_id:=AccountId
                                                        ,format:=Ext
                                                        }) ->
    {S1, S2} = check_url(Url),
    [<<S1/binary, "call_recording_">>
    ,{field, <<"call_id">>}
    ,<<".", Ext/binary>>
    ,<<S2/binary, "from=">>
    ,{field, <<"from">>}
    ,<<"&to=">>
    ,{field, <<"to">>}
    ,<<"&caller_id_name=">>
    ,{field, <<"caller_id_name">>}
    ,<<"&caller_id_number=">>
    ,{field, <<"caller_id_number">>}
    ,<<"&call_id=">>
    ,{field, <<"call_id">>}
    ,<<"&cdr_id=">>
    ,{field, <<"cdr_id">>}
    ,<<"&interaction_id=">>
    ,{field, <<"interaction_id">>}
    ,<<"&account_id=">>
    ,AccountId
    ].

-spec check_url(ne_binary()) -> {binary(), ne_binary()}.
check_url(Url) ->
    case kz_http_util:urlsplit(Url) of
        {_, _, _, <<>>, _} -> {<<>>, <<"?">>};
        {_, _, _, Params, _} -> {check_url_query(Params), <<"&">>}
    end.

-spec check_url_query(ne_binary()) -> binary().
check_url_query(Query) ->
    check_url_param(lists:last(binary:split(Query, <<"&">>, [global]))).

-spec check_url_param(ne_binary()) -> binary().
check_url_param(Param) ->
    case binary:split(Param, <<"=">>) of
        [_] -> <<"=">>;
        [_, <<>>] -> <<>>;
        _ -> <<"&recording=">>
    end.

-spec handler_from_url(ne_binary()) -> 'kz_att_ftp' | 'kz_att_http' | 'undefined'.
handler_from_url(Url) ->
    case kz_http_util:urlsplit(Url) of
        {<<"ftp">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"ftps">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"http">>, _, _, _, _} -> 'kz_att_http';
        {<<"https">>, _, _, _, _} -> 'kz_att_http';
        _ -> 'undefined'
    end.

-spec should_store_recording(ne_binary(), api_binary()) -> store_url().
should_store_recording(AccountId, Url) ->
    case kz_term:is_empty(Url) of
        'true' -> maybe_storage_plan(AccountId);
        'false' ->
            case handler_from_url(Url) of
                'undefined' ->
                    lager:debug("invalid protocol for url ~s : not saving attachment"),
                    'false';
                _ -> {'true', 'other', Url}
            end
    end.

-spec maybe_storage_plan(ne_binary()) -> store_url().
maybe_storage_plan(AccountId) ->
    AccountDb = kz_util:format_account_mod_id(AccountId),
    Plan = kzs_plan:get_dataplan(AccountDb, <<"call_recording">>),
    case maps:get('tag', Plan, 'local') =/= 'local'
        orelse maps:is_key('att_handler', Plan) of
        'true' -> {'true', 'local'};
        'false' -> should_store_recording()
    end.

-spec should_store_recording() -> store_url().
should_store_recording() ->
    case kz_media_config:should_store_recordings() of
        'true' -> {'true', 'local'};
        'false' -> 'false'
    end.

-spec save_recording(state()) -> 'ok'.
save_recording(#{media := {_, MediaName}, should_store := 'false'}) ->
    lager:info("not configured to store recording ~s", [MediaName]);
save_recording(#{media := Media}=Store) ->
    case maybe_store_recording_meta(Store) of
        {'error', Err} ->
            lager:warning("error storing metadata : ~p", [Err]),
            gen_server:cast(self(), {'store_failed', Store});
        Rev ->
            StoreUrl = fun()-> store_url(Store, Rev) end,
            store_recording(Media, StoreUrl, Store)
    end.

-spec store_recording({ne_binary(), ne_binary()}, ne_binary() | function(), map()) -> 'ok'.
store_recording({DirName, MediaName}, StoreUrl, #{event := JObj} = Map) ->
    Node = kz_call_event:switch_nodename(JObj),
    Filename = filename:join(DirName, MediaName),
    case kz_storage:store_file(Node, Filename, StoreUrl, Map) of
        {'error', Error} -> lager:error("error storing recording : ~p", [Error]);
        'ok' -> 'ok'
    end.

-spec record_call_command(ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) -> kz_json:object().
record_call_command(EndpointId, Inception, Data, Call) ->
    Format = get_format(kz_json:get_ne_binary_value(<<"format">>, Data)),
    TimeLimit = get_timelimit(kz_json:get_integer_value(<<"time_limit">>, Data)),
    SampleRate = kz_json:get_integer_value(<<"record_sample_rate">>, Data),
    DefaultRecordMinSec = kz_media_config:record_min_sec(),
    RecordMinSec = kz_json:get_integer_value(<<"record_min_sec">>, Data, DefaultRecordMinSec),
                                                %    AccountId = kapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    CallId = kapps_call:call_id(Call),
    RecordingId = kz_binary:rand_hex(16),
    MediaDocId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), RecordingId),
    DefaultMediaName = get_media_name(kz_binary:rand_hex(16), Format),
    MediaName = kz_json:get_value(?RECORDING_ID_KEY, Data, DefaultMediaName),
    Media = [{<<"Application-Name">>, <<"record_call">>}
            ,{<<"Record-Action">>, <<"start">>}
            ,{<<"Follow-Transfer">>, false}
            ,{<<"Time-Limit">>, TimeLimit}
            ,{<<"Media-Name">>, MediaName}
            ,{<<"Media-Recording-ID">>, MediaDocId}
            ,{<<"Media-Recording-Origin">>, Inception}
            ,{?MEDIA_RECORDING_ENDPOINT_ID, EndpointId}
            ,{<<"Record-Sample-Rate">>, SampleRate}
            ,{<<"Record-Min-Sec">>, kz_term:to_binary(RecordMinSec)}
            ,{<<"Media-Recorder">>, <<"kz_media_recording">>}
            ,{<<"Call-ID">>, CallId}
            ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
             | kz_api:default_headers(<<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    kz_json:from_list(Media).

maybe_log_start_recording(Pid, JObj) ->
    kz_util:put_callid(JObj),
    EndpointId = kz_call_event:custom_channel_var(JObj, ?MEDIA_RECORDING_ENDPOINT_ID),
    maybe_log_start_recording(Pid, EndpointId, JObj).

maybe_log_start_recording(_Pid, 'undefined', _JObj) -> 'ok';
maybe_log_start_recording(_Pid, EndpointId, JObj) ->
    {_Dir, Media} = get_response_media(JObj),
    lager:debug("endpoint ~s is being recorded in media ~s", [EndpointId, Media]).

maybe_save_recording(Pid, JObj) ->
    kz_util:put_callid(JObj),
    EndpointId = kz_call_event:custom_channel_var(JObj, ?MEDIA_RECORDING_ENDPOINT_ID),
    maybe_save_recording(Pid, EndpointId, JObj).

maybe_save_recording(_Pid, 'undefined', _JObj) -> 'ok';
maybe_save_recording(_Pid, EndpointId, JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    Media = {_, MediaId} = get_response_media(JObj),
    lager:debug("saving recording media ~s for endpoint ~s in account ~s", [MediaId, EndpointId, AccountId]),
    {'ok', Endpoint} = kz_endpoint:get(EndpointId, AccountId),
    Inception = kz_call_event:custom_channel_var(JObj, <<"Media-Recording-Origin">>),
    Data = kz_json:get_json_value(?ENDPOINT_INBOUND_RECORDING(Inception), Endpoint),

    {Year, Month, _} = erlang:date(),
    AccountDb = kz_util:format_account_modb(kazoo_modb:get_modb(AccountId, Year, Month),'encoded'),
    CallId = kz_call_event:call_id(JObj),
    CdrId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), CallId),
    DocId = kz_call_event:custom_channel_var(JObj, <<"Media-Recording-ID">>),
    InteractionId = kz_call_event:custom_channel_var(JObj, <<?CALL_INTERACTION_ID>>),
    Url = kz_json:get_ne_binary_value(<<"url">>, Data),
    ShouldStore = should_store_recording(AccountId, Url),
    Verb = kz_json:get_atom_value(<<"method">>, Data, 'put'),

    Store = #{url => Url
             ,media => Media
             ,doc_id => DocId
             ,doc_db => AccountDb
             ,cdr_id => CdrId
             ,interaction_id => InteractionId
             ,should_store => ShouldStore
             ,retries => ?STORAGE_RETRY_TIMES(AccountId)
             ,verb => Verb
             ,account_id => AccountId
             ,call_id => CallId
             ,event => JObj
             ,origin => <<"inbound from ", Inception/binary, " to endpoint">>
             },
    save_recording(Store).
