%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc Handles stop recording.
%%%
%%% @author Luis Azedo
%%% @end
%%%-----------------------------------------------------------------------------
-module(media_recording).

-behaviour(gen_listener).

-export([start_link/0]).

-export([handle_record_stop/2]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("media.hrl").

-define(SERVER, ?MODULE).

-type store_url() :: 'false' |
                     {'true', 'local'} |
                     {'true', 'other', kz_term:ne_binary()}.

-type state() :: map().

-define(STORAGE_RETRY_TIMES(AccountId)
       ,kz_media_config:storage_retry_times(AccountId)
       ).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'call', [{'restrict_to', ['RECORD_STOP']}]}]).

-define(RESPONDERS, [{{?MODULE, 'handle_record_stop'}, [{<<"*">>, <<"*">>}]}]).

-define(QUEUE_NAME, <<"recordings">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(MAX_RECORDING_LIMIT, kz_media_util:max_recording_time_limit()).
-define(CHECK_CHANNEL_STATUS_TIMEOUT, 5 * ?MILLISECONDS_IN_SECOND).
-define(RECORDING_ID_KEY, <<"media_name">>).

-spec start_link() -> kz_types:startlink_ret().
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

-spec handle_record_stop(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_record_stop(JObj, Props) ->
    kz_util:put_callid(JObj),
    Pid = props:get_value('server', Props),
    maybe_save_recording(Pid, JObj).

-spec init([]) -> {'ok', state()}.
init([]) ->
    lager:info("starting event listener for inbound endpoint record_call"),
    {'ok', #{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'store_succeeded', #{}}, State) ->
    lager:debug("store succeeded"),
    {'noreply', State};

handle_cast({'store_failed', #{retries := 0}, Error}, State) ->
    lager:debug("store failed : ~p, no more retries.", [Error]),
    {'noreply', State};

handle_cast({'store_failed', #{retries := Retries} = Store, Error}, State) ->
    Sleep = ?MILLISECONDS_IN_MINUTE * rand:uniform(10),
    lager:debug("store failed : ~p, retrying ~p more times, next in ~p minute(s)"
               ,[Error, Retries, Sleep / ?MILLISECONDS_IN_MINUTE]
               ),
    timer:send_after(Sleep, self(), {'retry_storage', Store}),
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

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'retry_storage', #{retries := Retries} = Store}, State) ->
    _ = kz_util:spawn(fun() -> save_recording(Store#{retries => Retries - 1}) end),
    {'noreply', State};

handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec store_recording_meta(map()) -> kz_term:ne_binary() | {'error', any()}.
store_recording_meta(#{media := {_, MediaName}
                      ,doc_db := Db
                      ,doc_id := DocId
                      ,cdr_id := CdrId
                      ,interaction_id := InteractionId
                      ,url := Url
                      ,call_id := CallId
                      ,event := JObj
                      ,account_id := AccountId
                      ,endpoint_id := EndpointId
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
                     ,{<<"endpoint_id">>, EndpointId}
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

-spec maybe_store_recording_meta(map()) -> kz_term:ne_binary() | {'error', any()}.
maybe_store_recording_meta(#{doc_db := Db
                            ,doc_id := DocId
                            }=State) ->
    case kz_datamgr:lookup_doc_rev(Db, {<<"call_recording">>, DocId}) of
        {'ok', Rev} -> Rev;
        _ -> store_recording_meta(State)
    end.

-spec store_url(map(), kz_term:ne_binary()) -> kz_term:ne_binary().
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

-spec handler_fields(kz_term:ne_binary(), state()) -> list().
handler_fields(Url, State) ->
    {Protocol, _, _, _, _} = kz_http_util:urlsplit(Url),
    handler_fields_for_protocol(Protocol, Url, State).

-spec handler_fields_for_protocol(kz_term:ne_binary(), kz_term:ne_binary(), state()) -> list().
handler_fields_for_protocol(<<"ftp", _/binary>>, _Url, #{extension:=Ext}) ->
    [<<"call_recording_">>
    ,{field, <<"call_id">>}
    ,<<".", Ext/binary>>
    ];
handler_fields_for_protocol(<<"http", _/binary>>, Url, #{account_id:=AccountId
                                                        ,extension:=Ext
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

-spec check_url(kz_term:ne_binary()) -> {binary(), kz_term:ne_binary()}.
check_url(Url) ->
    case kz_http_util:urlsplit(Url) of
        {_, _, _, <<>>, _} -> {<<>>, <<"?">>};
        {_, _, _, Params, _} -> {check_url_query(Params), <<"&">>}
    end.

-spec check_url_query(kz_term:ne_binary()) -> binary().
check_url_query(Query) ->
    check_url_param(lists:last(binary:split(Query, <<"&">>, [global]))).

-spec check_url_param(kz_term:ne_binary()) -> binary().
check_url_param(Param) ->
    case binary:split(Param, <<"=">>) of
        [_] -> <<"=">>;
        [_, <<>>] -> <<>>;
        _ -> <<"&recording=">>
    end.

-spec handler_from_url(kz_term:ne_binary()) -> 'kz_att_ftp' | 'kz_att_http' | 'undefined'.
handler_from_url(Url) ->
    case kz_http_util:urlsplit(Url) of
        {<<"ftp">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"ftps">>, _, _, _, _} -> 'kz_att_ftp';
        {<<"http">>, _, _, _, _} -> 'kz_att_http';
        {<<"https">>, _, _, _, _} -> 'kz_att_http';
        _ -> 'undefined'
    end.

-spec should_store_recording(kz_term:ne_binary(), kz_term:api_binary()) -> store_url().
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

-spec maybe_storage_plan(kz_term:ne_binary()) -> store_url().
maybe_storage_plan(AccountId) ->
    AccountDb = kz_util:format_account_mod_id(AccountId),
    Plan = kzs_plan:get_dataplan(AccountDb, <<"call_recording">>),
    case maps:get('tag', Plan, <<"local">>) =/= <<"local">>
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
save_recording(#{media := Media, pid := Pid}=Store) ->
    case maybe_store_recording_meta(Store) of
        {'error', Err} ->
            lager:warning("error storing metadata : ~p", [Err]),
            gen_server:cast(Pid, {'store_failed', Store});
        Rev ->
            StoreUrl = fun()-> store_url(Store, Rev) end,
            store_recording(Media, StoreUrl, Store)
    end.

-spec store_recording({kz_term:ne_binary(), kz_term:ne_binary()}, kz_term:ne_binary() | function(), map()) -> 'ok'.
store_recording({DirName, MediaName}, StoreUrl, #{event := JObj, pid := Pid} = Map) ->
    Node = kz_call_event:switch_nodename(JObj),
    Filename = filename:join(DirName, MediaName),
    case kz_storage:store_file(Node, Filename, StoreUrl, Map) of
        {'error', Error} -> gen_server:cast(Pid, {'store_failed', Map, Error});
        'ok' -> gen_server:cast(Pid, {'store_succeeded', Map})
    end.

maybe_save_recording(Pid, JObj) ->
    kz_util:put_callid(JObj),
    maybe_save_recording(kz_recording:recorder(JObj), Pid, JObj).

maybe_save_recording(?KZ_RECORDER, Pid, JObj) ->
    AccountId = kz_call_event:account_id(JObj),
    Media = {_, MediaId} = kz_recording:response_media(JObj),
    Ext = filename:extension(MediaId),
    lager:debug("saving recording media ~s in account ~s", [MediaId, AccountId]),
    Data = kz_recording:data(JObj, kz_json:new()),
    DocId = kz_recording:id(JObj),
    {Year, Month, _} = erlang:date(),
    AccountDb = kz_util:format_account_modb(kazoo_modb:get_modb(AccountId, Year, Month),'encoded'),
    CallId = kz_call_event:call_id(JObj),
    CdrId = ?MATCH_MODB_PREFIX(kz_term:to_binary(Year), kz_date:pad_month(Month), CallId),
    InteractionId = kz_call_event:custom_channel_var(JObj, <<?CALL_INTERACTION_ID>>),
    Url = kz_json:get_ne_binary_value(<<"url">>, Data),
    ShouldStore = should_store_recording(AccountId, Url),
    Verb = kz_json:get_ne_binary_value(<<"method">>, Data, <<"put">>),
    Origin = kz_json:get_ne_binary_value(<<"origin">>, Data, <<"no origin">>),
    EndpointId = kz_json:get_ne_binary_value(<<"endpoint_id">>, Data),

    Store = #{url => Url
             ,media => Media
             ,extension => Ext
             ,doc_id => DocId
             ,doc_db => AccountDb
             ,cdr_id => CdrId
             ,interaction_id => InteractionId
             ,should_store => ShouldStore
             ,retries => ?STORAGE_RETRY_TIMES(AccountId)
             ,verb => Verb
             ,account_id => AccountId
             ,endpoint_id => EndpointId
             ,call_id => CallId
             ,event => JObj
             ,origin => Origin
             ,pid => Pid
             },
    save_recording(Store);
maybe_save_recording(Recorder, _Pid, _JObj) ->
     lager:info("recorder ~s not handled", [Recorder]).
