%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_request).
-behaviour(gen_listener).

%% API
-export([new_request/2]).

-export([start_link/2]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([handle_fax_event/2
        ,handle_execute_complete/2
        ]).

-include("fax.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          call :: kapps_call:call()
               ,action = 'receive' :: 'receive' | 'transmit'
               ,owner_id :: api_binary()
               ,faxbox_id :: api_binary()
               ,fax_doc :: api_object()
               ,storage :: fax_storage()
               ,fax_option :: api_binary()
               ,fax_result :: api_object()
               ,fax_notify = 'undefined' :: api_object()
               ,fax_store_count = 0 :: integer()
               ,fax_id = 'undefined' :: api_binary()
               ,account_id = 'undefined' :: api_binary()
               ,fax_status :: api_object()
               ,page = 0  ::integer()
               ,status :: binary()
         }).
-type state() :: #state{}.

-type handle_cast_return() :: {'noreply', state()} |
                              {'stop', atom(), state()}.

-define(BINDINGS(CALL), [{'call', [{'callid', kapps_call:call_id(CALL)}
                                  ,{'restrict_to', [<<"CHANNEL_EXECUTE_COMPLETE">>
                                                   ,<<"CHANNEL_FAX_STATUS">>
                                                   ]}
                                  ]}
                        ,{'self', []}
                        ]).

-define(RESPONDERS, [{{?MODULE, 'handle_execute_complete'}
                     ,[{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}]
                     }
                    ,{{?MODULE, 'handle_fax_event'}
                     ,[{<<"call_event">>, <<"CHANNEL_FAX_STATUS">>}]
                     }
                    ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kapps_call:call(), kz_json:object()) -> startlink_ret().
start_link(Call, JObj) ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS(Call)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Call, JObj]
                           ).

-spec handle_execute_complete(kz_json:object(), kz_proplist()) -> 'ok'.
handle_execute_complete(JObj, Props) ->
    AppName = kz_json:get_value(<<"Application-Name">>, JObj),
    AppResp = kz_json:get_value(<<"Application-Response">>, JObj),
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'exec_completed', AppName, AppResp, JObj}).

-spec handle_fax_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Event = kz_json:get_value(<<"Application-Event">>, JObj),
    gen_server:cast(Srv, {'fax_status', Event , JObj}).

%%%===================================================================
%%% gen_listener callbacks
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
-spec init([kapps_call:call() | kz_json:object()]) -> {'ok', state()}.
init([Call, JObj]) ->
    kapps_call:put_callid(Call),
    gen_listener:cast(self(), 'start_action'),
    {'ok', #state{call = Call
                 ,action = get_action(JObj)
                 ,owner_id = kz_json:get_value(<<"Owner-ID">>, JObj)
                 ,faxbox_id = kz_json:get_value(<<"FaxBox-ID">>, JObj)
                 ,fax_option = kz_json:get_value(<<"Fax-T38-Option">>, JObj, 'false')
                 ,account_id = kapps_call:account_id(Call)
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
-spec handle_cast(any(), state()) -> handle_cast_return().
handle_cast('start_action', #state{call=_Call
                                  ,action='receive'
                                  ,owner_id=OwnerId
                                  ,faxbox_id=FaxBoxId
                                  }=State) ->
    lager:debug("receiving a fax for ~p/~p", [OwnerId,FaxBoxId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JObj}, State) ->
    Data = kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new()),
    TransferRate = kz_json:get_integer_value(<<"Fax-Transfer-Rate">>, Data, 1),
    lager:debug("fax status - negociate result - ~s : ~p",[State#state.fax_id, TransferRate]),
    Status = list_to_binary(["fax negotiated at ", kz_util:to_list(TransferRate)]),
    send_status(State, Status, Data),
    {'noreply', State#state{status=Status
                           ,page=1
                           ,fax_status=Data
                           }};
handle_cast({'fax_status', <<"pageresult">>, JObj}
           ,#state{page=Page
                  ,fax_id=JobId
                  }=State
           ) ->
    Data = kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new()),
    TransferredPages = kz_json:get_integer_value(<<"Fax-Transferred-Pages">>, Data, 0),
    lager:debug("fax status - page result - ~s : ~p : ~p"
               ,[JobId, TransferredPages, kz_util:current_tstamp()]
               ),
    Status = list_to_binary(["Received  Page ", kz_util:to_list(Page)]),
    send_status(State, Status, Data),
    {'noreply', State#state{page=TransferredPages
                           ,status=Status
                           ,fax_status=Data
                           }};
handle_cast({'fax_status', <<"result">>, JObj}, State) ->
    end_receive_fax(JObj, State);
handle_cast({'fax_status', Event, _JObj}, State) ->
    lager:debug("fax status not handled - ~s",[Event]),
    {'noreply', State};
handle_cast({'exec_completed', <<"receive_fax">>, Result, _JObj}, State) ->
    lager:debug("Fax Receive Result ~s",[Result]),
    {'noreply', State};
handle_cast({'exec_completed', App, Result, _JObj}, State) ->
    lager:debug("Fax exec not handled - ~s / ~s ",[App,Result]),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_}}, State) ->
    start_receive_fax(State);
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
handle_info({'DOWN', _Ref, 'process', Pid, 'normal'}, State) ->
    lager:debug("handler ~p down normally, request is done", [Pid]),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

-spec handle_event(kz_json:object(), state()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{call=Call}) ->
    kapps_call_command:hangup(Call),
    lager:debug("fax request terminating: ~p", [_Reason]).

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
-spec get_action(kz_json:object()) -> 'receive' | 'transmit'.
get_action(JObj) ->
    case kz_json:get_value(<<"Action">>, JObj) of
        <<"transmit">> -> 'transmit';
        _ -> 'receive'
    end.

-spec start_receive_fax(state()) -> {'noreply', state()}.
start_receive_fax(#state{call=Call
                        ,fax_option=ReceiveFlag
                        }=State) ->
    kapps_call:put_callid(Call),
    Storage = get_fax_storage(Call),
    Props = [{<<"Fax-Doc-ID">>, Storage#fax_storage.id}
            ,{<<"Fax-Doc-DB">>, Storage#fax_storage.db}
            ],
    NewCall = kapps_call:kvs_store_proplist(Props, Call),
    NewState = maybe_update_fax_settings(State#state{storage=Storage
                                                    ,fax_id=Storage#fax_storage.id
                                                    ,call=NewCall
                                                    }),
    ResourceFlag = kapps_call:custom_channel_var(<<"Resource-Fax-Option">>, Call),
    LocalFile = get_fs_filename(NewState),
    send_status(NewState, list_to_binary(["New Fax from ", kapps_call:caller_id_number(Call)]), ?FAX_START, 'undefined'),
    kapps_call_command:answer(Call),
    lager:debug("receive fax t.38 ~p / ~p", [ResourceFlag, ReceiveFlag]),
    kapps_call_command:receive_fax(ResourceFlag, ReceiveFlag, LocalFile, Call),
    {'noreply', NewState}.

-spec get_fax_storage(kapps_call:call()) -> fax_storage().
get_fax_storage(Call) ->
    AccountId = kapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    FaxDb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    FaxId = <<(kz_util:to_binary(Year))/binary
              ,(kz_util:pad_month(Month))/binary
              ,"-"
              ,(kz_util:rand_hex_binary(16))/binary
            >>,
    AttachmentId = kz_util:rand_hex_binary(16),
    Ext = kapps_config:get_binary(?CONFIG_CAT, <<"default_fax_extension">>, <<".tiff">>),
    FaxAttachmentId = <<AttachmentId/binary, Ext/binary>>,

    #fax_storage{id=FaxId
                ,db=FaxDb
                ,attachment_id=FaxAttachmentId
                }.

-spec maybe_update_fax_settings(state()) -> state().
maybe_update_fax_settings(#state{call=Call
                                ,owner_id=OwnerId
                                ,faxbox_id='undefined'
                                }=State) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} ->
            lager:debug("updating fax settings from user ~s", [OwnerId]),
            update_fax_settings(Call, kzd_user:fax_settings(JObj)),
            case kz_json:is_true(<<"fax_to_email_enabled">>, JObj, 'true') of
                'true' ->
                    UserEmail = kz_json:get_value(<<"email">>, JObj),
                    Notify = kz_json:set_value([<<"email">>,<<"send_to">>], [UserEmail] , kz_json:new()),
                    State#state{fax_notify=Notify};
                'false' -> State
            end;
        {'error', _} ->
            maybe_update_fax_settings_from_account(State)
    end;
maybe_update_fax_settings(#state{call=Call
                                ,faxbox_id=FaxBoxId
                                }=State) ->
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:open_doc(AccountDb, FaxBoxId) of
        {'ok', JObj} ->
            update_fax_settings(Call, JObj),
            State#state{fax_notify=get_faxbox_notify_list(JObj, AccountDb)};
        {'error', _} -> maybe_update_fax_settings_from_account(State)
    end.

-spec get_faxbox_notify_list(kz_json:object(), ne_binary()) -> kz_json:object().
get_faxbox_notify_list(FaxBoxDoc, AccountDb) ->
    DefaultNotify = default_notify(FaxBoxDoc),
    case kz_json:get_value(<<"owner_id">>, FaxBoxDoc) of
        'undefined' -> DefaultNotify;
        OwnerId ->
            case kz_datamgr:open_cache_doc(AccountDb, OwnerId) of
                {'ok', UserDoc} ->
                    List = kz_json:get_value([<<"email">>,<<"send_to">>], DefaultNotify, []),
                    maybe_add_owner_to_notify_list(List, kz_json:get_value(<<"email">>, UserDoc));
                _ ->
                    DefaultNotify
            end
    end.

-spec default_notify(kz_json:object()) -> kz_json:object().
default_notify(FaxBoxDoc) ->
    kz_json:get_value([<<"notifications">>,<<"inbound">>], FaxBoxDoc, kz_json:new()).

-spec maybe_add_owner_to_notify_list(list(), api_binary()) -> kz_json:object().
maybe_add_owner_to_notify_list(List, 'undefined') ->
    kz_json:set_value([<<"email">>, <<"send_to">>], List, kz_json:new());
maybe_add_owner_to_notify_list(List, OwnerEmail) ->
    NotifyList = fax_util:notify_email_list('undefined', OwnerEmail, List),
    kz_json:set_value([<<"email">>, <<"send_to">>], NotifyList, kz_json:new()).

-spec maybe_update_fax_settings_from_account(state()) -> any().
maybe_update_fax_settings_from_account(#state{call=Call}=State) ->
    case kz_account:fetch(kapps_call:account_id(Call)) of
        {'ok', JObj} ->
            lager:debug("updating fax settings from account"),
            update_fax_settings(Call, kz_account:fax_settings(JObj));
        {'error', _} ->
            lager:debug("no settings for local fax - missing account"),
            update_fax_settings(Call, kz_json:new())
    end,
    State.

-spec update_fax_settings(kapps_call:call(), kz_json:object()) -> any().
update_fax_settings(Call, JObj) ->
    ChannelVars = build_fax_settings(Call, JObj),
    kapps_call_command:set(kz_json:from_list(ChannelVars), 'undefined', Call).

-spec build_fax_settings(kapps_call:call(), kz_json:object()) -> kz_proplist().
build_fax_settings(Call, JObj) ->
    props:filter_undefined(
      [case kz_json:is_true(<<"override_fax_identity">>, JObj, 'true') of
           'false' ->
               {<<"Fax-Identity-Number">>, kapps_call:to_user(Call)};
           'true' ->
               {<<"Fax-Identity-Number">>, overridden_fax_identity(Call, JObj)}
       end,
       case kz_json:is_true(<<"override_callee_number">>, JObj, 'false') of
           'false' ->
               {<<"Callee-ID-Number">>, kapps_call:to_user(Call)};
           'true' ->
               {<<"Callee-ID-Number">>, overridden_callee_id(Call, JObj)}
       end
      ,{<<"Fax-Identity-Name">>, kz_json:get_value(<<"fax_header">>, JObj)}
      ,{<<"Fax-Timezone">>, kzd_fax_box:timezone(JObj)}
      ,{<<"Callee-ID-Name">>, callee_name(JObj)}
      ,{<<"Fax-Doc-ID">>, kapps_call:kvs_fetch(<<"Fax-Doc-ID">>, Call) }
      ,{<<"Fax-Doc-DB">>, kapps_call:kvs_fetch(<<"Fax-Doc-DB">>, Call) }
      ]).

-spec callee_name(kz_json:object()) -> ne_binary().
callee_name(JObj) ->
    kz_util:to_binary(
      kz_json:get_first_defined([<<"caller_name">>,<<"name">>], JObj)
     ).

-spec overridden_callee_id(kapps_call:call(), kz_json:object()) -> ne_binary().
overridden_callee_id(Call, JObj) ->
    kz_util:to_binary(
      kz_json:get_first_defined([<<"caller_id">>,<<"fax_identity">>], JObj
                               ,kapps_call:to_user(Call)
                               )
     ).

-spec overridden_fax_identity(kapps_call:call(), kz_json:object()) -> ne_binary().
overridden_fax_identity(Call, JObj) ->
    kz_util:to_binary(
      kz_json:get_first_defined([<<"fax_identity">>,<<"caller_id">>], JObj
                               ,kapps_call:to_user(Call)
                               )
     ).

-spec end_receive_fax(kz_json:object(), state()) -> handle_cast_return().
end_receive_fax(JObj, #state{call=Call}=State) ->
    kapps_call_command:hangup(Call),
    case kz_json:is_true([<<"Application-Data">>,<<"Fax-Success">>], JObj, 'false') of
        'true' -> maybe_store_fax(JObj, State);
        'false' ->
            notify_failure(JObj, State),
            {'stop', 'normal', State}
    end.

-spec maybe_store_fax(kz_json:object(), state()) -> handle_cast_return().
maybe_store_fax(JObj, #state{storage=#fax_storage{id=FaxId}}=State) ->
    case store_fax(JObj, State) of
        {'ok', FaxDoc} ->
            lager:debug("fax stored successfully into ~s / ~s", [kz_doc:account_db(FaxDoc), FaxId]),
            store_attachment(State#state{fax_doc=FaxDoc, fax_result=JObj});
        {'error', Error} ->
            lager:debug("store fax other resp: ~p", [Error]),
            notify_failure(JObj, Error, State),
            {'stop', 'normal', State}
    end.

-spec store_fax(kz_json:object(), state() ) ->
                       {'ok', kz_json:object()} |
                       {'error', any()}.
store_fax(JObj, #state{storage=#fax_storage{attachment_id=_AttachmentId}
                      }=State) ->
    case create_fax_doc(JObj, State) of
        {'ok', _Doc} = OK -> OK;
        Error -> Error
    end.

-spec get_fs_filename(state()) -> ne_binary().
get_fs_filename(#state{storage=#fax_storage{attachment_id=AttachmentId}}) ->
    LocalPath = kapps_config:get_binary(?CONFIG_CAT, <<"fax_file_path">>, <<"/tmp/">>),
    <<LocalPath/binary, AttachmentId/binary>>.

-spec store_attachment(state()) -> handle_cast_return().
store_attachment(#state{call=Call
                       ,fax_result=FaxResultObj
                       ,storage=#fax_storage{attachment_id=AttachmentId}
                       ,fax_doc=FaxDoc
                       }=State) ->
    FaxUrl = kz_media_url:store(FaxDoc, AttachmentId),
    FaxFile = get_fs_filename(State),
    case kapps_call_command:store_file(FaxFile, FaxUrl, Call) of
        'ok' ->
            notify_success(FaxResultObj, State),
            {'stop', 'normal', State};
        {'error', Error} ->
            notify_failure(FaxResultObj, Error, State),
            {'stop', 'normal', State}
    end.

-spec create_fax_doc(kz_json:object(), state()) ->
                            {'ok', kz_json:object()} |
                            {'error', any()}.
create_fax_doc(JObj, #state{owner_id = OwnerId
                           ,faxbox_id = FaxBoxId
                           ,fax_notify = Notify
                           ,call=Call
                           ,storage=#fax_storage{id=FaxDocId
                                                ,db=FaxDb
                                                }
                           }) ->
    {{Y,M,D}, {H,I,S}} = calendar:gregorian_seconds_to_datetime(kz_util:current_tstamp()),
    Name = list_to_binary(["fax message received at "
                          ,kz_util:to_binary(Y), "-", kz_util:to_binary(M), "-", kz_util:to_binary(D)
                          ," " , kz_util:to_binary(H), ":", kz_util:to_binary(I), ":", kz_util:to_binary(S)
                          ," UTC"
                          ]),

    ?MATCH_MODB_PREFIX(Year,Month,_) = FaxDocId,
    CdrId = <<(kz_util:to_binary(Year))/binary
              ,(kz_util:pad_month(Month))/binary
              ,"-"
              ,(kapps_call:call_id(Call))/binary
            >>,

    Props = props:filter_undefined(
              [{<<"name">>, Name}
              ,{<<"to_number">>, kapps_call:request_user(Call)}
              ,{<<"from_number">>, kapps_call:from_user(Call)}
              ,{<<"description">>, <<"fax document received">>}
              ,{<<"source_type">>, <<"incoming_fax">>}
              ,{<<"folder">>, <<"inbox">>}
              ,{<<"timestamp">>, kz_json:get_value(<<"Timestamp">>, JObj)}
              ,{<<"owner_id">>, OwnerId}
              ,{<<"faxbox_id">>, FaxBoxId}
              ,{<<"media_type">>, <<"tiff">>}
              ,{<<"call_id">>, kapps_call:call_id(Call)}
              ,{<<"cdr_doc_id">>, CdrId}
              ,{<<"_id">>, FaxDocId}
              ,{<<"rx_result">>, rx_result(JObj)}
              ,{<<"pvt_job_node">>, kz_util:to_binary(node())}
              ,{<<"notifications">>, Notify}
              ]),

    Doc = kz_doc:update_pvt_parameters(kz_json:from_list(Props)
                                      ,FaxDb
                                      ,[{'type', <<"fax">>}]
                                      ),
    kazoo_modb:save_doc(kapps_call:account_id(Call), Doc).

-spec rx_result(kz_json:object()) -> kz_json:object().
rx_result(JObj) ->
    kz_json:from_list(
      fax_util:fax_properties(
        kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new())
       )
     ).

-spec fax_fields(kz_json:object()) -> kz_json:object().
fax_fields(JObj) ->
    kz_json:from_list(
      [{K, V}
       || {<<"Fax-", _/binary>> = K, V} <- kz_json:to_proplist(JObj)
      ]).

-spec notify_fields(kapps_call:call(), kz_json:object()) -> kz_proplist().
notify_fields(Call, JObj) ->
    props:filter_empty(
      [{<<"From-User">>, kapps_call:from_user(Call)}
      ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
      ,{<<"To-User">>, kapps_call:to_user(Call)}
      ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
      ,{<<"Fax-Info">>, fax_fields(kz_json:get_value(<<"Application-Data">>, JObj))}
      ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
      ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
      ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
      ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
      ,{<<"Call-ID">>, kapps_call:call_id(Call)}
      ,{<<"Fax-Timestamp">>, kz_util:current_tstamp()}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).

-spec notify_failure(kz_json:object(), state()) -> 'ok'.
notify_failure(JObj, State) ->
    Reason = kz_json:get_value([<<"Application-Data">>,<<"Fax-Result">>], JObj),
    notify_failure(JObj, Reason, State).

-spec notify_failure(kz_json:object(), binary() | atom(), state()) -> 'ok'.
notify_failure(JObj, 'undefined', State) ->
    notify_failure(JObj, <<"unknown error">>, State);
notify_failure(JObj, NonBinary, State) when not is_binary(NonBinary) ->
    notify_failure(JObj, kz_util:to_binary(NonBinary), State);
notify_failure(JObj, Reason, #state{call=Call
                                   ,owner_id=OwnerId
                                   ,faxbox_id=FaxBoxId
                                   ,fax_notify=Notify
                                   ,account_id=AccountId
                                   ,storage=#fax_storage{id=FaxId, db=FaxDb}
                                   }=State) ->
    Data = kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new()),
    Status = list_to_binary(["Error receiving fax : ", Reason]),
    send_error_status(State, Status, Data),
    Message = props:filter_undefined(
                [{<<"Fax-ID">>, FaxId}
                ,{<<"Fax-Error">>, Reason}
                ,{<<"Owner-ID">>, OwnerId}
                ,{<<"FaxBox-ID">>, FaxBoxId}
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Account-DB">>, FaxDb}
                ,{<<"Fax-Notifications">>,  Notify}
                 | notify_fields(Call, JObj)
                ]),
    kapi_notifications:publish_fax_inbound_error(Message).

-spec notify_success(kz_json:object(), state()) -> 'ok'.
notify_success(JObj, #state{call=Call
                           ,owner_id=OwnerId
                           ,faxbox_id=FaxBoxId
                           ,fax_notify=Notify
                           ,account_id=AccountId
                           ,storage=#fax_storage{id=FaxId, db=FaxDb}
                           }=State) ->
    Data = kz_json:get_value(<<"Application-Data">>, JObj, kz_json:new()),
    Status = <<"Fax Successfuly received">>,
    send_status(State, Status, ?FAX_END, Data),

    Message = props:filter_undefined(
                [{<<"Fax-ID">>, FaxId}
                ,{<<"Owner-ID">>, OwnerId}
                ,{<<"FaxBox-ID">>, FaxBoxId}
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Account-DB">>, FaxDb}
                ,{<<"Fax-Notifications">>, Notify}
                 | notify_fields(Call, JObj)
                ]),
    kapi_notifications:publish_fax_inbound(Message).

-spec send_error_status(state(), ne_binary(), api_object()) -> 'ok'.
send_error_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_ERROR, FaxInfo).

-spec send_status(state(), ne_binary(), api_object()) -> 'ok'.
send_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_RECEIVE, FaxInfo).

-spec send_status(state(), ne_binary(), ne_binary(), api_object()) -> 'ok'.
send_status(#state{call=Call
                  ,account_id=AccountId
                  ,page=Page
                  ,fax_id=JobId
                  ,faxbox_id=FaxboxId
                  }
           ,Status, FaxState, FaxInfo
           ) ->
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                ,{<<"FaxBox-ID">>, FaxboxId}
                ,{<<"Account-ID">>, AccountId}
                ,{<<"Status">>, Status}
                ,{<<"Fax-State">>, FaxState}
                ,{<<"Fax-Info">>, FaxInfo}
                ,{<<"Direction">>, ?FAX_INCOMING}
                ,{<<"Page">>, Page}
                ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
                ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
                ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
                ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    kapi_fax:publish_status(Payload).

-spec new_request(kz_json:object(), kz_proplist()) -> sup_startchild_ret().
new_request(JObj, _Props) ->
    'true' = kapi_fax:req_v(JObj),
    fax_requests_sup:new(kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)), JObj).
