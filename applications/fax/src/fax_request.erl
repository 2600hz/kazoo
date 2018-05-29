%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz INC
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

-export([start_link/3]).

-export([cancel/1]).

%% gen_listener callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-include("fax.hrl").

-define(SERVER, ?MODULE).

-define(POLL_INTERVAL, 5000).

-record(state, {call :: kapps_call:call() | 'undefined'
               ,action = 'receive' :: 'receive' | 'transmit'
               ,owner_id :: api_ne_binary()
               ,faxbox_id :: api_ne_binary()
               ,fax_doc :: api_object()
               ,storage :: fax_storage()
               ,fax_option :: api_ne_binary()
               ,fax_result :: api_object()
               ,fax_notify = 'undefined' :: api_object()
               ,fax_store_count = 0 :: integer()
               ,fax_id = 'undefined' :: api_ne_binary()
               ,account_id = 'undefined' :: api_ne_binary()
               ,fax_status :: api_object()
               ,page = 0  ::integer()
               ,status :: api_ne_binary()
               ,monitor :: pid_ref() | 'undefined'
               }).
-type state() :: #state{}.

-type handle_cast_return() :: {'noreply', state()} |
                              {'stop', atom(), state()}.

-define(BINDINGS(CALL), [{'call', [{'callid', kapps_call:call_id(CALL)}
                                  ,{'restrict_to', [<<"CHANNEL_FAX_STATUS">>
                                                   ,<<"CHANNEL_DESTROY">>
                                                   ]}
                                  ]}
                        ,{'self', []}
                        ]).

-define(RESPONDERS, [{fun handle_fax_event/2
                     ,[{<<"call_event">>, <<"CHANNEL_FAX_STATUS">>}]
                     }
                    ,{fun handle_channel_event/2
                     ,[{<<"call_event">>, <<"CHANNEL_DESTROY">>}]
                     }
                    ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(kapps_call:call(), kz_json:object(), fax_storage()) -> startlink_ret().
start_link(Call, JObj, Storage) ->
    gen_listener:start_link({'local', kz_term:to_atom(Storage#fax_storage.id, 'true')}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS(Call)}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[Call, JObj, Storage]
                           ).

-spec handle_fax_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Event = kz_json:get_value(<<"Application-Event">>, JObj),
    gen_server:cast(Srv, {'fax_status', Event , JObj}).

-spec handle_channel_event(kz_json:object(), kz_proplist()) -> 'ok'.
handle_channel_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Event = kz_api:event_name(JObj),
    gen_server:cast(Srv, {'channel_event', Event , JObj}).

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
-spec init([kapps_call:call() | kz_json:object() | fax_storage()]) -> {'ok', state()}.
init([Call, JObj, Storage]) ->
    kapps_call:put_callid(Call),
    gen_listener:cast(self(), 'start_action'),
    {'ok', #state{call = Call
                 ,action = get_action(JObj)
                 ,owner_id = kz_json:get_ne_binary_value(<<"Owner-ID">>, JObj)
                 ,faxbox_id = kz_json:get_ne_binary_value(<<"FaxBox-ID">>, JObj)
                 ,fax_option = kz_json:get_ne_binary_value(<<"Fax-T38-Option">>, JObj, 'false')
                 ,account_id = kapps_call:account_id(Call)
                 ,fax_id=Storage#fax_storage.id
                 ,storage=Storage
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
    Status = list_to_binary(["fax negotiated at ", kz_term:to_list(TransferRate)]),
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
               ,[JobId, TransferredPages, kz_time:current_tstamp()]
               ),
    Status = list_to_binary(["Received  Page ", kz_term:to_list(Page)]),
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
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State};
handle_cast({'gen_listener',{'is_consuming',_}}, State) ->
    start_receive_fax(State);

handle_cast('store_document', State) ->
    {'noreply', State#state{status = <<"storing document">>}, ?POLL_INTERVAL};
handle_cast({'store_attachment', FaxDoc}, #state{monitor={_, Ref}}=State) ->
    erlang:demonitor(Ref),
    {'noreply', State#state{fax_doc=FaxDoc
                           ,status = <<"storing document">>
                           }, ?POLL_INTERVAL};
handle_cast('success', State) ->
    notify_success(State),
    {'stop', 'normal', State};

handle_cast('cancel', #state{fax_result='undefined'} = State) ->
    lager:warning("canceling active fax receiver"),
    {'stop', 'normal', State};

handle_cast('cancel', State) ->
    lager:warning("canceling fax receiver"),
    {'stop', 'normal', State};

handle_cast({'channel_event', <<"CHANNEL_DESTROY">>, _JObj}, #state{monitor='undefined'} = State) ->
    lager:warning("received channel destroy for fax receiver"),
    {'stop', 'normal', State};

handle_cast({'channel_event', <<"CHANNEL_DESTROY">>, _JObj}, State) ->
    {'noreply', State, ?POLL_INTERVAL};

handle_cast(_Msg, State) ->
    lager:debug("unhandled cast : ~p", [_Msg]),
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
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, #state{monitor={Pid, Ref}}=State) ->
    {'noreply', State, ?POLL_INTERVAL};
handle_info({'DOWN', Ref, 'process', Pid, Reason}, #state{monitor={Pid, Ref}}=State) ->
    lager:debug("process ~p down '~p', retrying", [Pid, Reason]),
    {'noreply', State, ?POLL_INTERVAL};
handle_info('timeout', #state{fax_doc='undefined'}=State) ->
    {'noreply', State#state{monitor=store_document(State)}};
handle_info('timeout', #state{}=State) ->
    {'noreply', State#state{monitor=store_attachment(State)}};
handle_info(_Info, State) ->
    lager:debug("unhandled info: ~p", [_Info]),
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
    kapps_call_command:hangup(Call).

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
                        ,fax_id=FaxId
                        }=State) ->
    NewState = maybe_update_fax_settings(State),
    ResourceFlag = kapps_call:custom_channel_var(<<"Resource-Fax-Option">>, Call),
    LocalFile = get_fs_filename(NewState),
    send_status(NewState, list_to_binary(["New Fax from ", kapps_call:caller_id_number(Call)]), ?FAX_START, 'undefined'),
    kapps_call_command:answer(Call),
    lager:debug("receive fax ~s - t.38 ~p / ~p", [FaxId, ResourceFlag, ReceiveFlag]),
    kapps_call_command:receive_fax(ResourceFlag, ReceiveFlag, LocalFile, Call),
    {'noreply', NewState}.

-spec get_fax_storage(kapps_call:call()) -> fax_storage().
get_fax_storage(Call) ->
    AccountId = kapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    FaxDb = kz_util:format_account_modb(AccountMODb, 'encoded'),
    FaxId = <<(kz_term:to_binary(Year))/binary
              ,(kz_date:pad_month(Month))/binary
              ,"-"
              ,(kz_binary:rand_hex(16))/binary
            >>,
    AttachmentId = kz_binary:rand_hex(16),
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
      ,{<<"RTCP-MUX">>, false}
      ,{<<"Origination-Call-ID">>, kapps_call:call_id(Call)}
      ]).

-spec callee_name(kz_json:object()) -> ne_binary().
callee_name(JObj) ->
    kz_term:to_binary(
      kz_json:get_first_defined([<<"caller_name">>,<<"name">>], JObj)
     ).

-spec overridden_callee_id(kapps_call:call(), kz_json:object()) -> ne_binary().
overridden_callee_id(Call, JObj) ->
    kz_term:to_binary(
      kz_json:get_first_defined([<<"caller_id">>,<<"fax_identity">>], JObj
                               ,kapps_call:to_user(Call)
                               )
     ).

-spec overridden_fax_identity(kapps_call:call(), kz_json:object()) -> ne_binary().
overridden_fax_identity(Call, JObj) ->
    kz_term:to_binary(
      kz_json:get_first_defined([<<"fax_identity">>,<<"caller_id">>], JObj
                               ,kapps_call:to_user(Call)
                               )
     ).

-spec end_receive_fax(kz_json:object(), state()) -> handle_cast_return().
end_receive_fax(JObj, #state{call=Call}=State) ->
    kapps_call_command:hangup(Call),
    case kz_json:is_true([<<"Application-Data">>,<<"Fax-Success">>], JObj, 'false') of
        'true' -> end_receive_fax(State#state{fax_result=JObj});
        'false' ->
            notify_failure(JObj, State),
            {'stop', 'normal', State}
    end.

-spec end_receive_fax(state()) -> handle_cast_return().
end_receive_fax(#state{page=Page, fax_result=JObj}=State) when Page =:= 0 ->
    notify_failure(JObj, State),
    {'stop', 'normal', State};
end_receive_fax(#state{}=State) ->
    {'noreply', State#state{monitor=store_document(State)}}.

-spec store_document(state()) -> pid_ref().
store_document(#state{}=State) ->
    kz_util:spawn_monitor(fun store_document/2, [self(), State]).

-spec store_document(pid(), state() ) -> 'ok'.
store_document(Pid, #state{fax_result=JObj
                          ,storage=#fax_storage{id=FaxId}
                          }=State) ->
    case create_fax_doc(JObj, State) of
        {'ok', FaxDoc} ->
            lager:debug("fax document stored successfully into ~s / ~s", [kz_doc:account_db(FaxDoc), FaxId]),
            gen_server:cast(Pid, {'store_attachment', FaxDoc});
        {'error', Error} ->
            lager:debug("error storing fax document ~s: ~p", [FaxId, Error]),
            gen_server:cast(Pid, 'store_document')
    end.

-spec store_attachment(state()) -> pid_ref().
store_attachment(#state{}=State) ->
    kz_util:spawn_monitor(fun store_attachment/2, [self(), State]).

-spec store_attachment(pid(), state()) -> 'ok'.
store_attachment(Pid, #state{call=Call
                            ,storage=#fax_storage{attachment_id=AttachmentId}
                            ,fax_doc=FaxDoc
                            ,fax_id=FaxId
                            }=State) ->
    FaxUrlFun = fun() -> kz_media_url:store(FaxDoc, AttachmentId) end,
    FaxFile = get_fs_filename(State),
    case kapps_call_command:store_file(FaxFile, FaxUrlFun, Call) of
        'ok' ->
            lager:debug("fax attachment stored successfully into ~s / ~s / ~s", [kz_doc:account_db(FaxDoc), FaxId, AttachmentId]),
            gen_server:cast(Pid, 'success');
        {'error', _} -> gen_server:cast(Pid, 'store_attachment')
    end.

-spec get_fs_filename(state()) -> ne_binary().
get_fs_filename(#state{storage=#fax_storage{attachment_id=AttachmentId}}) ->
    LocalPath = kapps_config:get_binary(?CONFIG_CAT, <<"fax_file_path">>, <<"/tmp/">>),
    <<LocalPath/binary, AttachmentId/binary>>.

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
    {{Y,M,D}, {H,I,S}} = calendar:gregorian_seconds_to_datetime(kz_time:current_tstamp()),
    Name = list_to_binary(["fax message received at "
                          ,kz_term:to_binary(Y), "-", kz_term:to_binary(M), "-", kz_term:to_binary(D)
                          ," " , kz_term:to_binary(H), ":", kz_term:to_binary(I), ":", kz_term:to_binary(S)
                          ," UTC"
                          ]),

    ?MATCH_MODB_PREFIX(Year,Month,_) = FaxDocId,
    CdrId = <<(kz_term:to_binary(Year))/binary
              ,(kz_date:pad_month(Month))/binary
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
              ,{<<"pvt_job_node">>, kz_term:to_binary(node())}
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
      ,{<<"Fax-Timestamp">>, kz_time:current_tstamp()}
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
    notify_failure(JObj, kz_term:to_binary(NonBinary), State);
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
    kapps_notify_publisher:cast(Message, fun kapi_notifications:publish_fax_inbound_error/1).

-spec notify_success(state()) -> 'ok'.
notify_success(#state{call=Call
                     ,owner_id=OwnerId
                     ,faxbox_id=FaxBoxId
                     ,fax_notify=Notify
                     ,account_id=AccountId
                     ,fax_result=JObj
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
    kapps_notify_publisher:cast(Message, fun kapi_notifications:publish_fax_inbound/1).

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
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, JObj)),
    Storage = get_fax_storage(Call),
    Props = [{<<"Fax-Doc-ID">>, Storage#fax_storage.id}
            ,{<<"Fax-Doc-DB">>, Storage#fax_storage.db}
            ],
    NewCall = kapps_call:kvs_store_proplist(Props, Call),
    fax_requests_sup:new(NewCall, JObj, Storage).

-spec cancel(ne_binary()) -> 'ok'.
cancel(Id) ->
    case whereis(kz_term:to_atom(Id, 'true')) of
        'undefined' -> io:format("job ~s not found", [Id]);
        Pid -> gen_server:cast(Pid, 'cancel')
    end.
