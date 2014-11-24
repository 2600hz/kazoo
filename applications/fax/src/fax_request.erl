%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fax_request).

-behaviour(gen_listener).

%% API
-export([start_link/2
        ,handle_fax_event/2
        ,handle_execute_complete/2
        ]).

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

-record(state, {
          call :: whapps_call:call()
         ,action = 'receive' :: 'receive' | 'transmit'
         ,owner_id :: api_binary()
         ,faxbox_id :: api_binary()
         ,storage :: fax_storage()
         ,fax_option :: api_binary()
         ,fax_result :: api_object()
         ,fax_notify = 'undefined' :: api_binaries()
         ,fax_store_count = 0 :: integer()
         ,fax_id = 'undefined' :: api_binary()
         ,account_id = 'undefined' :: api_binary()
         ,fax_status :: api_object()
         ,page = 0  ::integer()
         ,status :: binary()
         }).
-type state() :: #state{}.

-define(BINDINGS(CALL), [{'call', [{'callid', whapps_call:call_id(CALL)}
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
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Call, JObj) ->
    gen_listener:start_link(?MODULE
                            ,[{'bindings', ?BINDINGS(Call)}
                             ,{'responders', ?RESPONDERS}
                           ]
                          ,[Call, JObj]).


handle_execute_complete(JObj, Props) ->
    AppName = wh_json:get_value(<<"Application-Name">>, JObj),
    AppResp = wh_json:get_value(<<"Application-Response">>, JObj),
    Srv = props:get_value('server', Props),
    gen_server:cast(Srv, {'exec_completed', AppName, AppResp, JObj}).


handle_fax_event(JObj, Props) ->
    Srv = props:get_value('server', Props),
    Event = wh_json:get_value(<<"Application-Event">>, JObj),
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
init([Call, JObj]) ->
    whapps_call:put_callid(Call),
    gen_listener:cast(self(), 'start_action'),
    {'ok', #state{call = Call
                 ,action = get_action(JObj)
                 ,owner_id = wh_json:get_value(<<"Owner-ID">>, JObj)
                 ,faxbox_id = wh_json:get_value(<<"FaxBox-ID">>, JObj)
                 ,fax_option = wh_json:get_value(<<"Fax-T38-Option">>, JObj, 'false')
                 ,account_id = whapps_call:account_id(Call)
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
handle_cast('start_action', #state{call=_Call
                                  ,action='receive'
                                  ,owner_id=OwnerId
                                  ,faxbox_id=FaxBoxId
                                  }=State) ->
    lager:debug("receiving a fax for ~p/~p", [OwnerId,FaxBoxId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JObj}, State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    TransferRate = wh_json:get_integer_value(<<"Fax-Transfer-Rate">>, Data, 1),
    lager:debug("fax status - negociate result - ~s : ~p",[State#state.fax_id, TransferRate]),
    Status = list_to_binary(["Fax negotiated at ", wh_util:to_list(TransferRate)]),
    send_status(State, Status, Data),
    {'noreply', State#state{status=Status, page=1, fax_status=Data}};
handle_cast({'fax_status', <<"pageresult">>, JObj}, #state{page=Page, fax_id=JobId}=State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    TransferredPages = wh_json:get_integer_value(<<"Fax-Transferred-Pages">>, Data, 0),
    lager:debug("fax status - page result - ~s : ~p : ~p"
                ,[JobId, TransferredPages, wh_util:current_tstamp()]),
    Status = list_to_binary(["Received  Page ", wh_util:to_list(Page)]),
    send_status(State, Status, Data),
    {'noreply', State#state{page=TransferredPages, status=Status, fax_status=Data}};
handle_cast({'fax_status', <<"result">>, JObj}, State) ->
    end_receive_fax(JObj, State);
handle_cast({'fax_status', Event, _JObj}, State) ->
    lager:debug("fax status not handled - ~s",[Event]),
    {'noreply', State};
handle_cast({'exec_completed', <<"store_fax">>, Status, JObj}, State) ->
    check_retry_storage(Status, JObj, State);
handle_cast({'exec_completed', <<"receive_fax">>, Result, _JObj}, State) ->
    lager:debug("Fax Receive Result ~s",[Result]),
    {'noreply', State };
handle_cast({'exec_completed', App, Result, _JObj}, State) ->
    lager:debug("Fax exec not handled - ~s / ~s ",[App,Result]),
    {'noreply', State };
handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State };
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
handle_info({'DOWN', _Ref, 'process', Pid, 'normal'}, State) ->
    lager:debug("handler ~p down normally, request is done", [Pid]),
    {'stop', 'normal', State};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {'noreply', State}.

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
terminate(_Reason, #state{call=Call}) ->
    whapps_call_command:hangup(Call),
    lager:debug("fax request terminating: ~p", [_Reason]).

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
-spec get_action(wh_json:object()) -> 'receive' | 'transmit'.
get_action(JObj) ->
    case wh_json:get_value(<<"Action">>, JObj) of
        <<"transmit">> -> 'transmit';
        _ -> 'receive'
    end.

-spec start_receive_fax(state()) -> {'noreply', state()}.
start_receive_fax(#state{call=Call
                         ,fax_option=ReceiveFlag
                        }=State) ->
    whapps_call:put_callid(Call),
    Storage = get_fax_storage(Call),
    Props = [{<<"Fax-Doc-ID">>, Storage#fax_storage.id}
             ,{<<"Fax-Doc-DB">>, Storage#fax_storage.db}
            ],
    NewCall = whapps_call:kvs_store_proplist(Props, Call),
    NewState = maybe_update_fax_settings(State#state{storage=Storage
                                                     ,fax_id=Storage#fax_storage.id
                                                     ,call=NewCall
                                                    }),
    ResourceFlag = whapps_call:custom_channel_var(<<"Resource-Fax-Option">>, Call),
    LocalFile = get_fs_filename(NewState),
    send_status(NewState, list_to_binary(["New Fax from ", whapps_call:caller_id_number(Call)]), ?FAX_START, 'undefined'),
    whapps_call_command:answer(Call),
    whapps_call_command:receive_fax(ResourceFlag, ReceiveFlag, LocalFile, Call),
    {'noreply', NewState}.


-spec get_fax_storage(whapps_call:call()) -> fax_storage().
get_fax_storage(Call) ->
    AccountId = whapps_call:account_id(Call),
    {Year, Month, _} = erlang:date(),
    AccountMODb = kazoo_modb:get_modb(AccountId, Year, Month),
    FaxDb = wh_util:format_account_id(AccountMODb, 'encoded'),
    FaxId = <<(wh_util:to_binary(Year))/binary
              ,(wh_util:pad_month(Month))/binary
              ,"-"
              ,(wh_util:rand_hex_binary(16))/binary
            >>,
    FaxAttachmentId = wh_util:rand_hex_binary(16),
    #fax_storage{id=FaxId, db=FaxDb, attachment_id=FaxAttachmentId}.


-spec maybe_update_fax_settings(state()) -> state().
maybe_update_fax_settings(#state{call=Call
                                ,owner_id=OwnerId
                                ,faxbox_id='undefined'}=State) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
        {'ok', JObj} ->
            case wh_json:is_json_object(<<"fax_settings">>, JObj) of
                'false' -> maybe_update_fax_settings_from_account(State);
                'true' ->
                    FaxSettings = wh_json:get_value(<<"fax_settings">>, JObj),
                    update_fax_settings(Call, FaxSettings)
            end,
            case wh_json:is_true(<<"fax_to_email_enabled">>, JObj, 'false') of
                'true' ->
                    UserEmail = wh_json:get_value(<<"email">>, JObj),
                    Notify = wh_json:set_value([<<"email">>,<<"send_to">>], [UserEmail] , wh_json:new()),
                    State#state{fax_notify=Notify};
                'false' -> State
            end;
        {'error', _} ->
            maybe_update_fax_settings_from_account(State)
    end;
maybe_update_fax_settings(#state{call=Call
                                 ,faxbox_id=FaxBoxId
                                }=State) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, FaxBoxId) of
        {'ok', JObj} ->
            update_fax_settings(Call, JObj),
            Notify = get_faxbox_notify_list(JObj, AccountDb),
            State#state{fax_notify=Notify};
        {'error', _} -> maybe_update_fax_settings_from_account(State)
    end.

-spec get_faxbox_notify_list(wh_json:object(), ne_binary()) -> wh_json:object().
get_faxbox_notify_list(FaxBoxDoc, AccountDb) ->
    DefaultNotify = wh_json:get_value([<<"notifications">>,<<"inbound">>], FaxBoxDoc),
    case wh_json:get_value(<<"owner_id">>, FaxBoxDoc) of
        'undefined' -> DefaultNotify;
        OwnerId -> 
            case couch_mgr:open_cache_doc(AccountDb, OwnerId) of
                {'ok', UserDoc} -> 
                    List = wh_json:get_value([<<"email">>,<<"send_to">>], DefaultNotify, []),
                    maybe_add_owner_to_notify_list(List, wh_json:get_value(<<"email">>, UserDoc));
                _ -> 
                    DefaultNotify
            end
    end.

-spec maybe_add_owner_to_notify_list(list(), api_binary()) -> wh_json:object().
maybe_add_owner_to_notify_list(List, 'undefined') ->
    wh_json:set_value([<<"email">>, <<"send_to">>], List, wh_json:new());
maybe_add_owner_to_notify_list(List, OwnerEmail) ->
    NotifyList = fax_util:notify_email_list('undefined', OwnerEmail, List),
    wh_json:set_value([<<"email">>, <<"send_to">>], NotifyList, wh_json:new()).

-spec maybe_update_fax_settings_from_account(state()) -> any().
maybe_update_fax_settings_from_account(#state{call=Call}=State) ->
    AccountId = whapps_call:account_id(Call),
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_json_object(<<"fax_settings">>, JObj) of
                'true' ->
                    FaxSettings = wh_json:get_value(<<"fax_settings">>, JObj),
                    update_fax_settings(Call, FaxSettings);
                'false' ->
                    lager:debug("no settings for local fax"),
                    update_fax_settings(Call, wh_json:new())
            end;
        {'error', _} ->
            lager:debug("no settings for local fax - missing account"),
            update_fax_settings(Call, wh_json:new())

    end,
    State.

-spec update_fax_settings(whapps_call:call(), wh_json:object()) -> any().
update_fax_settings(Call, JObj) ->
    ChannelVars = build_fax_settings(Call, JObj),
    whapps_call_command:set(wh_json:from_list(ChannelVars), 'undefined', Call).

-spec build_fax_settings(whapps_call:call(), wh_json:object()) -> wh_proplist().
build_fax_settings(Call, JObj) ->
    props:filter_undefined(
      [case wh_json:is_true(<<"override_fax_identity">>, JObj, 'true') of
           'false' ->
               {<<"Fax-Identity-Number">>, whapps_call:to_user(Call)};
           'true' ->
               {<<"Fax-Identity-Number">>, wh_util:to_binary(
                  wh_json:get_first_defined([<<"fax_identity">>,<<"caller_id">>], JObj
                                           ,whapps_call:to_user(Call)))}
       end,
       case wh_json:is_true(<<"override_callee_number">>, JObj, 'false') of
           'false' ->
              {<<"Callee-ID-Number">>, whapps_call:to_user(Call)};
           'true' ->
              {<<"Callee-ID-Number">>, wh_util:to_binary(
                  wh_json:get_first_defined([<<"caller_id">>,<<"fax_identity">>], JObj
                                           ,whapps_call:to_user(Call)))}
       end
       ,{<<"Fax-Identity-Name">>, wh_json:get_value(<<"fax_header">>, JObj)}
       ,{<<"Fax-Timezone">>, wh_json:get_value(<<"fax_timezone">>, JObj)}
       ,{<<"Callee-ID-Name">>, wh_util:to_binary(
           wh_json:get_first_defined([<<"caller_name">>,<<"name">>], JObj))}
       ,{<<"Fax-Doc-ID">>, whapps_call:kvs_fetch(<<"Fax-Doc-ID">>, Call) }
       ,{<<"Fax-Doc-DB">>, whapps_call:kvs_fetch(<<"Fax-Doc-DB">>, Call) }
      ]).


-spec end_receive_fax(wh_json:object(), state()) -> state().
end_receive_fax(JObj, #state{call=Call}=State) ->
    whapps_call_command:hangup(Call),
    case wh_json:is_true([<<"Application-Data">>,<<"Fax-Success">>], JObj, 'false') of
        'false' -> notify_failure(JObj, State),
                   {'stop', 'normal', State};
        'true' -> maybe_store_fax(JObj, State)
    end.

-spec maybe_store_fax(wh_json:object(), state() ) -> state().
maybe_store_fax(JObj, State) ->
    case store_fax(JObj, #state{storage=#fax_storage{id=FaxId}}=State) of
        {'ok', FaxId} ->
            lager:debug("fax stored successfully into ~s", [FaxId]),
            {'noreply', store_attachment(State#state{fax_result=JObj})};
        {'error', Error} ->
            lager:debug("store fax other resp: ~p", [Error]),
            notify_failure(JObj, Error, State),
            {'stop', 'normal', State}
    end.


-spec store_fax(wh_json:object(), state() ) ->
          {'ok', ne_binary()} | {'error', any()}.
store_fax(JObj, #state{storage=#fax_storage{id=FaxDocId
                                             ,attachment_id=_AttachmentId
                                            }
                      }=State) ->
    case create_fax_doc(JObj, State) of
        {'ok', Doc} ->
            FaxDocId = wh_json:get_value(<<"_id">>, Doc),
            {'ok', FaxDocId};
        Error -> Error
    end.

-spec get_fs_filename(state()) -> ne_binary().
get_fs_filename(#state{storage=#fax_storage{attachment_id=AttachmentId}}) ->
    LocalPath = whapps_config:get_binary(?CONFIG_CAT, <<"fax_file_path">>, <<"/tmp/">>),
    Ext = whapps_config:get_binary(?CONFIG_CAT, <<"default_fax_extension">>, <<".tiff">>),
    <<LocalPath/binary, AttachmentId/binary, Ext/binary>>.

-spec store_attachment(state()) -> 'ok'.
store_attachment(#state{call=Call
                        ,fax_store_count=Count
                       }=State) ->
    FaxUrl = attachment_url(State),
    FaxFile = get_fs_filename(State),
    lager:debug("storing fax ~s to ~s", [FaxFile, FaxUrl]),
    whapps_call_command:store_fax(FaxUrl, FaxFile, Call),
    State#state{fax_store_count=Count+1}.

-spec check_retry_storage(ne_binary(), wh_json:object(), state()) ->
          {'noreply', state()} | {'stop', 'normal', state()}.
check_retry_storage(<<"success">>, JObj, #state{fax_result=FaxResultObj} =State) ->
    case check_for_upload(State) of
        'ok' -> notify_success(FaxResultObj, State),
                {'stop', 'normal', State};
        'error' -> maybe_retry_storage(<<"storage success but no attachment">>, JObj, State)
    end;
check_retry_storage(Error, JObj, #state{fax_result=FaxResultObj} = State) ->
    case check_for_upload(State) of
        'ok' ->
            lager:debug("got error ~s from store_fax but check for upload succeeded",[Error]),
            notify_success(FaxResultObj, State),
            {'stop', 'normal', State};
        'error' -> maybe_retry_storage(Error, JObj, State)
    end.

-spec maybe_retry_storage(binary(), wh_json:object(), state()) ->
          {'noreply', state()} | {'stop', 'normal', state()}.
maybe_retry_storage(Error, JObj, #state{fax_store_count=Count}=State) ->
    lager:debug("fax store error ~s - ~p",[Error, JObj]),
    case Count < whapps_config:get_integer(?CONFIG_CAT, <<"max_storage_retry">>, 5) of
        'true' -> {'noreply', store_attachment(State)};
        'false' -> notify_failure(JObj, Error, State),
                   {'stop', 'normal', State}
    end.

-spec check_for_upload(state()) -> 'ok' | 'error'.
check_for_upload(#state{call=_Call
                        ,storage=#fax_storage{id=FaxDocId
                                              ,db=FaxDb}
                       }=State) ->
    case couch_mgr:open_doc(FaxDb, FaxDocId) of
        {'ok', FaxDoc} ->
            check_upload_for_attachment(FaxDoc, State);
        {'error', Error} ->
            lager:debug("error reading document ~s/~s when looking for valid attachment : ~p"
                       ,[FaxDb, FaxDocId, Error]),
            'error'
    end.


-spec check_upload_for_attachment(wh_json:object(), state()) -> 'ok' | 'error'.
check_upload_for_attachment(FaxDoc, State) ->
    case wh_json:get_keys(<<"_attachments">>, FaxDoc) of
        [] -> 'error';
        [AttachmentName] ->
            check_attachment_for_data(FaxDoc, AttachmentName, State)
    end.

-spec check_attachment_for_data(wh_json:object(), state(), ne_binary()) -> 'ok' | 'error'.
check_attachment_for_data(FaxDoc, AttachmentName, _State) ->
    case wh_json:get_value([<<"_attachments">>, AttachmentName, <<"length">>], FaxDoc) of
        0 -> 'error';
        _Len -> 'ok'
    end.


-spec create_fax_doc(wh_json:object(), state()) ->
          {'ok', wh_json:object()} | {'error', any()}.
create_fax_doc(JObj, #state{owner_id = OwnerId
                           ,faxbox_id = FaxBoxId
                           ,fax_notify = Notify
                           ,call=Call
                           ,storage=#fax_storage{id=FaxDocId
                                                 ,db=FaxDb}
                           }) ->
    {{Y,M,D}, {H,I,S}} = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),
    Name = list_to_binary(["fax message received at "
                           ,wh_util:to_binary(Y), "-", wh_util:to_binary(M), "-", wh_util:to_binary(D)
                           ," " , wh_util:to_binary(H), ":", wh_util:to_binary(I), ":", wh_util:to_binary(S)
                           ," UTC"
                          ]),
    <<Year:4/binary, Month:2/binary, "-", _/binary>> = FaxDocId,
    CdrId = <<(wh_util:to_binary(Year))/binary
              ,(wh_util:pad_month(Month))/binary
              ,"-"
              ,(whapps_call:call_id(Call))/binary
            >>,

    Props = props:filter_undefined(
             [{<<"name">>, Name}
             ,{<<"to_number">>, whapps_call:request_user(Call)}
             ,{<<"from_number">>, whapps_call:from_user(Call)}
             ,{<<"description">>, <<"fax document received">>}
             ,{<<"source_type">>, <<"incoming_fax">>}
             ,{<<"timestamp">>, wh_json:get_value(<<"Timestamp">>, JObj)}
             ,{<<"owner_id">>, OwnerId}
             ,{<<"faxbox_id">>, FaxBoxId}
             ,{<<"media_type">>, <<"tiff">>}
             ,{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"cdr_doc_id">>, CdrId}
             ,{<<"_id">>, FaxDocId}
             ,{<<"rx_result">>,
               wh_json:from_list(
                 fax_util:fax_properties(
                   wh_json:get_value(<<"Application-Data">>, JObj)))}
             ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
             ,{<<"notifications">>, Notify}
            ]),

    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props)
                                       ,FaxDb
                                       ,[{'type', <<"fax">>}]
                                      ),
    kazoo_modb:save_doc(whapps_call:account_id(Call), Doc).

-spec attachment_url(state() ) -> ne_binary().
attachment_url(#state{storage=#fax_storage{id=FaxDocId
                                           ,attachment_id=AttachmentId
                                           ,db=AccountDb
                                          }
                     }) ->
    _ = case couch_mgr:open_doc(AccountDb, FaxDocId) of
            {'ok', JObj} ->
                case wh_json:get_keys(<<"_attachments">>, JObj) of
                    [] -> 'ok';
                    Existing -> [couch_mgr:delete_attachment(AccountDb, FaxDocId, Attach) || Attach <- Existing]
                end;
            {'error', _} -> 'ok'
        end,
    Rev = case couch_mgr:lookup_doc_rev(AccountDb, FaxDocId) of
              {'ok', R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    list_to_binary([wh_couch_connections:get_url(), AccountDb, "/", FaxDocId, "/", AttachmentId, ".tiff", Rev]).

-spec fax_fields(wh_json:object()) -> wh_proplist().
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- wh_json:to_proplist(JObj)].

-spec notify_fields(whapps_call:call(), wh_json:object()) -> wh_proplist().
notify_fields(Call, JObj) ->
    props:filter_empty(
      [{<<"From-User">>, whapps_call:from_user(Call)}
      ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
      ,{<<"To-User">>, whapps_call:to_user(Call)}
      ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
      ,{<<"Account-ID">>, whapps_call:account_id(Call)}
      ,{<<"Fax-Info">>,
        wh_json:from_list(
          fax_fields(
            wh_json:get_value(<<"Application-Data">>, JObj)))}
      ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
      ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
      ,{<<"Callee-ID-Number">>, whapps_call:callee_id_number(Call)}
      ,{<<"Callee-ID-Name">>, whapps_call:callee_id_name(Call)}
      ,{<<"Call-ID">>, whapps_call:call_id(Call)}
      ,{<<"Fax-Timestamp">>, wh_util:current_tstamp()}
      | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).


-spec notify_failure(wh_json:object(), state()) -> any().
notify_failure(JObj, State) ->
    Reason = wh_json:get_value([<<"Application-Data">>,<<"Fax-Result">>], JObj),
    notify_failure(JObj, Reason, State).
-spec notify_failure(wh_json:object(), api_binary(), state()) -> any().
notify_failure(JObj, 'undefined', State) ->
    notify_failure(JObj, <<"unknown error">>, State);    
notify_failure(JObj, Reason, #state{call=Call
                                   ,owner_id=OwnerId
                                   ,faxbox_id=FaxBoxId
                                   ,fax_notify=Notify
                                   ,storage=#fax_storage{id=FaxId}
                                   }=State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    Status = list_to_binary(["Error receiving fax : ", Reason]),
    send_error_status(State, Status, Data),
    Message = props:filter_undefined(
                 notify_fields(Call, JObj) ++
                     [{<<"Fax-ID">>, FaxId}
                     ,{<<"Fax-Error">>, Reason}
                     ,{<<"Owner-ID">>, OwnerId}
                     ,{<<"FaxBox-ID">>, FaxBoxId}
                     ,{<<"Fax-Notifications">>,  Notify}
                     ]),
    wapi_notifications:publish_fax_inbound_error(Message).

-spec notify_success(wh_json:object(), state()) -> any().
notify_success(JObj, #state{call=Call
                           ,owner_id=OwnerId
                           ,faxbox_id=FaxBoxId
                           ,fax_notify=Notify
                           ,storage=#fax_storage{id=FaxId}
                           }=State) ->
    Data = wh_json:get_value(<<"Application-Data">>, JObj, wh_json:new()),
    Status = <<"Fax Successfuly received">>,
    send_status(State, Status, ?FAX_END, Data),
    
    Message = props:filter_undefined(
                notify_fields(Call, JObj) ++
                    [{<<"Fax-ID">>, FaxId}
                    ,{<<"Owner-ID">>, OwnerId}
                    ,{<<"FaxBox-ID">>, FaxBoxId}
                    ,{<<"Fax-Notifications">>, Notify}
                    ]),
    wapi_notifications:publish_fax_inbound(Message).

-spec send_error_status(state(), ne_binary(), api_object()) -> any().
send_error_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_ERROR, FaxInfo).

-spec send_status(state(), ne_binary(), api_object()) -> any().
send_status(State, Status, FaxInfo) ->
    send_status(State, Status, ?FAX_RECEIVE, FaxInfo).

-spec send_status(state(), ne_binary(), ne_binary(), api_object()) -> any().
send_status(State, Status, FaxState, FaxInfo) ->
    #state{call=Call, account_id=AccountId, page=Page, fax_id=JobId, faxbox_id=FaxboxId} = State,
    Payload = props:filter_undefined(
                [{<<"Job-ID">>, JobId}
                 ,{<<"FaxBox-ID">>, FaxboxId}
                 ,{<<"Account-ID">>, AccountId}
                 ,{<<"Status">>, Status}
                 ,{<<"Fax-State">>, FaxState}
                 ,{<<"Fax-Info">>, FaxInfo}
                 ,{<<"Direction">>, ?FAX_INCOMING}
                 ,{<<"Page">>, Page}
                 ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
                 ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
                 ,{<<"Callee-ID-Number">>, whapps_call:callee_id_number(Call)}
                 ,{<<"Callee-ID-Name">>, whapps_call:callee_id_name(Call)}
                 | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    wapi_fax:publish_status(Payload).
