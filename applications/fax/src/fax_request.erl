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
         ,fax_id :: api_binary()
         ,fax_result :: api_object()
         ,fax_notify :: api_binaries()
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
handle_cast('start_action', #state{call=Call
                                   ,action='receive'
                                   ,owner_id=OwnerId
                                  }=State) ->
    lager:debug("receiving a fax for ~p", [OwnerId]),
    {'noreply', State};
handle_cast({'fax_status', <<"negociateresult">>, JObj}, State) ->
    TransferRate = wh_json:get_integer_value([<<"Application-Data">>,<<"Fax-Transfer-Rate">>], JObj, 1),
    lager:debug("inbound fax status - negociate result - ~p",[TransferRate]),
    %% TODO update stats/websockets/job
    %%      maybe setup timer to cancel job bassed on transmission rate     
    {'noreply', State};
handle_cast({'fax_status', <<"pageresult">>, JObj}, State) ->
    TransferredPages = wh_json:get_value([<<"Application-Data">>, <<"Fax-Transferred-Pages">>], JObj),
    lager:debug("inbound fax status - page result - ~p : ~p"
                ,[TransferredPages, wh_util:current_tstamp()]),
    %% TODO update stats/websockets/job 
    {'noreply', State};
handle_cast({'fax_status', <<"result">>, JObj}, State) ->
    %% TODO update stats/websockets/job 
    end_receive_fax(JObj, State);
%    {'stop', 'normal', State};
handle_cast({'fax_status', Event, JObj}, State) ->
    lager:debug("fax status not handled - ~s",[Event]),
    %% TODO update stats/websockets/job 
    {'noreply', State};
handle_cast({'exec_completed', <<"store_fax">>, <<"success">>, JObj}, State) ->
    check_for_upload(JObj, State),
    {'stop', 'normal', State};
handle_cast({'exec_completed', <<"store_fax">>, Error, JObj}, State) ->
    lager:debug("fax store error ~s - ~p",[Error, JObj]),
    check_for_upload(JObj, State),
    {'stop', 'normal', State};
handle_cast({'exec_completed', <<"receive_fax">>, Result, _JObj}, State) ->
    lager:debug("Fax Receive Result ~s",[Result]),
    {'noreply', State };
handle_cast({'exec_completed', App, Result, JObj}, State) ->
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
handle_info({'DOWN', Ref, 'process', Pid, 'normal'}, State) ->
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
start_receive_fax(#state{call=Call}=State) ->
    whapps_call:put_callid(Call),
    NewState = maybe_update_fax_settings(State),    
    ResourceFlag = whapps_call:custom_channel_var(<<"Resource-Fax-Option">>, Call),
    CallflowJObj = whapps_call:kvs_fetch('cf_flow', Call),
    ReceiveFlag = wh_json:get_value([<<"data">>, <<"media">>, <<"fax_option">>], CallflowJObj),    
    whapps_call_command:answer(Call),
    whapps_call_command:receive_fax(ResourceFlag, ReceiveFlag, Call),
    {'noreply', NewState}.


-spec maybe_update_fax_settings(state()) -> state().
maybe_update_fax_settings(#state{call=Call,owner_id=OwnerId}=State) ->
    case couch_mgr:open_doc(?WH_FAXES, OwnerId) of
        {'ok', JObj} ->
            update_fax_settings(Call, JObj),
            Notify = wh_json:get_value(<<"email_to">>, JObj),
            State#state{fax_notify=Notify};
        {'error', _} -> maybe_update_fax_settings_from_user(State)
    end.
    
-spec maybe_update_fax_settings_from_user(state()) -> any().
maybe_update_fax_settings_from_user(#state{call=Call,owner_id=OwnerId}=State) ->
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
                    State#state{fax_notify=[UserEmail]};
                'false' -> State
            end;
        {'error', _} ->
            maybe_update_fax_settings_from_account(State),
            State
    end.

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
                    lager:debug("no settings for local fax")
            end;
        {'error', _} ->
            lager:debug("no settings for local fax - missing account")
    end.

-spec update_fax_settings(whapps_call:call(), wh_json:object()) -> any().
update_fax_settings(Call, JObj) ->
    ChannelVars = build_fax_settings(Call, JObj),
    lager:debug("CHANNEL VARS ~p",[ChannelVars]),
    whapps_call_command:set(wh_json:from_list(ChannelVars), 'undefined', Call).

-spec build_fax_settings(whapps_call:call(), wh_json:object()) -> wh_proplist().
build_fax_settings(Call, JObj) ->
    props:filter_undefined(
      [case wh_json:is_true(<<"override_fax_identity">>, JObj, 'false') of
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
      ]).

    
-spec end_receive_fax(wh_json:object(), state()) -> state().
end_receive_fax(JObj, #state{call=Call, owner_id=OwnerId}=State) ->
    whapps_call_command:hangup(Call),
    case wh_json:is_true([<<"Application-Data">>,<<"Fax-Success">>], JObj, 'false') of
        'false' -> notify_failure(JObj, State),
                   {'stop', 'normal', State};
        'true' -> maybe_store_fax(JObj, State)
    end.

-spec maybe_store_fax(wh_json:object(), state() ) -> state().
maybe_store_fax(JObj, #state{call=Call, owner_id=OwnerId, fax_notify=Notify}=State) ->
    case store_fax(Call, OwnerId, JObj, Notify) of
        {'ok', FaxId} ->
            lager:debug("fax stored successfully into ~s", [FaxId]),
            {'noreply', State#state{fax_id=FaxId, fax_result=JObj} };
        {'error', Error} ->
            lager:debug("store fax other resp: ~p", [Error]),
            notify_failure(JObj, Error, State),
            {'stop', 'normal', State}
    end.


-spec store_fax(whapps_call:call(), ne_binary(), wh_json:object(), api_binaries()) -> 
          {'ok', ne_binary()} | {'error', any()}.
store_fax(Call, OwnerId, JObj, Notify) ->
    case create_fax_doc(Call, OwnerId, JObj, Notify) of
        {'ok', Doc} ->
            FaxDocId = wh_json:get_value(<<"_id">>, Doc),
            FaxFile = tmp_file(),
            FaxUrl = attachment_url(Call, FaxFile, FaxDocId),
            lager:debug("storing fax ~s to ~s", [FaxFile, FaxUrl]),
            whapps_call_command:store_fax(FaxUrl, Call),
            {'ok', FaxDocId};
        Error -> Error
    end.

-spec check_for_upload(wh_json:object(), state()) -> 'ok'.
check_for_upload(JObj, #state{call=Call, fax_id=FaxDocId}=State) ->
    case couch_mgr:open_doc(whapps_call:account_db(Call), FaxDocId) of
        {'ok', FaxDoc} ->
            check_upload_for_attachment(JObj, FaxDoc, State);
        {'error', Error} ->
            notify_failure(JObj, Error, State)
    end,
    'ok'.


-spec check_upload_for_attachment(wh_json:object(), wh_json:object(), state()) -> 'ok'.
check_upload_for_attachment(JObj, FaxDoc, State) ->
    case wh_json:get_keys(<<"_attachments">>, FaxDoc) of
        [] ->
            notify_failure(JObj, <<"no attachment uploaded">>, State );
        [AttachmentName] ->
            check_attachment_for_data(JObj, FaxDoc, AttachmentName, State)
    end,
    'ok'.

-spec check_attachment_for_data(wh_json:object(), wh_json:object(), state(), ne_binary()) -> 'ok'.
check_attachment_for_data(_JObj, FaxDoc, AttachmentName, #state{fax_result=JObj}=State) ->
    case wh_json:get_value([<<"_attachments">>, AttachmentName, <<"length">>], FaxDoc) of
        0 ->
            notify_failure(JObj, <<"no data available in attachment ", AttachmentName/binary>>, State );
        _Len ->
            notify_success(JObj, State)
    end,
    'ok'.


-spec create_fax_doc(whapps_call:call(), ne_binary(), wh_json:object(), api_binaries()) -> 
          {'ok', wh_json:object()} | {'error', any()}.
create_fax_doc(Call, OwnerId, JObj, Notify) ->
    AccountDb = whapps_call:account_db(Call),

    {{Y,M,D}, {H,I,S}} = calendar:gregorian_seconds_to_datetime(wh_util:current_tstamp()),

    Name = list_to_binary(["fax message received at "
                           ,wh_util:to_binary(Y), "-", wh_util:to_binary(M), "-", wh_util:to_binary(D)
                           ," " , wh_util:to_binary(H), ":", wh_util:to_binary(I), ":", wh_util:to_binary(S)
                           ," UTC"
                          ]),

    Props = [{<<"name">>, Name}
             ,{<<"to_number">>, whapps_call:request_user(Call)}
             ,{<<"from_number">>, whapps_call:from_user(Call)}
             ,{<<"description">>, <<"fax document received">>}
             ,{<<"source_type">>, <<"incoming_fax">>}
             ,{<<"timestamp">>, wh_json:get_value(<<"Timestamp">>, JObj)}
             ,{<<"owner_id">>, OwnerId}
             ,{<<"media_type">>, <<"tiff">>}
             ,{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"rx_results">>, 
               wh_json:from_list(
                 fax_util:fax_properties(
                   wh_json:get_value(<<"Application-Data">>, JObj)))}
             ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
            ] ++ get_notifications(Notify),

    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props)
                                       ,AccountDb
                                       ,[{'type', <<"private_media">>}]
                                      ),

    couch_mgr:save_doc(AccountDb, Doc).

-spec get_notifications(api_binaries()) -> wh_proplist().
get_notifications('undefined') ->
    [];
get_notifications(Notify) ->
    [{<<"notifications">>,
      wh_json:from_list([{<<"email">>,
         wh_json:from_list([{<<"send_to">>, Notify}])}])}]. 

-spec attachment_url(whapps_call:call(), ne_binary(), ne_binary()) -> ne_binary().
attachment_url(Call, File, FaxDocId) ->
    AccountDb = whapps_call:account_db(Call),
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
    list_to_binary([wh_couch_connections:get_url(), AccountDb, "/", FaxDocId, "/", File, Rev]).

-spec tmp_file() -> ne_binary().
tmp_file() ->
     <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".tiff">>.

-spec fax_fields(wh_json:object()) -> wh_proplist().
fax_fields(JObj) ->
    [{K,V} || {<<"Fax-", _/binary>> = K, V} <- wh_json:to_proplist(JObj)].

-spec notify_fields(whapps_call:call(), ne_binary(), wh_json:object()) -> wh_proplist().
notify_fields(Call, OwnerId, JObj) ->
    props:filter_empty(      
      [{<<"From-User">>, whapps_call:from_user(Call)}
      ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
      ,{<<"To-User">>, whapps_call:to_user(Call)}
      ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
      ,{<<"Account-ID">>, whapps_call:account_id(Call)}
      ,{<<"Owner-ID">>, OwnerId}
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
notify_failure(JObj, Reason, #state{call=Call, owner_id=OwnerId, fax_id=FaxId, fax_notify=Notify}=State) ->
    Message = props:filter_undefined(
                 notify_fields(Call, OwnerId, JObj) ++ 
                     [{<<"Fax-ID">>, FaxId}
                     ,{<<"Fax-Error">>, Reason}
                     ,{<<"Fax-Notifications">>, 
                       wh_json:from_list([{<<"email">>,
                            wh_json:from_list([{<<"send_to">>, Notify}])}])}                                     
                     ]),
    wapi_notifications:publish_fax_inbound_error(Message).
    
-spec notify_success(wh_json:object(), state()) -> any().
notify_success(JObj, #state{call=Call, owner_id=OwnerId, fax_id=FaxId, fax_notify=Notify}=State) ->
    Message = props:filter_undefined(
                notify_fields(Call, OwnerId, JObj) ++ 
                    [{<<"Fax-ID">>, FaxId}
                    ,{<<"Fax-Notifications">>, 
                       wh_json:from_list([{<<"email">>,
                           wh_json:from_list([{<<"send_to">>, Notify}])}])}                                     
                    ]),
    wapi_notifications:publish_fax_inbound(Message).
                     
