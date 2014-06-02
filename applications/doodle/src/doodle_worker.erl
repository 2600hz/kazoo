%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(doodle_worker).

-behaviour(gen_listener).

-export([start_link/1]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("doodle.hrl").
-include_lib("whistle_number_manager/include/wh_number_manager.hrl").



-record(state, {queue_name :: api_binary()
                ,pool :: api_pid()
                ,job_id :: api_binary()
                ,job :: api_object()
               }).
-type state() :: #state{}.


-define(BINDINGS, [{'self', []}]).
-define(RESPONDERS, []).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-define(LIST_BY_NUMBER, <<"callflow/listing_by_number">>).

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
start_link(_) ->
    gen_listener:start_link(?MODULE, [{'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).


%%%===================================================================
%%% gen_server callbacks
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
init([]) ->
    {'ok', #state{}}.

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
handle_cast({_, Pid, _}, #state{queue_name='undefined'}=State) when is_pid(Pid) ->
    lager:debug("worker received request with unknown queue name, rejecting", []),
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', State};
handle_cast({_, Pid, _}, #state{job_id=JobId}=State) when is_binary(JobId), is_pid(Pid) ->
    lager:debug("worker received request while still processing a job, rejecting", []),
    gen_server:cast(Pid, {'job_complete', self()}),
    {'noreply', State};
handle_cast({'attempt_transmission', Pid, Job}, #state{queue_name=Q}=State) ->
    JobId = wh_json:get_value(<<"id">>, Job),
    put('callid', JobId),
    case attempt_to_acquire_job(JobId) of
        {'ok', JObj} ->
            lager:debug("acquired job id ~s", [JobId]),
            gen_server:cast(self(), {'execute_job'}),
            {'noreply', State#state{job_id=JobId, pool=Pid, job=JObj}};            
        {'error', _Reason} ->
            lager:debug("failed to acquire job ~s: ~p", [JobId, _Reason]),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;
handle_cast({'execute_job'}, #state{job_id=JobId, queue_name=Q, job=JObj, pool=Pid}=State) ->
    try execute_job(JObj) of
        {'ok', Result} ->
            release_successful_job(Result),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)};
        {'pending', Result} ->
            release_pending_job(Result),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)};
        {'failed', Error, Result} ->
            release_failed_job(Error, Result),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)};
        {'failure', Error, Result} ->
            release_failed_job(Error, Result),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    catch
        Error:_R ->
            lager:debug("~s while processing job ~s: ~p", [Error, JobId, _R]),
            release_failed_job(Error, JObj),
            gen_server:cast(Pid, {'job_complete', self()}),
            {'noreply', reset(State)}
    end;

handle_cast({'gen_listener', {'created_queue', QueueName}}, State) ->
    lager:debug("worker discovered queue name ~s", [QueueName]),
    {'noreply', State#state{queue_name=QueueName}};
handle_cast({'gen_listener',{'is_consuming',_IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('job_timeout', #state{job=JObj}=State) ->
    release_failed_job(<<"timeout">>, JObj),
    {'noreply', reset(State)};
handle_info(_Info, State) ->
    lager:debug("doodle worker unhandled message: ~p", [_Info]),
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
-spec attempt_to_acquire_job(ne_binary()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', term()}.
attempt_to_acquire_job(Id) ->
    case couch_mgr:open_doc(?DOODLE_DB, Id) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"pending">> ->
                    couch_mgr:save_doc(?DOODLE_DB, wh_json:set_values([{<<"pvt_job_status">>, <<"processing">>}
                                                                      ,{<<"pvt_job_node">>, wh_util:to_binary(node())}
                                                                      ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                                                                     ],JObj));
                _Else ->
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {'error', 'job_not_available'}
            end
    end.

-spec bump_modified(ne_binary()) -> {'ok', wh_json:object()} | {'error', _}.
bump_modified(JobId) ->
    case couch_mgr:open_doc(?DOODLE_DB, JobId) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            case wh_json:get_value(<<"pvt_job_status">>, JObj) of
                <<"processing">> ->
                    lager:debug("bumped modified time for sms job ~s", [JobId]),
                    couch_mgr:save_doc(?DOODLE_DB, wh_json:set_value(<<"pvt_modified">>, wh_util:current_tstamp(), JObj));
                _Else ->
                    lager:debug("job not in an available status: ~s", [_Else]),
                    {'error', 'invalid_job_status'}
            end
    end.

release_failed_job(_Error, JObj) ->
    Result = [{<<"success">>, 'false'}
              ,{<<"error">>, _Error}
              ,{<<"time_elapsed">>, elapsed_time(JObj)}
             ],
    release_job(Result, JObj).

-spec release_pending_job(wh_json:object()) -> 'ok'.
release_pending_job(JObj) ->
    Result = props:filter_undefined([
               {<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"pending">>, <<"true">>}
             ]),
    release_job(Result, JObj).

-spec release_successful_job(wh_json:object()) -> 'ok'.
release_successful_job(JObj) ->
    Result = props:filter_undefined([
               {<<"time_elapsed">>, elapsed_time(JObj)}
              ,{<<"success">>, <<"true">>}
             ]),
    release_job(Result, JObj).



-spec release_job(wh_proplist(), wh_json:object()) -> 'ok' | 'failure'.
release_job(Result, JObj) ->
    Success = props:is_true(<<"success">>, Result, 'false'),
    Pending = props:is_true(<<"success">>, Result, 'false'),
    Updaters = [fun(J) ->
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         Retries = wh_json:get_integer_value(<<"retries">>, J, 1),
                         case Retries - Attempts >= 1 of
                             _ when Success ->
                                 lager:debug("releasing job with status: completed"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"completed">>, J);
                             _ when Pending ->
                                 lager:debug("releasing job with status: pending"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"pending">>, J);
                             'true' ->
                                 lager:debug("releasing job with status: pending"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"pending">>, J);
                             'false' ->
                                 lager:debug("releasing job with status: failed"),
                                 wh_json:set_value(<<"pvt_job_status">>, <<"failed">>, J)
                         end
                 end
                ,fun(J) ->
                         Attempts = wh_json:get_integer_value(<<"attempts">>, J, 0),
                         wh_json:set_value(<<"attempts">>, Attempts + 1, J)
                 end
               ],
    Update = lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters),
    couch_mgr:ensure_saved(?DOODLE_DB, Update),
    case Success of 'true' -> 'ok'; 'false' -> 'failure' end.







-spec elapsed_time(wh_json:object()) -> non_neg_integer().
elapsed_time(JObj) ->
    wh_util:current_tstamp() - wh_json:get_integer_value(<<"pvt_created">>, JObj, wh_util:current_tstamp()).

-spec execute_job(wh_json:object()) -> 'ok' | 'failure'.
execute_job(JObj) ->
    JobId = wh_json:get_value(<<"_id">>, JObj),
    Status = wh_json:get_value(<<"status">>, JObj),    
    execute_status(Status, JobId, JObj).

execute_status(<<"queued">>, JobId, JObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    MT = wh_json:get_value(<<"device">>, JObj),
    OwnerId = wh_json:get_value(<<"owner_id">>, JObj),
    To = wh_json:get_value(<<"to">>, JObj),
    From = wh_json:get_value(<<"from">>, JObj),
    Request = wh_json:get_value(<<"request">>, JObj),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(To, <<"@">>),
    IsReconciable = wnm_util:is_reconcilable(ToUser),
    
    % i know this shouldn't be here
    {'ok', Endpoint } = cf_endpoint:get(MT, AccountDb),
    CIDInternal = wh_json:get_value([<<"caller_id">>,<<"internal">>,<<"number">>], Endpoint, FromUser), 
    CIDExternal = wh_json:get_value([<<"caller_id">>,<<"external">>,<<"number">>], Endpoint, FromUser), 

    FlowId = wh_json:get_value(<<"flow_id">>, JObj),
    Flow   = wh_json:get_value(<<"flow">>, JObj),
    
    case wh_json:is_true(<<"is_offnet">>, JObj) of
        'false' ->
            lager:info("trying to delivery sms into originating account"),
            deliver_in_callflow(AccountId, ToUser, Flow, JobId, JObj, CIDInternal);
        'true' ->
            lager:info("no callflow"),
            deliver_offnet(wnm_util:to_e164(ToUser), JobId, JObj, CIDExternal)
    end;
execute_status(<<"ready">>, JobId, JObj) ->
    lager:info("execute ready doc ~p",[JObj]),
    URIS = wh_json:get_value(<<"target">>, JObj, []),
    Type = wh_json:get_value(<<"target_type">>, JObj, <<"on-net">>),
    deliver(Type, URIS, JobId, JObj); 
execute_status(<<"test">>, JobId, JObj) ->
    Module = wh_json:get_value(<<"module">>, JObj),
    exec_mod(Module, JObj).

deliver(<<"off-net">>, URIS, JobId, JObj) ->
    C = whapps_config:get_binary(?CONFIG_CAT, <<"offnet_contact">>, <<"<sip:someapp@127.0.0.1:5060>">>),    
    deliver_sms([parse_contact(C)], JobId, JObj);

deliver(<<"on-net">>, URIS, JobId, JObj) ->
    case lookup_contacts(URIS) of
        [] -> {'pending', JObj};
        [Contact] -> deliver_sms([Contact], JobId, JObj);
        [Contact|_Others] = Contacts -> deliver_sms(Contacts, JobId, JObj);
        _ -> {'pending', JObj}
    end.
    

lookup_contacts([]) -> [];
lookup_contacts([URI | URIS]) ->
  Contact = lookup_contact(URI),
  Contacts = lookup_contacts(URIS),
  Contact ++ Contacts.

lookup_contact(URI) ->
   [User, Realm] = binary:split(URI, <<"@">>),
    Props = [{<<"Username">>, User}
            ,{<<"Realm">>, Realm}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],  
    case whapps_util:amqp_pool_collect(Props
                                       ,fun wapi_registration:publish_query_req/1
                                       ,{'ecallmgr', 'true'}
                                      )
    of
        {'error', _E} ->
            lager:debug("error getting reg: ~p", [_E]),
            [];
        {_, JObjs} ->
            lager:info("Contacts JObjs ~p",[JObjs]),
            Contacts = [ parse_contact(Contact) || Contact <- extract_contacts(JObjs)],
            lager:info("Contact JObjs ~p",[Contacts]),
            Contacts
    end.   
    
parse_contact(C) ->
    [A] = nksip_parse:uris(C),
    A.

-spec extract_contacts(wh_json:objects()) -> ne_binaries().
extract_contacts(JObjs) ->
    sets:to_list(extract_contacts(JObjs, sets:new())).

-spec extract_contacts(wh_json:objects(), set()) -> set().
extract_contacts([], Set) -> Set;
extract_contacts([JObj|JObjs], Set) ->
    Fields = wh_json:get_value(<<"Fields">>, JObj, []),
    S = lists:foldl(fun(J, S) ->
                            case wh_json:get_ne_value(<<"Original-Contact">>, J) of
                                'undefined' -> S;
                                AuthId -> sets:add_element(AuthId, S)
                            end
                    end, Set, Fields),
    extract_contacts(JObjs, S).


deliver_sms(Contacts, JobId, JObj) ->
    lager:info("deliver contacts ~p , ~p",[Contacts, JObj]),
    Props = [
             {<<"Message-ID">>, <<"777777123456789">>}
%             {<<"Message-ID">>, wh_json:get_value(<<"_id">>, JObj)}
            ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
            ,{<<"Call-ID">>, wh_util:rand_hex_binary(16)}
            ,{<<"Body">>, wh_json:get_value(<<"body">>, JObj)}
            ,{<<"From">>, wh_json:get_value(<<"from">>, JObj)}
            ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"from_number">>, JObj)}
            ,{<<"To">>, wh_json:get_value(<<"to">>, JObj)}
            ,{<<"Request">>, wh_json:get_value(<<"request">>, JObj)}
            ,{<<"Body">>, wh_json:get_value(<<"body">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    do_deliver_sms(Contacts, Props, JobId, JObj),

    
    {'ok', JObj}.
    
    

do_deliver_sms([], Props, JobId, JObj) -> 'ok';
do_deliver_sms([ Contact | Contacts], Props, JobId, JObj) ->
    lager:info("SEND SMS ~p",[Contact]),
    C = Contact#uri{disp = <<>>},
    #uri{user=User, domain=IP, port=Port} = C,
    C1 = nksip_unparse:uri(C),
    
    Extras = [{<<"Contact">>, C1}
             ,{<<"Contact-IP">>, IP}
             ,{<<"Contact-Port">>, Port}
             ,{<<"Contact-Username">>, User}
              ],
    
    Payload = Props ++ Extras,
    lager:info("Payload ~p",[Payload]),

    
    wh_amqp_worker:cast('whapps_amqp_pool'
                       , Payload
                       ,fun wapi_sms:publish_message/1).
    




execute_callflow(AccountId, Number, Flow, JobId, JObj, CID) ->
    Data = wh_json:get_value(<<"data">>, Flow),
    DataId = wh_json:get_value([<<"data">>,<<"id">>], Flow),
    Module = wh_json:get_value(<<"module">>, Flow),
    Children = wh_json:get_value(<<"children">>, Flow, []),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Realm = wh_util:get_account_realm(AccountId),
    case lookup_devices(AccountDb, Realm, Module, DataId) of
        [] ->
            lager:info("URIS are empty"),
            {'failed', <<"URIS are empty">>, JObj};
        URIS ->
            lager:info("URIS ready ~p",[URIS]),
            set_ready(URIS, JobId, JObj, CID)
    end.



deliver_in_callflow(AccountId, Number, Flow, JobId, JObj, CID) ->
    Data = wh_json:get_value(<<"data">>, Flow),
    DataId = wh_json:get_value([<<"data">>,<<"id">>], Flow),
    Module = wh_json:get_value(<<"module">>, Flow),
    Children = wh_json:get_value(<<"children">>, Flow, []),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Realm = wh_util:get_account_realm(AccountId),
    case lookup_devices(AccountDb, Realm, Module, DataId) of
        [] ->
            lager:info("URIS are empty"),
            {'failed', <<"URIS are empty">>, JObj};
        URIS ->
            lager:info("URIS ready ~p",[URIS]),
            set_ready(URIS, JobId, JObj, CID)
    end.
            

            
set_ready(URIS, JobId, JObj, CID) ->
    Updaters = [fun(J) -> wh_json:set_value(<<"status">>, <<"ready">>, J) end
               ,fun(J) -> wh_json:set_value(<<"target">>, URIS, J) end
               ,fun(J) -> wh_json:set_value(<<"from_number">>, CID , J) end
               ],
    Update = lists:foldr(fun(F, J) -> F(J) end, JObj, Updaters),
    {'pending', Update}.


            




lookup_devices(AccountDb, AcctRealm, <<"device">>, Id) ->
    {'ok', Doc} = couch_mgr:open_cache_doc(AccountDb, Id),    
    get_uris_for_device(AcctRealm, Doc);
lookup_devices(AccountDb, AcctRealm, <<"user">>, Id) ->
    lager:info("lookup user devices ~s",[Id]),
    Options = [{'key', [Id, <<"device">>]}, 'include_docs'],
    lager:info("options ~p",[Options]),
    case couch_mgr:get_results(AccountDb, <<"cf_attributes/owned">>, Options) of
        {'ok', []} -> [];
        {'ok', [JObj]} ->
            lager:info("one"),
            Device = wh_json:get_value(<<"doc">>, JObj),
            get_uris_for_device(AcctRealm, Device);
        {'ok', [JObj | _Rest] = Docs} ->
            lager:info("varios"),
            get_uris_for_devices(AcctRealm, Docs);
        _E ->  lager:info("erro ~p",[_E]),[]
    end;
lookup_devices( _, _, _ , _) -> [].
    
                                             
get_uris_for_device(AcctRealm, [Doc | Docs]) ->
    lager:info("uris for device ~p",[Doc]),
    DeviceRealm = wh_json:get_value([<<"sip">>,<<"realm">>], Doc, []),
    Realms = [ AcctRealm | DeviceRealm ],
    Username = wh_json:get_value([<<"sip">>,<<"username">>], Doc),
    URIS = [ <<Username/binary,"@",Realm/binary>>  || Realm <- Realms],
    lager:info("uris for device ~p , ~p, ~p",[Realms,Username, URIS]),
    URIS.


    
get_uris_for_devices(_AcctRealm,[]) -> [];
get_uris_for_devices(AcctRealm, [JObj | JObjs]) ->
    Doc = wh_json:get_value(<<"doc">>, JObj),
    [ get_uris_for_device(AcctRealm, Doc) 
    | get_uris_for_devices(AcctRealm, JObjs)
    ].
    


                    
deliver_offnet(Number, JobId, JObj, CID) ->
    Realm = whapps_config:get_binary(<<"offnet_realm">>, ?CONFIG_CAT, <<"nowhere.com">>),    
    URIS = [<<Number/binary, "@", Realm/binary>>],
    NewJObj = wh_json:set_value(<<"target_type">>, <<"off-net">>, JObj),
    set_ready(URIS, JobId, NewJObj, CID).


get_number(Number) ->
    try wnm_number:get(Number) of
        #number{number_doc=JObj}=Number1 -> {'ok', JObj}
    catch
        _E1:_E2 ->
            {'error', _E2}
    end.    
    

-spec reset(state()) -> state().
reset(State) ->
    put('callid', ?LOG_SYSTEM_ID),
    State#state{job_id='undefined'
                ,job='undefined'
                ,pool='undefined'
               }.


exec_mod("user", JObj) ->
    JObj;
exec_mod("device", JObj) ->
    JObj;
exec_mod("offnet", JObj) ->
    JObj;
exec_mod(Module, JObj) -> JObj.
