%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Store registrations, do user lookups for contact strings
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_server).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("reg.hrl").

-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"registrar.queue">>).

-record(state, {
	   amqp_q = <<>> :: binary()
	  ,cache = undefined :: undefined | pid()
	 }).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

lookup(Realm, Username) ->
    gen_server:call(?SERVER, {lookup, Realm, Username}).

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
    process_flag(trap_exit, true),
    ?LOG_SYS("starting new registrar server"),
    ?LOG_SYS("ensuring database ~s exists", [?REG_DB]),
    couch_mgr:db_create(?REG_DB),
    ?LOG_SYS("ensuring database ~s exists", [?AUTH_DB]),
    couch_mgr:db_create(?AUTH_DB),
    lists:foreach(fun({DB, File}) ->
                          ?LOG_SYS("ensuring database ~s has view ~s", [DB, File]),
			  try
			      {ok, _} = couch_mgr:update_doc_from_file(DB, registrar, File)
			  catch
			      _:_ ->
				  couch_mgr:load_doc_from_file(DB, registrar, File)
			  end
		  end, ?JSON_FILES),
    {ok, #state{}, 0}.

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
handle_call({lookup, Realm, Username}, From, #state{cache=Cache}=State) ->
    spawn(fun() -> gen_server:reply(From, lookup_registration(Realm, Username, Cache)) end),
    {noreply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn_link(fun() ->
		       JObj = mochijson2:decode(Payload),
		       whapps_util:put_callid(JObj),
		       _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
	       end),
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    ?LOG_SYS("~p exited: ~p", [_Pid, _Reason]),
    {noreply, State};

handle_info(timeout, #state{cache=undefined}=State) ->
    case whereis(reg_cache) of
        Pid when is_pid(Pid) ->
            _ = prime_cache(Pid),
            erlang:monitor(process, Pid),
            {noreply, State#state{cache=Pid}, 0};
        _ ->
            ?LOG_SYS("could not locate cache, trying again in 1000 msec"),
            {noreply, State, 1000}
    end;

handle_info(timeout, #state{amqp_q = Q}=State) when not is_binary(Q) orelse Q =:= <<>> ->
    try
        {ok, Q} = start_amqp(),
        {noreply, State#state{amqp_q=Q}}
    catch
        _:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};

handle_info({'basic.consume_ok', _}, S) ->
    {noreply, S};

handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG_SYS("registrar cache process went down, attempting to reconnect"),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State, 1000}.

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
-spec terminate/2 :: (term(), #state{}) -> ok.
terminate(_Reason, _) ->
    ?LOG_SYS("registrar server ~p termination", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/0 :: () -> {ok, binary()} | {error, amqp_error}.
start_amqp() ->
    try
        _ = amqp_util:callmgr_exchange(),
	Q = amqp_util:new_queue(?REG_QUEUE_NAME, [{exclusive, false}]),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_QUERY),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTHN_REQ),
	amqp_util:basic_consume(Q, [{exclusive, false}]),
        ?LOG_SYS("connected to AMQP"),
	{ok, Q}
    catch
	_:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process AMQP messages
%% @end
%%--------------------------------------------------------------------
-spec process_req/3 :: (MsgType, JObj, State) -> ok when
      MsgType :: {binary(), binary()},
      JObj :: json_object(),
      State :: #state{}.
process_req({<<"directory">>, <<"authn_req">>}, JObj, #state{amqp_q=Queue, cache=Cache}) ->
    ?LOG_START("received SIP authentication request"),

    AuthU = wh_json:get_value(<<"Auth-User">>, JObj),
    AuthR = wh_json:get_value(<<"Auth-Domain">>, JObj),

    %% crashes if not found, no return necessary
    {ok, AuthJObj} = lookup_auth_user(AuthU, AuthR, Cache),

    AuthId = wh_json:get_value([<<"doc">>, <<"_id">>], AuthJObj),
    AccountId = case wh_json:get_value([<<"doc">>, <<"pvt_account_db">>], AuthJObj) of
		    undefined -> undefined;
		    AcctId -> whapps_util:get_db_name(AcctId, raw)
		end,

    CCVs = [CCV || CCV <- [{<<"Username">>, AuthU}
			   ,{<<"Realm">>, AuthR}
			   ,{<<"Account-ID">>, AccountId}
			   ,{<<"Inception">>, <<"on-net">>}
			   ,{<<"Authorizing-ID">>, AuthId}
			  ],
		   CCV =/= undefined],

    Defaults = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		,{<<"Custom-Channel-Vars">>, {struct, CCVs}}
		| whistle_api:default_headers(Queue % serverID is not important, though we may want to define it eventually
					      ,wh_json:get_value(<<"Event-Category">>, JObj)
					      ,<<"authn_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],

    {ok, Payload} = authn_response(wh_json:get_value(<<"value">>, AuthJObj), Defaults),
    RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
    send_resp(Payload, RespQ);

process_req({<<"directory">>, <<"reg_success">>}, JObj, #state{cache=Cache}) ->
    ?LOG_START("received registration success"),
    true = whistle_api:reg_success_v(JObj),

    [User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed

    AfterUnquoted = whistle_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

    JObj1 = wh_json:set_value(<<"Contact">>, Contact1, JObj),

    Id = whistle_util:to_binary(whistle_util:to_hex(erlang:md5(Contact1))),
    CacheKey = {?MODULE, Id},
    Expires = whistle_util:current_tstamp() + whistle_util:to_integer(wh_json:get_value(<<"Expires">>, JObj, 3600)),

    Username = wh_json:get_value(<<"Username">>, JObj),
    Realm = wh_json:get_value(<<"Realm">>, JObj),

    case wh_cache:fetch_local(Cache, CacheKey) of
	{error, not_found} ->
	    ?LOG("contact for ~s@~s not in cache", [Username, Realm]),

	    remove_old_regs(Username, Realm, Cache),
	    ?LOG("flushed users registrations"),

	    ?LOG("Cache miss, rm old and save new ~s for ~p seconds", [Id, Expires]),

	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, {?MODULE, registration, Realm, Username}, CacheKey),

	    {ok, _} = store_reg(JObj, Id, Contact1),
	    ?LOG_END("new contact hash ~s stored for ~p seconds", [Id, Expires]);
	{ok, _} ->
	    ?LOG("contact for ~s@~s found in cache", [Username, Realm]),
	    wh_cache:store_local(Cache, CacheKey, JObj1, Expires),
	    wh_cache:store_local(Cache, {?MODULE, registration, Realm, Username}, CacheKey),

	    ?LOG_END("not verifying with DB, assuming cached JSON is valid")
    end;

process_req({<<"directory">>, <<"reg_query">>}, ApiJObj, #state{amqp_q=Queue, cache=Cache}) ->
    ?LOG_START("received registration query"),
    true = whistle_api:reg_query_v(ApiJObj),

    Realm = wh_json:get_value(<<"Realm">>, ApiJObj),
    Username = wh_json:get_value(<<"Username">>, ApiJObj),

    {ok, RegJObj} = lookup_registration(Realm, Username, Cache),

    RespFields = case wh_json:get_value(<<"Fields">>, ApiJObj) of
		     [] ->
			 wh_json:delete_key(<<"_id">>, wh_json:delete_key(<<"_rev">>, RegJObj));
		     Fields ->
			 {struct, lists:foldl(fun(F, Acc) ->
						      [ {F, wh_json:get_value(F, RegJObj)} | Acc]
					      end, [], Fields)}
		 end,

    {ok, Payload} = whistle_api:reg_query_resp([ {<<"Fields">>, RespFields}
						 | whistle_api:default_headers(Queue
									       ,<<"directory">>
									       ,<<"reg_query_resp">>
									       ,?APP_NAME
									       ,?APP_VERSION)
					       ]),

    ?LOG_END("found contact for ~s@~s in registration", [Username, Realm]),

    RespServer = wh_json:get_value(<<"Server-ID">>, ApiJObj),
    amqp_util:targeted_publish(RespServer, Payload, <<"application/json">>).

-spec lookup_registration/3 :: (Realm, Username, Cache) -> {ok, json_object()} | {error, not_found} when
      Realm :: binary(),
      Username :: binary(),
      Cache :: pid().
lookup_registration(Realm, Username, Cache) ->
    case wh_cache:fetch_local(Cache, {?MODULE, registration, Realm, Username}) of
	{ok, CacheKey} ->
	    ?LOG_SYS("Found cached registration"),
	    wh_cache:fetch_local(Cache, CacheKey);
	{error, not_found} ->
	    case couch_mgr:get_results("registrations"
				       ,<<"registrations/newest">>
					   ,[{<<"startkey">>, [Realm, Username,?EMPTY_JSON_OBJECT]}
					     ,{<<"endkey">>, [Realm, Username, 0]}
					     ,{<<"descending">>, true}
					    ]) of
		{ok, []} ->
		    ?LOG_END("contact for ~s@~s not found", [Username, Realm]),
		    {error, not_found};
		{ok, [ViewRes | _]} ->
		    DocId = wh_json:get_value(<<"id">>, ViewRes),
		    ?LOG_SYS("Found registration in DB: ~s", [DocId]),
		    couch_mgr:open_doc(?REG_DB, DocId)
	    end
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% store a sucessful registration in the database
%% @end
%%-----------------------------------------------------------------------------
-spec store_reg/3 :: (JObj, Id, Contact) -> {ok, json_object() | json_objects()} when
      JObj :: json_object(),
      Id :: binary(),
      Contact :: binary().
store_reg(JObj, Id, Contact) ->
    RegDoc = wh_json:set_value(<<"_id">>, Id, wh_json:set_value(<<"Contact">>, Contact, JObj)),
    couch_mgr:ensure_saved(?REG_DB, RegDoc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% periodically remove expired registrations
%% @end
%%-----------------------------------------------------------------------------
-spec remove_old_regs/3 :: (User, Realm, Cache) -> ok when
      User :: binary(),
      Realm :: binary(),
      Cache :: pid().
remove_old_regs(User, Realm, Cache) ->
    case couch_mgr:get_results(<<"registrations">>, <<"registrations/newest">>,
			       [{<<"startkey">>, [Realm, User, 0]}, {<<"endkey">>, [Realm, User, ?EMPTY_JSON_OBJECT]}]) of
	{ok, OldDocs} ->
	    spawn(fun() ->
			  DelDocs = [ begin
					  ID = wh_json:get_value(<<"id">>, Doc),
					  wh_cache:erase_local(Cache, ID),
					  case couch_mgr:lookup_doc_rev(<<"registrations">>, ID) of
					      {ok, Rev} -> {struct, [{<<"_id">>, ID}, {<<"_rev">>, Rev}]};
					      _ -> ?EMPTY_JSON_OBJECT
					  end
				      end || Doc <- OldDocs ],
			  couch_mgr:del_docs(<<"registrations">>, DelDocs)
		  end), ok;
	_ -> ok
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user/3 :: (Name, Realm, Cache) -> {ok, json_object()} | {error, no_user_found} when
      Name :: binary(),
      Realm :: binary(),
      Cache :: pid().
lookup_auth_user(Name, Realm, Cache) ->
    ?LOG("looking up ~s@~s", [Name, Realm]),
    CacheKey = {?MODULE, Realm, Name},
    case wh_cache:fetch_local(Cache, CacheKey) of
	{error, not_found} ->
	    case couch_mgr:get_results(?AUTH_DB, <<"credentials/lookup">>, [{<<"key">>, [Realm, Name]}, {<<"include_docs">>, true}]) of
		{error, R} ->
		    ?LOG_END("failed to look up SIP credentials ~p", [R]),
		    {error, no_user_found};
		{ok, []} ->
		    ?LOG("~s@~s not found", [Name, Realm]),
		    {error, no_user_found};
		{ok, [User|_]} ->
		    ?LOG("Storing ~s@~s in cache", [Name, Realm]),
		    wh_cache:store_local(Cache, CacheKey, User),
		    {ok, User}
	    end;
	{ok, _}=OK ->
	    ?LOG("Pulling auth user from cache"),
	    OK
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if the user was known and send a reply if so
%% @end
%%-----------------------------------------------------------------------------
-spec authn_response/2 :: (AuthnResp, Prop) -> {ok, iolist()} | {error, string()} when
      AuthnResp :: json_object() | integer(),
      Prop :: proplist().
authn_response(?EMPTY_JSON_OBJECT, _) ->
    ?LOG_END("user is unknown");
authn_response(AuthInfo, Prop) ->
    Data = lists:umerge(auth_specific_response(AuthInfo), Prop),
    ?LOG_END("sending SIP authentication reply, with credentials"),
    whistle_api:authn_resp(Data).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% create a auth response proplist to send back when the user is known
%% @end
%%-----------------------------------------------------------------------------
-spec auth_specific_response/1 :: (AuthInfo) -> proplist() when
      AuthInfo :: json_object() | integer().
auth_specific_response(AuthInfo) ->
    Method = list_to_binary(string:to_lower(binary_to_list(wh_json:get_value(<<"method">>, AuthInfo, <<"password">>)))),
    [{<<"Auth-Password">>, wh_json:get_value(<<"password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"authn_resp">>}
     ,{<<"Access-Group">>, wh_json:get_value(<<"access_group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, wh_json:get_value(<<"tenant_id">>, AuthInfo, <<"ignore">>)}
    ].

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a payload to a targeted queue
%% @end
%%-----------------------------------------------------------------------------
send_resp(Payload, RespQ) ->
    amqp_util:targeted_publish(RespQ, Payload, <<"application/json">>).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% load the registrar cache with the contents from the registrar db
%% @end
%%-----------------------------------------------------------------------------
-spec prime_cache/1 :: (Pid) -> ok when
      Pid :: pid().
prime_cache(Pid) when is_pid(Pid) ->
    ?LOG_SYS("priming registrar cache"),
    {ok, Docs} = couch_mgr:all_docs(?REG_DB),
    Expires = whistle_util:current_tstamp() + 3600,
    _ = [ wh_cache:store_local(Pid, wh_json:get_value(<<"id">>, View), Expires) || View <- Docs ],
    ok.
