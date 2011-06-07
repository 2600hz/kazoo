%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Store registrations, do user lookups for contact strings
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(reg_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("reg.hrl").

-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"registrar.queue">>).

-record(state, {
	  is_amqp_up = true :: boolean()
	  ,my_q = {error, undefined} :: binary() | tuple(error, term())
	  ,cleanup_ref = undefined :: undefined | reference()
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
    couch_mgr:db_create(?REG_DB),
    couch_mgr:db_create(?AUTH_DB),
    lists:foreach(fun({DB, File}) ->
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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_info(timeout, #state{cache=undefined}=State) ->
    handle_info(timeout, State#state{cache=whereis(reg_cache)});

handle_info(timeout, #state{cache=Pid}=State) ->
    prime_cache(Pid),
    Q = start_amqp(),
    Ref = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    ?LOG_SYS("Starting timer for ~p msec: ~p", [?CLEANUP_RATE, Ref]),

    {noreply, State#state{cleanup_ref=Ref, my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info(Req, #state{my_q={error, _}}=State) ->
    Q = start_amqp(),
    handle_info(Req, State#state{my_q=Q, is_amqp_up=is_binary(Q)});

handle_info({timeout, _, _}, #state{is_amqp_up=false}=S) ->
    handle_info(timeout, S);

handle_info({amqp_host_down, _H}, S) ->
    Q = start_amqp(),
    {noreply, S#state{my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref, cache=Cache}=S) ->
    ?LOG_SYS("Time to clean old registrations"),
    spawn(fun() -> cleanup_registrations(Cache) end),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({timeout, Ref1, _}, #state{cleanup_ref=Ref}=S) ->
    ?LOG_SYS("wrong ref ~p, expected ~p~n", [Ref1, Ref]),
    _ = erlang:cancel_timer(Ref),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};

handle_info({'basic.consume_ok', _}, S) ->
    {noreply, S};

handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG("Cache(~p) went down: ~p", [Cache, _Reason]),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info(_Info, State) ->
    ?LOG("Unhandled message: ~p", [_Info]),
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
-spec(terminate/2 :: (_, #state{}) -> no_return()).
terminate(_, _) ->
    ok.

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
-spec(start_amqp/0 :: () -> binary() | tuple(error, amqp_error)).
start_amqp() ->
    try
	Q = amqp_util:new_queue(?REG_QUEUE_NAME, [{exclusive, false}]),

	amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_QUERY),
	amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTH_REQ),

	amqp_util:basic_consume(Q, [{exclusive, false}]),
	Q
    catch
	_:_ -> {error, amqp_error}
    end.

-spec(handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> no_return()).
handle_req(<<"application/json">>, Payload, State) ->
    {struct, Prop} = JObj = mochijson2:decode(Payload),

    put(callid, wh_json:get_value(<<"Msg-ID">>, JObj, wh_json:get_value(<<"Call-ID">>, JObj))),
    ?LOG_START("Received JSON: ~s", [Payload]),

    process_req(get_msg_type(Prop), JObj, State).

-spec(get_msg_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary())).
get_msg_type(Prop) ->
    { props:get_value(<<"Event-Category">>, Prop), props:get_value(<<"Event-Name">>, Prop) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, JObj, #state{my_q=Queue}) ->
    try
	?LOG("Authentication request"),

	AuthU = wh_json:get_value(<<"Auth-User">>, JObj),
	AuthR = wh_json:get_value(<<"Auth-Domain">>, JObj),

	Direction = <<"outbound">>, %% if we're authing, it's an outbound call or a registration; inbound from carriers is ACLed auth

	{ok, AuthJObj} = lookup_auth_user(AuthU, AuthR), %% crashes if not found, no return necessary

	Defaults = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		    ,{<<"Custom-Channel-Vars">>, {struct, [
							   {<<"Direction">>, Direction}
							   ,{<<"Username">>, AuthU}
							   ,{<<"Realm">>, AuthR}
							   ,{<<"Authorizing-ID">>, wh_json:get_value(<<"authorizing_id">>, AuthJObj, <<"ignore">>)}
							  ]
						 }}
		    | whistle_api:default_headers(Queue % serverID is not important, though we may want to define it eventually
						  ,wh_json:get_value(<<"Event-Category">>, JObj)
						  ,<<"auth_resp">>
						  ,?APP_NAME
						  ,?APP_VERSION)],
	{ok, JSON} = auth_response(AuthJObj, Defaults),
	RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
	send_resp(JSON, RespQ),
	?LOG_END("End of authentication")
    catch
	Type:Reason ->
	    ?LOG_END("Authentication exception: ~p:~p", [Type, Reason]),
	    ?LOG("Stacktrace: ~p~n", [erlang:get_stacktrace()])
    end;

process_req({<<"directory">>, <<"reg_success">>}, JObj, #state{cache=Cache}) ->
    try
	?LOG("Registration success"),
	true = whistle_api:reg_success_v(JObj),

	[User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed

	AfterUnquoted = whistle_util:to_binary(mochiweb_util:unquote(AfterAt)),
	Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

	Id = whistle_util:to_binary(whistle_util:to_hex(erlang:md5(Contact1))),
	Expires = whistle_util:current_tstamp() + whistle_util:to_integer(wh_json:get_value(<<"Expires">>, JObj, 3600)),

	case wh_cache:fetch_local(Cache, Id) of
	    {error, not_found} ->
<<<<<<< HEAD
		logger:info("~s | Log | ~p.~p(~p): cache miss, rm old and save new ~p ~p", [CallID, ?MODULE, ?LINE, self(), Id, Expires]),
		
=======
		?LOG("Cache miss, rm old and save new ~s for ~p seconds", [Id, Expires]),

>>>>>>> origin/master
		remove_old_regs(wh_json:get_value(<<"Username">>, JObj), wh_json:get_value(<<"Realm">>, JObj), Cache),
		wh_cache:store_local(Cache, Id, Expires, Expires),
		store_reg(JObj, Id, Contact1);
	    {ok, _} ->
		?LOG("Cache hit, check rev ~s", [Id]),
		case couch_mgr:lookup_doc_rev(<<"registrations">>, Id) of
		    {ok, _} ->
			?LOG("Cache hit, rev exists for ~s", [Id]),
			wh_cache:store_local(Cache, Id, Expires, Expires);
		    {error, _} ->
			User = wh_json:get_value(<<"Username">>, JObj),
			Realm = wh_json:get_value(<<"Realm">>, JObj),
			?LOG("Cache hit, rev missing for ~s@~s ~s", [User, Realm, Id]),
			wh_cache:store_local(Cache, Id, Expires, Expires),
			remove_old_regs(User, Realm, Cache),
			store_reg(JObj, Id, Contact1)
		end
	end,
	?LOG_END("Registration successfully saved/updated")
    catch
	Type:Reason ->
	    ?LOG_END("Registration success exception: ~p:~p", [Type, Reason]),
	    ?LOG("Stacktrace: ~p~n", [erlang:get_stacktrace()])
    end;

process_req({<<"directory">>, <<"reg_query">>}, JObj, #state{my_q=Queue}) ->
    try
	?LOG("Registration query"),
	true = whistle_api:reg_query_v(JObj),

	Domain = wh_json:get_value(<<"Realm">>, JObj),
	User = wh_json:get_value(<<"Username">>, JObj),

	case couch_mgr:get_results("registrations", <<"registrations/newest">>
				       ,[{<<"startkey">>, [Domain, User,?EMPTY_JSON_OBJECT]}, {<<"endkey">>, [Domain, User, 0]}, {<<"descending">>, true}]) of
	    {ok, []} ->
		?LOG("No reg_query_response for ~s@~s", [User, Domain]);
	    {ok, [ViewRes | _]} ->
		DocId = wh_json:get_value(<<"id">>, ViewRes),
		?LOG("Reg_query doc(~s) for ~s@~s", [DocId, User, Domain]),

		{ok, RegJObj} = couch_mgr:open_doc(?REG_DB, DocId),

		RespFields = case wh_json:get_value(<<"Fields">>, JObj) of
				 [] ->
				     {struct, RegDoc} = RegJObj,
				     lists:keydelete(<<"_rev">>, 1, lists:keydelete(<<"_id">>, 1, RegDoc));
				 Fields ->
				     lists:foldl(fun(F, Acc) ->
							 [ {F, wh_json:get_value(F, RegJObj)} | Acc]
						 end, [], Fields)
			     end,

		{ok, JSON} = whistle_api:reg_query_resp([ {<<"Fields">>, {struct, RespFields}}
							  | whistle_api:default_headers(Queue
											,<<"directory">>
											,<<"reg_query_resp">>
											,?APP_NAME
											,?APP_VERSION)
							]),

		RespServer = wh_json:get_value(<<"Server-ID">>, JObj),
		?LOG("Sending response to ~s: ~s", [RespServer, JSON]),
		amqp_util:targeted_publish(RespServer, JSON, <<"application/json">>),
		?LOG_END("Registration query finished")
	end
    catch
	Type:Reason ->
	    ?LOG_END("Registration query exception: ~p:~p", [Type, Reason]),
	    ?LOG("Stacktrace: ~p~n", [erlang:get_stacktrace()])
    end;
process_req({_Cat, _Evt},_JObj,_) ->
    ?LOG("Unhandled message: ~s:~s -> ~p", [_Cat, _Evt, _JObj]).

<<<<<<< HEAD
-spec(store_reg/3 :: (Prop :: proplist(), Id :: binary(), Contact :: binary()) -> no_return()).
=======
-spec(store_reg/3 :: (JObj :: json_object(), Id :: binary(), Contact :: binary()) -> no_return()).
>>>>>>> origin/master
store_reg({struct, Prop}, Id, Contact) ->
    MochiDoc = {struct, [{<<"Reg-Server-Timestamp">>, whistle_util:current_tstamp()}
			 ,{<<"Contact">>, Contact}
			 ,{<<"_id">>, Id}
			 | lists:keydelete(<<"Contact">>, 1, Prop)]
	       },
    couch_mgr:ensure_saved(?REG_DB, MochiDoc).

-spec(remove_old_regs/3 :: (User :: binary(), Realm :: binary(), Cache :: pid()) -> ok).
remove_old_regs(User, Realm, Cache) ->
    case couch_mgr:get_results(<<"registrations">>, <<"registrations/newest">>,
			       [{<<"startkey">>, [Realm, User, 0]}, {<<"endkey">>, [Realm, User, ?EMPTY_JSON_OBJECT]}]) of
	{ok, OldDocs} ->
	    spawn(fun() ->
			  DelDocs = [ begin
					  ID = wh_json:get_value(<<"id">>, Doc),
					  wh_cache:erase_local(Cache, ID),
					  {ok, Rev} = couch_mgr:lookup_doc_rev(<<"registrations">>, ID),
					  {struct, [{<<"_id">>, ID}, {<<"_rev">>, Rev}]}
				      end || Doc <- OldDocs ],
			  couch_mgr:del_docs(<<"registrations">>, DelDocs)
		  end), ok;
	_ -> ok
    end.

cleanup_registrations(Cache) ->
    %% get all documents with one or more tstamps < Now
    Now = whistle_util:current_tstamp(),
    Expired = wh_cache:filter_local(Cache, fun(_, V) -> V < Now end),
    lists:foreach(fun({K,_}) ->
			  {ok, D} = couch_mgr:open_doc(?REG_DB, K),
			  couch_mgr:del_doc(?REG_DB, D),
			  wh_cache:erase(Cache, K)
		  end, Expired).

-spec(lookup_auth_user/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_auth_user(Name, Realm) ->
    ?LOG("Looking up ~s@~s", [Name, Realm]),
    case couch_mgr:get_results(?AUTH_DB, <<"credentials/lookup">>, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, "No user/realm found"};
	{ok, [User|_]} ->
	    {ok, wh_json:get_value(<<"value">>, User)}
    end.

-spec(auth_response/2 :: (AuthInfo :: proplist() | integer(), Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
auth_response([], Prop) ->
    Data = lists:umerge(auth_specific_response(403), Prop),
    whistle_api:auth_resp(Data);
auth_response(AuthInfo, Prop) ->
    Data = lists:umerge(auth_specific_response(AuthInfo), Prop),
    whistle_api:auth_resp(Data).

-spec(auth_specific_response/1 :: (AuthInfo :: proplist() | integer()) -> proplist()).
auth_specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}];
auth_specific_response(AuthInfo) ->
    Method = list_to_binary(string:to_lower(binary_to_list(wh_json:get_value(<<"method">>, AuthInfo, <<"password">>)))),
    [{<<"Auth-Password">>, wh_json:get_value(<<"password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, wh_json:get_value(<<"access_group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, wh_json:get_value(<<"tenant_id">>, AuthInfo, <<"ignore">>)}
    ].

send_resp(Payload, RespQ) ->
    ?LOG("Sending JSON to ~s: ~s", [RespQ, Payload]),
    amqp_util:targeted_publish(RespQ, Payload, <<"application/json">>).

-spec(prime_cache/1 :: (Pid :: pid()) -> list()).
prime_cache(Pid) when is_pid(Pid) ->
    {ok, Docs} = couch_mgr:all_docs(?REG_DB),
    Expires = whistle_util:current_tstamp() + 3600,
    [ wh_cache:store_local(Pid, wh_json:get_value(<<"id">>, View), Expires) || View <- Docs ].
