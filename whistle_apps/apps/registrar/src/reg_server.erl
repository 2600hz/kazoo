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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("reg.hrl").

-define(SERVER, ?MODULE).
-define(REG_QUEUE_NAME, <<"registrar.queue">>).

-record(state, {
	   amqp_q = <<>> :: binary()
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
    case whereis(reg_cache) of
        Pid when is_pid(Pid) ->
            _ = prime_cache(Pid),

            ?LOG_SYS("starting cleanup timer for ~p msec", [?CLEANUP_RATE]),
            Ref = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok),

            erlang:monitor(process, Pid),

            {noreply, State#state{cache=Pid, cleanup_ref=Ref}, 0};
        _ ->
            ?LOG_SYS("could not locate cache, trying again in 1000 msec"),
            {noreply, State, 1000}
    end;

handle_info(timeout, #state{amqp_q = <<>>}=State) ->
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

handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref, cache=Cache}=S) ->
    spawn(fun() -> cleanup_registrations(Cache) end),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok),
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({timeout, Ref1, _}, #state{cleanup_ref=Ref}=S) ->
    ?LOG_SYS("bad cleanup timer ref ~p, expected ~p", [Ref1, Ref]),
    _ = erlang:cancel_timer(Ref),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok),
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
          end),
    {noreply, State};

handle_info({'basic.consume_ok', _}, S) ->
    {noreply, S};

handle_info({'DOWN', MRef, process, Cache, _Reason}, #state{cache=Cache}=State) ->
    ?LOG_SYS("registrar cache process went down, attempting to reconnect"),
    erlang:demonitor(MRef),
    {noreply, State#state{cache=undefined}, 50};

handle_info(_Info, State) ->
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
terminate(_Reason, _) ->
    ?LOG_SYS("registrar server ~p termination", [_Reason]),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec(start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error)).
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
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"authn_req">>}, JObj, #state{amqp_q=Queue}) ->
    try
	?LOG_START("received SIP authentication request"),

	AuthU = wh_json:get_value(<<"Auth-User">>, JObj),
	AuthR = wh_json:get_value(<<"Auth-Domain">>, JObj),

        %% crashes if not found, no return necessary
	{ok, AuthJObj} = lookup_auth_user(AuthU, AuthR),

        AccountId = whapps_util:get_db_name(wh_json:get_value([<<"doc">>, <<"pvt_account_db">>], AuthJObj), raw),
        AuthId = wh_json:get_value([<<"doc">>, <<"_id">>], AuthJObj, <<>>),

	Defaults = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
		    ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Username">>, AuthU}
							   ,{<<"Realm">>, AuthR}
                                                           ,{<<"Account-ID">>, AccountId}
                                                           ,{<<"Inception">>, <<"on-net">>}
                                                           ,{<<"Authorizing-ID">>, AuthId}
							  ]
						 }}
		    | whistle_api:default_headers(Queue % serverID is not important, though we may want to define it eventually
						  ,wh_json:get_value(<<"Event-Category">>, JObj)
						  ,<<"authn_resp">>
						  ,?APP_NAME
						  ,?APP_VERSION)],
	{ok, Payload} = authn_response(wh_json:get_value(<<"value">>, AuthJObj), Defaults),
	RespQ = wh_json:get_value(<<"Server-ID">>, JObj),
	send_resp(Payload, RespQ)
    catch
	Type:Reason ->
	    ?LOG("SIP authentication exception ~w: ~w", [Type, Reason]),
	    ?LOG_END("stacktrace ~w", [erlang:get_stacktrace()])
    end;

process_req({<<"directory">>, <<"reg_success">>}, JObj, #state{cache=Cache}) ->
    try
	?LOG_START("received registration success"),
	true = whistle_api:reg_success_v(JObj),

	[User, AfterAt] = binary:split(wh_json:get_value(<<"Contact">>, JObj), <<"@">>), % only one @ allowed

	AfterUnquoted = whistle_util:to_binary(mochiweb_util:unquote(AfterAt)),
	Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),

	Id = whistle_util:to_binary(whistle_util:to_hex(erlang:md5(Contact1))),
	Expires = whistle_util:current_tstamp() + whistle_util:to_integer(wh_json:get_value(<<"Expires">>, JObj, 3600)),

        Username = wh_json:get_value(<<"Username">>, JObj),
        Realm = wh_json:get_value(<<"Realm">>, JObj),

	case wh_cache:fetch_local(Cache, Id) of
	    {error, not_found} ->
		?LOG("contact for ~s@~s not in cache", [Username, Realm]),

		remove_old_regs(Username, Realm, Cache),
                ?LOG("flushed users registrations"),

		?LOG("Cache miss, rm old and save new ~s for ~p seconds", [Id, Expires]),

		wh_cache:store_local(Cache, Id, Expires, Expires),
		{ok, _} = store_reg(JObj, Id, Contact1),
                ?LOG_END("new contact hash ~s stored for ~p seconds", [Id, Expires]);
	    {ok, _} ->
		?LOG("contact for ~s@~s found in cache", [Username, Realm]),
		case couch_mgr:lookup_doc_rev(<<"registrations">>, Id) of
		    {ok, _} ->
			wh_cache:store_local(Cache, Id, Expires, Expires),
                        ?LOG_END("contact hash ~s requires no update", [Id]);
		    {error, _} ->
			?LOG("contact hash missing in db"),
			wh_cache:store_local(Cache, Id, Expires, Expires),

			remove_old_regs(User, Realm, Cache),
                        ?LOG("flushed users registrations, to be safe"),

			{ok, _} = store_reg(JObj, Id, Contact1),
                        ?LOG_END("added contact hash ~s to db", [Id])
		end
	end
    catch
	Type:Reason ->
	    ?LOG("registration processor exception ~w: ~w", [Type, Reason]),
	    ?LOG_END("stacktrace ~w", [erlang:get_stacktrace()])
    end;

process_req({<<"directory">>, <<"reg_query">>}, JObj, #state{amqp_q=Queue}) ->
    try
	?LOG_START("received registration query"),
	true = whistle_api:reg_query_v(JObj),

	Domain = wh_json:get_value(<<"Realm">>, JObj),
	User = wh_json:get_value(<<"Username">>, JObj),

	case couch_mgr:get_results("registrations", <<"registrations/newest">>
				       ,[{<<"startkey">>, [Domain, User,?EMPTY_JSON_OBJECT]}, {<<"endkey">>, [Domain, User, 0]}, {<<"descending">>, true}]) of
	    {ok, []} ->
		?LOG_END("contact for ~s@~s not found", [User, Domain]);
	    {ok, [ViewRes | _]} ->
		DocId = wh_json:get_value(<<"id">>, ViewRes),

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

		{ok, Payload} = whistle_api:reg_query_resp([ {<<"Fields">>, {struct, RespFields}}
							  | whistle_api:default_headers(Queue
											,<<"directory">>
											,<<"reg_query_resp">>
											,?APP_NAME
											,?APP_VERSION)
							]),

		?LOG_END("found contact for ~s@~s in registration ~s", [User, Domain, DocId]),

		RespServer = wh_json:get_value(<<"Server-ID">>, JObj),
		amqp_util:targeted_publish(RespServer, Payload, <<"application/json">>)
	end
    catch
	Type:Reason ->
	    ?LOG("registration query exception ~w: ~w", [Type, Reason]),
	    ?LOG_END("stacktrace ~w", [erlang:get_stacktrace()])
    end;

process_req({_, _}, _, _) ->
    {error, invalid_event}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% store a sucessful registration in the database
%% @end
%%-----------------------------------------------------------------------------
-spec(store_reg/3 :: (JObj :: json_object(), Id :: binary(), Contact :: binary()) -> tuple(ok, json_object() | json_objects())).
store_reg(JObj, Id, Contact) ->
    RegDoc = wh_json:set_value(<<"_id">>, Id, wh_json:set_value(<<"Contact">>, Contact, JObj)),
    couch_mgr:ensure_saved(?REG_DB, RegDoc).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% periodically remove expired registrations
%% @end
%%-----------------------------------------------------------------------------
-spec(remove_old_regs/3 :: (User :: binary(), Realm :: binary(), Cache :: pid()) -> ok).
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
%% remove expired registration document from the cache and db if it still
%% exists in either.  IE: get all documents with one or more tstamps < Now
%% @end
%%-----------------------------------------------------------------------------
cleanup_registrations(Cache) ->
    ?LOG_SYS("cleaning up expired registrations"),
    Now = whistle_util:current_tstamp(),
    Expired = wh_cache:filter_local(Cache, fun(_, V) -> V < Now end),
    lists:foreach(fun({K,_}) ->
			  {ok, Rev} = couch_mgr:lookup_doc_rev(?REG_DB, K),
			  {ok, _} = couch_mgr:del_doc(?REG_DB, {struct, [{<<"_id">>, K}, {<<"_rev">>, Rev}]}),
			  wh_cache:erase(Cache, K)
		  end, Expired).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec(lookup_auth_user/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, no_user_found)).
lookup_auth_user(Name, Realm) ->
    ?LOG("looking up ~s@~s", [Name, Realm]),
    case couch_mgr:get_results(?AUTH_DB, <<"credentials/lookup">>, [{<<"key">>, [Realm, Name]}, {<<"include_docs">>, true}]) of
	{error, R} ->
            ?LOG_END("failed to look up SIP credentials ~p", [R]),
	    {error, no_user_found};
	{ok, []} ->
            ?LOG("~s@~s not found", [Name, Realm]),
            {error, no_user_found};
	{ok, [User|_]} ->
	    {ok, User}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% determine if the user was known and send a reply if so
%% @end
%%-----------------------------------------------------------------------------
-spec(authn_response/2 :: (AuthInfo :: proplist() | integer(), Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
authn_response([], _) ->
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
-spec(auth_specific_response/1 :: (AuthInfo :: proplist() | integer()) -> proplist()).
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
-spec(prime_cache/1 :: (Pid :: pid()) -> list()).
prime_cache(Pid) when is_pid(Pid) ->
    ?LOG_SYS("priming registrar cache"),
    {ok, Docs} = couch_mgr:all_docs(?REG_DB),
    Expires = whistle_util:current_tstamp() + 3600,
    [ wh_cache:store_local(Pid, wh_json:get_value(<<"id">>, View), Expires) || View <- Docs ].
