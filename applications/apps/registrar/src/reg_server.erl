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

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  is_amqp_up = true :: boolean()
	  ,my_q = {error, undefined} :: binary() | tuple(error, term())
	  ,cleanup_ref = undefined :: undefined | reference()
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
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(timeout, State) ->
    Q = start_amqp(),
    Ref = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    format_log(info, "REG_SRV(~p): Starting timer for ~p msec: ~p~n", [self(), ?CLEANUP_RATE, Ref]),

    {noreply, State#state{cleanup_ref=Ref, my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info(Req, #state{my_q={error, _}}=State) ->
    Q = start_amqp(),
    handle_info(Req, State#state{my_q=Q, is_amqp_up=is_binary(Q)});

handle_info({timeout, _, _}, #state{is_amqp_up=false}=S) ->
    handle_info(timeout, S);

handle_info({amqp_host_down, _H}, S) ->
    Q = start_amqp(),
    {noreply, S#state{my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref}=S) ->
    format_log(info, "REG_SRV(~p): Time to clean old registrations~n", [self()]),
    spawn(fun() -> cleanup_registrations() end),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({timeout, Ref1, _}, #state{cleanup_ref=Ref}=S) ->
    format_log(info, "REG_SRV(~p): wrong ref ~p, expected ~p~n", [self(), Ref1, Ref]),
    _ = erlang:cancel_timer(Ref),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};

handle_info({'basic.consume_ok', _}, S) ->
    {noreply, S};

handle_info(_Info, State) ->
    format_log(info, "REG_SRV: unhandled info: ~p~n", [_Info]),
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
terminate(_Reason, #state{my_q=Q}) ->
    stop_amqp(Q),
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
-spec(start_amqp/0 :: () -> binary() | tuple(error, term())).
start_amqp() ->
    Q = amqp_util:new_queue(<<>>),

    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_SUCCESS),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_REG_QUERY),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_AUTH_REQ),

    amqp_util:basic_consume(Q),
    Q.

-spec(stop_amqp/1 :: (Q :: binary()) -> no_return()).
stop_amqp(Q) ->
    amqp_util:unbind_q_from_callmgr(Q).

-spec(handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> no_return()).
handle_req(<<"application/json">>, Payload, State) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "REG_SRV(~p): handle msg ~s~n", [self(), Payload]),
    process_req(get_msg_type(Prop), Prop, State).

-spec(get_msg_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary())).
get_msg_type(Prop) ->
    { props:get_value(<<"Event-Category">>, Prop), props:get_value(<<"Event-Name">>, Prop) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, State) ->
    AuthU = props:get_value(<<"Auth-User">>, Prop),
    AuthR = props:get_value(<<"Auth-Domain">>, Prop),

    Direction = <<"outbound">>, %% if we're authing, it's an outbound call or a registration; inbound from carriers is ACLed auth

    {ok, AuthProp} = lookup_auth_user(AuthU, AuthR), %% crashes if not found, no return necessary

    Defaults = [{<<"Msg-ID">>, props:get_value(<<"Msg-ID">>, Prop)}
		,{<<"Custom-Channel-Vars">>, {struct, [
						       {<<"Direction">>, Direction}
						       ,{<<"Username">>, AuthU}
						       ,{<<"Realm">>, AuthR}
						      ]
					     }}
		| whistle_api:default_headers(State#state.my_q % serverID is not important, though we may want to define it eventually
					      ,props:get_value(<<"Event-Category">>, Prop)
					      ,<<"auth_resp">>
					      ,?APP_NAME
					      ,?APP_VERSION)],
    {ok, JSON} = auth_response(AuthProp, Defaults),
    RespQ = props:get_value(<<"Server-ID">>, Prop),
    send_resp(JSON, RespQ);
process_req({<<"directory">>, <<"reg_success">>}, Prop, _State) ->
    true = whistle_api:reg_success_v(Prop),

    [User, AfterAt] = binary:split(props:get_value(<<"Contact">>, Prop), <<"@">>), % only one @ allowed

    AfterUnquoted = whistle_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),
    MochiDoc = {struct, [{<<"Reg-Server-Timestamp">>, whistle_util:current_tstamp()}
			 ,{<<"Contact">>, Contact1}
			 | lists:keydelete(<<"Contact">>, 1, Prop)]
	       },

    {ok, _} = couch_mgr:save_doc(?REG_DB, MochiDoc);
process_req({<<"directory">>, <<"reg_query">>}, Prop, State) ->
    true = whistle_api:reg_query_v(Prop),

    Domain = props:get_value(<<"Realm">>, Prop),
    User = props:get_value(<<"Username">>, Prop),

    case couch_mgr:get_results("registrations"
			       ,{"registrations", "newest"}
			       ,[{<<"key">>, [Domain, User]}
				 ,{<<"group">>, <<"true">>}
				]) of
	{ok, []} -> format_log(info, "REG_SRV: no req_query_resp for ~s@~s~n", [User, Domain]);
	{ok, [{struct, ViewRes} | _]} ->
	    {struct, Value} = props:get_value(<<"value">>, ViewRes),
	    DocId = props:get_value(<<"id">>, Value),
	    {ok, {struct, RegDoc}} = couch_mgr:open_doc(?REG_DB, DocId),

	    RespFields = case props:get_value(<<"Fields">>, Prop) of
			     [] ->
				 lists:keydelete(<<"_rev">>, 1, lists:keydelete(<<"_id">>, 1, RegDoc));
			     Fields ->
				 lists:foldl(fun(F, Acc) ->
						     [ {F, props:get_value(F, RegDoc)} | Acc]
					     end, [], Fields)
			 end,

	    {ok, JSON} = whistle_api:reg_query_resp([ {<<"Fields">>, {struct, RespFields}}
						      | whistle_api:default_headers(State#state.my_q
										    ,<<"directory">>
											,<<"reg_query_resp">>
											,?APP_NAME
										    ,?APP_VERSION)
						    ]),

	    RespServer = props:get_value(<<"Server-ID">>, Prop),
	    amqp_util:targeted_publish(RespServer, JSON, <<"application/json">>)
    end,
    ok;
process_req(_,_,_) ->
    not_handled.

cleanup_registrations() ->
    %% get all documents with one or more tstamps < Now
    {ok, Expired} = couch_mgr:get_results("registrations", {"registrations", "expirations"}, [{<<"endkey">>, whistle_util:current_tstamp()}]),
    lists:foreach(fun({struct, Doc}) ->
			  {ok, D} = couch_mgr:open_doc(?REG_DB, props:get_value(<<"id">>, Doc)),
			  couch_mgr:del_doc(?REG_DB, D)
		  end, Expired).

-spec(lookup_auth_user/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_auth_user(Name, Realm) ->
    logger:format_log(info, "REG.auth: Looking up ~p @ ~p~n", [Name, Realm]),
    case couch_mgr:get_results(?AUTH_DB, <<"credentials/lookup">>, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, "No user/realm found"};
	{ok, [{struct, User}|_]} ->
	    {ok, props:get_value(<<"value">>, User)}
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
    logger:format_log(info, "AuthInfo: ~p~n", [AuthInfo]),
    Method = list_to_binary(string:to_lower(binary_to_list(whapps_json:get_value(<<"method">>, AuthInfo, <<"password">>)))),
    [{<<"Auth-Password">>, whapps_json:get_value(<<"password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, whapps_json:get_value(<<"Access-Group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, whapps_json:get_value(<<"Tenant-ID">>, AuthInfo, <<"ignore">>)}
    ].

send_resp(Payload, RespQ) ->
    format_log(info, "REG_SERVE(~p): Paylowd to ~s: ~s~n", [self(), RespQ, Payload]),
    amqp_util:targeted_publish(RespQ, Payload, <<"application/json">>).
