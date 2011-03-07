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
-define(APP_VSN, <<"0.4.2">>).
-define(CLEANUP_RATE, 60000).

-record(state, {
	  amqp_host = "localhost" :: string()
	  ,is_amqp_up = true :: boolean()
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
    H = whapps_controller:get_amqp_host(),
    Q = start_amqp(H),
    format_log(info, "REG_SRV(~p): Q: ~s on H: ~s~n", [self(), Q, H]),

    Ref = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    format_log(info, "REG_SRV(~p): Starting timer for ~p msec: ~p~n", [self(), ?CLEANUP_RATE, Ref]),

    {noreply, State#state{cleanup_ref=Ref, amqp_host=H, my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info(Req, #state{my_q={error, _}}=State) ->
    H = whapps_controller:get_amqp_host(),
    Q = start_amqp(H),
    format_log(info, "REG_SRV(~p): restarting amqp with H: ~s; will retry in a bit if failed~n", [self(), H]),
    handle_info(Req, State#state{amqp_host=H, my_q=Q, is_amqp_up=is_binary(Q)});

handle_info({timeout, _, _}, #state{is_amqp_up=false}=S) ->
    handle_info(timeout, S);

handle_info({amqp_host_down, H}, S) ->
    format_log(info, "REG_SRV(~p): amqp host ~s went down, waiting a bit then trying again~n", [self(), H]),
    AHost = whapps_controller:get_amqp_host(),
    Q = start_amqp(AHost),
    {noreply, S#state{amqp_host=AHost, my_q=Q, is_amqp_up=is_binary(Q)}, 1000};

handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref}=S) ->
    format_log(info, "REG_SRV(~p): Time to clean old registrations~n", [self()]),
    spawn(fun() -> cleanup_registrations() end),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};

handle_info({timeout, Ref1, _}, #state{cleanup_ref=Ref}=S) ->
    format_log(info, "REG_SRV(~p): wrong ref ~p, expected ~p~n", [self(), Ref1, Ref]),
    erlang:cancel_timer(Ref),
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
terminate(_Reason, #state{amqp_host=Host, my_q=Q}) ->
    stop_amqp(Host, Q),
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
-spec(start_amqp/1 :: (Host :: string()) -> binary() | tuple(error, term())).
start_amqp(Host) ->
    Q = amqp_util:new_queue(Host, <<>>),
    amqp_util:bind_q_to_broadcast(Host, Q),
    amqp_util:basic_consume(Host, Q),
    Q.

-spec(stop_amqp/2 :: (Host :: string(), Q :: binary()) -> no_return()).
stop_amqp(Host, Q) ->
    amqp_util:unbind_q_from_broadcast(Host, Q),
    amqp_util:queue_delete(Host, Q).

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

    {ok, AuthProp} = lookup_auth_user(AuthU, AuthR),

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
					      ,?MODULE
					      ,?APP_VSN)],
    {ok, JSON} = auth_response(AuthProp, Defaults),
    RespQ = props:get_value(<<"Server-ID">>, Prop),
    send_resp(JSON, RespQ, State#state.amqp_host);
process_req({<<"directory">>, <<"reg_success">>}, Prop, _State) ->
    true = whistle_api:reg_success_v(Prop),

    [User, AfterAt] = binary:split(props:get_value(<<"Contact">>, Prop), <<"@">>), % only one @ allowed

    AfterUnquoted = whistle_util:to_binary(mochiweb_util:unquote(AfterAt)),
    Contact1 = binary:replace(<<User/binary, "@", AfterUnquoted/binary>>, [<<"<">>, <<">">>], <<>>, [global]),
    MochiDoc = {struct, [{<<"Reg-Server-Timestamp">>, current_tstamp()}
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

	    Fields = props:get_value(<<"Fields">>, Prop),
	    RespServer = props:get_value(<<"Server-ID">>, Prop),

	    RespFields = lists:foldl(fun(F, Acc) ->
					     [ {F, props:get_value(F, RegDoc)} | Acc]
				     end, [], Fields),
	    {ok, JSON} = whistle_api:reg_query_resp([ {<<"Fields">>, {struct, RespFields}}
						      | whistle_api:default_headers(State#state.my_q
										    ,<<"directory">>
											,<<"reg_query_resp">>
											,whistle_util:to_binary(?MODULE)
										    ,?APP_VSN)
						    ]),
	    amqp_util:targeted_publish(State#state.amqp_host, RespServer, JSON, <<"application/json">>)
    end,
    ok;
process_req(_,_,_) ->
    not_handled.

-spec(current_tstamp/0 :: () -> integer()).
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

cleanup_registrations() ->
    %% get all documents with one or more tstamps < Now
    {ok, Expired} = couch_mgr:get_results("registrations", {"registrations", "expirations"}, [{<<"endkey">>, current_tstamp()}]),
    lists:foreach(fun({struct, Doc}) ->
			  {ok, D} = couch_mgr:open_doc(?REG_DB, props:get_value(<<"id">>, Doc)),
			  couch_mgr:del_doc(?REG_DB, D)
		  end, Expired).

-spec(lookup_auth_user/2 :: (Name :: binary(), Realm :: binary()) -> tuple(ok, proplist()) | tuple(error, string())).
lookup_auth_user(Name, Realm) ->
    case couch_mgr:get_results(?AUTH_DB, ?AUTH_VIEW_USERAUTHREALM, [{<<"key">>, [Realm, Name]}]) of
	{error, _}=E -> E;
	{ok, []} -> {error, "No user/realm found"};
	{ok, [{struct, User}|_]} ->
	    {struct, Auth} = props:get_value(<<"value">>, User),
	    {ok, Auth}
    end.

-spec(auth_response/2 :: (AuthInfo :: proplist() | integer(), Prop :: proplist()) -> tuple(ok, iolist()) | tuple(error, string())).
auth_response([], Prop) ->
    Data = lists:umerge(auth_specific_response(403), Prop),
    whistle_api:auth_resp(Data);
auth_response(AuthInfo, Prop) ->
    Data = lists:umerge(auth_specific_response(AuthInfo), Prop),
    whistle_api:auth_resp(Data).

-spec(auth_specific_response/1 :: (AuthInfo :: proplist() | integer()) -> proplist()).
auth_specific_response(AuthInfo) when is_list(AuthInfo) ->
    Method = list_to_binary(string:to_lower(binary_to_list(props:get_value(<<"auth_method">>, AuthInfo)))),
    [{<<"Auth-Password">>, props:get_value(<<"auth_password">>, AuthInfo)}
     ,{<<"Auth-Method">>, Method}
     ,{<<"Event-Name">>, <<"auth_resp">>}
     ,{<<"Access-Group">>, props:get_value(<<"Access-Group">>, AuthInfo, <<"ignore">>)}
     ,{<<"Tenant-ID">>, props:get_value(<<"Tenant-ID">>, AuthInfo, <<"ignore">>)}
    ];
auth_specific_response(403) ->
    [{<<"Auth-Method">>, <<"error">>}
     ,{<<"Auth-Password">>, <<"403 Forbidden">>}
     ,{<<"Access-Group">>, <<"ignore">>}
     ,{<<"Tenant-ID">>, <<"ignore">>}].

send_resp(JSON, RespQ, Host) ->
    format_log(info, "TS_RESPONDER(~p): JSON to ~s: ~s~n", [self(), RespQ, JSON]),
    amqp_util:targeted_publish(Host, RespQ, JSON, <<"application/json">>).
