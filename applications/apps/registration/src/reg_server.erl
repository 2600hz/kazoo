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

-include("../include/amqp_client/include/amqp_client.hrl").
-include("reg.hrl").
-include("../../src/whistle_types.hrl").

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(APP_VSN, <<"0.4.2">>).
-define(CLEANUP_RATE, 60000).

-record(state, {
	  amqp_host = "localhost" :: string()
	  ,my_q = <<>> :: binary()
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
handle_info(_, #state{my_q={error, _}}=State) ->
    H = whapps_controller:get_amqp_host(),
    Q = start_amqp(H),
    format_log(info, "REG_SRV(~p): restarting amqp with H: ~s; will retry in a bit if failed~n", [self(), H]),
    {noreply, State#state{amqp_host=H, my_q=Q}, 5000};
handle_info(timeout, State) ->
    H = whapps_controller:get_amqp_host(),
    Q = start_amqp(H),
    format_log(info, "REG_SRV(~p): Q: ~s on H: ~s~n", [self(), Q, H]),

    Ref = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    format_log(info, "REG_SRV(~p): Starting timer for ~p msec: ~p~n", [self(), ?CLEANUP_RATE, Ref]),
    {noreply, State#state{cleanup_ref=Ref, amqp_host=H, my_q=Q}};
handle_info({amqp_host_down, H}, S) ->
    format_log(info, "REG_SRV(~p): amqp host ~s went down, waiting a bit then trying again~n", [self(), H]),
    AHost = whapps_controller:get_amqp_host(),
    Q = start_amqp(AHost),
    {noreply, S#state{amqp_host=AHost, my_q=Q}, 1000};
handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref}=S) ->
    format_log(info, "REG_SRV(~p): Time to clean old registrations~n", [self()]),
    spawn(fun() -> cleanup_registrations() end),
    NewRef = erlang:start_timer(?CLEANUP_RATE, ?SERVER, ok), % clean out every 60 seconds
    {noreply, S#state{cleanup_ref=NewRef}};
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};
handle_info(_Info, State) ->
    format_log(info, "REG_SRV: unhandled info: ~p~n", [_Info]),
    {noreply, State}.

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
    amqp_util:delete_queue(Host, Q).

-spec(handle_req/3 :: (ContentType :: binary(), Payload :: binary(), State :: #state{}) -> no_return()).
handle_req(<<"application/json">>, Payload, State) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
    format_log(info, "REG_SRV(~p): Recv JSON~nPayload: ~p~n", [self(), Prop]),
    process_req(get_msg_type(Prop), Prop, State).

-spec(get_msg_type/1 :: (Prop :: proplist()) -> tuple(binary(), binary())).
get_msg_type(Prop) ->
    { props:get_value(<<"Event-Category">>, Prop), props:get_value(<<"Event-Name">>, Prop) }.

-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), Prop :: proplist(), State :: #state{}) -> no_return()).
process_req({<<"directory">>, <<"auth_req">>}, Prop, _State) ->
    format_log(info, "REG_SRV: Lookup auth creds here for~n~p~n", [Prop]),
    ok;
process_req({<<"directory">>, <<"reg_success">>}, Prop, _State) ->
    format_log(info, "REG_SRV: Reg success~n~p~n", [Prop]),
    true = whistle_api:reg_success_v(Prop),

    Contact = props:get_value(<<"Contact">>, Prop),
    Unquoted = whistle_util:to_binary(mochiweb_util:unquote(Contact)),
    Contact1 = binary:replace(Unquoted, [<<"<">>, <<">">>], <<>>, [global]),
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
	    format_log(info, "REG_SRV: req_query_resp: ~s~n", [JSON]),
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
