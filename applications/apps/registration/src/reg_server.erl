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

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(APP_VSN, "0.3.0").

-compile({no_auto_import,[now/0]}). %% we define our own now/0 function

-type proplist() :: list(tuple(binary(), binary())) | [].

-record(state, {
	  amqp_host = "localhost" :: string()
	  ,my_q = undefined :: undefined | binary()
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
    H = net_adm:localhost(),
    Q = start_amqp(H),
    {ok, #state{amqp_host=H, my_q = Q}, 0}.

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
    Ref = erlang:start_timer(60000, ?SERVER, ok), % clean out every 60 seconds
    {noreply, State#state{cleanup_ref=Ref}};
handle_info({timeout, Ref, _}, #state{cleanup_ref=Ref}=S) ->
    spawn(fun() -> cleanup_registrations() end),
    {noreply, S};
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_req(Props#'P_basic'.content_type, Payload, State) end),
    {noreply, State};
handle_info(_Info, State) ->
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
-spec(start_amqp/1 :: (Host :: string()) -> binary()).
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

    Domain = props:get_value(<<"Realm">>, Prop),
    User = props:get_value(<<"Username">>, Prop),

    Id = <<User/binary, "@", Domain/binary>>,

    Doc = case couch_mgr:open_doc(?REG_DB, Id) of
	      {error, not_found} -> [{<<"_id">>, Id}, {<<"registrations">>, []}];
	      D when is_list(D) -> D
	  end,
    Regs = props:get_value(<<"registrations">>, Doc, []),

    Doc1 = [ {<<"registrations">>, [{struct, [ {<<"Reg-Server-Timestamp">>, current_tstamp()}
					       | Prop]}
				    | Regs]}
	     | lists:keydelete(<<"registrations">>, 1, Doc)],
    {ok, _} = couch_mgr:save_doc(?REG_DB, Doc1);
process_req({<<"directory">>, <<"reg_query">>}, Prop, State) ->
    format_log(info, "REG_SRV: Reg query~n~p~n", [Prop]),
    true = whistle_api:reg_query_v(Prop),

    Domain = props:get_value(<<"Host">>, Prop),
    User = props:get_value(<<"Username">>, Prop),
    Now = current_tstamp(),

    case couch_mgr:get_results("registrations"
			       ,{"registrations", "get_contact"}
			       ,[{<<"startkey">>, [Domain, User, Now]}]
			      ) of
	[] -> ok;
	[{ViewRes} | _] ->
	    Fields = props:get_value(<<"Fields">>, Prop),
	    RespServer = props:get_value(<<"Server-ID">>, Prop),

	    {RegDoc} = props:get_value(<<"value">>, ViewRes),

	    RespFields = lists:foldl(fun(F, Acc) ->
					     case props:get_value(F, RegDoc) of
						 undefined -> Acc;
						 V -> [ {F, V} | Acc]
					     end
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
    Now = current_tstamp(),
    %% get all documents with one or more tstamps < Now
    case couch_mgr:get_results("registrations", {"registrations", "expirations"}, [{<<"group">>, <<"true">>}]) of
	[] -> ok;
	[_|_]=Docs ->
	    lists:foreach(fun({[{<<"key">>, DocId}, {<<"value">>, Tstamp}]}) when Tstamp < Now ->
				  spawn(fun() -> remove_old_registrations(DocId, Now) end);
			     (_) -> ok
			  end, Docs)
    end.

remove_old_registrations(DocId, Now) ->
    [_] = Doc = couch_mgr:open_doc(DocId),
    Registrations = props:get_value(<<"registrations">>, Doc, []),
    CurrentRegs = lists:filter(fun({Reg}) ->
				       Expires = whistle_util:to_integer(props:get_value(<<"Reg-Server-Timestamp">>, Reg))
					   + whistle_util:to_integer(props:get_value(<<"Expires">>, Reg)),
				       Expires > Now
			       end, Registrations),
    Doc1 = [ {<<"registrations">>, CurrentRegs} | lists:keydelete(<<"registrations">>, 1, Doc)],
    case couch_mgr:save_doc(Doc1) of
	{error, conflict} -> remove_old_registrations(DocId, Now); % try again
	_ -> ok
    end.
