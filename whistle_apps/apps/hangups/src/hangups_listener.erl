%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for Hangup call events and record them to the database
%%% @end
%%% Created : 23 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hangups_listener).

-behaviour(gen_server).

%% API
-export([start_link/0, set_amqp_host/1, set_couch_host/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(HANGUP_DB, "hangups").

-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_amqp.hrl").

-import(logger, [format_log/3]).
-import(props, [get_value/2, get_value/3]).

-record(state, {
	  couch_host = "" :: string()
	  ,amqp_host = "" :: string()
	  ,q = <<>> :: binary()
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

set_amqp_host(H) ->
    gen_server:cast(?MODULE, {set_amqp_host, H}).

set_couch_host(H) ->
    gen_server:cast(?MODULE, {set_couch_host, H}).

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
    {ok, #state{}}.

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
handle_cast({set_amqp_host, H}, #state{q = <<>>}=State) ->
    try
	Q = start_amqp(H),
	{noreply, State#state{amqp_host=H, q=Q}}
    catch
	_:_ -> {noreply, State}
    end;
handle_cast({set_amqp_host, H}, #state{amqp_host=OldH, q=OldQ}=State) ->
    try
	Q = start_amqp(H),
	amqp_util:delete_queue(OldH, OldQ),
	{noreply, State#state{amqp_host=H, q=Q}}
    catch
	_:_ -> {noreply, State}
    end;
handle_cast({set_couch_host, H}, #state{}=State) ->
    try
	ok = couch_mgr:set_host(H),
	{noreply, State#state{couch_host=H}}
    catch
	_:_ -> {noreply, State}
    end;
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
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, #state{}=State) ->
    spawn(fun() -> handle_hangup(Props#'P_basic'.content_type, Payload) end),
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
terminate(_Reason, #state{q=Q}) ->
    amqp_util:delete_queue(Q),
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
    amqp_util:callevt_exchange(Host),
    Q = amqp_util:new_callevt_queue(Host, <<>>),
    amqp_util:bind_q_to_callevt(Host, Q, <<"*">>, cdr), % bind to all CDR events
    amqp_util:bind_q_to_callevt(Host, Q, <<"*">>, events), % bind to all Call events
    amqp_util:basic_consume(Host, Q),
    Q.

handle_hangup(<<"application/json">>, JSON) ->
    {struct, Prop} = mochijson2:decode(binary_to_list(JSON)),
    case get_value(<<"Hangup-Cause">>, Prop) of
	undefined -> ok;
	<<"ORIGINATOR_CANCEL">> -> ok;
	<<"NORMAL_CLEARING">> -> ok;
	_ErrCause ->
	    {ok, _} = couch_mgr:save_doc(?HANGUP_DB, create_error_doc(Prop))
    end.

%% Call-ID, Hangup-Cause, Timestamp, Call-Direction
create_error_doc(Prop) ->
    Fields = [<<"Call-ID">>, <<"Hangup-Cause">>, <<"Timestamp">>, <<"Call-Direction">>],
    lists:foldl(fun(F, Acc) -> [{F, get_value(F, Prop)} | Acc] end, [{<<"_id">>, get_value(<<"Call-ID">>, Prop)}], Fields).
