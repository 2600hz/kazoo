%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 18 Mar 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(change_handler).

-behaviour(gen_changes).

%% API
-export([start_link/2, stop/1, add_listener/2, add_listener/3, rm_listener/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_change/2
	 ,terminate/2, code_change/3]).

-include_lib("couchbeam/include/couchbeam.hrl").

-define(SERVER, ?MODULE). 

-record(listener, {
	  pid = undefined :: undefined | pid()
          ,monitor_ref = undefined :: undefined | reference()
	  ,doc = <<>> :: binary()
	 }).
-record(state, {
	  listeners = [] :: list(#listener{})
	  ,db = #db{}
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
start_link(Db, Options) ->
    gen_changes:start_link(?MODULE, Db, [ {heartbeat, 5000} | Options], [Db]).

stop(Srv) ->
    gen_changes:stop(Srv).

add_listener(Srv, Pid) ->
    add_listener(Srv, Pid, <<>>).

add_listener(Srv, Pid, Doc) ->
    gen_changes:cast(Srv, {add_listener, Pid, Doc}).

rm_listener(Srv, Pid, Doc) ->
    gen_changes:cast(Srv, {rm_listener, Pid, Doc}).

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
init([Db]) ->
    {ok, #state{db=Db}}.

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
handle_cast({add_listener, Pid, Doc}, #state{listeners=Ls}=State) ->
    case lists:any(fun(#listener{pid=Pid1, doc=Doc1}) when Pid =:= Pid1 andalso Doc =:= Doc1 -> true;
		      (_) -> false end, Ls) of
	true -> {noreply, State};
	false ->
	    Ref = erlang:monitor(process, Pid),
	    {noreply, State#state{listeners=[#listener{pid=Pid,doc=Doc,monitor_ref=Ref} | Ls]}}
    end;
handle_cast({rm_listener, Pid, Doc}, #state{listeners=Ls}=State) ->
    Ls1 = [ V || V <- Ls, rm_listener_(V, Doc, Pid)],
    {noreply, State#state{listeners=Ls1}}.

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
handle_info({'DOWN', _Ref, process, Pid, Info}, #state{listeners=Ls}=State) ->
    logger:format_log(info, "CH(~p): DOWN for ~p(~p)~n", [self(), Pid, Info]),
    Ls1 = [ V || V <- Ls, rm_listener_(V, Pid)],
    {noreply, State#state{listeners=Ls1}};
handle_info(_Info, State) ->
    logger:format_log(info, "CH(~p): Unhandled info ~p~n", [self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle change notifications from CouchDB
%%
%% @spec handle_change(ChangeRow, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_change(_, #state{listeners=[]}=State) ->
    {noreply, State};
handle_change({struct, Change}, #state{listeners=Ls}=State) ->
    spawn(fun() ->
		  DocID = props:get_value(<<"id">>, Change),
		  Send = case props:get_value(<<"deleted">>, Change) of
			     undefined ->
				 {document_changes, DocID, [ C || {struct, C} <- props:get_value(<<"changes">>, Change)]};
			     true ->
				 {document_deleted, DocID}
			 end,

		  lists:foreach(fun(#listener{pid=Pid, doc=DocID1}) when DocID =:= DocID1 ->
					Pid ! Send;
				   (#listener{pid=Pid, doc = <<>>}) ->
					Pid ! Send;
				   (_) -> ok
				end, Ls)
	  end),
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
terminate(_Reason, #state{listeners=Ls, db=Db}) ->
    logger:format_log(error, "CH(~p): Going down, down, down for ~p(~p)~n", [self(), Db#db.name, _Reason]),
    lists:foreach(fun(#listener{pid=Pid, monitor_ref=Ref, doc=Doc}) ->
			  Pid ! {change_handler_terminating, Db#db.name, Doc},
			  erlang:demonitor(Ref, [flush])
		  end, Ls),
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
rm_listener_(#listener{pid=Pid, monitor_ref=Ref}, Pid) ->
    erlang:demonitor(Ref, [flush]),
    false;
rm_listener_(_, _) -> true.

rm_listener_(#listener{pid=Pid, doc=Doc, monitor_ref=Ref}, Doc, Pid) ->
    erlang:demonitor(Ref, [flush]),
    false;
rm_listener_(_, _, _) -> true.
