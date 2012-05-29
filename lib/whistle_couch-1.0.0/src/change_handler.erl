%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% One change handler with BigCouch, stream changes to interested PIDs
%%% via Erlang messaging
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(change_handler).

-behaviour(gen_changes).

%% API
-export([start_link/2, stop/1, add_listener/2, add_listener/3, rm_listener/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_change/2
         ,terminate/2, code_change/3]).

-include("wh_couch.hrl").

-define(SERVER, ?MODULE). 

-record(listener, {
          pid = 'undefined' :: 'undefined' | pid()
          ,monitor_ref = 'undefined' :: 'undefined' | reference()
          ,doc = <<>> :: binary()
         }).
-record(state, {
          listeners = [] :: [#listener{},...] | []
          ,db = <<>> :: binary()
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
start_link(#db{name=DbName}=Db, Options) ->
    gen_changes:start_link(?MODULE, Db, [ {heartbeat, 500} | Options], [DbName]).

stop(Srv) ->
    gen_changes:stop(Srv).

add_listener(Srv, Pid) ->
    add_listener(Srv, Pid, <<>>).

add_listener(Srv, Pid, Doc) ->
    lager:debug("adding listener ~p for doc ~s to CH ~p", [Pid, Doc, Srv]),
    gen_changes:cast(Srv, {add_listener, Pid, Doc}).

rm_listener(Srv, Pid, Doc) ->
    lager:debug("removing listener ~p for doc ~s to CH ~p", [Pid, Doc, Srv]),
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
init([DbName]) ->
    lager:debug("starting change handler for ~s", [DbName]),
    {ok, #state{db=DbName}}.

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
handle_cast({add_listener, Pid, Doc}, #state{listeners=Ls}=State) ->
    case lists:any(fun(#listener{pid=Pid1, doc=Doc1}) when Pid =:= Pid1 andalso Doc =:= Doc1 -> true;
                      (_) -> false end, Ls) of
        true -> {noreply, State};
        false ->
            Ref = erlang:monitor(process, Pid),
            {noreply, State#state{listeners=[#listener{pid=Pid,doc=Doc,monitor_ref=Ref} | Ls]}}
    end;
handle_cast({rm_listener, Pid, Doc}, #state{listeners=Ls}=State) ->
    Ls1 = [ V || V <- Ls, keep_listener(V, Doc, Pid)],
    {noreply, State#state{listeners=Ls1}};
handle_cast(_, State) ->
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
handle_info({'DOWN', _Ref, process, Pid, Info}, #state{listeners=Ls}=State) ->
    lager:debug("DOWN recv for ~p(~p)", [Pid, Info]),
    Ls1 = [ V || V <- Ls, keep_listener(V, Pid)],
    {noreply, State#state{listeners=Ls1}};
handle_info(_Info, State) ->
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
handle_change(JObj, #state{listeners=Ls}=State) ->
    Change = wh_json:to_proplist(JObj),
    spawn(fun() ->
                  DocID = props:get_value(<<"id">>, Change),
                  Send = case props:get_value(<<"deleted">>, Change) of
                             undefined ->
                                 {document_changes, DocID, [ wh_json:to_proplist(C) || C <- props:get_value(<<"changes">>, Change)]};
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
    {noreply, State, hibernate}.

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
terminate(_Reason, #state{listeners=Ls, db=DbName}) ->
    lager:debug("going down, down, down for ~p(~p)", [DbName, _Reason]),
    lists:foreach(fun(#listener{pid=Pid, monitor_ref=Ref, doc=Doc}) ->
                          Pid ! {change_handler_terminating, DbName, Doc},
                          erlang:demonitor(Ref, [flush])
                  end, Ls).

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
-spec keep_listener/2 :: (#listener{}, pid()) -> boolean().
keep_listener(#listener{pid=Pid, monitor_ref=Ref}, Pid) ->
    erlang:demonitor(Ref, [flush]),
    false;
keep_listener(_, _) -> true.

-spec keep_listener/3 :: (#listener{}, binary(), pid()) -> boolean().
keep_listener(#listener{pid=Pid, doc=Doc, monitor_ref=Ref}, Doc, Pid) ->
    erlang:demonitor(Ref, [flush]),
    false;
keep_listener(_, _, _) -> true.
