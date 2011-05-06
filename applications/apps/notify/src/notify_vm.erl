%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Handle updating devices and emails about voicemails
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_vm).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("notify.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

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
handle_info(timeout, _) ->
    Boxes = find_all_vmboxes(),
    [ couch_mgr:add_change_handler(Db, Box) || {Db, Box, _} <- Boxes ],
    {noreply, Boxes};

handle_info({document_changes, DocID, Changes}, Boxes) ->
    spawn(fun() ->
		  logger:format_log(info, "VM_NOTIFY(~p): change on ~p: ~p~n", [self(), DocID, Changes]),
		  {Db, _, Rev} = lists:keyfind(DocID, 2, Boxes),
		  case Changes of
		      [[{<<"rev">>, NewRev}]] when NewRev > Rev ->
			  {ok, Doc} = couch_mgr:open_doc(Db, DocID);
		      _ -> ok
		  end
	  end),
    {noreply, Boxes};

handle_info({document_deleted, DocID}, Boxes) ->
    logger:format_log(info, "VM_NOTIFY(~p): ~p was deleted~n", [self(), DocID]),
    {noreply, Boxes};

handle_info({change_handler_terminating, Db, Doc}, Boxes) ->
    logger:format_log(info, "VM_NOTIFY(~p): change handler down on ~p: ~p~n", [self(), Db, Doc]),
    couch_mgr:add_change_handler(Db, Doc),
    {noreply, Boxes};

handle_info(_Info, Boxes) ->
    {noreply, Boxes}.

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
terminate(_Reason, _State) ->
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

%% Return a list of {account DB, vmbox ID, vmbox REV} tuples
-spec(find_all_vmboxes/0 :: () -> list(tuple(binary(), binary(), binary()))).
find_all_vmboxes() ->
    {ok, AcctView} = couch_mgr:get_results(<<"accounts">>, <<"accounts/listing_by_id">>, []),
    AcctDBs = [ whapps_util:get_db_name(wh_json:get_value([<<"value">>, <<"id">>], Acct), encoded) || Acct <- AcctView],
    lists:foldr(fun(AcctDB, Acc0) ->
			try
			{ok, VMBoxView} = couch_mgr:get_results(AcctDB, <<"vmboxes/listing_by_id">>, []),
			lists:foldr(fun(VMB, Acc1) ->
					    Id = wh_json:get_value([<<"value">>, <<"id">>], VMB),
					    {ok, Rev} = couch_mgr:lookup_doc_rev(AcctDB, Id),
					    [ {AcctDB, Id, Rev} | Acc1 ]
				    end, Acc0, VMBoxView)
			catch A:B -> logger:format_log(info, "outer fold ~p:~p~n~p~n", [A, B, erlang:get_stacktrace()]) end
		end, [], AcctDBs).
