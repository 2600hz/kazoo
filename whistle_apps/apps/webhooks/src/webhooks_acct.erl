%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for an account with web hooks enabled
%%%
%%% Supported webhooks:
%%%   authn_req -> allow web client to authn registerer
%%%   authz_req -> allow web client to authz caller
%%%   route_req -> allow web client to send call instructions
%%%   route_win -> if web client's route_resp won, bind to callevts
%%%   call_events -> bind to all call events concerning account
%%%
%%% Ideas:
%%%   Create per-session unique URL for web client to push commands
%%%   
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_acct).

-behaviour(gen_listener).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
	 ,handle_event/2, terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE). 

-record(state, {acctdb = 'undefined' :: 'undefined' | ne_binary()
               ,acctid = 'undefined' :: 'undefined' | ne_binary()
	       ,webhooks = [] :: orddict:orddict() % {bind_event, [webhookJObj,...]}
               ,realm = 'undefined' :: 'undefined' | ne_binary()
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
start_link(AcctID, Webhooks) ->
    gen_listener:start_link(?MODULE, [{responders, []}
				      ,{bindings, [{self, []}]}
				      ,{basic_qos, 1}
				     ], [AcctID, Webhooks]).

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
init([AcctDB, Webhooks]) ->
    AcctID = whapps_util:get_db_name(AcctDB, raw),
    put(callid, AcctID),

    gen_listener:cast(self(), start_bindings),

    {ok, AcctDoc} = couch_mgr:open_doc(AcctDB, AcctID),
    Realm = wh_json:get_value(<<"realm">>, AcctDoc),

    ?LOG("Starting webhooks listener for ~s (~s)", [AcctID, Realm]),

    {ok, #state{acctdb=AcctDB
		,acctid=AcctID
		,realm=Realm
		,webhooks=lists:foldl(fun(Hook, Ord) ->
					      orddict:update(wh_json:get_value(<<"bind_event">>, Hook)
							     ,fun(Old) -> [Hook | Old] end
							     ,[Hook], Ord)
				      end, orddict:new(), Webhooks)
	       }}.

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
handle_cast(start_bindings, #state{webhooks=Ord}=State) ->
    Self = self(),
    [spawn(fun() -> maybe_add_binding(Self, Key) end) || Key <- orddict:fetch_keys(Ord)],
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
handle_info(_Info, State) ->
    {noreply, State}.

handle_event(_JObj, #state{webhooks=Webhooks,realm=Realm, acctid=AcctId}) ->
    {reply, [{hooks, Webhooks}, {realm, Realm}, {acctid, AcctId}]}.

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
-spec maybe_add_binding/2 :: (pid(), ne_binary()) -> 'ok'.
maybe_add_binding(Srv, Key) ->
    HookModBin = list_to_binary([<<"hook_">>, Key]),
    try
	HookMod = wh_util:to_atom(HookModBin),
	HookMod:add_binding(Srv),
	?LOG("Hook ~s bound queue", [HookMod])
    catch
	error:badarg ->
	    ?LOG_SYS("hook module ~s not found", [HookModBin]),
	    case code:where_is_file(wh_util:to_list(<<HookModBin/binary, ".beam">>)) of
		non_existing ->
		    ?LOG_SYS("beam file not found for ~s", [HookModBin]),
		    throw({invalid_hook, Key});
		_Path ->
		    ?LOG_SYS("beam file found: ~s", [_Path]),
		    wh_util:to_atom(HookModBin, true), %% put atom into atom table
		    maybe_add_binding(Srv, Key) % try again
	    end;
	error:undef ->
	    ?LOG_SYS("Hook ~s doesn't exist or add_binding/1 isn't exported", [HookModBin]),
	    throw({invalid_hook, Key});
        E:R ->
	    ?LOG("Unexpected error: ~p:~p", [E, R]),
            io:format("~p ~p~n", [E, R]),
	    ok
    end.
