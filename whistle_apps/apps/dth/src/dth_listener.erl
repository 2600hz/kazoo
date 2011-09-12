%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Listener for authn_req, reg_success, and reg_query AMQP requests
%%% @end
%%% Created : 13 Jan 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, stop/1]).

%% gen_listener callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("dth.hrl").

-define(RESPONDERS, [
		     {cdr_handler, [{<<"call_detail">>, <<"cdr">>}]}
		     ,{blacklist_req, [{<<"dth">>, <<"blacklist_req">>}]}
		    ]).
-define(BINDINGS, [
		   {cdrs, [{callid, <<"*">>}]}
		   ,{dth, []}
		  ]).

-define(SERVER, ?MODULE).
-define(BLACKLIST_REFRESH, 60000).

-record(state, {
	  wsdl_model = undefined :: 'undefined' | term()
	 ,cache = undefined :: 'undefined' | pid()
	 ,dth_cdr_url = <<>> :: binary()
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
    gen_listener:start_link(?MODULE, [{responders, ?RESPONDERS}
				      ,{bindings, ?BINDINGS}
				     ], []).
-spec stop/1 :: (Srv) -> ok when
      Srv :: atom() | pid().
stop(Srv) ->
    gen_listener:stop(Srv).

%%%===================================================================
%%% gen_listener callbacks
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
    {ok, Configs} = file:consult([code:priv_dir(dth), "/startup.config"]),
    URL = props:get_value(dth_cdr_url, Configs),

    erlang:send_after(0, self(), blacklist_refresh),
    
    WSDLFile = [code:priv_dir(dth), "/dthsoap.wsdl"],
    WSDLHrlFile = [code:lib_dir(dth, include), "/dthsoap.hrl"],

    true = filelib:is_regular(WSDLFile),

    case filelib:is_regular(WSDLHrlFile) of
        true -> ok;
	false ->
	    true = filelib:is_regular(WSDLFile),
	    ok = detergent:write_hrl(WSDLFile, WSDLHrlFile)
    end,

    Cache = whereis(dth_cache),
    erlang:monitor(process, Cache),

    {ok, #state{wsdl_model=detergent:initModel(WSDLFile)
		,dth_cdr_url=URL
		,cache=Cache
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
handle_call(_Req, _From, State) ->
    {noreply, State}.

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
handle_info(blacklist_refresh, #state{wsdl_model=WSDL, cache=Cache}=State) ->
    erlang:send_after(?BLACKLIST_REFRESH, self(), blacklist_refresh),
    spawn(fun() -> refresh_blacklist(Cache, WSDL) end),
    {noreply, State};
handle_info({'DOWN', _, process, Pid, Reason}, #state{cache=Pid}=State) ->
    ?LOG_SYS("Cache ~p went down: ~p", [Pid, Reason]),
    {noreply, State#state{cache=undefined}, 0};
handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling AMQP event objects
%%
%% @spec handle_event(JObj, State) -> {reply, Props}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, #state{dth_cdr_url=Url, cache=Cache, wsdl_model=WSDL}) ->
    {reply, [{cdr_url, Url}, {cache, Cache}, {wsdl, WSDL}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_listener when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_listener terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate/2 :: (term(), #state{}) -> ok.
terminate(_Reason, _) ->
    ?LOG_SYS("dth: ~p termination", [_Reason]).

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
-spec refresh_blacklist/2 :: (Cache, WSDL) -> ok when
      Cache :: pid(),
      WSDL :: #wsdl{}.
refresh_blacklist(Cache, WSDL) ->
    {ok, _, [Response]} = detergent:call(WSDL, "GetBlockList", []),
    BlockListEntries = get_blocklist_entries(Response),
    ?LOG_SYS("Entries: ~p", [BlockListEntries]),
    wh_cache:store_local(Cache, dth_util:blacklist_cache_key(), BlockListEntries).

-spec get_blocklist_entries/1 :: (Response) -> [{binary(),binary()},...] | [] when
      Response :: #'p:GetBlockListResponse'{}.
get_blocklist_entries(#'p:GetBlockListResponse'{
			 'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
			   'BlockListEntry'=undefined
			  }}) ->
    {struct, []};
get_blocklist_entries(#'p:GetBlockListResponse'{
			 'GetBlockListResult'=#'p:ArrayOfBlockListEntry'{
			   'BlockListEntry'=Entries
			  }}) when is_list(Entries) ->
    %% do some formatting of the entries to be [{ID, Reason}]
    {struct, [{wh_util:to_binary(ID), wh_util:to_binary(Reason)} || #'p:BlockListEntry'{'CustomerID'=ID, 'BlockReason'=Reason} <- Entries]}.
