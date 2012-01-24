%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Listener for a account's webhook(s).
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hook_acct_listener).

-behaviour(gen_listener).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
         ,handle_event/2, terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE). 

-record(state, {acct_db = 'undefined' :: 'undefined' | ne_binary()
               ,acct_id = 'undefined' :: 'undefined' | ne_binary()
               ,webhook = 'undefined' :: 'undefined' | wh_json:json_object()
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
start_link(AcctDB, Webhooks) ->
    gen_listener:start_link(?MODULE, [ % calls immediately to the supervisor to start child
                                      {responders, [{hook_req_sup, {<<"*">>, <<"*">>}}]}
                                      ,{bindings, [{self, []}]}
                                     ], [AcctDB, Webhooks]).

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
init([AcctDB, Webhook]) ->
    AcctID = whapps_util:get_db_name(AcctDB, raw),
    put(callid, AcctID),

    {ok, AcctDoc} = couch_mgr:open_doc(AcctDB, AcctID),
    Realm = wh_json:get_value(<<"realm">>, AcctDoc),

    BindOptions = case wh_json:get_value(<<"bind_options">>, Webhook, []) of
                      Prop when is_list(Prop) -> Prop;
                      JObj -> wh_json:to_proplist(JObj) % maybe [{restrict_to, [call, events]},...] or other json-y type
                  end,
    
    gen_listener:add_binding(self()
                             ,wh_json:get_value(<<"bind_event">>, Webhook)
                             ,[{realm, Realm}, {acct_id, AcctID} | BindOptions]
                            ),

    ?LOG("Starting webhook listener for ~s (~s)", [AcctID, Realm]),

    {ok, #state{acct_db=AcctDB
                ,acct_id=AcctID
                ,realm=Realm
                ,webhook=Webhook
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
handle_call(get_callback_uri, _From, #state{webhook=Hook}=State) ->
    {reply, wh_json:get_value(<<"callback_uri">>, Hook), State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

handle_event(_JObj, #state{webhook=Webhook, realm=Realm, acct_id=AcctId}) ->
    {reply, [{hook, Webhook}, {realm, Realm}, {acct_id, AcctId}]}.

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
