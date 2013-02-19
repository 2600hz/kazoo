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
-export([start_link/2, stop/1, update_config/2]).

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
start_link(AcctDB, Webhook) ->
    gen_listener:start_link(?MODULE, [{bindings, []}
                                      ,{responders, []}
                                     ], [AcctDB, Webhook]).

stop(Srv) ->
    gen_listener:cast(Srv, stop).

update_config(Pid, JObj) ->
    gen_listener:cast(Pid, {config_change, JObj}).

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
    AcctID = wh_util:format_account_id(AcctDB, raw),
    put(callid, AcctID),

    {ok, AcctDoc} = couch_mgr:open_doc(AcctDB, AcctID),
    Realm = wh_json:get_value(<<"realm">>, AcctDoc),

    BindOptions = case wh_json:get_value(<<"bind_options">>, Webhook, []) of
                      Prop when is_list(Prop) -> Prop;
                      JObj -> wh_json:to_proplist(JObj) % maybe [{restrict_to, [call, events]},...] or other json-y type
                  end,

    BindEvent = wh_json:get_atom_value(<<"bind_event">>, Webhook),
    setup_binding(BindEvent, BindOptions, Realm, AcctID),

    lager:debug("Starting webhook listener(~s) for ~s (~s)", [BindEvent, AcctID, Realm]),

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
handle_cast({config_change, ConfJObj}, #state{webhook=Hook, realm=Realm, acct_id=AcctID}=State) ->
    JObj = wh_json:get_value(<<"Doc">>, ConfJObj),

    Hook1 =  wh_json:set_values([
                                 {<<"callback_uri">>, wh_json:get_value(<<"callback_uri">>, JObj)}
                                 ,{<<"callback_method">>, wh_json:get_value(<<"callback_method">>, JObj)}
                                 ,{<<"retries">>, wh_json:get_value(<<"retries">>, JObj)}
                                ], Hook),

    case {wh_json:get_atom_value(<<"bind_event">>, Hook), wh_json:get_atom_value(<<"bind_event">>, JObj)} of
        {A, A} ->
            lager:debug("no big changes to be done"),
            {noreply, State#state{webhook=Hook1}};
        {Old, New} ->
            lager:debug("changing hook from ~s to ~s", [Old, New]),
            OldBindOptions = case wh_json:get_value(<<"bind_options">>, Hook, []) of
                                 Prop1 when is_list(Prop1) -> Prop1;
                                 JObj1 -> wh_json:to_proplist(JObj1) % maybe [{restrict_to, [call, events]},...] or other json-y type
                             end,
            tear_down_binding(Old, OldBindOptions, Realm, AcctID),

            NewBindOptions = case wh_json:get_value(<<"bind_options">>, JObj, []) of
                                 Prop2 when is_list(Prop2) -> Prop2;
                                 JObj2 -> wh_json:to_proplist(JObj2) % maybe [{restrict_to, [call, events]},...] or other json-y type
                             end,

            setup_binding(New, NewBindOptions, Realm, AcctID),
            {noreply, State#state{webhook=JObj}}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.

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
    lager:debug("unhandled message: ~p", [_Info]),
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
tear_down_binding(BindEvent, BindOptions, Realm, AcctID) ->
    gen_listener:rm_binding(self()
                            ,BindEvent
                            ,[{realm, Realm}, {acct_id, AcctID} | BindOptions]
                           ),
    EventType = webhooks_util:api_call(BindEvent, fun(Mod) -> Mod:req_event_type() end),
    gen_listener:rm_responder(self()
                              ,{hook_req_sup, handle_req} % send to the super
                              ,EventType
                             ).

setup_binding(BindEvent, BindOptions, Realm, AcctID) ->
    true = lists:member(BindEvent, ?HOOKS_SUPPORTED),
    gen_listener:add_binding(self()
                             ,BindEvent
                             ,[{realm, Realm}, {acct_id, AcctID} | BindOptions]
                            ),
    EventType = webhooks_util:api_call(BindEvent, fun(Mod) -> Mod:req_event_type() end),
    gen_listener:add_responder(self()
                               ,{hook_req_sup, handle_req} % send to the super
                               ,EventType
                              ).
