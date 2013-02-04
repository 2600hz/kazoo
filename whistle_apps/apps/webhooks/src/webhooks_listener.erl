%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Listener for a webhook. This module should do almost nothing with the
%%% the response besides receive it and pass it along to
%%% wh_api:disambiguate_and_publish/3
%%%
%%% @end
%%% Created : 29 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(webhooks_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0, handle_req/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
         ,handle_event/2, terminate/2, code_change/3]).

-include("webhooks.hrl").

-define(SERVER, ?MODULE).

-record(state, {hooks_started = dict:new() :: dict() % {{Acct, DocId}, {PidOfHookAcctListener, Ref}}
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
    gen_listener:start_link(?MODULE, [{responders, [{{?MODULE, handle_req}, [{<<"configuration">>, <<"*">>}]} ]}
                                      ,{bindings, [ {conf, [ {doc_type, <<"webhooks">>} ]} ]}
                                     ], []).

handle_req(JObj, Props) ->
    true = wapi_conf:doc_update_v(JObj),
    gen_listener:cast(props:get_value(server, Props), {config_change, JObj}).

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
    Dict = start_known_webhooks(),
    lager:debug("webhooks listener started"),
    {ok, #state{hooks_started=Dict}}.

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
handle_call(_, _From, State) ->
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
handle_cast({config_change, JObj}, #state{hooks_started=Dict}=State) ->
    true = wapi_conf:doc_update_v(JObj),

    AcctDB = wapi_conf:get_account_db(JObj),
    ID = wapi_conf:get_id(JObj),

    DocAction = wh_json:get_value(<<"Event-Name">>, JObj), % doc_created, doc_edited, doc_deleted

    lager:debug("config change ~s to ~s.~s", [DocAction, AcctDB, ID]),

    Key = {AcctDB, ID},

    case dict:find(Key, Dict) of
        {ok, {Pid, _}} ->
            lager:debug("server ~p found for webhooks doc", [Pid]),
            _ = handle_action(DocAction, Pid, JObj),
            {noreply, State};
        error ->
            PubDoc = wh_json:get_value(<<"doc">>, JObj),
            lager:debug("unknown webhook doc, starting handler: ~s", [wh_json:get_value(<<"bind_event">>, JObj)]),
            {ok, Pid} = hook_acct_sup:start_listener(AcctDB, PubDoc),
            Ref = erlang:monitor(process, Pid),
            {noreply, State#state{hooks_started=dict:store(Key, {Pid, Ref}, Dict)}, hibernate}
    end.

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
handle_info({'DOWN', Ref, process, Pid, normal}, #state{hooks_started=Dict}=State) ->
    Dict1 = dict:filter(fun({AcctDB, DocID}, {Pid1, Ref1}) when Pid =:= Pid1 andalso Ref =:= Ref1 ->
                                lager:debug("hook handler for ~s.~s down normally", [AcctDB, DocID]),
                                false;
                           (_, _) -> true
                        end, Dict),
    {noreply, State#state{hooks_started=Dict1}, hibernate};
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{hooks_started=Dict}=State) ->
    Dict1 = dict:filter(fun({AcctDB, DocID}, {Pid1, Ref1}) when Pid =:= Pid1 andalso Ref =:= Ref1 ->
                                lager:debug("hook handler for ~s.~s down: ~p", [AcctDB, DocID, Reason]),
                                false;
                           (_, _) -> true
                        end, Dict),
    {noreply, State#state{hooks_started=Dict1}, hibernate};
handle_info(_Info, State) ->
    lager:debug("unhandled message: ~p", [_Info]),
    {noreply, State}.

handle_event(_JObj, _State) ->
    {reply, []}.

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
    lager:debug("webhooks listener going down").

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
-spec start_known_webhooks() -> dict().
start_known_webhooks() ->
    lists:foldl(fun(AcctDb, HooksAcc) ->
                        lists:foldl(fun({DocId, Pid}, Hooks1Acc) ->
                                            Ref = erlang:monitor(process, Pid),
                                            dict:store({AcctDb, DocId}, {Pid, Ref}, Hooks1Acc)
                                    end, HooksAcc, maybe_start_handler(AcctDb))
                end, dict:new(), whapps_util:get_all_accounts(encoded)).

%% returns [{DocId, Pid},...]
-spec maybe_start_handler(ne_binary()) -> [{ne_binary(), pid()},...] | [].
maybe_start_handler(Db) ->
    case couch_mgr:get_results(Db, <<"webhooks/crossbar_listing">>, [{<<"include_docs">>, true}]) of
        {ok, []} -> lager:debug("No webhooks in ~s", [Db]), [];
        {ok, WebHooks} ->
            lager:debug("Starting webhooks listener(s) for ~s: ~b", [Db, length(WebHooks)]),
            [begin
                 {ok, Pid} = hook_acct_sup:start_listener(Db, wh_json:get_value(<<"doc">>, Hook)),
                 {wh_json:get_value(<<"id">>, Hook), Pid}
             end
             || Hook <- WebHooks];
        {error, _E} ->
            lager:debug("Failed to load webhooks view for account ~s", [Db]),
            []
    end.

handle_action(<<"doc_created">>, Pid, _JObj) ->
    lager:debug("doc was created but we know the pid (~p) already?", [Pid]);
handle_action(<<"doc_edited">>, Pid, JObj) ->
    lager:debug("webhook updated for ~p", [Pid]),
    hook_acct_listener:update_config(Pid, JObj);
handle_action(<<"doc_deleted">>, Pid, _) ->
    lager:debug("webhook deleted, shutting down ~p", [Pid]),
    hook_acct_listener:stop(Pid).
