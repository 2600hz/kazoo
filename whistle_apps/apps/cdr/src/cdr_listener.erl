%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for CDR events and record them to the database
%%% TODO: convert to gen_listener
%%% @end
%%% Created : 23 Nov 2010 by James Aimonetti <james@2600hz.org>
%%% Updated : 19 Jun 2011 by Edouard Swiac <edouard@2600hz.org>
%%%-------------------------------------------------------------------
-module(cdr_listener).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("cdr.hrl").

-define(SERVER, ?MODULE).
-record(state, {amqp_q = <<>> :: binary()}).
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
    ?LOG_SYS("starting CDR listener"),
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
handle_info(timeout, #state{amqp_q = <<>>}=State) ->
    try
        {ok, Q} = start_amqp(),
        create_anonymous_cdr_db(?ANONYMOUS_CDR_DB),
        {noreply, State#state{amqp_q=Q}, hibernate}
    catch
        _:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}, hibernate}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    {ok, _} = timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    {ok, _} = timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    {ok, _} = timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}, hibernate};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() -> handle_cdr(Payload) end),
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
terminate(_Reason, _State) ->
    ?LOG_SYS("CDR listener ~p termination", [_Reason]),
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
-spec start_amqp/0 :: () -> tuple(ok, binary()).
start_amqp() ->
    try
        _ = amqp_util:callevt_exchange(),
        Q = amqp_util:new_callmgr_queue(<<>>),
        amqp_util:bind_q_to_callevt(Q, <<"*">>, cdr), % bind to all CDR events
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process th payload of an AMQPrequests
%% @end
%%--------------------------------------------------------------------
-spec handle_cdr/1 :: (Payload) -> no_return() when
      Payload :: binary().
handle_cdr(Payload) ->
    JObj = mochijson2:decode(Payload),
    AccountDb = whapps_util:get_db_name(wh_json:get_value([<<"Custom-Channel-Vars">>,<<"Account-ID">>], JObj), encoded),

    Db = case couch_mgr:db_exists(AccountDb) of
        true -> AccountDb;
        false -> ?ANONYMOUS_CDR_DB
    end,

    NormDoc = wh_json:normalize_jobj(JObj),
    DocOpts = [{type, cdr}, {crossbar_doc_vsn, 1}],
    JObj1 = wh_doc:update_pvt_parameters(NormDoc, Db, DocOpts),
    Id = wh_json:get_value(<<"call_id">>, NormDoc, couch_mgr:get_uuid()),
    NewDoc = wh_json:set_value(<<"_id">>, Id, JObj1),

    case couch_mgr:save_doc(Db, NewDoc) of
        {ok, _} ->
            ?LOG("CDR for Call-ID:~p stored", [Id]),
            stored;
        {error, _} ->
            ?LOG("___+++ Error, Cannot save Doc. Trying to find existing doc with ID ~p", [Id]),
            case couch_mgr:open_doc(Db, Id) of
                {ok, ExistingDoc} ->
                    ?LOG("___+++ CDR Doc ~p Found! Figuring out if CDR is related to that CDR...", [wh_json:get_value(<<"_id">>, ExistingDoc)]),
		    NewOtherLegCallId = wh_json:get_value(<<"other_leg_call_id">>, NewDoc),
		    RelatedCdrs = wh_json:get_value(<<"related_cdrs">>, ExistingDoc, []),
                    case wh_json:get_value(<<"other_leg_call_id">>, ExistingDoc) == NewOtherLegCallId
                        orelse lists:any(fun(Doc) -> (wh_json:get_value(<<"other_leg_call_id">>, Doc) == NewOtherLegCallId) end, RelatedCdrs) of
                        true ->
                            ?LOG("___+++ other_leg_call_id are equals, ignoring that Doc"),
                            ignore;
                        false ->
                            ?LOG("___+++ other_leg_call_id are not  equals, appending ..."),
                            DocToSave = append_cdr_to_doc(ExistingDoc, NewDoc),
                            couch_mgr:save_doc(Db, wh_doc:update_pvt_modified(DocToSave)),
                            ?LOG("New CDR for Call-ID:~p appended", [Id])
                    end;
                {error, _} ->
                    ignore
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creation of the anonymous_cdr DB if it doesn't exists
%% @end
%%--------------------------------------------------------------------
-spec create_anonymous_cdr_db/1 :: (DB) -> no_return() when
      DB :: binary().
create_anonymous_cdr_db(DB) ->
    couch_mgr:db_create(DB),
    case couch_mgr:load_doc_from_file(DB, cdr, <<"cdr.json">>) of
	{ok, _} -> ok;
	{error, _} -> couch_mgr:update_doc_from_file(DB, cdr, <<"cdr.json">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Append the CDR to the list of CDR of the doc, or create that
%% list from the existing CDR
%% @end
%%--------------------------------------------------------------------
-spec append_cdr_to_doc/2 :: (ExistingDoc, NewDoc) -> json_object() when
      ExistingDoc :: json_object(),
      NewDoc :: json_object().
append_cdr_to_doc(ExistingDoc, NewDoc) ->
    DocFinal = case wh_json:get_value(<<"related_cdrs">>, ExistingDoc) of
                   undefined ->
                       ?LOG("___+++ related_cdrs field doesnt exist, creating list of existing doc and new doc ..."),
                       PublicFields = [{struct, wh_doc:jobj_to_list(wh_doc:public_fields(NewDoc))} | [{struct, wh_doc:jobj_to_list(wh_doc:public_fields(ExistingDoc))}]],
                       JObj = wh_json:set_value(<<"related_cdrs">>, PublicFields, {struct, []}),

                       ?LOG("___+++ Appending previously existing pvt fields ..."),
                       PvtFields = wh_doc:jobj_to_list(wh_doc:private_fields(ExistingDoc)),
                       lists:foldl(fun({Key, Val}, JObj1) -> wh_json:set_value(Key, Val, JObj1)  end, JObj, PvtFields);
                   CdrList ->
                       ?LOG("___+++ related_cdrs field exists, appending ..."),
                       wh_json:set_value(<<"related_cdrs">>, lists:append(CdrList, [{struct, wh_doc:jobj_to_list(wh_doc:public_fields(NewDoc))}]), ExistingDoc)
               end,
    DocFinal.

