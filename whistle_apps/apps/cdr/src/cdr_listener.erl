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

-behaviour(gen_listener).

%% API
-export([start_link/0, handle_cdr/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
	 ,terminate/2, code_change/3]).

-include("cdr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, handle_cdr}, [{<<"call_detail">>, <<"cdr">>}]}]).
-define(BINDINGS, [{call, [{restrict_to, [cdr]}, {callid, <<"*">>}]}]).
-define(QUEUE_NAME, <<"">>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

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
				      ,{queue_name, ?QUEUE_NAME}
				      ,{queue_options, ?QUEUE_OPTIONS}
				      ,{consume_options, ?CONSUME_OPTIONS}
				     ], []).

-spec handle_cdr/2 :: (json_object(), proplist()) -> no_return().
handle_cdr(JObj, _Props) ->
    true  = wapi_call:cdr_v(JObj),

    AccountDb = wh_util:format_account_id(wh_json:get_value([<<"Custom-Channel-Vars">>,<<"Account-ID">>], JObj), encoded),

    Db = case couch_mgr:db_exists(AccountDb) of
	     true -> AccountDb;
	     false -> ?ANONYMOUS_CDR_DB
	 end,

    NormDoc = wh_json:normalize_jobj(JObj),
    DocOpts = [{type, cdr}, {crossbar_doc_vsn, 1}],
    JObj1 = wh_doc:update_pvt_parameters(NormDoc, Db, DocOpts),

    Id = wh_json:get_value(<<"call_id">>, NormDoc, couch_mgr:get_uuid()),
    NewDoc = wh_json:set_value(<<"_id">>, Id, JObj1),

    ?LOG_SYS(Id, "Saving CDR to ~s", [Db]),

    couch_mgr:save_doc(Db, NewDoc).

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
    _ = create_anonymous_cdr_db(?ANONYMOUS_CDR_DB),

    ?LOG_SYS("Init complete"),

    {ok, ok}.

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
handle_info(_Info, State) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creation of the anonymous_cdr DB if it doesn't exists
%% @end
%%--------------------------------------------------------------------
-spec create_anonymous_cdr_db/1 :: (ne_binary()) -> {'ok', json_object()} | {'error', term()}.
create_anonymous_cdr_db(DB) ->
    couch_mgr:db_create(DB),
    couch_mgr:revise_doc_from_file(DB, cdr, <<"cdr.json">>).
