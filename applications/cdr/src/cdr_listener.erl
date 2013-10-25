%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%% Listen for CDR events and record them to the database
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Edouard Swiac
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_listener).

-behaviour(gen_listener).

%% API
-export([start_link/0
         ,handle_cdr/2
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("cdr.hrl").

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, 'handle_cdr'}, [{<<"call_detail">>, <<"cdr">>}]}]).
-define(BINDINGS, [{'call', [{'restrict_to', ['cdr']}
                             ,{'callid', <<"*">>}
                            ]}]).
-define(QUEUE_NAME, <<"cdr_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

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
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], []).

-spec handle_cdr(wh_json:object(), wh_proplist()) -> any().
handle_cdr(JObj, _Props) ->
    'true' = wapi_call:cdr_v(JObj),
    _ = wh_util:put_callid(JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>,<<"Account-ID">>], JObj),
    Timestamp = wh_json:get_integer_value(<<"Timestamp">>, JObj),
    prepare_and_save(AccountId, Timestamp, JObj).

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
    {'ok', 'ok'}.

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
    {'reply', {'error', 'not_implemented'}, State}.

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
    {'noreply', State}.

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
    {'noreply', State}.

handle_event(_JObj, _State) ->
    {'reply', []}.

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
    lager:debug("cdr listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec prepare_and_save(account_id(), pos_integer(), wh_json:object()) -> wh_json:object().
prepare_and_save(AccountId, Timestamp, JObj) ->
    Routines = [fun normalize/3
                ,fun update_pvt_parameters/3
                ,fun set_doc_id/3
                ,fun save_cdr/3
               ],
    lists:foldl(fun(F, J) ->
                        F(AccountId, Timestamp, J) 
                end, JObj, Routines).

-spec normalize(api_binary(), pos_integer(), wh_json:object()) -> wh_json:object().
normalize(_, _, JObj) ->
    wh_json:normalize_jobj(JObj).    

-spec update_pvt_parameters(api_binary(), pos_integer(), wh_json:object()) -> wh_json:object().
update_pvt_parameters('undefined', _, JObj) ->
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
            ],
    wh_doc:update_pvt_parameters(JObj, ?WH_ANONYMOUS_CDR_DB, Props);
update_pvt_parameters(AccountId, Timestamp, JObj) ->
    AccountMODb = wh_util:format_account_id(AccountId, Timestamp),    
    Props = [{'type', 'cdr'}
             ,{'crossbar_doc_vsn', 2}
            ],
    wh_doc:update_pvt_parameters(JObj, AccountMODb, Props).

-spec set_doc_id(api_binary(), pos_integer(), wh_json:object()) -> wh_json:object().
set_doc_id(_, Timestamp, JObj) ->
    CallId = wh_json:get_value(<<"call_id">>, JObj),
    DocId = cdr_util:get_cdr_doc_id(Timestamp, CallId),
    wh_json:set_value(<<"_id">>, DocId, JObj).

-spec save_cdr(api_binary(), pos_integer(), wh_json:object()) -> wh_json:object().
save_cdr(_, _, JObj) ->
    CDRDb = wh_json:get_value(<<"pvt_account_db">>, JObj),
    case cdr_util:save_cdr(CDRDb, JObj) of
        {'error', 'max_retries'} -> 
            lager:error("write fail: ~s", [CDRDb]);
        'ok' -> 'ok'
    end.
