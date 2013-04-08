%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%%
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(hangups_listener).

-behaviour(gen_listener).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([start_link/0]).
-export([handle_cdr/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-define(SERVER, ?MODULE).

-define(RESPONDERS, [{{?MODULE, 'handle_cdr'}, [{<<"call_detail">>, <<"cdr">>}]}]).
-define(BINDINGS, [{'call', [{'restrict_to', ['cdr']}, {'callid', <<"*">>}]}]).
-define(QUEUE_NAME, <<"hangups_listener">>).
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

-spec handle_cdr(wh_json:json_object(), proplist()) -> no_return().
handle_cdr(JObj, _Props) ->
    'true' = wapi_call:cdr_v(JObj),
    IgnoreCauses = whapps_config:get(<<"hangups">>, <<"ignore_hangup_causes">>, [<<"NO_ANSWER">>
                                                                                     ,<<"USER_BUSY">>
                                                                                     ,<<"NO_USER_RESPONSE">>
                                                                                     ,<<"LOSE_RACE">>
                                                                                     ,<<"ATTENDED_TRANSFER">>
                                                                                     ,<<"ORIGINATOR_CANCEL">>
                                                                                     ,<<"NORMAL_CLEARING">>
                                                                                     ,<<"ALLOTTED_TIMEOUT">>
                                                                                ]),
    HangupCause = wh_json:get_value(<<"Hangup-Cause">>, JObj, <<"unknown">>),
    case lists:member(HangupCause, IgnoreCauses) of
        'true' -> 'ok';
        'false' ->
            AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
            lager:debug("abnormal call termination: ~s", [HangupCause]),
            wh_notify:system_alert("~s ~s to ~s (~s) on ~s(~s)"
                                   ,[wh_util:to_lower_binary(HangupCause)
                                     ,find_source(JObj)
                                     ,find_destination(JObj)
                                     ,find_direction(JObj)
                                     ,find_realm(JObj, AccountId)
                                     ,AccountId
                                    ], wh_json:to_proplist(JObj))
    end.

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
    lager:debug("started hangups listener"),
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
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
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
    lager:debug("hangups listener ~p termination", [_Reason]),
    'ok'.

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
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_realm(wh_json:object(), ne_binary()) -> ne_binary().
find_realm(JObj, AccountId) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' -> get_account_realm(AccountId);
        Realm -> Realm
    end.

-spec get_account_realm(api_binary()) -> ne_binary().
get_account_realm('undefined') -> <<"unknown">>;
get_account_realm(AccountId) ->
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> wh_json:get_value(<<"realm">>, JObj, <<"unknown">>);
        {'error', _} -> <<"unknown">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_destination(wh_json:object()) -> ne_binary().
find_destination(JObj) ->
    case catch binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> use_to_as_destination(JObj)
    end.

-spec use_to_as_destination(wh_json:object()) -> ne_binary().
use_to_as_destination(JObj) ->
    case catch binary:split(wh_json:get_value(<<"To-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> wh_json:get_value(<<"Callee-ID-Number">>, JObj, <<"unknown">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_source(wh_json:object()) -> ne_binary().
find_source(JObj) ->
    case catch binary:split(wh_json:get_value(<<"From-Uri">>, JObj), <<"@">>) of
        [Num|_] -> Num;
        _ -> wh_json:get_value(<<"Caller-ID-Number">>, JObj, <<"unknown">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_direction(wh_json:object()) -> ne_binary().
find_direction(JObj) ->
    wh_json:get_value(<<"Call-Direction">>, JObj, <<"unknown">>).
