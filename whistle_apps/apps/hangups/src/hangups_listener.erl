%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for CDR events publish non-normal termination notifications 
%%% @end
%%%
%%% Contributors:
%%%   James Aimonetti
%%%   Karl Anderson
%%% Created : 23 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hangups_listener).

-behaviour(gen_listener).

-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_types.hrl").

%% API
-export([start_link/0, handle_cdr/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2
         ,terminate/2, code_change/3]).

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

-spec handle_cdr/2 :: (wh_json:json_object(), proplist()) -> no_return().
handle_cdr(JObj, _Props) ->
    true = wapi_call:cdr_v(JObj),
    IgnoreCauses = whapps_config:get(<<"hangups">>, <<"ignore_hangup_causes">>, [<<"NO_ANSWER">>
                                                                                     ,<<"USER_BUSY">>
                                                                                     ,<<"NO_USER_RESPONSE">>
                                                                                     ,<<"LOSE_RACE">>
                                                                                     ,<<"ATTENDED_TRANSFER">>
                                                                                     ,<<"ORIGINATOR_CANCEL">>
                                                                                ]),
    HangupCause = wh_json:get_value(<<"hangup_cause">>, JObj, <<"unknown">>),
    case lists:member(HangupCause, IgnoreCauses) of
        true -> ok;
        false -> ?LOG(hangup_cause_to_alert_level(HangupCause)
                      ,"abnormal call termination ~s"
                      ,[HangupCause, {extra_data, [{details, JObj}]}])
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
    ?LOG_SYS("started hangups listener"),
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
    ?LOG_SYS("hangups listener ~p termination", [_Reason]),
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
-spec hangup_cause_to_alert_level/1 :: (ne_binary()) -> atom().
hangup_cause_to_alert_level(<<"UNALLOCATED_NUMBER">>) ->
    warning;
hangup_cause_to_alert_level(<<"NO_ROUTE_DESTINATION">>) ->
    warning;
hangup_cause_to_alert_level(<<"NORMAL_UNSPECIFIED">>) ->
    warning;
hangup_cause_to_alert_level(<<"USER_BUSY">>) ->
    info;
hangup_cause_to_alert_level(<<"ORIGINATOR_CANCEL">>) ->
    info;
hangup_cause_to_alert_level(<<"NO_ANSWER">>) ->
    info;
hangup_cause_to_alert_level(<<"LOSE_RACE">>) ->
    info;
hangup_cause_to_alert_level(<<"ATTENDED_TRANSFER">>) ->
    info;
hangup_cause_to_alert_level(<<"CALL_REJECTED">>) ->
    info;
hangup_cause_to_alert_level(_) ->
    error.
