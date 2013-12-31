%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz Inc
%%% @doc
%%% Periodically checks the hangup stats for anomalies
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hangups_monitoring).

-behaviour(gen_server).

%% API
-export([start_link/0
         ,check_stats/0
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("hangups.hrl").

-define(STAT_CHECK_MSG, 'stat_check').
-define(HANGUPS_TO_MONITOR
        ,[<<"WRONG_CALL_STATE">>
          ,<<"NO_ROUTE_DESTINATION">>
          ,<<"CALL_REJECT">>
          ,<<"MANDATORY_IE_MISSING">>
          ,<<"PROGRESS_TIMEOUT">>
          ,<<"RECOVERY_ON_TIMER_EXPIRE">>
         ]).


-record(state, {stat_timer_ref :: reference()}).

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
    gen_server:start_link(?MODULE, [], []).

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
    {'ok', #state{stat_timer_ref=start_timer()}}.

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
    lager:debug("unhandled cast: ~p", [_Msg]),
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
handle_info(?STAT_CHECK_MSG, State) ->
    _P = spawn(?MODULE, 'check_stats', []),
    {'noreply', State#state{stat_timer_ref=start_timer()}, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

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
terminate(_Reason, #state{stat_timer_ref=Ref}) ->
    _ = erlang:cancel_timer(Ref),
    lager:debug("hangups_monitor going down: ~p", [_Reason]).

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
-spec start_timer() -> reference().
start_timer() ->
    erlang:send_after(?MILLISECONDS_IN_MINUTE, self(), ?STAT_CHECK_MSG).

-spec check_stats() -> 'ok'.
check_stats() ->
    lists:foreach(fun check_stats/1, ?HANGUPS_TO_MONITOR).

check_stats(HC) ->
    Stats = folsom_metrics_meter:get_values(hangups_util:meter_name(HC)),
    maybe_alert(HC, Stats).

-spec maybe_alert(ne_binary(), list()) -> 'ok'.
-spec maybe_alert(ne_binary(), list(), atom()) -> boolean().
maybe_alert(HC, Stats) ->
    case lists:any(fun(Key) ->
                           maybe_alert(HC, Stats, Key)
                   end, props:get_keys(Stats))
    of
        'true' -> send_alert(HC);
        'false' -> 'ok'
    end.

maybe_alert(_, _, 'acceleration') -> 'false';
maybe_alert(_, _, 'count') -> 'false';
maybe_alert(HC, Stats, Key) ->
    case whapps_config:get_float(<<?APP_NAME/binary, ".", HC/binary>>, Key) of
        'undefined' -> 'false';
        Threshold ->
            maybe_alert_on_threshold(props:get_value(Key, Stats), Threshold)
    end.

-spec maybe_alert_on_threshold(number(), number()) -> boolean().
maybe_alert_on_threshold(Value, Threshold) ->
    Value > Threshold.

-spec send_alert(ne_binary()) -> 'ok'.
send_alert(HC) ->
    wh_notify:system_alert("~s alerted past configured threshold"
                           ,[wh_util:to_lower_binary(HC)]
                           ,wh_json:from_list(
                              hangups_query_listener:meter_resp(hangups_util:meter_name(HC))
                             )
                          ).
