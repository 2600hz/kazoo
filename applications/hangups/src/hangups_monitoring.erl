%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz Inc
%%% @doc
%%% Periodically checks the hangup stats for anomalies
%%%
%%% Config values to set for threshold checks:
%%%   "one", "five", "fifteen", "day", "mean"
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
-export([hangups_to_monitor/0]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("hangups.hrl").

-define(SERVER, ?MODULE).

-define(STAT_CHECK_MSG, 'stat_check').


-record(state, {stat_timer_ref :: reference()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

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
    kz_util:put_callid(?MODULE),
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
    _P = kz_util:spawn(fun check_stats/0),
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

%% @public
-spec hangups_to_monitor() -> ne_binaries().
hangups_to_monitor() ->
    kapps_config:get(?APP_NAME
                    ,<<"hangups_to_monitor">>
                    ,[<<"WRONG_CALL_STATE">>
                     ,<<"NO_ROUTE_DESTINATION">>
                     ,<<"CALL_REJECT">>
                     ,<<"MANDATORY_IE_MISSING">>
                     ,<<"PROGRESS_TIMEOUT">>
                     ,<<"RECOVERY_ON_TIMER_EXPIRE">>
                     ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec start_timer() -> reference().
start_timer() ->
    erlang:send_after(?MILLISECONDS_IN_MINUTE, self(), ?STAT_CHECK_MSG).

-spec check_stats() -> 'ok'.
check_stats() ->
    kz_util:put_callid(?MODULE),
    lists:foreach(fun check_stats/1, hangups_to_monitor()).

check_stats(HC) ->
    MeterName = hangups_util:meter_name(HC),
    try folsom_metrics_meter:get_values(MeterName) of
        Stats -> maybe_alert(HC, Stats)
    catch
        'error':{'badmatch', []} ->
            lager:debug("no stats for hangup cause ~s, skipping", [HC])
    end.

-spec maybe_alert(ne_binary(), list()) -> 'ok'.
maybe_alert(HangupCause, Stats) ->
    PastThreshold = fun(Key) -> maybe_alert(HangupCause, Stats, Key) end,
    case lists:any(PastThreshold, props:get_keys(Stats)) of
        'true' -> send_alert(HangupCause);
        'false' -> 'ok'
    end.

-spec maybe_alert(ne_binary(), list(), atom()) -> boolean().
maybe_alert(_HangupCause, _Stats, 'acceleration') -> 'false';
maybe_alert(_HangupCause, _Stats, 'count') -> 'false';
maybe_alert(_HangupCause, _Stats, 'mean') -> 'false';
maybe_alert(HangupCause, Stats, Key) ->
    ConfigName = hangups_util:meter_name(HangupCause),
    Threshold  = kapps_config:get_float(ConfigName, folsom_field(Key)),
    Value      = props:get_value(Key, Stats),
    maybe_alert_on_threshold(Value, Threshold, Key).

-spec maybe_alert_on_threshold(number(), number() | 'undefined', atom()) -> boolean().
maybe_alert_on_threshold(_Value, 'undefined', _Key) -> 'false';
maybe_alert_on_threshold(Value, Threshold, Key) ->
    Value > Threshold * folsom_minutes(Key).

-spec send_alert(ne_binary()) -> 'ok'.
send_alert(HangupCause) ->
    lager:debug("hangup cause ~s past threshold, system alerting", [HangupCause]),
    Meter = hangups_util:meter_name(HangupCause),
    kz_notify:detailed_alert("~s alerted past configured threshold"
                            , [kz_util:to_lower_binary(HangupCause)]
                            , hangups_query_listener:meter_resp(Meter)
                            ).

-spec folsom_minutes(atom()) -> pos_integer().
folsom_minutes('one') -> 1;
folsom_minutes('five') -> 5;
folsom_minutes('fifteen') -> 15;
folsom_minutes('day') -> 1440.

-spec folsom_field(atom()) -> ne_binary().
folsom_field('one') -> <<"one">>;
folsom_field('five') -> <<"five">>;
folsom_field('fifteen') -> <<"fifteen">>;
folsom_field('day') -> <<"day">>.
