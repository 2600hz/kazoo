%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Periodically checks the hangup stats for anomalies
%%% Config values to set for threshold checks:
%%%   "one", "five", "fifteen", "day", "mean"
%%%
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-define(SERVER, ?MODULE).

-define(STAT_CHECK_MSG, 'stat_check').

-record(state, {stat_timer_ref :: reference()
               }).
-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link(?SERVER, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    {'ok', #state{stat_timer_ref=start_timer()}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(?STAT_CHECK_MSG, State) ->
    _P = kz_process:spawn(fun check_stats/0),
    {'noreply', State#state{stat_timer_ref=start_timer()}, 'hibernate'};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{stat_timer_ref=Ref}) ->
    _ = erlang:cancel_timer(Ref),
    lager:debug("hangups_monitor going down: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_timer() -> reference().
start_timer() ->
    erlang:send_after(?MILLISECONDS_IN_MINUTE, self(), ?STAT_CHECK_MSG).

-spec check_stats() -> 'ok'.
check_stats() ->
    kz_log:put_callid(?MODULE),
    lists:foreach(fun check_stats/1, hangups_config:monitored_hangup_causes()).

-spec check_stats(kz_term:ne_binary()) -> 'ok'.
check_stats(HC) ->
    MeterName = hangups_util:meter_name(HC),
    try folsom_metrics_meter:get_values(MeterName) of
        Stats -> maybe_alert(HC, Stats)
    catch
        'error':{'badmatch', []} -> 'ok'
    end.

-spec maybe_alert(kz_term:ne_binary(), list()) -> 'ok'.
maybe_alert(HangupCause, Stats) ->
    case metrics_exceeded(HangupCause, Stats) of
        [] -> 'ok';
        _MetricsExceeded ->
            lager:info("hangup cause ~s exceeded thresholds in metrics ~p"
                      ,[HangupCause, _MetricsExceeded]
                      ),
            send_alert(HangupCause)
    end.

-spec metrics_exceeded(kz_term:ne_binary(), list()) -> kz_term:atoms().
metrics_exceeded(HangupCause, Stats) ->
    [Key || Key <- props:get_keys(Stats),
            threshold_exceeded(HangupCause, Stats, Key)
    ].

-spec threshold_exceeded(kz_term:ne_binary(), list(), atom()) -> boolean().
threshold_exceeded(_HangupCause, _Stats, 'acceleration') -> 'false';
threshold_exceeded(_HangupCause, _Stats, 'count') -> 'false';
threshold_exceeded(_HangupCause, _Stats, 'mean') -> 'false';
threshold_exceeded(HangupCause, Stats, Key) ->
    ConfigName = hangups_util:meter_name(HangupCause),
    Threshold  = kapps_config:get_float(ConfigName, folsom_field(Key), 0.0),
    Value      = props:get_value(Key, Stats),
    is_threshold_exceeded(Value, Threshold, Key).

-spec is_threshold_exceeded(number(), number(), atom()) -> boolean().
is_threshold_exceeded(Value, Threshold, Key) ->
    Threshold > 0.0
        andalso Value > Threshold * folsom_minutes(Key).

-spec send_alert(kz_term:ne_binary()) -> 'ok'.
send_alert(HangupCause) ->
    Meter = hangups_util:meter_name(HangupCause),
    kz_notify:detailed_alert("~s alerted past configured threshold"
                            ,[kz_term:to_lower_binary(HangupCause)]
                            ,hangups_query_listener:meter_resp(Meter)
                            ).

-spec folsom_minutes(atom()) -> pos_integer().
folsom_minutes('one') -> 1;
folsom_minutes('five') -> 5;
folsom_minutes('fifteen') -> 15;
folsom_minutes('day') -> 1440.

-spec folsom_field(atom()) -> kz_term:ne_binary().
folsom_field('one') -> <<"one">>;
folsom_field('five') -> <<"five">>;
folsom_field('fifteen') -> <<"fifteen">>;
folsom_field('day') -> <<"day">>.
