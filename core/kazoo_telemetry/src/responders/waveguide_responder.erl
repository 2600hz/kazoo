%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(waveguide_responder).

-behaviour(gen_statem).

-export([start_link/0
        ,status/0
        ,stop/0
        ]).

-export([init/1
        ,callback_mode/0
        ,terminate/3
        ,code_change/4
        ]).

-export([grace_period/3
        ,activating/3
        ,handshaking/3
        ,submitting/3
        ,waiting/3
        ]).

-define(SERVER, ?MODULE).

-define(HANDSHAKE_TIMER, 5000).
-define(RETRY_INTERVAL, 60000).
-define(TRANSITION_TIMER, 0).

-define(MAX_RETRIES, 3).

-include("waveguide.hrl").

-record(state, {retry = 0 :: non_neg_integer()}).

-type state() :: #state{}.
-type statem_events() :: 'ping' | 'timeout' | 'wakeup'.
-type statem_state() :: 'activating' | 'grace_period' | 'handshaking' | 'submitting' | 'waiting'.
-type statem_reply() :: {'next_state', statem_state(), state()} |
                        'repeat_state_and_data' |
                        {'keep_state', state()} |
                        {'keep_state_and_data', [{'state_timeout', non_neg_integer(), 'timeout' | 'retry'}]}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the waveguide statem
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link()  ->
    gen_statem:start_link({'local', ?SERVER}, ?MODULE, [], []).

-spec stop() -> 'ok'.
stop() ->
    lager:notice("shutting down ~s", [?MODULE]),
    gen_statem:stop(?SERVER).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status() -> {statem_state(), state()}.
status() ->
    sys:get_state(?MODULE).

%%%=============================================================================
%%% gen_statem callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the waveguide statem
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', 'grace_period', state()}.
init([]) ->
    process_flag('trap_exit', 'true'),
    {'ok', 'grace_period', #state{}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec callback_mode() -> ['state_functions' | 'state_enter'].
callback_mode() ->
    ['state_functions','state_enter'].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), atom(), statem_state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("~s terminating: ~p", [?SERVER, _Reason]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), atom(), statem_state(), any()) -> {'ok', atom(), statem_state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec grace_period(gen_statem:event_type(), statem_events(), state()) -> statem_reply().
grace_period('enter', _OldState, State) ->
    _ = timer:apply_after(?TRANSITION_TIMER, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State};
grace_period('cast', 'ping', State) ->
    case wg_util:days_remaining() of
        Days when Days > 0 ->
            lager:notice("kazoo telemetry grace period has ~p days remaining", [Days]),
            _ = create_alert(Days),
            {'keep_state_and_data', [{'state_timeout', ?DAY_IN_MS, 'retry'}]};
        _ ->
            {'next_state', 'activating', State}
    end;
grace_period('state_timeout', 'retry', _State) ->
    lager:notice("grace period expired ... activating telemetry."),
    'repeat_state_and_data'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activating(gen_statem:event_type(), statem_events(), state()) -> statem_reply().
activating('enter', _OldState, #state{retry=Retries}=State) when Retries < ?MAX_RETRIES ->
    _ = timer:apply_after(?TRANSITION_TIMER, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State};
activating('enter', _OldState, State) ->
    _ = timer:apply_after(?RETRY_INTERVAL, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State#state{retry=0}};
activating('cast', 'ping', #state{retry=Retries}=State) ->
    {'ok', Data} = waveguide_reducer:ping(?WG_ACTIVATION_PING),
    case wg_httpc:post(Data) of
        {'ok', _} ->
            {'next_state', 'handshaking', State};
        {'retry', _} ->
            {'keep_state', State#state{retry=Retries+1}, [{'state_timeout', ?TRANSITION_TIMER, 'retry'}]}
    end;
activating('state_timeout', 'retry', _State) ->
    'repeat_state_and_data'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handshaking(gen_statem:event_type(), statem_events(), state()) -> statem_reply().
handshaking('enter', _OldState, #state{retry=Retries}=State) when Retries < ?MAX_RETRIES ->
    _ = timer:apply_after(?TRANSITION_TIMER, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State};
handshaking('enter', _OldState, State) ->
    _ = timer:apply_after(?RETRY_INTERVAL, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State#state{retry=0}};
handshaking('cast', 'ping', #state{retry=Retries}=State) ->
    {'ok', Data} = waveguide_reducer:ping(?WG_HANDSHAKE_PING),
    case wg_httpc:post(Data) of
        {'retry', _} ->
            {'keep_state', State#state{retry=Retries+1}, [{'state_timeout', ?TRANSITION_TIMER, 'retry'}]};
        {'ok', _} ->
            {'next_state', 'submitting', State}
    end;
handshaking('state_timeout', 'retry', _State) ->
    'repeat_state_and_data'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec submitting(gen_statem:event_type(), statem_events(), state()) -> statem_reply().
submitting('enter', _OldState, #state{retry=Retries}=State) when Retries < ?MAX_RETRIES ->
    _ = timer:apply_after(?TRANSITION_TIMER, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State};
submitting  ('enter', _OldState, State) ->
    _ = timer:apply_after(?RETRY_INTERVAL, 'gen_statem', 'cast', [?SERVER, 'ping']),
    {'keep_state', State#state{retry=0}};
submitting('cast', 'ping', #state{retry=Retries}=State) ->
    {'ok', Data} = waveguide_reducer:ping(?WG_MAIN_PING),
    case wg_httpc:post(Data) of
        {'retry', _} ->
            {'keep_state', State#state{retry=Retries+1}, [{'state_timeout', ?TRANSITION_TIMER, 'timeout'}]};
        {'ok', _} ->
            {'next_state', 'waiting', State}
    end;
submitting('state_timeout', 'timeout', _State) ->
    'repeat_state_and_data'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec waiting(gen_statem:event_type(), statem_events(), state()) -> statem_reply().
waiting('enter', _OldState, State) ->
    {'keep_state', State, [{'state_timeout', ?TM_COLLECTION_INTERVAL*1000, 'wakeup'}]};
waiting('state_timeout', 'wakeup', State) ->
    {'next_state', 'submitting', State}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc create an alert in alerts database
%% @end
%%------------------------------------------------------------------------------
-spec create_alert(non_neg_integer()) -> 'ok'.
create_alert(Days) ->
    DaysBin = integer_to_binary(Days),
    case kapps_util:get_master_account_id() of
        {'ok', AccountId} ->
            Props = [{<<"category">>, <<"kazoo_telemetry">>}
                    ],
            From = [kz_json:from_list([{<<"type">>, <<"account">>}
                                      ,{<<"value">>, AccountId}
                                      ])
                   ],
            To = [kz_json:from_list([{<<"type">>, AccountId}
                                    ,{<<"value">>, <<"admins">>}
                                    ])
                 ],
            Title = <<"kazoo telemetry grace period active">>,
            Msg = <<"kazoo telemetry will automatically enable in ",DaysBin/binary," days.">>,
            {'ok', AlertJObj} = kapps_alert:create(Title, Msg, From, To, Props),
            {'ok', _} = kapps_alert:save(AlertJObj),
            'ok';
        _ -> 'ok'
    end.

