%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Ilya Ashchepkov)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ananke_listener).
-behaviour(gen_listener).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([load_schedules/0]).

-include("ananke.hrl").

-record(state, {}).
-type state() :: #state{}.

-define(SERVER, ?MODULE).

-define(BINDINGS, [{'notifications', [{'restrict_to', ['voicemail_saved']}]}
                  ]).

-define(RESPONDERS, [{'ananke_vm_callback', [{<<"notification">>, <<"voicemail_saved">>}]}
                    ]).
-define(QUEUE_NAME, <<"ananke_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                           ,?MODULE
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ,{'queue_name', ?QUEUE_NAME}
                            ,{'queue_options', ?QUEUE_OPTIONS}
                            ,{'consume_options', ?CONSUME_OPTIONS}
                            ]
                           ,[]
                           ).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    %% we should wait about 7-10 seconds before gen_leader syncronization
    %% and leader election
    %% after gen_leader syncronization this task will be scheduled only once
    _ = kz_process:spawn(fun load_schedules/0),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), any(), state()) -> {'noreply', state()}.
handle_call(_Request, _From, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> {'noreply', state()}.
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast('load_schedules', State) ->
    Schedules = kapps_config:get_jsons(?CONFIG_CAT, <<"schedules">>, []),
    NormalizedSchedules = [normalize_schedule(S) || S <- Schedules],
    lists:foreach(fun schedule/1, NormalizedSchedules),
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> {'noreply', state()}.
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), state()) -> {'reply', []}.
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

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

-type time() :: amqp_cron_task:oneshot() | amqp_cron_task:cron() | amqp_cron_task:sleeper().
-type time_token_value() :: 'all' | integer() | kz_term:integers().
-type amqp_cron_callback() :: {atom(), atom(), list()} | {fun(), list()}.
-spec normalize_schedule(kz_json:object()) -> {kz_term:ne_binary(), time(), amqp_cron_callback()}.
normalize_schedule(Schedule) ->
    Action = kz_json:get_value(<<"action">>, Schedule),
    ActionType = kz_json:get_value(<<"type">>, Action),
    ActionFun = action_fun(ActionType, Action),
    TimeSchedule = time_schedule(Schedule),
    ActionName = action_name(ActionType, Action, TimeSchedule),
    {ActionName, TimeSchedule, ActionFun}.

-spec time_schedule(kz_json:object()) -> time().
time_schedule(Schedule) ->
    GetTimeTokenFun = get_time_token_value(Schedule),
    case kz_json:get_value(<<"type">>, Schedule) of
        <<"every">> ->
            [Minutes, Hours, MonthDays, Monthes, Weekdays] =
                lists:map(GetTimeTokenFun
                         ,[{<<"minutes">>, 'all'}
                          ,{<<"hours">>, 'all'}
                          ,{<<"month_days">>, 'all'}
                          ,{<<"monthes">>, 'all'}
                          ,{<<"weekdays">>, 'all'}
                          ]),
            {'cron', {Minutes, Hours, MonthDays, Monthes, Weekdays}};
        <<"once">> ->
            [Second, Minute, Hour, Day, Month, Year] =
                lists:map(GetTimeTokenFun
                         ,[{<<"second">>, 0}
                          ,{<<"minute">>, 0}
                          ,{<<"hour">>, 0}
                          ,{<<"day">>, 1}
                          ,{<<"month">>, 1}
                          ,{<<"year">>, 1970}
                          ]),
            {'oneshot', {{Year, Month, Day}, {Hour, Minute, Second}}};
        <<"periodic">> ->
            [Seconds, Minutes, Hours, Days] =
                lists:map(GetTimeTokenFun
                         ,[{<<"seconds">>, 0}
                          ,{<<"minutes">>, 0}
                          ,{<<"hours">>, 0}
                          ,{<<"days">>, 0}
                          ]),
            {'sleeper', (Seconds
                         + ?SECONDS_IN_MINUTE * Minutes
                         + ?SECONDS_IN_HOUR * Hours
                         + ?SECONDS_IN_DAY * Days) * ?MILLISECONDS_IN_SECOND}
    end.

-spec schedule({kz_term:ne_binary(), time(), amqp_cron_task:execargs()}) -> {'ok', pid()} | {'error', any()}.
schedule({Name, Time, Action}) ->
    lager:info("scheduling ~p", [Name]),
    amqp_cron:schedule_task(Name, Time, Action).

-spec get_time_token_value(kz_json:object()) -> fun(({kz_term:ne_binary(), any()}) -> time_token_value()).
get_time_token_value(JObj) ->
    fun({TokenName, Default}) ->
            parse_time_token(TokenName, JObj, Default)
    end.

-spec parse_time_token(kz_term:ne_binary(), kz_json:object(), time_token_value()) -> time_token_value().
parse_time_token(TokenName, Schedule, Default) ->
    case kz_json:get_value(TokenName, Schedule, Default) of
        <<"all">> -> 'all';
        'all' -> 'all';
        Tokens when is_list(Tokens) -> [kz_term:to_integer(X) || X <- Tokens];
        Token -> kz_term:to_integer(Token)
    end.

-spec action_fun(kz_term:ne_binary(), kz_json:object()) -> amqp_cron_callback().
action_fun(<<"check_voicemail">>, JObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, JObj),
    VmboxId = kz_json:get_value(<<"vmbox_id">>, JObj),
    {'ananke_vm_callback', 'check', [AccountId, VmboxId]};
action_fun(<<"account_crawl">>, _) ->
    {'ananke_account_crawler', 'start_task', []};
action_fun(Type, _JObj) ->
    {fun unknown_type/1, [Type]}.

-spec action_name(kz_term:ne_binary(), kz_json:object(), time()) -> kz_term:ne_binary().
action_name(ActionType, Action, Times) ->
    ActionSuffix = action_suffixes(ActionType, Action),
    kz_binary:join([ActionType | ActionSuffix] ++ [time_suffix(Times)], "-").

-spec action_suffixes(kz_term:ne_binary(), kz_json:object()) -> kz_term:ne_binaries().
action_suffixes(<<"check_voicemail">>, JObj) ->
    [kz_json:get_value(<<"account_id">>, JObj), kz_json:get_value(<<"vmbox_id">>, JObj)];
action_suffixes(_Type, _JObj) -> [].

-spec time_suffix(time()) -> kz_term:ne_binary().
time_suffix({'cron', {Minutes, Hours, MDays, Months, Weekdays}}) ->
    Time = [time_tokens_to_binary(T) || T <- [Minutes, Hours, MDays, Months, Weekdays]],
    kz_binary:join(Time, "-");
time_suffix({'oneshot', {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    Time = [kz_term:to_binary(B) || B <- [Year, Month, Day, Hour, Minute, Second]],
    kz_binary:join(Time,  "-");
time_suffix({'sleeper', MilliSeconds}) ->
    kz_term:to_binary(MilliSeconds).

-spec time_tokens_to_binary(time_token_value()) -> kz_term:ne_binary().
time_tokens_to_binary('all') -> <<"all">>;
time_tokens_to_binary(Tokens) when is_list(Tokens) ->
    kz_binary:join([kz_term:to_binary(X) || X <- Tokens], ",").

-spec unknown_type(kz_term:api_binary()) -> 'ok'.
unknown_type(Type) ->
    lager:warning("no function for type ~p", [Type]).

-spec load_schedules() -> normal.
load_schedules() ->
    timer:sleep(60 * ?MILLISECONDS_IN_SECOND),
    _ = amqp_cron:schedule_task('load_schedules'
                               ,{'oneshot', 60000}
                               ,{'gen_listener', 'cast', [?MODULE, 'load_schedules']}
                               ),
    'normal'.
