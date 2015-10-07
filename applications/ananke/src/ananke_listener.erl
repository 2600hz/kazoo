%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
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

-include("ananke.hrl").

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'notifications', [{'restrict_to', ['new_voicemail']}]}]).

-define(RESPONDERS, [{'ananke_vm_callback', [{<<"notification">>, <<"voicemail_new">>}]}
                    ]).
-define(QUEUE_NAME, <<"ananke_listener">>).
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
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?MODULE}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}
                              ,{'queue_options', ?QUEUE_OPTIONS}
                              ,{'consume_options', ?CONSUME_OPTIONS}
                             ]
                            ,[]).

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
-spec init([]) -> {'ok', #state{}}.
init([]) ->
    %% we should wait about 7-10 seconds before gen_leader syncronization
    %% and leader election
    %% after gen_leader syncronization this task will be scheduled only once
    leader_cron:schedule_task('load_schedules', {'oneshot', 60000}
                             ,{'gen_listener', 'cast', [?MODULE, 'load_schedules']}),
    {'ok', #state{}}.

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
-spec handle_call(any(), any(), #state{}) -> {'noreply', #state{}}.
handle_call(_Request, _From, State) ->
    {'noreply', State}.

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
-spec handle_cast(any(), #state{}) -> {'noreply', #state{}}.
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast('load_schedules', State) ->
    Schedules = whapps_config:get(?CONFIG_CAT, <<"schedules">>, []),
    NormalizedSchedules = lists:map(fun normalize_schedule/1, Schedules),
    _ = lists:foreach(fun schedule/1, NormalizedSchedules),
    {'noreply', State};
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
-spec handle_info(any(), #state{}) -> {'noreply', #state{}}.
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
-spec handle_event(wh_json:object(), #state{}) -> {'reply', []}.
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
-spec terminate(any(), #state{}) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec code_change(any(), #state{}, any()) -> {'ok', #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-type time() :: leader_cron_task:oneshot() | leader_cron_task:cron() | leader_cron_task:sleeper().
-type time_token_value() :: 'all' | integer() | integers().
-type leader_cron_callback() :: {atom(), atom(), list()} | {fun(), list()}.
-spec normalize_schedule(wh_json:object()) -> {ne_binary(), time(), leader_cron_callback()}.
normalize_schedule(Schedule) ->
    Action = wh_json:get_value(<<"action">>, Schedule),
    ActionType = wh_json:get_value(<<"type">>, Action),
    ActionFun = action_fun(ActionType, Action),
    TimeSchedule = time_schedule(Schedule),
    ActionName = action_name(ActionType, Action, TimeSchedule),
    {ActionName, TimeSchedule, ActionFun}.

-spec time_schedule(wh_json:object()) -> time().
time_schedule(Schedule) ->
    GetTimeTokenFun = get_time_token_value(Schedule),
    case wh_json:get_value(<<"type">>, Schedule) of
        <<"every">> ->
            [Minutes, Hours, MonthDays, Monthes, Weekdays] = lists:map(GetTimeTokenFun
                                                                       ,[{<<"minutes">>, 'all'}
                                                                         ,{<<"hours">>, 'all'}
                                                                         ,{<<"month_days">>, 'all'}
                                                                         ,{<<"monthes">>, 'all'}
                                                                         ,{<<"weekdays">>, 'all'}
                                                                        ]),
            {'cron', {Minutes, Hours, MonthDays, Monthes, Weekdays}};
        <<"once">> ->
            [Second, Minute, Hour, Day, Month, Year] = lists:map(GetTimeTokenFun
                                                                 ,[{<<"second">>, 0}
                                                                   ,{<<"minute">>, 0}
                                                                   ,{<<"hour">>, 0}
                                                                   ,{<<"day">>, 1}
                                                                   ,{<<"month">>, 1}
                                                                   ,{<<"year">>, 1970}
                                                                  ]),
            {'oneshot', {{Year, Month, Day}, {Hour, Minute, Second}}};
        <<"periodic">> ->
            [Seconds, Minutes, Hours, Days] = lists:map(GetTimeTokenFun
                                                        ,[{<<"seconds">>, 0}
                                                          ,{<<"minutes">>, 0}
                                                          ,{<<"hours">>, 0}
                                                          ,{<<"days">>, 0}
                                                         ]),
            {'sleeper', ?MILLISECONDS_IN_SECOND * (Seconds
                                                   + ?SECONDS_IN_MINUTE * Minutes
                                                   + ?SECONDS_IN_HOUR * Hours
                                                   + ?SECONDS_IN_DAY * Days)}
    end.

-spec schedule({ne_binary(), time(), fun()}) -> {'ok', pid()} | {'error', term()}.
schedule({Name, Time, Action}) ->
    lager:info("scheduling ~p", [Name]),
    leader_cron:schedule_task(Name, Time, Action).

-spec get_time_token_value(wh_json:object()) -> fun(({ne_binary(), any()}) -> time_token_value()).
get_time_token_value(JObj) ->
    fun({TokenName, Default}) ->
            parse_time_token(TokenName, JObj, Default)
    end.

-spec parse_time_token(ne_binary(), wh_json:object(), time_token_value()) -> time_token_value().
parse_time_token(TokenName, Schedule, Default) ->
    case wh_json:get_value(TokenName, Schedule, Default) of
        <<"all">> ->
            'all';
        'all' ->
            'all';
        Tokens when is_list(Tokens) ->
            [wh_util:to_integer(X) || X <- Tokens];
        Token ->
            wh_util:to_integer(Token)
    end.

-spec action_fun(ne_binary(), wh_json:object()) -> leader_cron_callback().
action_fun(<<"check_voicemail">>, JObj) ->
    AccountId = wh_json:get_value(<<"account_id">>, JObj),
    VmboxId = wh_json:get_value(<<"vmbox_id">>, JObj),
    {'ananke_vm_callback', 'check', [AccountId, VmboxId]};
action_fun(Type, _JObj) ->
    {fun unknown_type/1, [Type]}.

-spec action_name(ne_binary(), wh_json:object(), time()) -> ne_binary().
action_name(ActionType, Action, Times) ->
    ActionSuffix = action_suffixes(ActionType, Action),
    wh_util:join_binary([ActionType | ActionSuffix] ++ [time_suffix(Times)], "-").

-spec action_suffixes(ne_binary(), wh_json:object()) -> ne_binaries().
action_suffixes(<<"check_voicemail">>, JObj) ->
    [wh_json:get_value(<<"account_id">>, JObj), wh_json:get_value(<<"vmbox_id">>, JObj)];
action_suffixes(_Type, _JObj) ->
    [].

-spec time_suffix(time()) -> ne_binary().
time_suffix({'cron', {Minutes, Hours, MonthDays, Monthes, Weekdays}}) ->
    wh_util:join_binary(
      lists:map(fun time_tokens_to_binary/1
                ,[Minutes, Hours, MonthDays, Monthes, Weekdays])
      , "-");
time_suffix({'oneshot', {{Year, Month, Day}, {Hour, Minute, Second}}}) ->
    wh_util:join_binary(
      lists:map(fun wh_util:to_binary/1
                ,[Year, Month, Day, Hour, Minute, Second])
      , "-");
time_suffix({'sleeper', MilliSeconds}) ->
    wh_util:to_binary(MilliSeconds).

-spec time_tokens_to_binary(time_token_value()) -> ne_binary().
time_tokens_to_binary('all') ->
    <<"all">>;
time_tokens_to_binary(Tokens) when is_list(Tokens) ->
    wh_util:join_binary([wh_util:to_binary(X) || X <- Tokens], ",").

-spec unknown_type(api_binary()) -> 'ok'.
unknown_type(Type) ->
    lager:warning("no function for type ~p", [Type]).
