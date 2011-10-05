%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 12 June 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_temporal_route).

-include("../callflow.hrl").

-export([handle/2]).

-import(calendar, [gregorian_seconds_to_datetime/1, datetime_to_gregorian_seconds/1
                   ,date_to_gregorian_days/1, date_to_gregorian_days/3, universal_time/0
                   ,last_day_of_the_month/2, gregorian_days_to_date/1
                  ]).

-define(FIND_RULES, <<"">>).

-type improper_month() :: non_neg_integer().
-type improper_day() :: non_neg_integer().
-type improper_date() :: tuple(wh_year(), improper_month(), improper_day()).
-type strict_ordinal() :: binary(). %%<<"first">> | <<"second">> | <<"third">> | <<"fourth">> | <<"fifth">>.
-type broad_ordinal() :: binary(). %%<<"every">> | <<"last">>.
-type ordinal() :: strict_ordinal() | broad_ordinal().
-type wday() :: binary(). %%<<"monday">> | <<"tuesday">> | <<"wensday">> | <<"thursday">>
%%                 | <<"friday">> | <<"saturday">> | <<"sunday">>.
-type cycle_type() :: binary(). %%<<"date">> | <<"daily">> | <<"weekly">> | <<"monthly">> | <<"yearly">>.

-record(keys, {enable = <<"1">>
               ,disable = <<"2">>
               ,reset = <<"3">>
              }).
-record(prompts, {marked_disabled = <<"/system_media/temporal-marked_disabled">>
                  ,marked_enabled = <<"/system_media/temporal-marked_enabled">>
                  ,marker_reset = <<"/system_media/temporal-marker_reset">>
                  ,main_menu = <<"/system_media/temporal-menu">>
                 }).

-define(RULE_DEFAULT_NAME, <<"no_name">>).
-define(RULE_DEFAULT_CYCLE, <<>>).
-define(RULE_DEFAULT_INTERVAL, 1).
-define(RULE_DEFAULT_DAYS, []).
-define(RULE_DEFAULT_WDAYS, []).
-define(RULE_DEFAULT_ORDINAL, <<"first">>).
-define(RULE_DEFAULT_MONTH, 1).
-define(RULE_DEFAULT_START_DATE, {2011,1,1}).
-define(RULE_DEFAULT_WTIME_START, 0).
-define(RULE_DEFAULT_WTIME_STOP, ?SECONDS_IN_DAY).

-record(rule, {id = <<>> :: binary()
               ,enabled = undefined :: undefined | binary()
               ,name = ?RULE_DEFAULT_NAME :: binary()
               ,cycle = ?RULE_DEFAULT_CYCLE :: cycle_type()
               ,interval = ?RULE_DEFAULT_INTERVAL :: non_neg_integer()
               ,days = ?RULE_DEFAULT_DAYS :: [wh_day()]
               ,wdays = ?RULE_DEFAULT_WDAYS :: [wday()]
               ,ordinal = ?RULE_DEFAULT_ORDINAL :: ordinal()
               ,month = ?RULE_DEFAULT_MONTH :: wh_month()
               ,start_date = ?RULE_DEFAULT_START_DATE :: wh_date()
               ,wtime_start = ?RULE_DEFAULT_WTIME_START :: non_neg_integer()
               ,wtime_stop = ?RULE_DEFAULT_WTIME_STOP :: non_neg_integer()
              }).

-define(TEMPORAL_DEFAULT_TIMEZONE, <<"America/Los_Angeles">>).

-record(temporal, {local_sec = 0 :: non_neg_integer()
                   ,local_date = {2011, 1, 1} :: wh_date()
                   ,local_time = {0, 0, 0} :: wh_time()
                   ,routes = [] :: proplist()
                   ,timezone = ?TEMPORAL_DEFAULT_TIMEZONE :: binary()
                   ,prompts=#prompts{}
                   ,keys=#keys{}
                  }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return().
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    Temporal = get_temporal_route(Data, Call),
    case wh_json:get_value(<<"action">>, Data) of
        <<"menu">> ->
            ?LOG("temporal rules main menu"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            {ok, _} = temporal_route_menu(Temporal, Rules, Call),
            CFPid ! {stop};
        <<"enable">> ->
            ?LOG("force temporal rules to enable"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            {ok, _} = enable_temporal_rules(Temporal, Rules, Call),
            CFPid ! {stop};
        <<"disable">> ->
            ?LOG("force temporal rules to disable"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            {ok, _} = disable_temporal_rules(Temporal, Rules, Call),
            CFPid ! {stop};
        <<"reset">> ->
            ?LOG("resume normal temporal rule operation"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            {ok, _} = reset_temporal_rules(Temporal, Rules, Call),
            CFPid ! {stop};
        _ ->
            Rules = get_temporal_rules(Temporal, Call),
            case process_rules(Temporal, Rules, Call) of
                default ->
                    CFPid ! {continue};
                ChildId ->
                    CFPid ! {continue, ChildId}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Test all rules in reference to the current temporal routes, and
%% returns the first valid callflow, or the default.
%% @end
%%--------------------------------------------------------------------
-spec process_rules/3 :: (Temporal, Rules, Call) -> 'default' | binary() when
      Temporal :: #temporal{},
      Rules :: [#rule{},...] | [],
      Call :: #cf_call{}.
process_rules(Temporal, [#rule{enabled = <<"false">>, id=Id, name=Name}|Rs], Call) ->
    ?LOG("time based rule ~s (~s) disabled", [Id, Name]),
    process_rules(Temporal, Rs, Call);
process_rules(_, [#rule{enabled = <<"true">>, id=Id, name=Name}|_], _) ->
    ?LOG("time based rule ~s (~s) is forced active", [Id, Name]),
    Id;
process_rules(#temporal{local_sec=LSec, local_date={Y, M, D}}=T,
              [#rule{id=Id, name=Name, wtime_start=TStart, wtime_stop=TStop}=R|Rs]
              ,Call) ->
    ?LOG("processing temporal rule ~s (~s)", [Id, Name]),
    BaseDate = next_rule_date(R, {Y, M, D - 1}),
    BastTime = calendar:datetime_to_gregorian_seconds({BaseDate, {0,0,0}}),
    case {BastTime + TStart, BastTime + TStop} of
        {Start, _} when LSec < Start ->
            ?LOG("rule applies in the future ~w", [calendar:gregorian_seconds_to_datetime(Start)]),
            process_rules(T, Rs, Call);
        {_, End} when LSec > End ->
            ?LOG("rule was valid today but expired ~w", [calendar:gregorian_seconds_to_datetime(End)]),
            process_rules(T, Rs, Call);
        {_, End} ->
            ?LOG("within active time window until ~w", [calendar:gregorian_seconds_to_datetime(End)]),
            Id
    end;
process_rules(_, [], _) ->
    ?LOG("continuing with default callflow"),
    default.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds and returns a list of rule records that have do not occur in
%% the future as well as pertain to this temporal route mapping.
%% @end
%%--------------------------------------------------------------------
-spec get_temporal_rules/2 :: (Temporal, Call) -> [#rule{},...] when
      Temporal :: #temporal{},
      Call :: #cf_call{}.
get_temporal_rules(#temporal{local_sec=LSec, routes=Routes}, Call) ->
    [#rule{id = ID
           ,enabled =
               wh_json:get_binary_boolean([<<"doc">>, <<"enabled">>], R)
           ,name =
               wh_json:get_value([<<"doc">>, <<"name">>], R, ?RULE_DEFAULT_NAME)
           ,cycle =
               wh_json:get_value([<<"doc">>, <<"cycle">>], R, ?RULE_DEFAULT_CYCLE)
           ,interval =
               wh_json:get_integer_value([<<"doc">>, <<"interval">>], R, ?RULE_DEFAULT_INTERVAL)
           ,days =
               wh_json:get_value([<<"doc">>, <<"days">>], R, ?RULE_DEFAULT_DAYS)
           ,wdays =
               wh_json:get_value([<<"doc">>, <<"wdays">>], R, ?RULE_DEFAULT_WDAYS)
           ,ordinal =
               wh_json:get_value([<<"doc">>, <<"ordinal">>], R, ?RULE_DEFAULT_ORDINAL)
           ,month =
               wh_json:get_value([<<"doc">>, <<"month">>], R, ?RULE_DEFAULT_MONTH)
           ,start_date =
               get_date(wh_json:get_integer_value([<<"doc">>, <<"start_date">>], R, LSec))
           ,wtime_start =
               wh_json:get_integer_value([<<"doc">>, <<"time_window_start">>], R, ?RULE_DEFAULT_WTIME_START)
           ,wtime_stop =
               wh_json:get_integer_value([<<"doc">>, <<"time_window_stop">>], R, ?RULE_DEFAULT_WTIME_STOP)
          }
     || R <- cf_attributes:temporal_rules(Call),
        lists:member(ID = wh_json:get_value(<<"id">>, R), Routes)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads the temporal record with data from the db.
%% @end
%%--------------------------------------------------------------------
-spec get_temporal_route/2 :: (JObj, Call) -> #temporal{} when
      JObj :: json_object(),
      Call :: #cf_call{}.
get_temporal_route(JObj, #cf_call{cf_pid=CFPid}) ->
    ?LOG("loading temporal route"),

    CFPid ! {get_branch_keys},
    load_current_time(#temporal{routes =
                                    receive {branch_keys, Keys} -> Keys after 1000 -> [] end
                                ,timezone =
                                    wh_json:get_value(<<"timezone">>, JObj, ?TEMPORAL_DEFAULT_TIMEZONE)
                               }).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts a term and tries to convert it to a wh_date()
%% @end
%%--------------------------------------------------------------------
-spec get_date/1 :: (Seconds) -> wh_date() when
      Seconds :: non_neg_integer().
get_date(Seconds) when is_integer(Seconds) ->
    {Date, _} = calendar:gregorian_seconds_to_datetime(Seconds),
    Date.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Present the caller with the option to enable, disable, or reset
%% the provided temporal rules.
%% @end
%%--------------------------------------------------------------------
-spec temporal_route_menu/3 :: (Temporal, Rules, Call) -> cf_api_std_return() when
      Temporal :: #temporal{},
      Rules :: [#rule{},...],
      Call :: #cf_call{}.
temporal_route_menu(#temporal{keys=#keys{enable=Enable, disable=Disable, reset=Reset}
                              ,prompts=Prompts}=Temporal, Rules, Call) ->
    case cf_call_command:b_play_and_collect_digit(Prompts#prompts.main_menu, Call) of
        {ok, Enable} ->
            enable_temporal_rules(Temporal, Rules, Call);
        {ok, Disable} ->
            disable_temporal_rules(Temporal, Rules, Call);
        {ok, Reset} ->
            reset_temporal_rules(Temporal, Rules, Call);
        {error, _} ->
	    {ok, wh_json:new()};
        {ok, _} ->
            temporal_route_menu(Temporal, Rules, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%--------------------------------------------------------------------
-spec disable_temporal_rules/3 :: (Temporal, Rules, Call) -> cf_api_std_return() when
      Temporal :: #temporal{},
      Rules :: [#rule{},...] | [],
      Call :: #cf_call{}.
disable_temporal_rules(#temporal{prompts=#prompts{marked_disabled=Disabled}}, [], Call) ->
    cf_call_command:b_play(Disabled, Call);
disable_temporal_rules(Temporal, [Id|T]=Rules, #cf_call{account_db=Db}=Call) ->
    try
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        case couch_mgr:save_doc(Db, wh_json:set_value(<<"enabled">>, false, JObj)) of
            {ok, _} ->
                ?LOG("set temporal rule ~s to disabled", [Id]),
                disable_temporal_rules(Temporal, T, Call);
            {error, conflict} ->
                ?LOG("conflict during disable of temporal rule ~s, trying again", [Id]),
                disable_temporal_rules(Temporal, Rules, Call);
            {error, R1} ->
                ?LOG("unable to update temporal rule ~s, ~p",[Id, R1]),
                disable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            ?LOG("unable to update temporal rules ~p",[R2]),
            disable_temporal_rules(Temporal, T, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%--------------------------------------------------------------------
-spec reset_temporal_rules/3 :: (Temporal, Rules, Call) -> cf_api_std_return() when
      Temporal :: #temporal{},
      Rules :: [#rule{},...] | [],
      Call :: #cf_call{}.
reset_temporal_rules(#temporal{prompts=#prompts{marker_reset=Reset}}, [], Call) ->
    cf_call_command:b_play(Reset, Call);
reset_temporal_rules(Temporal, [Id|T]=Rules, #cf_call{account_db=Db}=Call) ->
    try
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        case couch_mgr:save_doc(Db, wh_json:delete_key(<<"enabled">>, JObj)) of
            {ok, _} ->
                ?LOG("reset temporal rule ~s", [Id]),
                reset_temporal_rules(Temporal, T, Call);
            {error, conflict} ->
                ?LOG("conflict during reset of temporal rule ~s, trying again", [Id]),
                reset_temporal_rules(Temporal, Rules, Call);
            {error, R1} ->
                ?LOG("unable to reset temporal rule ~s, ~p",[Id, R1]),
                reset_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            ?LOG("unable to reset temporal rule ~s ~p",[Id, R2]),
            reset_temporal_rules(Temporal, T, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%--------------------------------------------------------------------
-spec enable_temporal_rules/3 :: (Temporal, Rules, Call) -> cf_api_std_return() when
      Temporal :: #temporal{},
      Rules :: [#rule{},...] | [],
      Call :: #cf_call{}.
enable_temporal_rules(#temporal{prompts=#prompts{marked_enabled=Enabled}}, [], Call) ->
    cf_call_command:b_play(Enabled, Call);
enable_temporal_rules(Temporal, [Id|T]=Rules, #cf_call{account_db=Db}=Call) ->
    try
        {ok, JObj} = couch_mgr:open_doc(Db, Id),
        case couch_mgr:save_doc(Db, wh_json:set_value(<<"enabled">>, true, JObj)) of
            {ok, _} ->
                ?LOG("set temporal rule ~s to enabled active", [Id]),
                enable_temporal_rules(Temporal, T, Call);
            {error, conflict} ->
                ?LOG("conflict during enable of temporal rule ~s, trying again", [Id]),
                enable_temporal_rules(Temporal, Rules, Call);
            {error, R1} ->
                ?LOG("unable to enable temporal rule ~s, ~p",[Id, R1]),
                enable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            ?LOG("unable to enable temporal rule ~s ~p",[Id, R2]),
            enable_temporal_rules(Temporal, T, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determines the appropriate gregorian seconds to be used as the
%% current date/time for this temporal route selection
%% @end
%%--------------------------------------------------------------------
-spec load_current_time/1 :: (#temporal{}) -> #temporal{}.
load_current_time(#temporal{timezone=Timezone}=Temporal)->
    {LocalDate, LocalTime} = localtime:utc_to_local(
			       calendar:universal_time()
                               ,wh_util:to_list(Timezone)
                              ),
    ?LOG("local time for ~s is {~w,~w}", [Timezone, LocalDate, LocalTime]),
    Temporal#temporal{local_sec=calendar:datetime_to_gregorian_seconds({LocalDate, LocalTime})
                      ,local_date=LocalDate
                      ,local_time=LocalTime}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% The big daddy
%% Calculates the date of the next event given the type, interval,
%% rule, start date, and current date.
%%
%% GOTCHA!
%% Weird predictions? Bet your weekdays or days are not in order....
%%   - monday, tuesday, wensday, thursday, friday, saturday, sunday
%%   - 1,2,3..31
%% @end
%%--------------------------------------------------------------------
-spec next_rule_date/2 :: (Rule, Current) -> wh_date() when
      Rule :: #rule{},
      Current :: wh_date().
next_rule_date(#rule{cycle = <<"date">>, start_date=Date0}, _) ->
    Date0;

next_rule_date(#rule{cycle = <<"daily">>, interval=I0
                     ,start_date={Y0, M0, D0}}, {Y1, M1, D1}) ->
    %% Calculate the distance in days as a function of
    %%   the interval and fix
    DS0 = date_to_gregorian_days({Y0, M0, D0}),
    DS1 = date_to_gregorian_days({Y1, M1, D1}),
    Offset = trunc( ( DS1 - DS0 ) / I0 ) * I0,
    normalize_date({Y0, M0, D0 + Offset + I0});

next_rule_date(#rule{cycle = <<"weekly">>, interval=I0
                     ,wdays=Weekdays, start_date={Y0, M0, D0}}, {Y1, M1, D1}) ->
    DOW0 = day_of_the_week({Y1, M1, D1}),
    Distance = iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}),
    Offset = trunc( Distance / I0 ) * I0,
    case [ DOW1 || DOW1 <- [to_dow(D) || D <- Weekdays], DOW1 > DOW0 ] of
        %% During an 'active' week but before the last weekday in the list
        %%   move to the next day this week
        [Day|_] when Distance =:= Offset ->
            normalize_date({Y1, M1, D1 + Day - DOW0});
        %% Empty list:
        %%   The last DOW during an 'active' week,
        %% Non Empty List that failed the guard:
        %%   During an 'inactive' week
        _ ->
            {WY0, W0} = iso_week_number({Y0, M0, D0}),
            {Y2, M2, D2} = iso_week_to_gregorian_date({WY0, W0 + Offset + I0}),
            normalize_date({Y2, M2, ( D2 - 1 ) + to_dow( hd( Weekdays ) )})
    end;

next_rule_date(#rule{cycle = <<"monthly">>, interval=I0, ordinal = <<"every">>
                         ,wdays=[Weekday], start_date={Y0, M0, _}}, {Y1, M1, D1}) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso find_next_weekday({Y1, M1, D1}, Weekday) of
        %% If the next occurence of the weekday is during an 'active' month
        %%   and does not span the current month/year then it is correct
        {Y1, M1, _}=Date ->
            Date;
        %% In the special case were the next weekday does span the current
        %%   month/year but it should be every month (I0 == 1) then the
        %%   date is also correct
        {_,_,_}=Date when I0 =:= 1 ->
            Date;
        %% During an 'inactive' month, or when it inappropriately spans
        %%   a month/year boundary calculate the next iteration
        _ ->
            find_ordinal_weekday(Y0, M0 + Offset + I0, Weekday, <<"first">>)
    end;

next_rule_date(#rule{cycle = <<"monthly">>, interval=I0, ordinal = <<"last">>
                         ,wdays=[Weekday], start_date={Y0, M0, _}}, {Y1, M1, D1}) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso find_last_weekday({Y1, M1, 1}, Weekday) of
        %% If today is before the occurace day on an 'active' month since
        %%   the 'last' only happens once per month if we havent passed it
        %%   then it must be this month
        {_, _, D2}=Date when D1 < D2 ->
            Date;
        %% In an 'inactive' month or when we have already passed
        %%   the last occurance of the DOW
        _ ->
            find_last_weekday({Y0, M0 + Offset + I0, 1}, Weekday)
    end;

%% WARNING: There is a known bug when requesting the fifth occurance
%%   of a weekday when I0 > 1 and the current month only has four instances
%%   of the given weekday, the calculation is incorrect.  I was told not
%%   to worry about that now...
next_rule_date(#rule{cycle = <<"monthly">>, interval=I0, ordinal=Ordinal
                     ,wdays=[Weekday], start_date={Y0, M0, _}}, {Y1, M1, D1}) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso {find_ordinal_weekday(Y1, M1, Weekday, Ordinal), I0} of
        %% If today is before the occurance day on an 'active' month and
        %%   the occurance does not cross month/year boundaries then the
        %%   calculated date is accurate
        {{_, M1, D2}=Date, _} when D1 < D2, I0 > 1 ->
            Date;
        %% If today is before the occurance day on an 'active' month and
        %%   the iterval =:= 1 then it happens every month so it doesnt
        %%   matter if it crosses month/year boundaries
        {{_, M2, D2}=Date, 1} when D1 < D2; M1 < M2 ->
            Date;
        %% false:
        %%   In an 'inactive' month
        %% {wh_date(), integer()}:
        %%   We have already passed the last occurance of the DOW
        _ ->
            find_ordinal_weekday(Y0, M0 + Offset + I0, Weekday, Ordinal)
    end;

next_rule_date(#rule{cycle = <<"monthly">>, interval=I0
                     ,days=Days, start_date={Y0, M0, _}}, {Y1, M1, D1}) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case [D || D <- Days, D > D1] of
        %% The day hasn't happend on an 'active' month
        [Day|_] when Distance =:= Offset ->
            normalize_date({Y0, M0 + Offset, Day});
        %% Empty List:
        %%   All of the days in the list have already happened
        %% Non Empty List that failed the guard:
        %%   The day hasn't happend on an 'inactive' month
        _ ->
            normalize_date({Y0, M0 + Offset + I0, hd( Days )})
    end;

%% WARNING: This function does not ensure the provided day actually
%%   exists in the month provided.  For temporal routes that isnt
%%   an issue because we will 'pass' the invalid date and compute
%%   the next
next_rule_date(#rule{cycle = <<"yearly">>, interval=I0, month=Month
                 ,days=[Day], start_date={Y0, _, _}}, {Y1, M1, D1}) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    %% If it is currently an 'active' year before the requested
    %%   month or on the requested month but before the day
    %%   then just provide the occurance.  Otherwise work out
    %%   the next occurance.
    case M1 =< Month andalso D1 < Day of
        %% It is before the requested month or
        %%   on the requested month but before the day
        %%   but only on an 'active' year
        true when Distance =:= Offset -> {Y1, Month, Day};
        %% true:
        %%   The guard failed, so this is an 'inactive'
        %%   month.
        %% false:
        %%   The requested month or day in that month has
        %%   already passed
        _ -> {Y0 + Offset + I0, Month, Day}
    end;

next_rule_date(#rule{cycle = <<"yearly">>, interval=I0, ordinal = <<"every">>
                     ,month=Month, wdays=[Weekday], start_date={Y0, _, _}}, {Y1, M1, D1}) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso find_next_weekday({Y1, Month, D1}, Weekday) of
        %% During an 'active' year before the target month the calculated
        %%   occurance is accurate
        {Y1, Month, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurance day it is accurate
        {Y1, Month, D2}=Date when M1 =:= Month, D1 < D2 ->
            Date;
        %% During an 'inactive' year, or after the target month
        %%   calculate the next iteration
        _ ->
            find_ordinal_weekday(Y0 + Offset + I0, Month, Weekday, <<"first">>)
    end;

next_rule_date(#rule{cycle = <<"yearly">>, interval=I0, ordinal = <<"last">>
                         ,month=Month, wdays=[Weekday], start_date={Y0, _, _}}, {Y1, M1, D1}) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso find_last_weekday({Y1, Month, 1}, Weekday) of
        %% During an 'active' year before the target month the calculated
        %%   occurance is accurate
        {Y1, _, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurance day it is accurate
        {Y1, _, D2}=Date when M1 =:= Month, D1 < D2 ->
            Date;
        %% During an 'inactive' year, or after the target month
        %%   calculate the next iteration
        _ ->
            find_last_weekday({Y0 + Offset + I0, Month, 1}, Weekday)
    end;

next_rule_date(#rule{cycle = <<"yearly">>, interval=I0, ordinal=Ordinal
                     ,month=Month, wdays=[Weekday], start_date={Y0, _, _}}, {Y1, M1, D1}) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset andalso find_ordinal_weekday(Y1, Month, Weekday, Ordinal) of
        %% During an 'active' year before the target month the calculated
        %%   occurance is accurate
        {Y1, Month, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurance day it is accurate
        {Y1, Month, D2}=Date when M1 =:= Month, D1 < D2 ->
            Date;
        %% During an 'inactive' year or after the calculated
        %%   occurance determine the next iteration
        _ ->
            find_ordinal_weekday(Y0 + Offset + I0, Month, Weekday, Ordinal)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes dates, for example corrects for months that are given
%% with more days then they have (ie: {2011, 1, 36} -> {2011, 2, 5}).
%% I have been refering to this as 'spanning a month/year border'
%% @end
%%--------------------------------------------------------------------
-spec normalize_date/1 :: (Date :: improper_date()) -> wh_date().
normalize_date({Y, M, D}) when M =:= 13 ->
    normalize_date({Y + 1, 1, D});
normalize_date({Y, M, D}) when M > 12 ->
    normalize_date({Y + 1, M - 12, D});
normalize_date({Y, M, D}=Date) ->
    case last_day_of_the_month(Y, M) of
        Days when D > Days ->
            normalize_date({Y, M + 1, D - Days});
        _ ->
            Date
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the ordinal words to cardinal numbers representing
%% the position
%% @end
%%--------------------------------------------------------------------
-spec from_ordinal/1 :: (Ordinal :: strict_ordinal()) -> 0..4.
from_ordinal(<<"first">>) -> 0;
from_ordinal(<<"second">>) -> 1;
from_ordinal(<<"third">>) -> 2;
from_ordinal(<<"fourth">>) -> 3;
from_ordinal(<<"fifth">>) -> 4.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Map the days of the week to cardinal numbers representing the
%% position, in accordance with ISO 8601
%% @end
%%--------------------------------------------------------------------
-spec to_dow/1 :: (wday()) -> wh_daynum().
to_dow(<<"monday">>) -> 1;
to_dow(<<"tuesday">>) -> 2;
to_dow(<<"wensday">>) -> 3;
to_dow(<<"thursday">>) -> 4;
to_dow(<<"friday">>) -> 5;
to_dow(<<"saturday">>) -> 6;
to_dow(<<"sunday">>) -> 7.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates the date of the next occurance of a weekday from the given
%% start date.
%%
%% NOTICE!
%% It is possible for this function to cross month/year boundaries.
%% @end
%%--------------------------------------------------------------------
-spec find_next_weekday/2 :: (Date, Weekday) -> wh_date() when
      Date :: wh_date(),
      Weekday :: wday().
find_next_weekday({Y, M, D}, Weekday) ->
    RefDOW = to_dow(Weekday),
    case day_of_the_week({Y, M, D}) of
        %% Today is the DOW we wanted, calculate for next week
        RefDOW ->
            normalize_date({Y, M, D + 7});
        %% If the DOW has not occured this week yet
        DOW when RefDOW > DOW ->
            normalize_date({Y, M, D + (RefDOW - DOW)});
        %% If the DOW occurance has already happend, calculate
        %%   for the next week using the current DOW as a reference
        DOW ->
            normalize_date({Y, M, D + ( 7 - DOW ) + RefDOW})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Safety wrapper on date_of_dow used to loop over failing attempts
%% until the date can be calculated.  The date can be provided as an
%% improper date.
%%
%% NOTICE!
%% It is possible for this function to cross month/year boundaries.
%% @end
%%--------------------------------------------------------------------
-spec find_ordinal_weekday/4 :: (Year, Month, Weekday, Ordinal) -> wh_date() when
      Year :: wh_year(),
      Month :: improper_month(),
      Weekday :: wday(),
      Ordinal :: strict_ordinal().
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) when M1 =:= 13 ->
    find_ordinal_weekday(Y1 + 1, 1, Weekday, Ordinal);
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) when M1 > 12 ->
    find_ordinal_weekday(Y1 + 1, M1 - 12, Weekday, Ordinal);
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) ->
    try
        date_of_dow(Y1, M1, Weekday, Ordinal)
    catch
        _:_ ->
            find_ordinal_weekday(Y1, M1 + 1, Weekday, Ordinal)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates the date of the last occurance of a weekday within a
%% given month/year.  The date can be provided as an improper date.
%%
%% Assumption/Principle:
%%   A DOW can never occur more than four times in a month.
%% ---------------------------------------------------------
%% First attempt to calulate the date of the fouth DOW
%% occurance.  Since the function corrects an invalid
%% date by crossing month/year boundries, cause a badmatch
%% if this happens. Therefore, during the exception the last
%% occurance MUST be in the third week.
%% @end
%%--------------------------------------------------------------------
-spec find_last_weekday/2 :: (Date, Weekday) -> wh_date() when
      Date :: improper_date(),
      Weekday :: wday().
find_last_weekday({Y, M, D}, Weekday) when M =:= 13 ->
    find_last_weekday({Y + 1, 1, D}, Weekday);
find_last_weekday({Y, M, D}, Weekday) when M > 12 ->
    find_last_weekday({Y + 1, M - 12, D}, Weekday);
find_last_weekday({Y, M, _}, Weekday) ->
    try
        {Y, M, _} = date_of_dow(Y, M, Weekday, <<"fifth">>)
    catch
        _:_ ->
            date_of_dow(Y, M, Weekday, <<"fourth">>)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unsafe calculation of the date for a specific day of the week, this
%% function will explode on occasion.
%% @end
%%--------------------------------------------------------------------
-spec date_of_dow/4 :: (Year, Month, Weekday, Ordinal) -> wh_date() when
      Year :: wh_year(),
      Month :: improper_month(),
      Weekday :: wday(),
      Ordinal :: strict_ordinal().
date_of_dow(Year, 1, Weekday, Ordinal) ->
    date_of_dow(Year - 1, 13, Weekday, Ordinal);
date_of_dow(Year, Month, Weekday, Ordinal) ->
    RefDate = {Year, Month - 1, last_day_of_the_month(Year, Month - 1)},
    RefDays = date_to_gregorian_days(RefDate),
    DOW = to_dow(Weekday),
    Occurance = from_ordinal(Ordinal),
    Days = case day_of_the_week(RefDate) of
               DOW ->
                   RefDays + 7 + (7 * Occurance );
               RefDOW when DOW < RefDOW ->
                   RefDays + DOW + (7 - RefDOW) + (7 * Occurance);
               RefDOW ->
                   RefDays + abs(DOW - RefDOW) + (7 * Occurance)
          end,
    {Y, M, D} = gregorian_days_to_date(Days),
    normalize_date({Y, M, D}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates the distance, in total ISO weeks, between two dates
%%
%% I rather dislike this approach, but it is the best of MANY evils that I came up with...
%% The idea here is to find the difference (in days) between the ISO 8601 mondays
%% of the start and end dates.  This takes care of all the corner cases for us such as:
%%    - Start date in ISO week of previous year
%%    - End date in ISO week of previous year
%%    - Spanning years
%% All while remaining ISO 8601 compliant.
%% @end
%%--------------------------------------------------------------------
-spec iso_week_difference/2 :: (Week0, Week1) -> non_neg_integer() when
      Week0 :: wh_date(),
      Week1 :: wh_date().
iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}) ->
    DS0 = date_to_gregorian_days(iso_week_to_gregorian_date(iso_week_number({Y0, M0, D0}))),
    DS1 = date_to_gregorian_days(iso_week_to_gregorian_date(iso_week_number({Y1, M1, D1}))),
    trunc( abs( DS0 - DS1 ) / 7 ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Caclulates the gregorian date of a given ISO 8601 week
%% @end
%%--------------------------------------------------------------------
-spec iso_week_to_gregorian_date/1 :: (wh_iso_week()) -> wh_date().
iso_week_to_gregorian_date({Year, Week}) ->
    Jan1 = date_to_gregorian_days(Year, 1, 1),
    Offset = 4 - day_of_the_week(Year, 1, 4),
    Days =
        if
            Offset =:= 0 ->
                Jan1 + ( Week * 7 );
            true ->
                Jan1 + Offset + ( ( Week - 1 ) * 7 )
        end,
    gregorian_days_to_date(Days).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for calender:iso_week_number introduced in R14B02 (?) using
%% a local copy on older systems
%% @end
%%--------------------------------------------------------------------
-spec iso_week_number/1 :: (wh_date()) -> wh_iso_week().
iso_week_number(Date) ->
    case erlang:function_exported(calendar, iso_week_number, 1) of
	true -> calendar:iso_week_number(Date);
	false -> our_iso_week_number(Date)
    end.

-spec day_of_the_week/1 :: (wh_date()) -> wh_day().
day_of_the_week({Year, Month, Day}=Date) ->
    case erlang:function_exported(calendar, day_of_the_week, 1) of
	true -> calendar:day_of_the_week(Date);
	false -> our_day_of_the_week(Year, Month, Day)
    end.

-spec day_of_the_week/3 :: (wh_year(), wh_month(), wh_day()) -> wh_day().
day_of_the_week(Year, Month, Day) ->
    case erlang:function_exported(calendar, day_of_the_week, 3) of
	true -> calendar:day_of_the_week(Year, Month, Day);
	false -> our_day_of_the_week(Year, Month, Day)
    end.

%% TAKEN FROM THE R14B02 SOURCE FOR calender.erl
our_iso_week_number({Year,_Month,_Day}=Date) ->
    D = calendar:date_to_gregorian_days(Date),
    W01_1_Year = gregorian_days_of_iso_w01_1(Year),
    W01_1_NextYear = gregorian_days_of_iso_w01_1(Year + 1),
    if W01_1_Year =< D andalso D < W01_1_NextYear ->
	    % Current Year Week 01..52(,53)
	    {Year, (D - W01_1_Year) div 7 + 1};
	D < W01_1_Year ->
	    % Previous Year 52 or 53
	    PWN = case day_of_the_week(Year - 1, 1, 1) of
		4 -> 53;
		_ -> case day_of_the_week(Year - 1, 12, 31) of
			4 -> 53;
			_ -> 52
		     end
		end,
	    {Year - 1, PWN};
	W01_1_NextYear =< D ->
	    % Next Year, Week 01
	    {Year + 1, 1}
    end.

-spec gregorian_days_of_iso_w01_1/1 :: (calendar:year()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
    D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
    DOW = calendar:day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
	D0101 - DOW + 1;
    true ->
	D0101 + 7 - DOW + 1
    end.

%% day_of_the_week(Year, Month, Day)
%% day_of_the_week({Year, Month, Day})
%%
%% Returns: 1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7.
-spec our_day_of_the_week/3 :: (calendar:year(), calendar:month(), calendar:day()) -> calendar:daynum().
our_day_of_the_week(Year, Month, Day) ->
    (calendar:date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

daily_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,2}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2011,6,1}}, {2011,6,1})),
    %%  increment over month boundary
    ?assertEqual({2011,2,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,7,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2011,6,1}}, {2011,6,30})),
    %% increment over year boundary
    ?assertEqual({2011,1,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2010,6,1}}, {2010,12,31})),
    %% leap year (into)
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2008,1,1}}, {2008,2,28})),
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2008,1,1}}, {2008,2,28})),
    %% leap year (over)
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2008,1,1}}, {2008,2,29})),
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2008,1,1}}, {2008,2,29})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2008,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2009,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"daily">>, start_date={2010,1,1}}, {2011,1,1})),
    %% even step (small)
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,1,1}}, {2011,1,29})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,6,5}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,7,3}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,6,1}}, {2011,6,29})),
    %% odd step (small)
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"daily">>, interval=7, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"daily">>, interval=7, start_date={2011,1,1}}, {2011,1,29})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"daily">>, interval=7, start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,6,8}, next_rule_date(#rule{cycle = <<"daily">>, interval=7, start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,7,6}, next_rule_date(#rule{cycle = <<"daily">>, interval=7, start_date={2011,6,1}}, {2011,6,29})),
    %% even step (large)
    ?assertEqual({2011,2,18}, next_rule_date(#rule{cycle = <<"daily">>, interval=48, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"daily">>, interval=48, start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,7,19}, next_rule_date(#rule{cycle = <<"daily">>, interval=48, start_date={2011,6,1}}, {2011,6,1})),
    %% odd step (large)
    ?assertEqual({2011,3,27}, next_rule_date(#rule{cycle = <<"daily">>, interval=85, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"daily">>, interval=85, start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,8,25}, next_rule_date(#rule{cycle = <<"daily">>, interval=85, start_date={2011,6,1}}, {2011,6,1})),
    %% current date on (interval)
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,1,5}}, {2011,1,5})),
    %% current date after (interval)
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,1,5}}, {2011,1,6})),
    %% shift start date
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,2,1}}, {2011,2,3})),
    ?assertEqual({2011,2,6}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={2011,2,2}}, {2011,2,3})),
    %% long span
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,4,12}, next_rule_date(#rule{cycle = <<"daily">>, interval=4, start_date={1983,4,11}}, {2011,4,11})).


weekly_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %%  increment over month boundary
    ?assertEqual({2011,2,7}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,1}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,25})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,26})),
    ?assertEqual({2011,2,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,27})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,28})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,29})),
    ?assertEqual({2011,2,6}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,30})),
    %%  increment over year boundary
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2010,1,1}}, {2010,12,27})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"tuesday">>], start_date={2010,1,1}}, {2010,12,28})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"wensday">>], start_date={2010,1,1}}, {2010,12,29})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"thursday">>], start_date={2010,1,1}}, {2010,12,30})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,1}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"saturday">>], start_date={2010,1,1}}, {2010,12,25})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"sunday">>], start_date={2010,1,1}}, {2010,12,26})),
    %%  leap year (into)
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,28})),
    %%  leap year (over)
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,28})),
    ?assertEqual({2008,3,7}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,29})),
    %% current date on (simple)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,4})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,13}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,6})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,7})),
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2009,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>], start_date={2010,1,2}}, {2011,1,1})),
    %% multiple DOWs
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>,<<"tuesday">>,<<"wensday">>,<<"thursday">>,<<"friday">>,<<"saturday">>,<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>,<<"tuesday">>,<<"wensday">>,<<"thursday">>,<<"friday">>,<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"tuesday">>,<<"wensday">>,<<"thursday">>,<<"friday">>,<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"wensday">>,<<"thursday">>,<<"friday">>,<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"thursday">>,<<"friday">>,<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"friday">>,<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% last DOW of an active week
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, wdays=[<<"monday">>,<<"tuesday">>,<<"wensday">>,<<"thursday">>,<<"friday">>], start_date={2011,1,1}}, {2011,1,7})),
    %% even step (small)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,13}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    %%                and google calendar agrees (thunderbird and outlook be damned!)
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,16}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% odd step (small)
    ?assertEqual({2011,1,17}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,21}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,22}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,23}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% even step (large)
    ?assertEqual({2011,6,13}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,14}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,15}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,16}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,17}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,18}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,6,19}, next_rule_date(#rule{cycle = <<"weekly">>, interval=24, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% odd step (large)
    ?assertEqual({2011,9,12}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,13}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,14}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,15}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,16}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,17}, next_rule_date(#rule{cycle = <<"weekly">>, interval=37, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    %%     SIDE NOTE: No event engines seem to agree on this case, so I am doing what makes sense to me
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=36, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,9,11}, next_rule_date(#rule{cycle = <<"weekly">>, interval=36, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% multiple DOWs with step (currently on start)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,4})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,6})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,7})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,8})),
    %% multiple DOWs with step (start in past)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,9})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,10})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,11})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,12})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,13})),
    ?assertEqual({2011,1,24}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,14})),
    ?assertEqual({2011,1,24}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,15})),
    ?assertEqual({2011,1,24}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,16})),
    %% multiple DOWs over month boundary
    ?assertEqual({2011,2,7}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2011,1,1}}, {2011,1,28})),
    %% multiple DOWs over year boundary
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=2, wdays=[<<"monday">>, <<"wensday">>, <<"friday">>], start_date={2010,1,1}}, {2010,12,31})),
    %% current date on (interval)
    ?assertEqual({2011,1,17}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,4})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,6})),
    ?assertEqual({2011,1,21}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,7})),
    ?assertEqual({2011,1,22}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,23}, next_rule_date(#rule{cycle = <<"weekly">>, interval=3, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    %% shift start date
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"monday">>], start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"tuesday">>], start_date={2005,2,8}}, {2011,1,1})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"wensday">>], start_date={2006,3,15}}, {2011,1,1})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"thursday">>], start_date={2007,4,22}}, {2011,1,1})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"friday">>], start_date={2008,5,29}}, {2011,1,1})),
    ?assertEqual({2011,1,22}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"saturday">>], start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=5, wdays=[<<"sunday">>], start_date={2010,7,8}}, {2011,1,1})),
    %% long span
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"weekly">>, interval=4, wdays=[<<"monday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,5,2}, next_rule_date(#rule{cycle = <<"weekly">>, interval=4, wdays=[<<"monday">>], start_date={1983,4,11}}, {2011,4,11})).

monthly_every_recurrence_test() ->
    %% basic increment (also crosses month boundary)
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,17}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,10})),
    ?assertEqual({2011,1,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,17})),
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,24})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,4})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,11})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,18})),
    ?assertEqual({2011,2,1}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,25})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,12})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,19})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,26})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,13}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,6})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,13})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,20})),
    ?assertEqual({2011,2,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,27})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,7})),
    ?assertEqual({2011,1,21}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,14})),
    ?assertEqual({2011,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,21})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,28})),
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,8})),
    ?assertEqual({2011,1,22}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,15})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,22})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,29})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,1,16}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,9})),
    ?assertEqual({2011,1,23}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,16})),
    ?assertEqual({2011,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,23})),
    ?assertEqual({2011,2,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,30})),
    %% increment over year boundary
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2010,1,1}}, {2010,12,27})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2010,1,1}}, {2010,12,28})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2010,1,1}}, {2010,12,29})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2010,1,1}}, {2010,12,30})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,1}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2010,1,1}}, {2010,12,25})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2010,1,1}}, {2010,12,26})),
    %% leap year (into)
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,28})),
    %% leap year (over)
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,28})),
    %% current date on (simple)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,10}}, {2011,1,11})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,17}}, {2011,1,19})),
    %% current date after (simple)
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,10}}, {2011,1,14})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,17}}, {2011,1,21})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2005,2,1}}, {2011,1,4})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2006,3,1}}, {2011,1,12})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2007,4,1}}, {2011,1,20})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2008,5,1}}, {2011,1,28})),
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2010,7,1}}, {2011,1,2})),
    %% even step (small)
    ?assertEqual({2011,3,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,25})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,26})),
    ?assertEqual({2011,3,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,27})),
    ?assertEqual({2011,3,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,28})),
    ?assertEqual({2011,3,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,29})),
    ?assertEqual({2011,3,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,30})),
    %% odd step (small)
    ?assertEqual({2011,9,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,6,1}}, {2011,6,27})),
    ?assertEqual({2011,9,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,6,1}}, {2011,6,28})),
    ?assertEqual({2011,9,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,6,1}}, {2011,6,29})),
    ?assertEqual({2011,9,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2011,6,1}}, {2011,6,30})),
    ?assertEqual({2011,9,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2011,6,1}}, {2011,6,24})),
    ?assertEqual({2011,9,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2011,6,1}}, {2011,6,25})),
    ?assertEqual({2011,9,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2011,6,1}}, {2011,6,26})),
    %% current date on (interval)
    ?assertEqual({2011,5,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,5,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,10}}, {2011,1,25})),
    ?assertEqual({2011,5,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,17}}, {2011,1,26})),
    %% current date after (interval)
    ?assertEqual({2011,5,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,2,2})),
    ?assertEqual({2011,5,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2011,1,10}}, {2011,3,14})),
    ?assertEqual({2011,5,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2011,1,17}}, {2011,3,21})),
    %% shift start date
    ?assertEqual({2011,2,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,5,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"tuesday">>], start_date={2005,2,1}}, {2011,1,1})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"wensday">>], start_date={2006,3,1}}, {2011,1,1})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"thursday">>], start_date={2007,4,1}}, {2011,1,1})),
    ?assertEqual({2011,4,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"friday">>], start_date={2008,5,1}}, {2011,1,1})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"saturday">>], start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,5,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"sunday">>], start_date={2010,7,1}}, {2011,1,1})),
    %% long span
    ?assertEqual({2011,3,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"every">>, wdays=[<<"monday">>], start_date={1983,4,11}}, {2011,1,1})).

monthly_last_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% basic increment (mid year)
    ?assertEqual({2011,6,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,6,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,6,1}}, {2011,6,1})),
    %% increment over month boundary
    ?assertEqual({2011,2,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,22}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,23}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,2,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% increment over year boundary
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2010,1,1}}, {2010,12,31})),
    ?assertEqual({2011,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2010,1,1}}, {2010,12,31})),
    %% leap year
    ?assertEqual({2008,2,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,23}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2004,12,1}}, {2011,1,1})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2005,10,1}}, {2011,1,1})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2006,11,1}}, {2011,1,1})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2007,9,1}}, {2011,1,1})),
    ?assertEqual({2011,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2008,8,1}}, {2011,1,1})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2009,7,1}}, {2011,1,1})),
    ?assertEqual({2011,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2010,6,1}}, {2011,1,1})),
    %% even step (small)
    ?assertEqual({2011,3,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,31}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step (small)
    ?assertEqual({2011,4,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,24}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% even step (large)
    ?assertEqual({2014,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=36, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step (large)
    ?assertEqual({2014,2,24}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,22}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2014,2,23}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% shift start date
    ?assertEqual({2011,3,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={2010,12,1}}, {2011,1,1})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={2010,10,1}}, {2011,1,1})),
    ?assertEqual({2011,2,23}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={2010,11,1}}, {2011,1,1})),
    ?assertEqual({2011,3,31}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={2010,9,1}}, {2011,1,1})),
    ?assertEqual({2011,2,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={2010,8,1}}, {2011,1,1})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={2010,7,1}}, {2011,1,1})),
    ?assertEqual({2011,3,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={2010,6,1}}, {2011,1,1})),
    %% long span
    ?assertEqual({2011,3,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"monday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"tuesday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"wensday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,31}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"thursday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"friday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"saturday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"last">>, wdays=[<<"sunday">>], start_date={1983,4,11}}, {2011,1,1})).

monthly_every_ordinal_recurrence_test() ->
    %% basic first
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% basic second
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,12}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,13}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,14}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,8}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% basic third
    ?assertEqual({2011,1,17}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,18}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,21}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,16}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% basic fourth
    ?assertEqual({2011,1,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,22}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,23}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% basic fifth
    ?assertEqual({2011,1,31}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,1}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,30}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,1})),
    %% on occurance
    ?assertEqual({2011,2,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,2,14}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,10})),
    ?assertEqual({2011,2,21}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,17})),
    ?assertEqual({2011,2,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,24})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% leap year first
    ?assertEqual({2008,2,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,7}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% leap year second
    ?assertEqual({2008,2,11}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,12}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,13}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,14}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,8}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,9}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,10}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% leap year third
    ?assertEqual({2008,2,18}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,19}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,20}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,21}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,15}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,16}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,17}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% leap year fourth
    ?assertEqual({2008,2,25}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,26}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,28}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,22}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,23}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,24}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% leap year fifth
    ?assertEqual({2008,3,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"monday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"wensday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,6}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"thursday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"saturday">>], start_date={2008,1,1}}, {2008,2,1})),
    ?assertEqual({2008,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"sunday">>], start_date={2008,1,1}}, {2008,2,1})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,3}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,11}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2005,2,1}}, {2011,1,1})),
    ?assertEqual({2011,1,19}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2006,3,1}}, {2011,1,1})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2007,4,1}}, {2011,1,1})),
    ?assertEqual({2011,2,4}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2008,5,1}}, {2011,1,1})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,1,9}, next_rule_date(#rule{cycle = <<"monthly">>, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2010,7,1}}, {2011,1,1})),
    %% even step first (small)
    ?assertEqual({2011,3,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"first">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% even step second (small)
    ?assertEqual({2011,3,14}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,8}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,9}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,10}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,11}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,12}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,13}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% even step third (small)
    ?assertEqual({2011,3,21}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,15}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,16}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,17}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,18}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,19}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,20}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"third">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% even step fourth (small)
    ?assertEqual({2011,3,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,22}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,23}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,24}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% even step fifth (small)
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,3,31}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step first (small)
    ?assertEqual({2011,4,4}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,1}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,3}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"first">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step second (small)
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,12}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,13}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,14}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,8}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,9}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,10}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step third (small)
    ?assertEqual({2011,4,18}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,19}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,20}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,21}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,15}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,16}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,17}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"third">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step fourth (small)
    ?assertEqual({2011,4,25}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,22}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,23}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,24}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fourth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% odd step fifth (small)
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"monday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"wensday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"thursday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,4,30}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"saturday">>], start_date={2011,1,1}}, {2011,1,31})),
%%!!    ?assertEqual({2011, ?, ??}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, ordinal = <<"fifth">>, wdays=[<<"sunday">>], start_date={2011,1,1}}, {2011,1,31})),
    %% shift start date
    ?assertEqual({2011,2,7}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"first">>, wdays=[<<"monday">>], start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,5,10}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"second">>, wdays=[<<"tuesday">>], start_date={2005,2,1}}, {2011,1,1})),
    ?assertEqual({2011,3,16}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"third">>, wdays=[<<"wensday">>], start_date={2006,3,1}}, {2011,1,1})),
    ?assertEqual({2011,1,27}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"fourth">>, wdays=[<<"thursday">>], start_date={2007,4,1}}, {2011,1,1})),
    ?assertEqual({2011,4,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"fifth">>, wdays=[<<"friday">>], start_date={2008,5,1}}, {2011,1,1})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"first">>, wdays=[<<"saturday">>], start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,5,8}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"second">>, wdays=[<<"sunday">>], start_date={2010,7,1}}, {2011,1,1})),
    %% long span
    ?assertEqual({2011,3,28}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"fourth">>, wdays=[<<"monday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,29}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"first">>, wdays=[<<"wensday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,10}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"second">>, wdays=[<<"thursday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,18}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"third">>, wdays=[<<"friday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,26}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"fourth">>, wdays=[<<"saturday">>], start_date={1983,4,11}}, {2011,1,1})),
    ?assertEqual({2011,3,6}, next_rule_date(#rule{cycle = <<"monthly">>, interval=5, ordinal = <<"first">>, wdays=[<<"sunday">>], start_date={1983,4,11}}, {2011,1,1})).

monthly_date_recurrence_test() ->
    %% basic increment
    lists:foreach(fun(D) ->
                          ?assertEqual({2011,1,D + 1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[D + 1], start_date={2011,1,1}}, {2011,1,D}))
                  end, lists:seq(1, 30)),
    lists:foreach(fun(D) ->
                          ?assertEqual({2011,6,D + 1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[D + 1], start_date={2011,6,1}}, {2011,6,D}))
                  end, lists:seq(1, 29)),
    %% same day, before
    ?assertEqual({2011,3,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[25], start_date={2011,1,1}}, {2011,3,24})),
    %% increment over month boundary
    ?assertEqual({2011,2,1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[1], start_date={2011,1,1}}, {2011,1,31})),
    ?assertEqual({2011,7,1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[1], start_date={2011,6,1}}, {2011,6,30})),
    %% increment over year boundary
    ?assertEqual({2011,1,1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[1], start_date={2010,1,1}}, {2010,12,31})),
    %% leap year (into)
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"monthly">>, days=[29], start_date={2008,1,1}}, {2008,2,28})),
    %% leap year (over)
    ?assertEqual({2008,3,1}, next_rule_date(#rule{cycle = <<"monthly">>, days=[1], start_date={2008,1,1}}, {2008,2,29})),
    %% shift start date (no impact)
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, days=[2], start_date={2008,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, days=[2], start_date={2009,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, days=[2], start_date={2010,1,1}}, {2011,1,1})),
    %% multiple dates
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,3})),
    ?assertEqual({2011,1,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,4})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,5})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,6})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,7})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,8})),
    ?assertEqual({2011,1,10}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,9})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,10})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,11})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,12})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,13})),
    ?assertEqual({2011,1,15}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,14})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,15})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,16})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,17})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,18})),
    ?assertEqual({2011,1,20}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,19})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,20})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,21})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,22})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,23})),
    ?assertEqual({2011,1,25}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,24})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,25})),
    ?assertEqual({2011,2,5}, next_rule_date(#rule{cycle = <<"monthly">>, days=[5,10,15,20,25], start_date={2011,1,1}}, {2011,1,26})),
    %% even step (small)
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, days=[2], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,5,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, days=[2], start_date={2011,1,1}}, {2011,3,2})),
    ?assertEqual({2011,7,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, days=[2], start_date={2011,1,1}}, {2011,5,2})),
    ?assertEqual({2011,6,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, days=[2], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,8,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=2, days=[2], start_date={2011,6,1}}, {2011,6,2})),
    %% odd step (small)
    ?assertEqual({2011,4,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,7,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2011,1,1}}, {2011,4,2})),
    ?assertEqual({2011,10,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2011,1,1}}, {2011,7,2})),
    ?assertEqual({2011,6,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2011,9,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2011,6,1}}, {2011,6,2})),
    %% even step (large)
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=24, days=[2], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2013,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=24, days=[2], start_date={2011,1,1}}, {2011,1,2})),
    ?assertEqual({2011,6,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=24, days=[2], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2013,6,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=24, days=[2], start_date={2011,6,1}}, {2011,6,2})),
    %% odd step (large)
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, days=[2], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2014,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, days=[2], start_date={2011,1,1}}, {2011,4,2})),
    ?assertEqual({2011,6,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, days=[2], start_date={2011,6,1}}, {2011,6,1})),
    ?assertEqual({2014,7,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=37, days=[2], start_date={2011,6,1}}, {2011,6,2})),
    %% shift start date
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2007,5,1}}, {2011,1,1})),
    ?assertEqual({2011,3,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2008,6,2}}, {2011,1,1})),
    ?assertEqual({2011,1,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2009,7,3}}, {2011,1,1})),
    ?assertEqual({2011,2,2}, next_rule_date(#rule{cycle = <<"monthly">>, interval=3, days=[2], start_date={2010,8,4}}, {2011,1,1})),
    %% long span
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"monthly">>, interval=4, days=[11], start_date={1983,4,11}}, {2011,1,1})).

yearly_date_recurrence_test() ->
    %% basic increment
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,2,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,3,1})),
    %% same month, before
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,4,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,4,10})),
    %% increment over year boundary
    ?assertEqual({2012,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2011,1,1}}, {2011,4,11})),
    %% leap year (into)
    ?assertEqual({2008,2,29}, next_rule_date(#rule{cycle = <<"yearly">>, month=2, days=[29], start_date={2008,1,1}}, {2008,2,28})),
    %% leap year (over)
    ?assertEqual({2009,2,29}, next_rule_date(#rule{cycle = <<"yearly">>, month=2, days=[29], start_date={2008,1,1}}, {2008,2,29})),
    %% shift start date (no impact)
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2008,10,11}}, {2011,1,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2009,11,11}}, {2011,1,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, month=4, days=[11], start_date={2010,12,11}}, {2011,1,1})),
    %% even step (small)
    ?assertEqual({2013,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, month=4, days=[11], start_date={2011,1,1}}, {2011,4,11})),
    ?assertEqual({2015,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, month=4, days=[11], start_date={2011,1,1}}, {2014,4,11})),
    %% odd step (small)
    ?assertEqual({2014,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=3, month=4, days=[11], start_date={2011,1,1}}, {2011,4,11})),
    ?assertEqual({2017,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=3, month=4, days=[11], start_date={2011,1,1}}, {2016,4,11})),
    %% shift start dates
    ?assertEqual({2013,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=5, month=4, days=[11], start_date={2008,10,11}}, {2011,1,1})),
    ?assertEqual({2014,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=5, month=4, days=[11], start_date={2009,11,11}}, {2011,1,1})),
    ?assertEqual({2015,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=5, month=4, days=[11], start_date={2010,12,11}}, {2011,1,1})),
    %% long span
    ?assertEqual({2013,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=5, month=4, days=[11], start_date={1983,4,11}}, {2011,1,1})).

yearly_every_recurrence_test() ->
    ok.

yearly_last_recurrence_test() ->
    ok.

yearly_every_ordinal_recurrence_test() ->
    %% basic first
    ?assertEqual({2011,4,4}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,5}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,6}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,7}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,1}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,2}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,3}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    %% basic second
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,12}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,13}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,14}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,8}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,9}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,10}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    %% basic third
    ?assertEqual({2011,4,18}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,19}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,20}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,21}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,15}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,16}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,17}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    %% basic fourth
    ?assertEqual({2011,4,25}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,26}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,27}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,28}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,22}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,23}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,24}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    %% basic fifth
    ?assertEqual({2012,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
%%!!    ?assertEqual({2013,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
%%!!    ?assertEqual({2014,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
%%!!    ?assertEqual({2015,4,28}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,29}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
%%!!    ?assertEqual({2017,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,1,1})),
    %% same month, before
    ?assertEqual({2011,4,4}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,4,1})),
    ?assertEqual({2011,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,4,10})),
    %% current date on (simple)
    ?assertEqual({2011,4,4}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,3,11})),
    ?assertEqual({2012,4,2}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,4,11})),
    %% current date after (simple)
    ?assertEqual({2012,4,2}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,6,21})),
    %% shift start dates (no impact)
    ?assertEqual({2011,4,4}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2004,1,1}}, {2011,1,1})),
    ?assertEqual({2011,4,12}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"tuesday">>], month=4, start_date={2005,2,1}}, {2011,1,1})),
    ?assertEqual({2011,4,20}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"third">>, wdays=[<<"wensday">>], month=4, start_date={2006,3,1}}, {2011,1,1})),
    ?assertEqual({2011,4,28}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fourth">>, wdays=[<<"thursday">>], month=4, start_date={2007,4,1}}, {2011,1,1})),
    ?assertEqual({2011,4,29}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"fifth">>, wdays=[<<"friday">>], month=4, start_date={2008,5,1}}, {2011,1,1})),
    ?assertEqual({2011,4,2}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"first">>, wdays=[<<"saturday">>], month=4, start_date={2009,6,1}}, {2011,1,1})),
    ?assertEqual({2011,4,10}, next_rule_date(#rule{cycle = <<"yearly">>, ordinal = <<"second">>, wdays=[<<"sunday">>], month=4, start_date={2010,7,1}}, {2011,1,1})),
    %% even step first (small)
    ?assertEqual({2013,4,1}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,2}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,3}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,4}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,5}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,6}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,7}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"first">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    %% even step second (small)
    ?assertEqual({2013,4,8}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,9}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,10}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,11}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,12}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,13}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,14}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"second">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    %% even step third (small)
    ?assertEqual({2013,4,15}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,16}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,17}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,18}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,19}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,20}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,21}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"third">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    %% even step fourth (small)
    ?assertEqual({2013,4,22}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,23}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,24}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,25}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,26}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,27}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,28}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fourth">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    %% basic fifth (small)
    ?assertEqual({2013,4,29}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"monday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ?assertEqual({2013,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"tuesday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
%%!!    ?assertEqual({2014,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"wensday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
%%!!    ?assertEqual({2015,4,28}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"thursday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
%%!!    ?assertEqual({2013,4,29}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"friday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
%%!!    ?assertEqual({2013,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"saturday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
%%!!    ?assertEqual({2017,4,30}, next_rule_date(#rule{cycle = <<"yearly">>, interval=2, ordinal = <<"fifth">>, wdays=[<<"sunday">>], month=4, start_date={2011,1,1}}, {2011,5,1})),
    ok.

-endif.
