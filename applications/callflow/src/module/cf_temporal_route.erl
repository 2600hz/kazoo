%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "action": "menu" | "enable" | "disable" | "reset"
%%%   ,"rules":[] // list of rules
%%%   // optional after here
%%%   ,"interdigit_timeout":2000
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_temporal_route).

-include("../callflow.hrl").
-include("./cf_temporal_route.hrl").

-export([handle/2
         ,normalize_date/1
        ]).

-ifdef(TEST).
-export([next_rule_date/2
         ,sort_wdays/1
        ]).
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    Temporal = get_temporal_route(Data, Call),
    case wh_json:get_value(<<"action">>, Data) of
        <<"menu">> ->
            lager:info("temporal rules main menu"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            _ = temporal_route_menu(Temporal, Rules, Call),
            cf_exe:stop(Call);
        <<"enable">> ->
            lager:info("force temporal rules to enable"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            _ = enable_temporal_rules(Temporal, Rules, Call),
            cf_exe:stop(Call);
        <<"disable">> ->
            lager:info("force temporal rules to disable"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            _ = disable_temporal_rules(Temporal, Rules, Call),
            cf_exe:stop(Call);
        <<"reset">> ->
            lager:info("resume normal temporal rule operation"),
            Rules = wh_json:get_value(<<"rules">>, Data, []),
            _ = reset_temporal_rules(Temporal, Rules, Call),
            cf_exe:stop(Call);
        _ ->
            Rules = get_temporal_rules(Temporal, Call),
            case process_rules(Temporal, Rules, Call) of
                'default' ->
                    cf_exe:continue(Call);
                ChildId ->
                    cf_exe:continue(ChildId, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Test all rules in reference to the current temporal routes, and
%% returns the first valid callflow, or the default.
%% @end
%%--------------------------------------------------------------------
-spec process_rules(temporal(), rules(), whapps_call:call()) ->
                           'default' | binary().
process_rules(Temporal, [#rule{enabled='false'
                               ,id=Id
                               ,name=Name
                              }|Rs], Call) ->
    lager:info("time based rule ~s (~s) disabled", [Id, Name]),
    process_rules(Temporal, Rs, Call);
process_rules(_, [#rule{enabled='true'
                        ,id=Id
                        ,name=Name
                        ,rule_set=RuleSet
                       }|_], _) ->
    lager:info("time based rule ~s (~s) is forced active part of rule set? ~p", [Id, Name, RuleSet]),
    case RuleSet of
        'true' -> <<"rule_set">>;
        'false' -> Id
    end;
process_rules(#temporal{local_sec=LSec
                        ,local_date={Y, M, D}
                       }=T
              ,[#rule{id=Id
                      ,name=Name
                      ,wtime_start=TStart
                      ,wtime_stop=TStop
                      ,rule_set=RuleSet
                     }=Rule
                |Rules
               ]
              ,Call) ->
    lager:info("processing temporal rule ~s (~s) part of rule set? ~p", [Id, Name, RuleSet]),
    PrevDay = normalize_date({Y, M, D - 1}),
    BaseDate = next_rule_date(Rule, PrevDay),
    BaseTime = calendar:datetime_to_gregorian_seconds({BaseDate, {0,0,0}}),
    case {BaseTime + TStart, BaseTime + TStop} of
        {Start, _} when LSec < Start ->
            lager:info("rule applies in the future ~w", [calendar:gregorian_seconds_to_datetime(Start)]),
            process_rules(T, Rules, Call);
        {_, End} when LSec > End ->
            lager:info("rule was valid today but expired ~w", [calendar:gregorian_seconds_to_datetime(End)]),
            process_rules(T, Rules, Call);
        {_, End} ->
            lager:info("within active time window until ~w", [calendar:gregorian_seconds_to_datetime(End)]),
            case RuleSet of
                'true' -> <<"rule_set">>;
                'false' -> Id
            end
    end;
process_rules(_, [], _) ->
    lager:info("continuing with default callflow"),
    'default'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds and returns a list of rule records that have do not occur in
%% the future as well as pertain to this temporal route mapping.
%% @end
%%--------------------------------------------------------------------
-spec get_temporal_rules(temporal(), whapps_call:call()) -> rules().
get_temporal_rules(#temporal{local_sec=LSec
                             ,routes=Routes
                             ,timezone=TZ
                             ,rule_set=RuleSet
                            }, Call) ->
    get_temporal_rules(Routes, LSec, whapps_call:account_db(Call), RuleSet, TZ, []).

-spec get_temporal_rules(ne_binaries(), non_neg_integer(), ne_binary(), boolean(), ne_binary(), rules()) -> rules().
get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Rules) when is_binary(TZ) ->
    Now = localtime:utc_to_local(calendar:universal_time()
                                 ,wh_util:to_list(TZ)
                                ),
    get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Now, Rules).

-spec get_temporal_rules(ne_binaries(), non_neg_integer(), ne_binary(), boolean(), ne_binary(), wh_datetime(), rules()) ->
                                rules().
get_temporal_rules([], _, _, _, _, _, Rules) -> lists:reverse(Rules);
get_temporal_rules([Route|Routes], LSec, AccountDb, RuleSet, TZ, Now, Rules) ->
    case couch_mgr:open_cache_doc(AccountDb, Route) of
        {'error', _R} ->
            lager:info("unable to find temporal rule ~s in ~s", [Route, AccountDb]),
            get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Now, Rules);
        {'ok', JObj} ->
            Days = lists:foldr(
                        fun(Day, Acc) ->
                            [wh_util:to_integer(Day)|Acc]
                        end
                        ,[]
                        ,wh_json:get_value(<<"days">>, JObj, ?RULE_DEFAULT_DAYS)
                   ),
            Rule = #rule{id = Route
                         ,enabled =
                             wh_json:is_true(<<"enabled">>, JObj, 'undefined')
                         ,name =
                             wh_json:get_value(<<"name">>, JObj, ?RULE_DEFAULT_NAME)
                         ,cycle =
                             wh_json:get_value(<<"cycle">>, JObj, ?RULE_DEFAULT_CYCLE)
                         ,interval =
                             wh_json:get_integer_value(<<"interval">>, JObj, ?RULE_DEFAULT_INTERVAL)
                         ,days = Days
                         ,wdays =
                             sort_wdays(
                               wh_json:get_value(<<"wdays">>, JObj, ?RULE_DEFAULT_WDAYS)
                              )
                         ,ordinal =
                             wh_json:get_value(<<"ordinal">>, JObj, ?RULE_DEFAULT_ORDINAL)
                         ,month =
                             wh_json:get_integer_value(<<"month">>, JObj, ?RULE_DEFAULT_MONTH)
                         ,start_date =
                             get_date(wh_json:get_integer_value(<<"start_date">>, JObj, LSec), TZ)
                         ,wtime_start =
                             wh_json:get_integer_value(<<"time_window_start">>, JObj, ?RULE_DEFAULT_WTIME_START)
                         ,wtime_stop =
                             wh_json:get_integer_value(<<"time_window_stop">>, JObj, ?RULE_DEFAULT_WTIME_STOP)
                         ,rule_set = RuleSet
                        },
            case date_difference(Now, {Rule#rule.start_date, {0,0,0}}) of
                'future' ->
                    lager:warning("rule ~p is in the future discarding", [Rule#rule.name]),
                    get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Now, Rules);
                'past' ->
                    get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Now, [Rule | Rules]);
                'equal' ->
                    get_temporal_rules(Routes, LSec, AccountDb, RuleSet, TZ, Now, [Rule | Rules])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec date_difference(wh_datetime(), wh_datetime()) -> 'future' | 'equal' | 'past'.
date_difference(Date1, Date2) ->
    case calendar:time_difference(Date1, Date2) of
        {D, _} when D > 0 -> 'future';
        {D, _} when D < 0 -> 'past';
        {0, {0, 0, 0}} -> 'equal';
        {0, _} -> 'future'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads the temporal record with data from the db.
%% @end
%%--------------------------------------------------------------------
-spec get_temporal_route(wh_json:object(), whapps_call:call()) -> temporal().
get_temporal_route(JObj, Call) ->
    lager:info("loading temporal route"),
    Keys = case wh_json:get_value(<<"rules">>, JObj, []) of
               [] ->
                   {'branch_keys', Rules} = cf_exe:get_branch_keys(Call),
                   Rules;
               Rules -> Rules
           end,
    {IsRuleSet, Routes} =
        case wh_json:get_value(<<"rule_set">>, JObj) of
            'undefined' -> {'false', Keys};
            RuleSet -> {'true', get_rule_set(RuleSet, Call)}
        end,
    load_current_time(#temporal{routes = Routes
                                ,rule_set = IsRuleSet
                                ,timezone = cf_util:get_timezone(JObj, Call)
                                ,interdigit_timeout =
                                    wh_json:get_integer_value(<<"interdigit_timeout">>
                                                              ,JObj
                                                              ,whapps_call_command:default_interdigit_timeout()
                                                             )
                               }).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Loads rules set from account db.
%% @end
%%--------------------------------------------------------------------
-spec get_rule_set(ne_binary(), whapps_call:call()) -> ne_binaries().
get_rule_set(RuleSetId, Call) ->
    AccountDb = whapps_call:account_db(Call),
    lager:info("loading temporal rule set ~s", [RuleSetId]),
    case couch_mgr:open_cache_doc(AccountDb, RuleSetId) of
        {'error', _E} ->
            lager:error("failed to load ~s in ~s", [RuleSetId, AccountDb]),
            [];
        {'ok', JObj} -> wh_json:get_value(<<"temporal_rules">>, JObj, [])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Accepts a term and tries to convert it to a wh_date()
%% @end
%%--------------------------------------------------------------------
-spec get_date(non_neg_integer(), ne_binary()) -> wh_date().
get_date(Seconds, TZ) when is_integer(Seconds) ->
    {Date, _} = localtime:utc_to_local(
                  calendar:gregorian_seconds_to_datetime(Seconds)
                  ,wh_util:to_list(TZ)
                 ),
    Date.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Present the caller with the option to enable, disable, or reset
%% the provided temporal rules.
%% @end
%%--------------------------------------------------------------------
-spec temporal_route_menu(temporal(), rules(), whapps_call:call()) -> cf_api_std_return().
temporal_route_menu(#temporal{keys=#keys{enable=Enable
                                         ,disable=Disable
                                         ,reset=Reset
                                        }
                              ,prompts=#prompts{main_menu=MainMenu}
                              ,interdigit_timeout=Interdigit
                             }=Temporal
                    ,Rules
                    ,Call
                   ) ->
    NoopId = whapps_call_command:prompt(MainMenu, Call),

    case whapps_call_command:collect_digits(1
                                            ,whapps_call_command:default_collect_timeout()
                                            ,Interdigit
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Enable} ->
            enable_temporal_rules(Temporal, Rules, Call);
        {'ok', Disable} ->
            disable_temporal_rules(Temporal, Rules, Call);
        {'ok', Reset} ->
            reset_temporal_rules(Temporal, Rules, Call);
        {'error', _} ->
            {'ok', wh_json:new()};
        {'ok', _} ->
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
-spec disable_temporal_rules(temporal(), rules(), whapps_call:call()) -> cf_api_std_return().
disable_temporal_rules(#temporal{prompts=#prompts{marked_disabled=Disabled}}, [], Call) ->
    whapps_call_command:b_prompt(Disabled, Call);
disable_temporal_rules(Temporal, [Id|T]=Rules, Call) ->
    try
        AccountDb = whapps_call:account_db(Call),
        {'ok', JObj} = couch_mgr:open_doc(AccountDb, Id),
        case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"enabled">>, 'false', JObj)) of
            {'ok', _} ->
                lager:info("set temporal rule ~s to disabled", [Id]),
                disable_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during disable of temporal rule ~s, trying again", [Id]),
                disable_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to update temporal rule ~s, ~p",[Id, R1]),
                disable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to update temporal rules ~p",[R2]),
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
-spec reset_temporal_rules(temporal(), rules(), whapps_call:call()) -> cf_api_std_return().
reset_temporal_rules(#temporal{prompts=#prompts{marker_reset=Reset}}, [], Call) ->
    whapps_call_command:b_prompt(Reset, Call);
reset_temporal_rules(Temporal, [Id|T]=Rules, Call) ->
    try
        AccountDb = whapps_call:account_db(Call),
        {'ok', JObj} = couch_mgr:open_doc(AccountDb, Id),
        case couch_mgr:save_doc(AccountDb, wh_json:delete_key(<<"enabled">>, JObj)) of
            {'ok', _} ->
                lager:info("reset temporal rule ~s", [Id]),
                reset_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during reset of temporal rule ~s, trying again", [Id]),
                reset_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to reset temporal rule ~s, ~p",[Id, R1]),
                reset_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to reset temporal rule ~s ~p",[Id, R2]),
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
-spec enable_temporal_rules(temporal(), rules(), whapps_call:call()) -> cf_api_std_return().
enable_temporal_rules(#temporal{prompts=#prompts{marked_enabled=Enabled}}, [], Call) ->
    whapps_call_command:b_prompt(Enabled, Call);
enable_temporal_rules(Temporal, [Id|T]=Rules, Call) ->
    try
        AccountDb = whapps_call:account_db(Call),
        {'ok', JObj} = couch_mgr:open_doc(AccountDb, Id),
        case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"enabled">>, 'true', JObj)) of
            {'ok', _} ->
                lager:info("set temporal rule ~s to enabled active", [Id]),
                enable_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during enable of temporal rule ~s, trying again", [Id]),
                enable_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to enable temporal rule ~s, ~p",[Id, R1]),
                enable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to enable temporal rule ~s ~p",[Id, R2]),
            enable_temporal_rules(Temporal, T, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determines the appropriate gregorian seconds to be used as the
%% current date/time for this temporal route selection
%% @end
%%--------------------------------------------------------------------
-spec load_current_time(temporal()) -> temporal().
load_current_time(#temporal{timezone=Timezone}=Temporal)->
    {LocalDate, LocalTime} = localtime:utc_to_local(
                               calendar:universal_time()
                               ,wh_util:to_list(Timezone)
                              ),
    lager:info("local time for ~s is {~w,~w}", [Timezone, LocalDate, LocalTime]),
    Temporal#temporal{local_sec=calendar:datetime_to_gregorian_seconds({LocalDate, LocalTime})
                      ,local_date=LocalDate
                      ,local_time=LocalTime
                     }.

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
-spec next_rule_date(rule(), wh_date()) -> wh_date().
next_rule_date(#rule{cycle = <<"date">>
                     ,start_date=Date0
                    }
               ,_Date
              ) ->
    Date0;
next_rule_date(#rule{cycle = <<"daily">>
                     ,interval=I0
                     ,start_date={Y0, M0, D0}
                    }
               ,{Y1, M1, D1}
              ) ->
    %% Calculate the distance in days as a function of
    %%   the interval and fix
    DS0 = calendar:date_to_gregorian_days({Y0, M0, D0}),
    DS1 = calendar:date_to_gregorian_days({Y1, M1, D1}),
    Offset = trunc( ( DS1 - DS0 ) / I0 ) * I0,
    normalize_date({Y0, M0, D0 + Offset + I0});
next_rule_date(#rule{cycle = <<"weekly">>
                     ,interval=I0
                     ,wdays=Weekdays
                     ,start_date={Y0, M0, D0}=_StartDate
                    }
               ,{Y1, M1, D1}=_PrevDate
              ) ->
    DOW0 = day_of_the_week({Y1, M1, D1}),
    Distance = iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}),
    Offset = trunc( Distance / I0 ) * I0,

    case find_active_days(Weekdays, DOW0) of
        %% During an 'active' week but before the last weekday in the list
        %%   move to the next day this week
        [Day|_] when Distance =:= Offset ->
            lager:debug("next day in rule is ~w", [Day]),
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

next_rule_date(#rule{cycle = <<"monthly">>
                     ,interval=I0
                     ,days=[_|_]=Days
                     ,start_date={Y0, M0, _}
                    }, {Y1, M1, D1}) ->
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

next_rule_date(#rule{cycle = <<"monthly">>
                     ,interval=I0
                     ,ordinal = <<"every">>
                     ,wdays=[Weekday]
                     ,start_date={Y0, M0, _}
                    }, {Y1, M1, D1}) ->
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

next_rule_date(#rule{cycle = <<"monthly">>
                     ,interval=I0
                     ,ordinal = <<"last">>
                     ,wdays=[Weekday]
                     ,start_date={Y0, M0, _}
                    }, {Y1, M1, D1}) ->
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
next_rule_date(#rule{cycle = <<"monthly">>
                     ,interval=I0
                     ,ordinal=Ordinal
                     ,wdays=[Weekday]
                     ,start_date={Y0, M0, _}
                    }, {Y1, M1, D1}) ->
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

%% WARNING: This function does not ensure the provided day actually
%%   exists in the month provided.  For temporal routes that isnt
%%   an issue because we will 'pass' the invalid date and compute
%%   the next
next_rule_date(#rule{cycle = <<"yearly">>
                     ,interval=I0
                     ,month=Month
                     ,days=[_|_]=Days
                     ,start_date={Y0, _, _}
                    }, {Y1, M1, D1}) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset of
        %% If this is not an 'active' year it will be the first specified
        %% day (of days) next interval year(s)
        'false' ->  {Y0 + Offset + I0, Month, hd(Days)};
        %% If this an 'active' year but the month has not occured yet
        %% it will be on the first day (of days) that month
        'true' when M1 < Month -> {Y1, Month, hd(Days)};
        %% If this an 'active' year but the month has not occured yet
        %% it will be on the first day (of days) next interval year(s)
        'true' when M1 > Month -> {Y0 + Offset + I0, Month, hd(Days)};
        'true' ->
            case lists:dropwhile(fun(D) -> D1 >= D end, Days) of
                %% if this is the month but the all the days have passed
                %% it will be on the first day (of days) next interval year(s)
                [] -> {Y0 + Offset + I0, Month, hd(Days)};
                %% if not all the days have passed is is the next day after the
                %% ones that have passed
                [Day|_] ->
                    {Y1, Month, Day}
            end
    end;

next_rule_date(#rule{cycle = <<"yearly">>
                     ,interval=I0
                     ,ordinal = <<"every">>
                     ,month=Month
                     ,wdays=[Weekday]
                     ,start_date={Y0, _, _}
                    }, {Y1, M1, D1}) ->
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

next_rule_date(#rule{cycle = <<"yearly">>
                     ,interval=I0
                     ,ordinal = <<"last">>
                     ,month=Month
                     ,wdays=[Weekday]
                     ,start_date={Y0, _, _}
                    }, {Y1, M1, D1}) ->
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

next_rule_date(#rule{cycle = <<"yearly">>
                     ,interval=I0
                     ,ordinal=Ordinal
                     ,month=Month
                     ,wdays=[Weekday]
                     ,start_date={Y0, _, _}
                    }, {Y1, M1, D1}) ->
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
-spec normalize_date(improper_date()) -> wh_date().
normalize_date({Y, 13, D}) ->
    normalize_date({Y + 1, 1, D});
normalize_date({Y, 0, D}) ->
    normalize_date({Y - 1, 12, D});
normalize_date({Y, M, D}) when M > 12 ->
    normalize_date({Y + 1, M - 12, D});
normalize_date({Y, M, D}) when M < 1 ->
    normalize_date({Y - 1, M + 12, D});
normalize_date({Y, M, D}) when D < 1 ->
    {Y1, M1, _} = normalize_date({Y, M - 1, 1}),
    D0 = calendar:last_day_of_the_month(Y1, M1),
    normalize_date({Y1, M1, D + D0});
normalize_date({Y, M, D}=Date) ->
    case calendar:last_day_of_the_month(Y, M) of
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
-spec from_ordinal(strict_ordinal()) -> 0..4.
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
-spec to_dow(wday()) -> wh_daynum().
to_dow(<<"monday">>) -> 1;
to_dow(<<"tuesday">>) -> 2;
to_dow(<<"wednesday">>) -> 3;
to_dow(<<"wensday">>) -> 3;
to_dow(<<"thursday">>) -> 4;
to_dow(<<"friday">>) -> 5;
to_dow(<<"saturday">>) -> 6;
to_dow(<<"sunday">>) -> 7.

-spec to_wday(wh_daynum()) -> wday().
to_wday(1) -> <<"monday">>;
to_wday(2) -> <<"tuesday">>;
to_wday(3) -> <<"wednesday">>;
to_wday(4) -> <<"thursday">>;
to_wday(5) -> <<"friday">>;
to_wday(6) -> <<"saturday">>;
to_wday(7) -> <<"sunday">>.

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
-spec find_next_weekday(wh_date(), wday()) -> wh_date().
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
-spec find_ordinal_weekday(wh_year(), improper_month(), wday(), strict_ordinal()) -> wh_date().
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
-spec find_last_weekday(improper_date(), wday()) -> wh_date().
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
-spec date_of_dow(wh_year(), improper_month(), wday(), strict_ordinal()) -> wh_date().
date_of_dow(Year, 1, Weekday, Ordinal) ->
    date_of_dow(Year - 1, 13, Weekday, Ordinal);
date_of_dow(Year, Month, Weekday, Ordinal) ->
    RefDate = {Year, Month - 1, calendar:last_day_of_the_month(Year, Month - 1)},
    RefDays = calendar:date_to_gregorian_days(RefDate),
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
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
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
-spec iso_week_difference(wh_date(), wh_date()) -> non_neg_integer().
iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}) ->
    DS0 = calendar:date_to_gregorian_days(iso_week_to_gregorian_date(iso_week_number({Y0, M0, D0}))),
    DS1 = calendar:date_to_gregorian_days(iso_week_to_gregorian_date(iso_week_number({Y1, M1, D1}))),
    trunc( abs( DS0 - DS1 ) / 7 ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Caclulates the gregorian date of a given ISO 8601 week
%% @end
%%--------------------------------------------------------------------
-spec iso_week_to_gregorian_date(wh_iso_week()) -> wh_date().
iso_week_to_gregorian_date({Year, Week}) ->
    Jan1 = calendar:date_to_gregorian_days(Year, 1, 1),
    Offset = 4 - day_of_the_week(Year, 1, 4),
    Days =
        if
            Offset =:= 0 ->
                Jan1 + ( Week * 7 );
            'true' ->
                Jan1 + Offset + ( ( Week - 1 ) * 7 )
        end,
    calendar:gregorian_days_to_date(Days).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for calender:iso_week_number introduced in R14B02 (?) using
%% a local copy on older systems
%% @end
%%--------------------------------------------------------------------
-spec iso_week_number(wh_date()) -> wh_iso_week().
iso_week_number(Date) ->
    case erlang:function_exported('calendar', 'iso_week_number', 1) of
        'true' -> calendar:iso_week_number(Date);
        'false' -> our_iso_week_number(Date)
    end.

-spec day_of_the_week(wh_date()) -> wh_day().
day_of_the_week({Year, Month, Day}=Date) ->
    case erlang:function_exported('calendar', 'day_of_the_week', 1) of
        'true' -> calendar:day_of_the_week(Date);
        'false' -> our_day_of_the_week(Year, Month, Day)
    end.

-spec day_of_the_week(wh_year(), wh_month(), wh_day()) -> wh_day().
day_of_the_week(Year, Month, Day) ->
    case erlang:function_exported('calendar', 'day_of_the_week', 3) of
        'true' -> calendar:day_of_the_week(Year, Month, Day);
        'false' -> our_day_of_the_week(Year, Month, Day)
    end.

%% TAKEN FROM THE R14B02 SOURCE FOR calender.erl
our_iso_week_number({Year,_Month,_Day}=Date) ->
    D = calendar:date_to_gregorian_days(Date),
    W01_1_Year = gregorian_days_of_iso_w01_1(Year),
    W01_1_NextYear = gregorian_days_of_iso_w01_1(Year + 1),
    if W01_1_Year =< D andalso D < W01_1_NextYear ->
            %% Current Year Week 01..52(,53)
            {Year, (D - W01_1_Year) div 7 + 1};
       D < W01_1_Year ->
            %% Previous Year 52 or 53
            PWN = case day_of_the_week(Year - 1, 1, 1) of
                      4 -> 53;
                      _ -> case day_of_the_week(Year - 1, 12, 31) of
                               4 -> 53;
                               _ -> 52
                           end
                  end,
            {Year - 1, PWN};
       W01_1_NextYear =< D ->
            %% Next Year, Week 01
            {Year + 1, 1}
    end.

-spec gregorian_days_of_iso_w01_1(calendar:year()) -> non_neg_integer().
gregorian_days_of_iso_w01_1(Year) ->
    D0101 = calendar:date_to_gregorian_days(Year, 1, 1),
    DOW = calendar:day_of_the_week(Year, 1, 1),
    if DOW =< 4 ->
            D0101 - DOW + 1;
       'true' ->
            D0101 + 7 - DOW + 1
    end.

%% day_of_the_week(Year, Month, Day)
%% day_of_the_week({Year, Month, Day})
%%
%% Returns: 1 | .. | 7. Monday = 1, Tuesday = 2, ..., Sunday = 7.
-spec our_day_of_the_week(calendar:year(), calendar:month(), calendar:day()) -> calendar:daynum().
our_day_of_the_week(Year, Month, Day) ->
    (calendar:date_to_gregorian_days(Year, Month, Day) + 5) rem 7 + 1.

-spec find_active_days(ne_binaries(), wh_day()) -> [wh_daynum()].
find_active_days(Weekdays, DOW0) ->
    [DOW1
     || DOW1 <- [to_dow(D) || D <- Weekdays],
        DOW1 > DOW0
    ].

-spec sort_wdays([wday()]) -> [wday()].
sort_wdays([]) -> [to_wday(D) || D <- lists:seq(1, 7)];
sort_wdays(WDays0) ->
    {_, WDays1} = lists:unzip(
                    lists:keysort(1, [{to_dow(Day), Day} || Day <- WDays0])
                   ),
    WDays1.
