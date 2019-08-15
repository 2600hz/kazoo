%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Controls and picks Callflows based rules.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>One of: `menu', `enable', `disable', `reset'.</dd>
%%%
%%%   <dt>`rules'</dt>
%%%   <dd>List of the rules.</dd>
%%%
%%%   <dt>`interdigit_timeout'</dt>
%%%   <dd>How long to wait for the next DTMF, in milliseconds. Default is 2000.</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_temporal_route).

-behaviour(gen_cf_action).

-include("callflow.hrl").
-include("cf_temporal_route.hrl").

-export([handle/2]).

-ifdef(TEST).
-export([next_rule_date/2
        ,sort_wdays/1
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    Temporal = get_temporal_route(Data, Call),
    case action(Data) of
        <<"menu">> ->
            lager:info("temporal rules main menu"),
            _ = temporal_route_menu(Temporal, rule_ids(Data), Call),
            cf_exe:continue(Call);
        <<"enable">> ->
            lager:info("force temporal rules to enable"),
            _ = enable_temporal_rules(Temporal, rule_ids(Data), Call),
            cf_exe:continue(Call);
        <<"disable">> ->
            lager:info("force temporal rules to disable"),
            _ = disable_temporal_rules(Temporal, rule_ids(Data), Call),
            cf_exe:continue(Call);
        <<"reset">> ->
            lager:info("resume normal temporal rule operation"),
            _ = reset_temporal_rules(Temporal, rule_ids(Data), Call),
            cf_exe:continue(Call);
        _Action ->
            Rules = get_temporal_rules(Temporal, Call),
            case process_rules(Temporal, Rules, Call) of
                'default' ->
                    cf_exe:continue(Call);
                ChildId ->
                    cf_exe:continue(ChildId, Call)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Test all rules in reference to the current temporal routes, and
%% returns the first valid callflow, or the default.
%% @end
%%------------------------------------------------------------------------------
-spec process_rules(temporal(), rules(), kapps_call:call()) ->
                           'default' | binary().
process_rules(Temporal
             ,[#rule{enabled='false'
                    ,id=Id
                    ,name=Name
                    }
               | Rules
              ]
             ,Call
             ) ->
    lager:info("time based rule ~p (~s) disabled", [Id, Name]),
    process_rules(Temporal, Rules, Call);
process_rules(_Temporal
             ,[#rule{enabled='true'
                    ,id=Id
                    ,name=Name
                    }
               | _Rules
              ]
             ,_Call
             ) ->
    lager:info("time based rule ~p (~s) is forced active", [Id, Name]),
    Id;
process_rules(Temporal
             ,[#rule{id=Id
                    ,name=Name
                    ,cycle = <<>>
                    }
               | Rules
              ]
             ,Call
             ) ->
    lager:error("time based rule ~p (~s) is invalid, skipping", [Id, Name]),
    process_rules(Temporal, Rules, Call);
process_rules(#temporal{local_sec=LSec
                       ,local_date={Y, M, D}
                       }=T
             ,[#rule{cycle=Cycle
                    ,id=Id
                    ,name=Name
                    ,wtime_start=TStart
                    ,wtime_stop=TStop
                    }=Rule
               | Rules
              ]
             ,Call
             ) ->
    lager:info("processing temporal rule ~s (~s)", [Id, Name]),

    %% Weekly logic becomes convoluted when prev date is passed for SearchDate.
    %% This creates lots of edge cases so pass today in weekly only.
    SearchDate = case Cycle of
                     <<"weekly">> ->
                         {Y, M, D};
                     _ ->
                         kz_date:normalize({Y, M, D - 1})
                 end,
    BaseDate = kz_date:normalize(
                 next_rule_date(Rule, SearchDate)
                ),
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
            Id
    end;
process_rules(_Temporal, [], _Call) ->
    lager:info("continuing with default callflow"),
    'default'.

%%------------------------------------------------------------------------------
%% @doc Finds and returns a list of rule records that have do not occur in
%% the future as well as pertain to this temporal route mapping.
%% @end
%%------------------------------------------------------------------------------
-spec get_temporal_rules(temporal(), kapps_call:call()) -> rules().
get_temporal_rules(#temporal{local_sec=LSec
                            ,routes=Routes
                            ,timezone=TZ
                            }
                  ,Call
                  ) ->
    get_temporal_rules(Routes, LSec, kapps_call:account_db(Call), TZ, []).

-spec get_temporal_rules(kz_json:path(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary(), rules()) -> rules().
get_temporal_rules(Routes, LSec, AccountDb, <<TZ/binary>>, Rules) ->
    NowDatetime = kz_time:adjust_utc_datetime(calendar:universal_time(), TZ),
    get_temporal_rules(Routes, LSec, AccountDb, TZ, NowDatetime, Rules).

-spec get_temporal_rules(routes(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary(), kz_time:datetime(), rules()) -> rules().
get_temporal_rules([], _, _, _, _, Rules) -> lists:reverse(Rules);
get_temporal_rules([{Route, Id}|Routes], LSec, AccountDb, TZ, NowDatetime, Rules) ->
    case kz_datamgr:open_cache_doc(AccountDb, Route) of
        {'error', _R} ->
            lager:info("unable to find temporal rule ~s in ~s", [Route, AccountDb]),
            get_temporal_rules(Routes, LSec, AccountDb, TZ, NowDatetime, Rules);
        {'ok', JObj} ->
            maybe_build_rule(Routes, LSec, AccountDb, TZ, NowDatetime, Rules, Id, JObj)
    end.

-spec maybe_build_rule(routes(), non_neg_integer(), kz_term:ne_binary(), kz_term:ne_binary(), kz_time:datetime(), rules(), kz_term:ne_binary(), kzd_temporal_rules:doc()) -> rules().
maybe_build_rule(Routes, LSec, AccountDb, TZ, NowDatetime, Rules, Id, RulesDoc) ->
    StartDate = kz_date:from_gregorian_seconds(kzd_temporal_rules:start_date(RulesDoc, LSec), TZ),
    RuleName = kzd_temporal_rules:name(RulesDoc, ?RULE_DEFAULT_NAME),

    case kz_date:relative_difference(NowDatetime, {StartDate, {0,0,0}}) of
        'future' ->
            lager:warning("rule ~p is in the future discarding", [RuleName]),
            get_temporal_rules(Routes, LSec, AccountDb, TZ, NowDatetime, Rules);
        _ ->
            get_temporal_rules(Routes, LSec, AccountDb, TZ, NowDatetime, [build_rule(Id, RulesDoc, StartDate, RuleName) | Rules])
    end.

-spec build_rule(kz_term:ne_binary(), kzd_temporal_rules:doc(), kz_time:date(), kz_term:ne_binary()) -> rule().
build_rule(Id, RulesDoc, StartDate, RuleName) ->
    #rule{cycle = kzd_temporal_rules:cycle(RulesDoc, ?RULE_DEFAULT_CYCLE)
         ,days = days_in_rule(RulesDoc)
         ,enabled = kzd_temporal_rules:enabled(RulesDoc, 'undefined')
         ,id = Id
         ,interval = kzd_temporal_rules:interval(RulesDoc, ?RULE_DEFAULT_INTERVAL)
         ,month = kzd_temporal_rules:month(RulesDoc, ?RULE_DEFAULT_MONTH)
         ,name = RuleName
         ,ordinal = kzd_temporal_rules:ordinal(RulesDoc, ?RULE_DEFAULT_ORDINAL)
         ,start_date = StartDate
         ,wdays = sort_wdays(kzd_temporal_rules:wdays(RulesDoc, ?RULE_DEFAULT_WDAYS))
         ,wtime_start = kzd_temporal_rules:time_window_start(RulesDoc, ?RULE_DEFAULT_WTIME_START)
         ,wtime_stop = kzd_temporal_rules:time_window_stop(RulesDoc, ?RULE_DEFAULT_WTIME_STOP)
         }.

-spec days_in_rule(kzd_temporal_rules:doc()) -> kz_term:integers().
days_in_rule(RulesDoc) ->
    lists:foldr(fun add_day/2, [], kzd_temporal_rules:days(RulesDoc, ?RULE_DEFAULT_DAYS)).

-spec add_day(kz_term:ne_binary() | integer(), kz_term:integers()) -> kz_term:integers().
add_day(Day, Acc) -> [kz_term:to_integer(Day)|Acc].

%%------------------------------------------------------------------------------
%% @doc Loads the temporal record with data from the db.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_load_rules(kz_json:object(), kapps_call:call(), routes()) -> routes().
maybe_load_rules(Data, _Call, Routes) ->
    Rules = rule_ids(Data),
    lager:info("loaded ~p routes from rules", [length(Rules)]),
    Routes ++ [{X, X} || X <- Rules].

-spec maybe_load_branch_keys(kz_json:object(), kapps_call:call(), routes()) -> routes().
maybe_load_branch_keys(_Data, Call, Routes) ->
    {'branch_keys', Rules} = cf_exe:get_branch_keys(Call),
    lager:info("loaded ~p routes from branch_keys", [length(Rules)]),
    Routes ++ [{X, X} || X <- Rules].

-spec maybe_load_rulesets(kz_json:object(), kapps_call:call(), routes()) -> routes().
maybe_load_rulesets(Data, Call, Routes) ->
    case rule_set_id(Data) of
        'undefined' ->
            lager:info("no rule_set id configured"),
            Routes;
        RuleSetId ->
            lager:info("loading rules from rule_set ~p", [RuleSetId]),
            Routes ++ [{X, <<"rule_set">>} || X <- get_rule_set(RuleSetId, Call)]
    end.

-spec maybe_expand_rulesets(kz_json:object(), kapps_call:call(), routes()) -> routes().
maybe_expand_rulesets(_Data, Call, Rules) ->
    try_load_rulesets(Call, lists:flatten(Rules), []).

-spec try_load_rulesets(kapps_call:call(), routes(), routes()) -> routes().
try_load_rulesets(_Call, [], Acc) ->
    lists:flatten(Acc);

try_load_rulesets(Call, [{_,<<"rule_set">>}=H|T], Acc) ->
    try_load_rulesets(Call, T, Acc ++ [H]);

try_load_rulesets(Call, [{H,_}|T], Acc) ->
    UseRoutes = case get_rule_set(H, Call) of
                    [] ->
                        [{H, H}];
                    SetVals ->
                        lager:info("loaded ~p rules from rule_set ~p", [length(SetVals), H]),
                        [{X, H} || X <- SetVals]
                end,
    try_load_rulesets(Call, T, Acc ++ UseRoutes).

-spec get_temporal_route(kz_json:object(), kapps_call:call()) -> temporal().
get_temporal_route(Data, Call) ->
    lager:info("loading temporal route..."),

    Routes = lists:foldl(fun(F, A) -> F(Data, Call, A) end
                        ,[]
                        ,[fun maybe_load_rules/3
                         ,fun maybe_load_branch_keys/3
                         ,fun maybe_load_rulesets/3
                         ,fun maybe_expand_rulesets/3
                         ]),

    lager:info("routes are: ~p", [Routes]),

    load_current_time(#temporal{routes = Routes
                               ,timezone = cf_util:get_timezone(Data, Call)
                               ,interdigit_timeout = interdigit_timeout(Data)
                               }).

%%------------------------------------------------------------------------------
%% @doc Loads rules set from account db.
%% @end
%%------------------------------------------------------------------------------
-spec get_rule_set(route() | kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binaries().
get_rule_set({Id, Id}, Call) ->
    get_rule_set(Id, Call);

get_rule_set(Id, Call) ->
    AccountDb = kapps_call:account_db(Call),
    lager:info("loading temporal rule set ~s", [Id]),
    case kz_datamgr:open_cache_doc(AccountDb, Id) of
        {'error', _E} ->
            lager:error("failed to load ~s in ~s", [Id, AccountDb]),
            [];
        {'ok', TemporalRulesSet} ->
            kzd_temporal_rules_sets:temporal_rules(TemporalRulesSet, [])
    end.

%%------------------------------------------------------------------------------
%% @doc Present the caller with the option to enable, disable, or reset
%% the provided temporal rules.
%% @end
%%------------------------------------------------------------------------------
-spec temporal_route_menu(temporal(), rule_ids(), kapps_call:call()) -> cf_api_std_return().
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
    NoopId = kapps_call_command:prompt(MainMenu, Call),

    case kapps_call_command:collect_digits(1
                                          ,kapps_call_command:default_collect_timeout()
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
            {'ok', kz_json:new()};
        {'ok', _} ->
            temporal_route_menu(Temporal, Rules, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%------------------------------------------------------------------------------
-spec disable_temporal_rules(temporal(), rule_ids(), kapps_call:call()) -> cf_api_std_return().
disable_temporal_rules(#temporal{prompts=#prompts{marked_disabled=Disabled}}, [], Call) ->
    kapps_call_command:b_prompt(Disabled, Call);
disable_temporal_rules(Temporal, [RuleId|T]=Rules, Call) ->
    try
        AccountDb = kapps_call:account_db(Call),
        {'ok', JObj} = kz_datamgr:open_doc(AccountDb, RuleId),
        case kz_datamgr:save_doc(AccountDb, kzd_temporal_rules:set_enabled(JObj, 'false')) of
            {'ok', _} ->
                lager:info("set temporal rule ~s to disabled", [RuleId]),
                disable_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during disable of temporal rule ~s, trying again", [RuleId]),
                disable_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to update temporal rule ~s, ~p", [RuleId, R1]),
                disable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to update temporal rules ~p", [R2]),
            disable_temporal_rules(Temporal, T, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%------------------------------------------------------------------------------
-spec reset_temporal_rules(temporal(), rule_ids(), kapps_call:call()) -> cf_api_std_return().
reset_temporal_rules(#temporal{prompts=#prompts{marker_reset=Reset}}, [], Call) ->
    kapps_call_command:b_prompt(Reset, Call);
reset_temporal_rules(Temporal, [RuleId|T]=Rules, Call) ->
    try
        AccountDb = kapps_call:account_db(Call),
        {'ok', JObj} = kz_datamgr:open_doc(AccountDb, RuleId),
        case kz_datamgr:save_doc(AccountDb, kzd_temporal_rules:delete_enabled(JObj)) of
            {'ok', _} ->
                lager:info("reset temporal rule ~s", [RuleId]),
                reset_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during reset of temporal rule ~s, trying again", [RuleId]),
                reset_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to reset temporal rule ~s, ~p", [RuleId, R1]),
                reset_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to reset temporal rule ~s ~p", [RuleId, R2]),
            reset_temporal_rules(Temporal, T, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Retrieve and update the enabled key on the temporal rule document.
%% Also plays messages to the caller based on the results of that
%% operation.
%% @end
%%------------------------------------------------------------------------------
-spec enable_temporal_rules(temporal(), rule_ids(), kapps_call:call()) -> cf_api_std_return().
enable_temporal_rules(#temporal{prompts=#prompts{marked_enabled=Enabled}}, [], Call) ->
    kapps_call_command:b_prompt(Enabled, Call);
enable_temporal_rules(Temporal, [RuleId|T]=Rules, Call) ->
    try
        AccountDb = kapps_call:account_db(Call),
        {'ok', RuleDoc} = kz_datamgr:open_doc(AccountDb, RuleId),
        case kz_datamgr:save_doc(AccountDb, kzd_temporal_rules:set_enabled(RuleDoc, 'true')) of
            {'ok', _} ->
                lager:info("set temporal rule ~s to enabled active", [RuleId]),
                enable_temporal_rules(Temporal, T, Call);
            {'error', 'conflict'} ->
                lager:info("conflict during enable of temporal rule ~s, trying again", [RuleId]),
                enable_temporal_rules(Temporal, Rules, Call);
            {'error', R1} ->
                lager:info("unable to enable temporal rule ~s, ~p", [RuleId, R1]),
                enable_temporal_rules(Temporal, T, Call)
        end
    catch
        _:R2 ->
            lager:info("unable to enable temporal rule ~s ~p", [RuleId, R2]),
            enable_temporal_rules(Temporal, T, Call)
    end.

%%------------------------------------------------------------------------------
%% @doc determines the appropriate Gregorian seconds to be used as the
%% current date/time for this temporal route selection
%% @end
%%------------------------------------------------------------------------------
-spec load_current_time(temporal()) -> temporal().
load_current_time(#temporal{timezone=Timezone}=Temporal)->
    {LocalDate, LocalTime} = kz_time:adjust_utc_datetime(calendar:universal_time(), Timezone),
    lager:info("local time for ~s is {~w,~w}", [Timezone, LocalDate, LocalTime]),
    Temporal#temporal{local_sec=calendar:datetime_to_gregorian_seconds({LocalDate, LocalTime})
                     ,local_date=LocalDate
                     ,local_time=LocalTime
                     }.

%%------------------------------------------------------------------------------
%% @doc The big daddy
%% Calculates the date of the next event given the type, interval,
%% rule, start date, and current date.
%%
%% GOTCHA!
%% Weird predictions? Bet your weekdays or days are not in order....
%%   - monday, tuesday, wensday, thursday, friday, saturday, sunday
%%   - 1,2,3..31
%% @end
%%------------------------------------------------------------------------------
-spec next_rule_date(rule(), kz_time:date()) -> kz_time:date().
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
    kz_date:normalize({Y0, M0, D0 + Offset + I0});
next_rule_date(#rule{cycle = <<"weekly">>
                    ,interval=Interval
                    ,wdays=Weekdays
                    ,start_date={Y0, M0, D0}=StartDate
                    }=Rule
              ,{Y1, M1, D1}=Today
              ) ->
    DOW0 = calendar:day_of_the_week({Y1, M1, D1}),
    Distance = iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}),
    Offset = trunc( Distance / Interval ) * Interval,
    Weekday = calendar:day_of_the_week(StartDate),

    %%TODO: remove these log lines when we are happy that this just works
    lager:debug("today is: ~p dow: ~p, startdate is: ~p, start dow is ~b, interval is: ~b, distance is: ~b, offset is: ~b, rule days found: ~p"
               ,[Today, DOW0, StartDate, Weekday, Interval, Distance, Offset, find_active_days(Weekdays, DOW0)]
               ),

    case find_active_days(Weekdays, DOW0) of
        %% When the start date is in the future but within the week,
        %% skip over the invalid rule dates by recursively calling
        %% self with Today as StartDate
        [_Day|_] when Today < StartDate
                      andalso Distance =:= Offset ->
            lager:debug("rule starts in the future jumping to search from ~p", [StartDate]),
            next_rule_date(Rule, StartDate);

        %% When today is the first rule day and also the start date return the start date
        [Day|_] when Today =:= StartDate
                     andalso Day =:= DOW0
                     andalso Distance =:= Offset ->
            lager:debug("rule starts today ~b", [Day]),
            StartDate;

        %% During an 'active' week that fails the previous guards, move to the next day this week
        [Day|_] when Distance =:= Offset ->
            lager:debug("next day in rule is ~w and day is ~w", [Day, DOW0]),
            kz_date:normalize({Y1, M1, D1 + Day - DOW0});

        %% Empty list:
        %% Non Empty List that failed the guard:
        %%   During an 'inactive' week
        _Val ->
            lager:debug("no rule found for this week"),
            {WY0, W0} = calendar:iso_week_number({Y0, M0, D0}),
            {Y2, M2, D2} = kz_date:from_iso_week({WY0, W0 + Offset + Interval}),
            kz_date:normalize({Y2, M2, ( D2 - 1 ) + kz_date:wday_to_dow( hd( Weekdays ) )})
    end;
next_rule_date(#rule{cycle = <<"monthly">>
                    ,interval=I0
                    ,days=[_|_]=Days
                    ,start_date={Y0, M0, _}
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case [D || D <- Days, D > D1] of
        %% The day hasn't happened on an 'active' month
        [Day|_] when Distance =:= Offset ->
            M01 = M0 + Offset,
            kz_date:normalize({Y0 + (M01 div 12), M01 rem 12, Day});
        %% Empty List:
        %%   All of the days in the list have already happened
        %% Non Empty List that failed the guard:
        %%   The day hasn't happened on an 'inactive' month
        _ ->
            M01 = M0 + Offset + I0,
            kz_date:normalize({Y0 + (M01 div 12), M01 rem 12, hd(Days)})
    end;

next_rule_date(#rule{cycle = <<"monthly">>
                    ,interval=I0
                    ,ordinal = <<"every">>
                    ,wdays=[Weekday]
                    ,start_date={Y0, M0, _}
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso kz_date:find_next_weekday({Y1, M1, D1}, Weekday)
    of
        %% If the next occurrence of the weekday is during an 'active' month
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
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso find_last_weekday({Y1, M1, 1}, Weekday)
    of
        %% If today is before the occurrence day on an 'active' month since
        %%   the 'last' only happens once per month if we haven't passed it
        %%   then it must be this month
        {_, _, D2}=Date when D1 < D2 ->
            Date;
        %% In an 'inactive' month or when we have already passed
        %%   the last occurrence of the DOW
        _ ->
            find_last_weekday({Y0, M0 + Offset + I0, 1}, Weekday)
    end;

%% WARNING: There is a known bug when requesting the fifth occurrence
%%   of a weekday when I0 > 1 and the current month only has four instances
%%   of the given weekday, the calculation is incorrect.  I was told not
%%   to worry about that now...
next_rule_date(#rule{cycle = <<"monthly">>
                    ,interval=I0
                    ,ordinal=Ordinal
                    ,wdays=[Weekday]
                    ,start_date={Y0, M0, _}
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = ( Y1 - Y0 ) * 12 - M0 + M1,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso {find_ordinal_weekday(Y1, M1, Weekday, Ordinal), I0}
    of
        %% If today is before the occurrence day on an 'active' month and
        %%   the occurrence does not cross month/year boundaries then the
        %%   calculated date is accurate
        {{_, M1, D2}=Date, _} when D1 < D2, I0 > 1 ->
            Date;
        %% If today is before the occurrence day on an 'active' month and
        %%   the interval =:= 1 then it happens every month so it doesn't
        %%   matter if it crosses month/year boundaries
        {{_, M2, D2}=Date, 1} when D1 < D2; M1 < M2 ->
            Date;
        %% false:
        %%   In an 'inactive' month
        %% {kz_time:date(), integer()}:
        %%   We have already passed the last occurrence of the DOW
        _ ->
            find_ordinal_weekday(Y0, M0 + Offset + I0, Weekday, Ordinal)
    end;

%% WARNING: This function does not ensure the provided day actually
%%   exists in the month provided.  For temporal routes that isn't
%%   an issue because we will 'pass' the invalid date and compute
%%   the next
next_rule_date(#rule{cycle = <<"yearly">>
                    ,interval=I0
                    ,month=Month
                    ,days=[_|_]=Days
                    ,start_date={Y0, _, _}
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset of
        %% If this is not an 'active' year it will be the first specified
        %% day (of days) next interval year(s)
        'false' ->
            {Y0 + Offset + I0, Month, hd(Days)};
        %% If this an 'active' year but the month has not occurred yet
        %% it will be on the first day (of days) that month
        'true' when M1 < Month ->
            {Y1, Month, hd(Days)};
        %% If this an 'active' year but the month has not occurred yet
        %% it will be on the first day (of days) next interval year(s)
        'true' when M1 > Month ->
            {Y0 + Offset + I0, Month, hd(Days)};
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
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso kz_date:find_next_weekday({Y1, Month, D1}, Weekday)
    of
        %% During an 'active' year before the target month the calculated
        %%   occurrence is accurate
        {Y1, Month, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurrence day it is accurate
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
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso find_last_weekday({Y1, Month, 1}, Weekday)
    of
        %% During an 'active' year before the target month the calculated
        %%   occurrence is accurate
        {Y1, _, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurrence day it is accurate
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
                    }
              ,{Y1, M1, D1}
              ) ->
    Distance = Y1 - Y0,
    Offset = trunc( Distance / I0 ) * I0,
    case Distance =:= Offset
        andalso find_ordinal_weekday(Y1, Month, Weekday, Ordinal)
    of
        %% During an 'active' year before the target month the calculated
        %%   occurrence is accurate
        {Y1, Month, _}=Date when M1 < Month ->
            Date;
        %% During an 'active' year on the target month before the
        %%   calculated occurrence day it is accurate
        {Y1, Month, D2}=Date when M1 =:= Month, D1 < D2 ->
            Date;
        %% During an 'inactive' year or after the calculated
        %%   occurrence, determine the next iteration
        _ ->
            find_next_yearly_ordinal_weekday(Y0 + Offset + I0, Month, Weekday, Ordinal, I0)
    end.

-spec find_next_yearly_ordinal_weekday(kz_time:year(), kz_time:month(), wday(), kz_time:ordinal(), interval()) -> kz_time:date().
find_next_yearly_ordinal_weekday(Y0, M0, Weekday, Ordinal, Interval) ->
    case find_ordinal_weekday(Y0, M0, Weekday, Ordinal) of
        {_Y1, M0, _D1}=Date ->
            %% found a date in the same month
            Date;
        {_Y1, _M1, _D1} ->
            %% might be a "fifth" day in the next month,
            %% let's try again
            find_next_yearly_ordinal_weekday(Y0 + Interval, M0, Weekday, Ordinal, Interval)
    end.

%%------------------------------------------------------------------------------
%% @doc Safety wrapper on date_of_dow used to loop over failing attempts
%% until the date can be calculated.  The date can be provided as an
%% improper date.
%%
%% <div class="notice">It is possible for this function to cross month/year boundaries.</div>
%% @end
%%------------------------------------------------------------------------------
-spec find_ordinal_weekday(kz_time:year(), improper_month(), wday(), kz_time:ordinal()) -> kz_time:date().
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) when M1 =:= 13 ->
    find_ordinal_weekday(Y1 + 1, 1, Weekday, Ordinal);
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) when M1 > 12 ->
    find_ordinal_weekday(Y1 + 1, M1 - 12, Weekday, Ordinal);
find_ordinal_weekday(Y1, M1, Weekday, Ordinal) ->
    try date_of_dow(Y1, M1, Weekday, Ordinal)
    catch
        _:_ ->
            find_ordinal_weekday(Y1, M1 + 1, Weekday, Ordinal)
    end.

%%------------------------------------------------------------------------------
%% @doc Calculates the date of the last occurrence of a weekday within a
%% given month/year.  The date can be provided as an improper date.
%%
%% Assumption/Principle:
%%   A DOW can never occur more than four times in a month.
%% @end
%%------------------------------------------------------------------------------
%% First attempt to calculate the date of the fourth DOW
%% occurrence.  Since the function corrects an invalid
%% date by crossing month/year boundaries, cause a badmatch
%% if this happens. Therefore, during the exception the last
%% occurrence MUST be in the third week.
%% @end
%%------------------------------------------------------------------------------
-spec find_last_weekday(improper_date(), wday()) -> kz_time:date().
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

%%------------------------------------------------------------------------------
%% @doc Unsafe calculation of the date for a specific day of the week, this
%% function will explode on occasion.
%% @end
%%------------------------------------------------------------------------------
-spec date_of_dow(kz_time:year(), improper_month(), wday(), strict_ordinal()) -> kz_time:date().
date_of_dow(Year, 1, Weekday, Ordinal) ->
    date_of_dow(Year - 1, 13, Weekday, Ordinal);
date_of_dow(Year, Month, Weekday, Ordinal) ->
    RefDate = {Year, Month - 1, calendar:last_day_of_the_month(Year, Month - 1)},
    RefDays = calendar:date_to_gregorian_days(RefDate),
    DOW = kz_date:wday_to_dow(Weekday),
    Occurance = kz_date:ordinal_to_position(Ordinal),
    Days = case calendar:day_of_the_week(RefDate) of
               DOW ->
                   RefDays + 7 + (7 * Occurance );
               RefDOW when DOW < RefDOW ->
                   RefDays + DOW + (7 - RefDOW) + (7 * Occurance);
               RefDOW ->
                   RefDays + abs(DOW - RefDOW) + (7 * Occurance)
           end,
    {Y, M, D} = calendar:gregorian_days_to_date(Days),
    kz_date:normalize({Y, M, D}).

%%------------------------------------------------------------------------------
%% @doc Calculates the distance, in total ISO weeks, between two dates
%% I rather dislike this approach, but it is the best of MANY evils that I came up with...
%% The idea here is to find the difference (in days) between the ISO 8601 Mondays
%% of the start and end dates.  This takes care of all the corner cases for us such as:
%%    - Start date in ISO week of previous year
%%    - End date in ISO week of previous year
%%    - Spanning years
%% All while remaining ISO 8601 compliant.
%% @end
%%------------------------------------------------------------------------------
-spec iso_week_difference(kz_time:date(), kz_time:date()) -> non_neg_integer().
iso_week_difference({Y0, M0, D0}, {Y1, M1, D1}) ->
    DS0 = calendar:date_to_gregorian_days(kz_date:from_iso_week(calendar:iso_week_number({Y0, M0, D0}))),
    DS1 = calendar:date_to_gregorian_days(kz_date:from_iso_week(calendar:iso_week_number({Y1, M1, D1}))),
    trunc( abs( DS0 - DS1 ) / 7 ).

-spec find_active_days(kz_term:ne_binaries(), kz_time:day()) -> [kz_time:daynum()].
find_active_days(Weekdays, DOW0) ->
    [DOW1
     || DOW1 <- [kz_date:wday_to_dow(D) || D <- Weekdays],
        DOW1 >= DOW0
    ].

-spec sort_wdays([wday()]) -> [wday()].
sort_wdays([]) -> [kz_date:dow_to_wday(D) || D <- lists:seq(1, 7)];
sort_wdays(WDays0) ->
    {_, WDays1} = lists:unzip(
                    lists:keysort(1, [{kz_date:wday_to_dow(Day), Day} || Day <- WDays0])
                   ),
    WDays1.

-spec interdigit_timeout(kz_json:object()) -> integer().
interdigit_timeout(Data) ->
    kz_json:get_integer_value(<<"interdigit_timeout">>
                             ,Data
                             ,kapps_call_command:default_interdigit_timeout()
                             ).

-type rule_ids() :: kz_term:ne_binaries().
-spec rule_ids(kz_json:object()) -> rule_ids().
rule_ids(Data) ->
    kz_json:get_list_value(<<"rules">>, Data, []).

-spec action(kz_json:object()) -> kz_term:api_ne_binary().
action(Data) ->
    kz_json:get_ne_binary_value(<<"action">>, Data).

-spec rule_set_id(kz_json:object()) -> kz_term:api_ne_binary().
rule_set_id(Data) ->
    kz_json:get_ne_binary_value(<<"rule_set">>, Data).
