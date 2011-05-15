%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Check user's account for appropriate credit
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_credit).

-include("ts.hrl").

%% API
-export([check/1]).

check(Flags) ->
    ok.

process_rates({<<"_id">>, _}=ID) -> ID;
process_rates({<<"_rev">>, _}=Rev) -> Rev;
process_rates({RouteName, {struct, RouteOptions}}) ->
    RoutesRegexStrs = props:get_value(<<"routes">>, RouteOptions, []),
    {struct, Options} = props:get_value(<<"options">>, RouteOptions, ?EMPTY_JSON_OBJECT),
    ROs0 = props:delete(<<"routes">>, RouteOptions),
    {RouteName, [{<<"routes">>, lists:map(fun(Str) -> {ok, R} = re:compile(Str), R end, RoutesRegexStrs)}
		 ,{<<"options">>, Options}
		 | props:delete(<<"options">>, ROs0)]}.

set_rate_flags(Flags, Rates) ->
    Dir = Flags#route_flags.direction,
    User = Flags#route_flags.to_user,
    Rates0 = props:delete(<<"_rev">>, props:delete(<<"_id">>, Rates)),
    Rates1 = lists:filter(fun({_RateName, RateData}) ->
				  lists:member(Dir, props:get_value(<<"direction">>, RateData)) andalso
				      lists:any(fun(Regex) ->
							re:run(User, Regex) =/= nomatch
						end, props:get_value(<<"routes">>, RateData))
			  end, Rates0),
    %% wh_timer:tick("post first filter"),
    %% Filter on Options - All flag options must be in Rate options
    Rates2 = lists:filter(fun({_RateName, RateData}) ->
				  options_match(Flags#route_flags.route_options, props:get_value(<<"options">>, RateData, []))
			  end, Rates1),
    %% wh_timer:tick("post second filter"),

    try
    case lists:usort(fun sort_rates/2, Rates2) of
	[] ->
	    %% wh_timer:tick("post usort empty"),
	    logger:format_log(error, "TS_CREDIT(~p): No Rate found for ~p~n", [self(), User]),
	    {error, no_route_found};
	[{RateName, RateData} | _] ->
	    %% wh_timer:tick("post usort data found"),
	    logger:format_log(info, "TS_CREDIT(~p): Rate to use ~p~n", [self(), RateName]),

	    case ts_acctmgr:reserve_trunk(Flags#route_flags.account_doc_id, Flags#route_flags.callid
					  ,(Flags#route_flags.rate * Flags#route_flags.rate_minimum + Flags#route_flags.surcharge)
					  ,Flags#route_flags.flat_rate_enabled) of
		{ok, flat_rate} ->
		    {ok, set_flat_flags(Flags, Dir)};
		{ok, per_min} ->
		    {ok, set_rate_flags(Flags, Dir, RateData, RateName)};
		{error, entry_exists}=E ->
		    logger:format_log(error, "TS_CREDIT(~p): Failed to reserve trunk for already existing call-id~n", [self()]),
		    E;
		{error, no_funds}=E1 ->
		    logger:format_log(error, "TS_CREDIT(~p): No funds/flat-rate trunks to route call over.~n", [self()]),
		    E1;
		{error, no_account}=E2 ->
		    logger:format_log(error, "TS_CREDIT(~p): No account id passed.~n", [self()]),
		    E2;
		{error, no_callid}=E3 ->
		    logger:format_log(error, "TS_CREDIT(~p): No call id passed.~n", [self()]),
		    E3;
		{error, not_found}=E4 ->
		    logger:format_log(error, "TS_CREDIT(~p): acctmgr get_results failed.~n", [self()]),
		    E4
	    end
    end
    catch A:B -> logger:format_log(error, "TS_CREDIT(~p): EXCEPTION: ~p:~p~n~p~n", [self(), A, B, erlang:get_stacktrace()]),
		 {error, B}
    end.


%% Return true of RateA has higher weight than RateB
sort_rates({_RNameA, RateDataA}, {_RNameB, RateDataB}) ->
    ts_util:constrain_weight(props:get_value(<<"weight">>, RateDataA, 1)) >= ts_util:constrain_weight(props:get_value(<<"weight">>, RateDataB, 1)).

set_rate_flags(Flags, <<"inbound">>=In, RateData, RateName) ->
    logger:format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [In, RateName]),
    Flags#route_flags{
      rate = whistle_util:to_float(props:get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(props:get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(props:get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(props:get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     };
set_rate_flags(Flags, <<"outbound">>=Out, RateData, RateName) ->
    logger:format_log(info, "TS_CREDIT.set_rate_flags(~p): ~p~n", [Out, RateName]),
    Flags#route_flags{
      rate = whistle_util:to_float(props:get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(props:get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(props:get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(props:get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     }.


set_flat_flags(Flags, <<"inbound">>=In) ->
    logger:format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [In]),
    Flags#route_flags{
      rate = 0.0
      ,rate_increment = 0
      ,rate_minimum = 0
      ,surcharge = 0.0
      ,rate_name = <<>>
      ,flat_rate_enabled = true
     };
set_flat_flags(Flags, <<"outbound">>=Out) ->
    logger:format_log(info, "TS_CREDIT.set_flat_flags for ~p~n", [Out]),
    Flags#route_flags{
      rate = 0.0
      ,rate_increment = 0
      ,rate_minimum = 0
      ,surcharge = 0.0
      ,rate_name = <<>>
      ,flat_rate_enabled = true
     }.

%% match options set in Flags to options available in Rate
%% All options set in Flags must be set in Rate to be usable
-spec(options_match/2 :: (RouteOptions :: list(binary()) | json_object(), RateOptions :: list(binary())) -> boolean()).
options_match({struct, RouteOptions}, RateOptions) ->
    options_match(RouteOptions, RateOptions);
options_match(RouteOptions, RateOptions) ->
    logger:format_log(info, "TS_CREDIT.options_match:~nDID Flags: ~p~nRoute Options: ~p~n", [RouteOptions, RateOptions]),
    lists:all(fun(Opt) -> props:get_value(Opt, RateOptions, false) =/= false end, RouteOptions).
