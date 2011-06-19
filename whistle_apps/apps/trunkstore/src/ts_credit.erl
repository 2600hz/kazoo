%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Check user's account for appropriate credit
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_credit).

-include("ts.hrl").

%% API
-export([start_link/0, check/1, reserve/5]).

-spec(start_link/0 :: () -> ignore).
start_link() ->
    couch_mgr:db_create(?TS_RATES_DB),
    {ok, DBInfo} = couch_mgr:db_info(?TS_RATES_DB),

    _ = case wh_json:get_value(<<"doc_count">>, DBInfo) of
	    0 -> couch_mgr:load_doc_from_file(?TS_RATES_DB, trunkstore, <<"sample_rate_doc.json">>);
	    _ -> ok
	end,

    _ = case couch_mgr:update_doc_from_file(?TS_RATES_DB, trunkstore, <<"lookuprates.json">>) of
	    {ok, _} -> ok;
	    _ -> couch_mgr:load_doc_from_file(?TS_RATES_DB, trunkstore, <<"lookuprates.json">>)
	end,
    ignore.

-spec(reserve/5 :: (ToDID :: binary(), CallID :: binary(), AcctID :: binary(), Direction :: inbound | outbound, RouteOpts :: json_object()) -> tuple(ok, proplist())).
reserve(ToDID, CallID, AcctID, inbound, RouteOpts) ->
    {ok, #route_flags{
      rate = R
      ,rate_increment = RI
      ,rate_minimum = RM
      ,surcharge = S
      ,rate_name = RN
%%       ,flat_rate_enabled = FRE
      }} = ?MODULE:check(#route_flags{to_user=ToDID, direction = <<"inbound">>, route_options=RouteOpts, account_doc_id=AcctID, callid=CallID, flat_rate_enabled = true}),
    {ok, [{<<"Rate">>, R}, {<<"Rate-Increment">>, RI}, {<<"Rate-Minimum">>, RM}, {<<"Surcharge">>, S}, {<<"Rate-Name">>, RN}]}.

-spec(check/1 :: (Flags :: #route_flags{}) -> tuple(ok, #route_flags{}) | tuple(error, atom())).
check(#route_flags{to_user=To, direction=Direction, route_options=RouteOptions
		  ,account_doc_id=AccountDocId, callid=CallID, flat_rate_enabled=FlatRateEnabled}=Flags) ->
    <<Start:1/binary, _/binary>> = Number = whistle_util:to_1npan(To),
    case couch_mgr:get_results(?TS_RATES_DB, <<"lookuprates/lookuprate">>, [{<<"startkey">>, whistle_util:to_integer(Start)}
									    ,{<<"endkey">>, whistle_util:to_integer(Number)}]) of
	{ok, []} -> {error, no_route_found};
	{error, _} -> {error, no_route_found};
	{ok, Rates} ->
	    Matching = filter_rates(To, Direction, RouteOptions, Rates),
	    case lists:usort(fun sort_rates/2, Matching) of
		[] -> {error, no_route_found};
		[Rate|_] ->
		    %% wh_timer:tick("post usort data found"),
		    ?LOG("Rate to use: ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),

		    BaseCost = whistle_util:to_float(wh_json:get_value(<<"rate_cost">>, Rate)) * whistle_util:to_integer(wh_json:get_value(<<"rate_minimum">>, Rate))
			+ whistle_util:to_float(wh_json:get_value(<<"rate_surcharge">>, Rate)),

		    case ts_acctmgr:reserve_trunk(AccountDocId, CallID, BaseCost, FlatRateEnabled) of
			{ok, flat_rate} ->
			    {ok, set_flat_flags(Flags, Direction)};
			{ok, per_min} ->
			    {ok, set_rate_flags(Flags, Direction, Rate)};
			{error, entry_exists}=E ->
			    ?LOG("Failed reserving a trunk; call-id exists in DB"),
			    E;
			{error, no_funds}=E1 ->
			    ?LOG("Failed reserving a trunk; no funds or flat-rate trunks"),
			    E1;
			{error, no_account}=E2 ->
			    ?LOG("Failed reserving a trunk; no account passed: ~p tried", [AccountDocId]),
			    E2;
			{error, no_callid}=E3 ->
			    ?LOG("Failed reserving a trunk; no call id passed: ~p tried", [CallID]),
			    E3;
			{error, not_found}=E4 ->
			    ?LOG("Failed reserving a trunk; ts_acctmgr:reserve_trunk/4 failed"),
			    E4
		    end
	    end
    end.

filter_rates(To, Direction, RouteOptions, Rates) ->
    [ begin
	  Rate = wh_json:get_value(<<"value">>, R),
	  wh_json:set_value(<<"rate_name">>, wh_json:get_value(<<"id">>, R), Rate)
      end || R <- Rates, matching_rate(To, Direction, RouteOptions, R)].

matching_rate(To, Direction, RouteOptions, Rate) ->
    %% need to match direction and options at some point too
    Routes = wh_json:get_value([<<"value">>, <<"routes">>], Rate),

    lists:member(Direction, wh_json:get_value([<<"value">>, <<"direction">>], Rate, [])) andalso
	options_match(RouteOptions, wh_json:get_value([<<"value">>, <<"options">>], Rate, [])) andalso
	lists:any(fun(Regex) -> re:run(To, Regex) =/= nomatch end, Routes).

%% Return true of RateA has higher weight than RateB
sort_rates(RateA, RateB) ->
    ts_util:constrain_weight(wh_json:get_value(<<"weight">>, RateA, 1)) >= ts_util:constrain_weight(wh_json:get_value(<<"weight">>, RateB, 1)).

set_rate_flags(Flags, <<"inbound">>, RateData) ->
    RateName = wh_json:get_value(<<"rate_name">>, RateData),
    Flags#route_flags{
      rate = whistle_util:to_float(wh_json:get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(wh_json:get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(wh_json:get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(wh_json:get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     };
set_rate_flags(Flags, <<"outbound">>, RateData) ->
    RateName = wh_json:get_value(<<"rate_name">>, RateData),
    Flags#route_flags{
      rate = whistle_util:to_float(wh_json:get_value(<<"rate_cost">>, RateData))
      ,rate_increment = whistle_util:to_integer(wh_json:get_value(<<"rate_increment">>, RateData))
      ,rate_minimum = whistle_util:to_integer(wh_json:get_value(<<"rate_minimum">>, RateData))
      ,surcharge = whistle_util:to_float(wh_json:get_value(<<"rate_surcharge">>, RateData, 0))
      ,rate_name = RateName
      ,flat_rate_enabled = false
     }.

set_flat_flags(Flags, <<"inbound">>) ->
    Flags#route_flags{
      rate = 0.0
      ,rate_increment = 0
      ,rate_minimum = 0
      ,surcharge = 0.0
      ,rate_name = <<>>
      ,flat_rate_enabled = true
     };
set_flat_flags(Flags, <<"outbound">>) ->
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
-spec(options_match/2 :: (RouteOptions :: list(binary()), RateOptions :: list(binary()) | json_object()) -> boolean()).
options_match(RouteOptions, {struct, RateOptions}) ->
    options_match(RouteOptions, RateOptions);
options_match(RouteOptions, RateOptions) ->
    lists:all(fun(Opt) -> props:get_value(Opt, RateOptions, false) =/= false end, RouteOptions).
