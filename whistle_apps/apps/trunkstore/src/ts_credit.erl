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
-export([start_link/0, reserve/5]).

-spec start_link/0 :: () -> ignore.
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
    ?LOG("setup ~s DB for rates", [?TS_RATES_DB]),
    ignore.

-spec reserve/5 :: (ToDID, CallID, AcctID, Direction, RouteOptions) -> {ok, proplist()} | {error, no_rate_found} when
      ToDID :: binary(),
      CallID :: binary(),
      AcctID :: binary(),
      Direction :: inbound | outbound,
      RouteOptions :: [binary(),...] | [].
reserve(ToDID, CallID, AcctID, Direction, RouteOptions) ->
    <<Start:1/binary, _/binary>> = Number = whistle_util:to_1npan(ToDID),
    ?LOG("searching for rates in the range ~s to ~s", [Start, Number]),
    case couch_mgr:get_results(?TS_RATES_DB, <<"lookuprates/lookuprate">>, [{<<"startkey">>, whistle_util:to_integer(Start)}
									    ,{<<"endkey">>, whistle_util:to_integer(Number)}]) of
	{ok, []} -> ?LOG("rate lookup had no results"), {error, no_rate_found};
	{error, _E} -> ?LOG("rate lookup error: ~p", [_E]), {error, no_rate_found};
	{ok, Rates} ->
	    _ = [?LOG("fetched rate definition: ~s", [wh_json:get_value(<<"id">>, Rate)]) || Rate <- Rates],
	    Matching = filter_rates(ToDID, Direction, RouteOptions, Rates),
	    case lists:usort(fun sort_rates/2, Matching) of
		[] -> ?LOG("no rates left after filter"), {error, no_rate_found};
		[Rate|_] ->
		    %% wh_timer:tick("post usort data found"),
		    ?LOG("using rate definition ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),

		    BaseCost = whistle_util:to_float(wh_json:get_value(<<"rate_cost">>, Rate)) * whistle_util:to_integer(wh_json:get_value(<<"rate_minimum">>, Rate))
			+ whistle_util:to_float(wh_json:get_value(<<"rate_surcharge">>, Rate)),

		    _ = ts_acctmgr:reserve_trunk(AcctID, CallID, BaseCost, ts_util:is_flat_rate_eligible(ToDID)),

		    {ok, [{<<"Rate">>, whistle_util:to_binary(wh_json:get_value(<<"rate_cost">>, Rate))}
			  ,{<<"Rate-Increment">>, whistle_util:to_binary(wh_json:get_value(<<"rate_increment">>, Rate))}
			  ,{<<"Rate-Minimum">>, whistle_util:to_binary(wh_json:get_value(<<"rate_minimum">>, Rate))}
			  ,{<<"Surcharge">>, whistle_util:to_binary(wh_json:get_value(<<"rate_surcharge">>, Rate, 0))}
			  ,{<<"Rate-Name">>, whistle_util:to_binary(wh_json:get_value(<<"rate_name">>, Rate))}
			  ,{<<"Base-Cost">>, whistle_util:to_binary(BaseCost)}
			 ]}
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

%% match options set in Flags to options available in Rate
%% All options set in Flags must be set in Rate to be usable
%% RouteOptions come from client's DID/server/account
-spec options_match/2 :: (RouteOptions, RateOptions) -> boolean() when
      RouteOptions :: list(binary()) | json_object(),
      RateOptions :: list(binary()) | json_object().
options_match(RouteOptions, {struct, RateOptions}) ->
    options_match(RouteOptions, RateOptions);
options_match({struct, RouteOptions}, RateOptions) ->
    options_match(RouteOptions, RateOptions);
options_match([], []) ->
    true;
options_match([], _) ->
    true;
options_match(RouteOptions, RateOptions) ->
    lists:all(fun(Opt) -> props:get_value(Opt, RateOptions, false) =/= false end, RouteOptions).
