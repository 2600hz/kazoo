-module(ts_importer).

-export([import_rates_csv/1]).

-include("ts.hrl").

%% import the CSV into the rates DB 
import_rates_csv(PathToCSV) ->
    couch_mgr:db_create(?TS_RATES_DB),
    {ok, Contents} = file:read_file(PathToCSV),
    parse(binary:split(Contents, <<"\n">>, [global]), []).

parse([], Docs) ->
    couch_mgr:save_docs(?TS_RATES_DB, Docs);
parse([<<>>], Docs) ->
    couch_mgr:save_docs(?TS_RATES_DB, Docs);
parse(Lines, Docs) when length(Docs) >= 100 ->
    spawn(fun() -> couch_mgr:save_docs(?TS_RATES_DB, Docs) end),
    ?LOG_SYS("Saving 100 docs"),
    parse(Lines, []);
parse([Line | Lines], Docs) ->
    {Prefix, CC, De, InternalCost, RC} = case binary:split(Line, <<",">>, [global]) of
					     [P, C | Rest] ->
						 [R, I | Rest1] = lists:reverse(Rest),
						 {P, C, list_to_binary(Rest1), I, R} 
					 end,

    CountryCode = binary:replace(CC, <<"\"">>, <<>>, [global]),
    Desc = binary:replace(De, <<"\"">>, <<>>, [global]),
    RateCost = binary:replace(RC, <<"\n">>, <<>>),

    RateId = <<CountryCode/binary, "-", Prefix/binary>>,
    RateObj = case couch_mgr:open_doc(?TS_RATES_DB, RateId) of
		  {ok, {struct, [_|_]}=JObj} ->
		      JObj;
		  {error, _} ->
		      ?EMPTY_JSON_OBJECT
	      end,
    Doc = set_vs(RateObj, [
			   {<<"direction">>, [<<"inbound">>, <<"outbound">>], optional}
			   ,{<<"options">>, ?EMPTY_JSON_OBJECT, optional}
			   ,{<<"flatrate">>, true, optional}
			   ,{<<"rate_increment">>, <<"60">>, optional}
			   ,{<<"rate_minimum">>, <<"60">>, optional}
			   ,{<<"rate_surcharge">>, <<"0.00">>, optional}
			   ,{<<"rate_cost">>, RateCost}
			   ,{<<"internal_cost">>, InternalCost}
			   ,{<<"routes">>, [<<"^011", (wh_util:to_binary(Prefix))/binary, "(\\d*)$">>, <<"^\\+", (wh_util:to_binary(Prefix))/binary, "(\\d*)$">>]}
			   ,{<<"weight">>, (byte_size(Prefix) * 10), optional}
			   ,{<<"iso_country_code">>, CountryCode}
			   ,{<<"rate_name">>, Desc}
			   ,{<<"_id">>, RateId}
			   ,{<<"prefix">>, Prefix}
			  ]),

    parse(Lines, [Doc | Docs]).

set_vs(JObj, KVs) ->
    lists:foldr(fun({K, V}, JObj0) ->
			wh_json:set_value(K, V, JObj0);
		   ({K, V, optional}, JObj0) ->
			case wh_json:get_value(K, JObj0) of
			    undefined -> wh_json:set_value(K, V, JObj0);
			    _ -> JObj0
			end
		end, JObj, KVs).
