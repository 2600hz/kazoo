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
    spawn(fun() -> couch_mgr:save_docs(Docs) end),
    logger:format_log(info, "Saving 100 docs~n", []),
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
    %% logger:format_log(info, "Line: ~p~n", [Line]),
    %% logger:format_log(info, "Pieces: ~p, ~p, ~p, ~p, ~p~n", [Prefix, CountryCode, Desc, InternalCost, RateCost]),

    RateId = <<CountryCode/binary, "-", Prefix/binary>>,
    RateObj = case couch_mgr:open_doc(?TS_RATES_DB, RateId) of
		  {ok, {struct, [_|_]}=JObj} ->
		      JObj;
		  {error, _} ->
		      ?EMPTY_JSON_OBJECT
	      end,
    Doc = set_vs(RateObj, [
			   {<<"direction">>, [<<"inbound">>, <<"outbound">>]}
			   ,{<<"options">>, ?EMPTY_JSON_OBJECT}
			   ,{<<"flatrate">>, true}
			   ,{<<"rate_increment">>, <<"60">>}
			   ,{<<"rate_minimum">>, <<"60">>}
			   ,{<<"rate_surcharge">>, <<"0.00">>}
			   ,{<<"rate_cost">>, RateCost}
			   ,{<<"internal_cost">>, InternalCost}
			   ,{<<"routes">>, [<<"^011", (whistle_util:to_binary(Prefix))/binary, "(\\d*)$">>]}
			   ,{<<"weight">>, (byte_size(Prefix) * 10)}
			   ,{<<"iso_country_code">>, CountryCode}
			   ,{<<"rate_name">>, Desc}
			   ,{<<"_id">>, RateId}
			  ]),

    parse(Lines, [Doc | Docs]).

set_vs(JObj, KVs) ->
    lists:foldr(fun({K, V}, JObj0) ->
			wh_json:set_value(K, V, JObj0)
		end, JObj, KVs).

%% Rate Doc
%% routes = [ "^011<country-code>(\\d*)$" ]
%% rate_cost = *cost, in $0.00, we charge
%% rate_increment = 60
%% rate_minimum = 60
%% rate_surcharge = 0.00
%% internal_cost = $$$

%% weight = length(<country-code>) * 10

%% direction = ["inbound", "outbound"] || existing
%% options = {} || existing
%% flatrate = true || existing
%% iso_country_code = "EGY" || existing


%% CSV Fields "E164 Prefix","ISO country code","Description","Our Cost","Advertised Rate"
%% E164 Prefix -> routes.<country-code>
%% ISO Country Code -> iso_country_code
%% Description -> part of _id
%% Our Cost -> internal_rate
%% Advertised Rate -> rate_cost

%% 20,"EGY","Egypt - Fixed",0.0796,0.09552
%% 2010,"EGY","Egypt - Mobile",0.0927,0.11124
%% 2011,"EGY","Egypt - Mobile Etisalat",0.0807,0.09684
%% 2012,"EGY","Egypt - Mobile",0.0927,0.11124
%% 2014,"EGY","Egypt - Mobile Etisalat",0.0807,0.09684
%% 20150,"EGY","Egypt, Arab Republic of",0.0927,0.11124
%% 20151,"EGY","Egypt, Arab Republic of",0.0927,0.11124
%% 20152,"EGY","Egypt, Arab Republic of",0.0807,0.09684
%% 2016,"EGY","Egypt - Mobile",0.0927,0.11124
%% 2017,"EGY","Egypt - Mobile",0.0927,0.11124
%% 2018,"EGY","Egypt - Mobile",0.0927,0.11124
%% 2019,"EGY","Egypt - Mobile",0.0927,0.11124
%% 212,"MAR","Morocco - Fixed",0.0159,0.01908
