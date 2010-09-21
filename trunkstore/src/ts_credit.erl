%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Check user's account for appropriate credit
%%% @end
%%% Created : 20 Sep 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_credit).

-export([check/1]).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2]).

-include("ts.hrl").

check(Flags) ->
    Rate = 0,
    Options = [],
    case ts_couch:has_view(?TS_DB, ?TS_VIEW_RATES) andalso
	ts_couch:get_results(?TS_DB, ?TS_VIEW_RATES, Options) of
	false ->
	    format_log(error, "TS_CREDIT(~p): No ~p view found while looking up ~p~n"
		       ,[self(), ?TS_VIEW_RATES]),
	    {error, "No DIDLOOKUP view"};
	[] ->
	    format_log(info, "TS_CREDIT(~p): No Rates defined~n", [self()]),
	    {error, "No matching rates"};
	[{ViewProp} | _Rest] ->
	    format_log(info, "TS_CREDIT(~p): Rates found for~n~p~n", [self(), ViewProp]),
	    {struct, Rates} = mochijson2:decode(get_value(<<"value">>, ViewProp)),
	    {ok, Flags};
	_Else ->
	    format_log(error, "TS_CREDIT(~p): Got something unexpected~n~p~n", [self(), _Else]),
	    {error, "Unexpected error in credit check"}
    end.

