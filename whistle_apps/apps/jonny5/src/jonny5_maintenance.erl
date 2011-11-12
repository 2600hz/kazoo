%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Maintenance functions for introspection of the jonny 5 whapp
%%% @end
%%% Created :  2 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(jonny5_maintenance).

-export([refresh/0, refresh/1, local_summary/0, local_summary/1, local_summary/2]).

-include("jonny5.hrl").

-define(SUMMARY_ROW_FORMAT, "| ~35.s | ~13.s | ~13.s | ~13.s |~n").
-define(SUMMARY_HEADER, io:format(?SUMMARY_ROW_FORMAT, [<<"ACCOUNT ID">>, <<"TWO WAY (MAX)">>, <<"INBOUND (MAX)">>, <<"PREPAY">>])).

-define(TRUNKS_ROW_FORMAT, "| ~35.s | ~10.s |~n").
-define(TRUNKS_HEADER, io:format(?TRUNKS_ROW_FORMAT, [<<"Call ID">>, <<"Trunk Type">>])).

refresh() ->
    j5_util:refresh_all_accounts().

refresh(AcctID) ->
    j5_util:refresh_account(AcctID).

local_summary() ->
    case j5_util:fetch_all_accounts() of
	[] -> io:format("No accounts are being tracked~n");
	Accts ->
	    ?SUMMARY_HEADER,
	    do_summary(Accts, fun print_summary/1)
    end.

local_summary(AcctID) ->
    local_summary(AcctID, summary).

-spec local_summary/2 :: (ne_binary(), 'details' | 'summary') -> 'ok'.
local_summary(AcctID, details) ->
    case j5_util:fetch_account(AcctID) of
	{error, not_found} ->
	    io:format("Account ~s not being tracked~n", [AcctID]);
	AcctJObj ->
	    do_summary([AcctJObj], fun print_details/1)
    end;
local_summary(AcctID, _) ->
    case j5_util:fetch_account(AcctID) of
	{error, not_found} ->
	    io:format("Account ~s not being tracked~n", [AcctID]);
	AcctJObj ->
	    ?SUMMARY_HEADER,
	    do_summary([AcctJObj], fun print_summary/1)
    end.

-spec do_summary/2 :: (json_objects(), fun((json_object()) -> 'ok')) -> no_return().
do_summary(JObjs, PrintFun) ->
    _ = [PrintFun(JObj) || JObj <- JObjs],
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary/1 :: (json_object()) -> 'ok' | no_return().
print_summary(AcctJObj) ->
    TwoWayMax = wh_json:get_binary_value(<<"max_two_way">>, AcctJObj),
    TwoWayAvail = wh_json:get_binary_value(<<"two_way">>, AcctJObj),

    InMax = wh_json:get_binary_value(<<"max_inbound">>, AcctJObj),
    InAvail = wh_json:get_binary_value(<<"inbound">>, AcctJObj),

    Prepay = wh_json:get_binary_value(<<"prepay">>, AcctJObj),

    AcctName = wh_json:get_value(<<"account">>, AcctJObj),

    Trunks = wh_json:get_value(<<"trunks">>, AcctJObj, []),

    Two = list_to_binary([TwoWayAvail, "(", TwoWayMax, ")"]),
    In = list_to_binary([InAvail, "(", InMax, ")"]),

    io:format(?SUMMARY_ROW_FORMAT, [AcctName, Two, In, Prepay]),
    case Trunks of
	[] -> ok;
	Ts ->
	    ?TRUNKS_HEADER,
	    [io:format(?TRUNKS_ROW_FORMAT, [wh_json:get_value(<<"callid">>, TObj), wh_json:get_value(<<"type">>, TObj)])
	       || TObj <- Ts]
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_details/1 :: (json_object()) -> ['ok',...].
print_details(AcctJObj) ->
    [io:format("~s: ~s~n", [K, wh_util:to_list(V)]) || {K, V} <- wh_json:to_proplist(AcctJObj)].
