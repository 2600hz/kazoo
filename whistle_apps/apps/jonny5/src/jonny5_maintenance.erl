%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Maintenance functions for introspection of the jonny 5 whapp
%%% @end
%%% Created :  2 Nov 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(jonny5_maintenance).

-export([refresh/0, refresh/1, local_summary/0, local_summary/1, local_summary/2
	 ,remote_summary/0, remote_summary/1, remote_summary/2]).

-include("jonny5.hrl").

-define(LOCAL_SUMMARY_ROW_FORMAT, " ~4.s | ~35.s | ~35.s | ~13.s | ~13.s | ~13.s |~n").
-define(LOCAL_SUMMARY_HEADER, io:format(?LOCAL_SUMMARY_ROW_FORMAT, [<<>>, <<"J5 NODE">>, <<"ACCOUNT ID">>, <<"TWO WAY (MAX)">>, <<"INBOUND (MAX)">>, <<"PREPAY">>])).

-define(REMOTE_SUMMARY_ROW_FORMAT, " ~4.s | ~35.s | ~35.s | ~13.s | ~13.s | ~13.s |~n").
-define(REMOTE_SUMMARY_HEADER, io:format(?REMOTE_SUMMARY_ROW_FORMAT, [<<>>, <<"J5 NODE">>, <<"ACCOUNT ID">>, <<"TWO WAY (MAX)">>, <<"INBOUND (MAX)">>, <<"PREPAY">>])).

-define(REMOTE_TIMEOUT, 1000).

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
	    ?LOCAL_SUMMARY_HEADER,
	    do_summary(Accts, fun(JObj) -> print_summary(JObj, ?LOCAL_SUMMARY_ROW_FORMAT) end)
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
	    ?LOCAL_SUMMARY_HEADER,
	    do_summary([AcctJObj], fun(JObj) -> print_summary(JObj, ?LOCAL_SUMMARY_ROW_FORMAT) end)
    end.

-spec remote_summary/0 :: () -> 'ok'.
-spec remote_summary/1 :: (ne_binary()) -> 'ok'.
-spec remote_summary/2 :: (ne_binary(), 'details' | 'summary') -> 'ok'.
remote_summary() ->
    case j5_util:fetch_all_accounts() of
	[] -> io:format("No accounts are being tracked~n");
	Accts ->
	    Q = start_amqp(),
	    _ = [begin
		     AcctId = wapi_jonny5:get_acct_id(Acct),
		     publish_req(Q, AcctId)
		 end
		 || Acct <- Accts],

	    ?REMOTE_SUMMARY_HEADER,
	    do_remote_summary(fun(JObj, C) -> print_summary(JObj, ?REMOTE_SUMMARY_ROW_FORMAT, C) end, ?REMOTE_TIMEOUT)
    end.

remote_summary(AcctId) ->
    remote_summary(AcctId, summary).

remote_summary(AcctId, details) ->
    Q = start_amqp(),
    publish_req(Q, AcctId),
    ?REMOTE_SUMMARY_HEADER,
    do_remote_summary(fun print_details/1, ?REMOTE_TIMEOUT);
remote_summary(AcctId, _) ->
    Q = start_amqp(),
    publish_req(Q, AcctId),
    ?REMOTE_SUMMARY_HEADER,
    do_remote_summary(fun(JObj, C) -> print_summary(JObj, ?REMOTE_SUMMARY_ROW_FORMAT, C) end, ?REMOTE_TIMEOUT).

publish_req(Q, AcctId) ->
    wapi_money:publish_balance_req([{<<"Account-ID">>, AcctId}
				    ,{<<"Server-ID">>, Q}
				    ,{<<"App-Name">>, ?MODULE}
				    ,{<<"App-Version">>, ?APP_VERSION}
				   ]).

do_remote_summary(PrintFun, Timeout) ->
    do_remote_summary(PrintFun, Timeout, 0).

do_remote_summary(PrintFun, Timeout, Count) ->
    receive
	#'basic.consume_ok'{} -> do_remote_summary(PrintFun, Timeout, Count);
	{#'basic.deliver'{}, #amqp_msg{props = #'P_basic'{content_type = ?DEFAULT_CONTENT_TYPE}, payload=Payload}} ->
	    JObj0 = wh_json:decode(Payload),
	    true = wapi_money:balance_resp_v(JObj0),
	    JObj = wh_json:normalize_jobj(JObj0),

	    _ = PrintFun(JObj, Count),
	    do_remote_summary(PrintFun, Timeout, Count+1);
	_Msg ->
	    io:format("Unhandled Msg: ~p~n", [_Msg]),
	    do_remote_summary(PrintFun, Timeout, Count)
    after Timeout ->
	    io:format("Finished waiting...~n", [])
    end.

-spec do_summary/2 :: (json_objects(), fun((json_object()) -> 'ok')) -> no_return().
do_summary(JObjs, PrintFun) ->
    _ = [PrintFun(JObj) || JObj <- JObjs],
    ok.

-spec start_amqp/0 :: () -> ne_binary().
start_amqp() ->
    Q = amqp_util:new_queue(),
    wapi_self:bind_q(Q, []),
    amqp_util:basic_consume(Q),
    Q.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary/2 :: (json_object(), nonempty_string()) -> 'ok' | no_return().
-spec print_summary/3 :: (json_object(), nonempty_string(), binary() | integer()) -> 'ok' | no_return().
print_summary(AcctJObj, RowFormatStr) ->
    print_summary(AcctJObj, RowFormatStr, <<>>).
print_summary(AcctJObj, RowFormatStr, Count) when is_integer(Count) ->
    print_summary(AcctJObj, RowFormatStr, wh_util:to_list(Count));
print_summary(AcctJObj, RowFormatStr, Count) ->
    TwoWayMax = wh_json:get_binary_value(<<"max_two_way">>, AcctJObj, <<"0">>),
    TwoWayAvail = wh_json:get_binary_value(<<"two_way">>, AcctJObj, <<"0">>),

    InMax = wh_json:get_binary_value(<<"max_inbound">>, AcctJObj, <<"0">>),
    InAvail = wh_json:get_binary_value(<<"inbound">>, AcctJObj, <<"0">>),

    Prepay = wh_json:get_binary_value(<<"prepay">>, AcctJObj, <<"0">>),

    AcctName = wh_json:get_value(<<"account_id">>, AcctJObj),

    Trunks = wh_json:get_value(<<"trunks">>, AcctJObj, []),

    Node = wh_json:get_value(<<"node">>, AcctJObj, wh_util:to_binary(node())),

    Two = list_to_binary([TwoWayAvail, "(", TwoWayMax, ")"]),
    In = list_to_binary([InAvail, "(", InMax, ")"]),

    io:format(RowFormatStr, [Count, Node, AcctName, Two, In, Prepay]),
    case Trunks of
	[] -> ok;
	Ts ->
	    ?TRUNKS_HEADER,
	    [io:format(?TRUNKS_ROW_FORMAT, [wh_json:get_value(<<"callid">>, TObj, <<"?">>), wh_json:get_value(<<"type">>, TObj, <<"?">>)])
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
