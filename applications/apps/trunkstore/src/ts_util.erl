%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% utility functions for Trunkstore
%%%
%%% Some functions make use of the inet_parse module. This is an undocumented
%%% module, and as such the functions may change or be removed.
%%%
%%% @end
%%% Created : 24 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_util).

-export([find_ip/1, filter_active_calls/2, get_media_handling/1, current_tstamp/0]).
-export([constrain_weight/1, is_ipv4/1, is_ipv6/1, get_base_channel_vars/1]).

-include("ts.hrl").
-include_lib("kernel/include/inet.hrl"). %% for hostent record, used in find_ip/1

-spec(find_ip/1 :: (Domain :: binary() | list()) -> list()).
find_ip(Domain) when is_binary(Domain) ->
    find_ip(binary_to_list(Domain));
find_ip(Domain) when is_list(Domain) ->
    case inet_parse:address(Domain) of
	{ok, _I} ->
	    io:format("ts_util: is an ip: ~p (~p)~n", [Domain, _I]),
	    Domain;
	Huh ->
	    io:format("ts_util: is a domain: ~p (~p)~n", [Domain, Huh]),
	    case inet:gethostbyname(Domain, inet) of %% eventually we'll want to support both IPv4 and IPv6
		{error, _Err} ->
		    io:format("ts_util: err getting hostname: ~p~n", [_Err]),
		    Domain;
		{ok, Hostent} when is_record(Hostent, hostent) ->
		    case Hostent#hostent.h_addr_list of
			[] -> Domain;
			[Addr | _Rest] -> inet_parse:ntoa(Addr)
		    end
	    end
    end.

is_ipv4(Address) ->
    case inet_parse:ipv4_address(whistle_util:to_list(Address)) of
	{ok, _} -> true;
	{error, _} -> false
    end.

is_ipv6(Address) ->
    case inet_parse:ipv6_address(whistle_util:to_list(Address)) of
	{ok, _} -> true;
	{error, _} -> false
    end.

%% FilterOn: CallID | flat_rate | per_min
%% Remove active call entries based on what Filter criteria is passed in
-spec(filter_active_calls/2 :: (FilterOn :: binary() | flat_rate | per_min, ActiveCalls :: active_calls()) -> active_calls()).
filter_active_calls(flat_rate, ActiveCalls) ->
    lists:filter(fun({_,flat_rate}) -> false; (_) -> true end, ActiveCalls);
filter_active_calls(per_min, ActiveCalls) ->
    lists:filter(fun({_,per_min}) -> false; (_) -> true end, ActiveCalls);
filter_active_calls(CallID, ActiveCalls) ->
    lists:filter(fun({CallID1,_}) when CallID =:= CallID1 -> false;
		    (CallID1) when CallID =:= CallID1 -> false;
		    (_) -> true end, ActiveCalls).

-spec(get_media_handling/1 :: (Type :: binary() | undefined) -> binary()).
get_media_handling(<<"process">>) -> <<"process">>;
get_media_handling(_) -> <<"bypass">>.

%% returns, in seconds, the current timestamp
-spec(current_tstamp/0 :: () -> integer()).
current_tstamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec(constrain_weight/1 :: (W :: binary() | integer()) -> integer()).
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(whistle_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.

%% return rate information as channel vars
get_base_channel_vars(Flags) ->
    ChannelVars0 = [{<<"Rate">>, whistle_util:to_binary(Flags#route_flags.rate)}
		    ,{<<"Rate-Increment">>, whistle_util:to_binary(Flags#route_flags.rate_increment)}
		    ,{<<"Rate-Minimum">>, whistle_util:to_binary(Flags#route_flags.rate_minimum)}
		    ,{<<"Surcharge">>, whistle_util:to_binary(Flags#route_flags.surcharge)}
		   ],

    case binary:longest_common_suffix([Flags#route_flags.callid, <<"-failover">>]) of
	0 -> ChannelVars0;
	_ -> [{<<"Failover-Route">>, <<"true">>} | ChannelVars0]
    end.
