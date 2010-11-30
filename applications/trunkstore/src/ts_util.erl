%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% utility functions for Trunkstore
%%% @end
%%% Created : 24 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_util).

-export([find_ip/1]).

-include_lib("kernel/include/inet.hrl"). %% for hostent record, used in find_ip/1

-spec(find_ip/1 :: (Domain :: binary() | list()) -> list()).
find_ip(Domain) when is_binary(Domain) ->
    find_ip(binary_to_list(Domain));
find_ip(Domain) when is_list(Domain) ->
    case inet:gethostbyname(Domain, inet) of %% eventually we'll want to support both IPv4 and IPv6
	{error, _Err} ->
	    Domain;
	{ok, Hostent} when is_record(Hostent, hostent) ->
	    case Hostent#hostent.h_addr_list of
		[] -> Domain;
		[Addr | _Rest] -> inet_parse:ntoa(Addr)
	    end
    end.
