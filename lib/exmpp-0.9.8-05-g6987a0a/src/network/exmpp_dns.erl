%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jack Moffitt <jack@metajack.im>

%% 
%% @doc SRV lookup support
%% This module provides functions to return an ordered list of servers
%% for a particular SRV service at a domain.  This list is randomized according
%% to the weights and priorities in the SRV record.
%%
%% As a convenience, if SRV records are not found, a one element list of the
%% Domain and Default port will be returned.  This allows the function to 
%% also return useful data when the domain does not support SRV, for instance 
%% at localhost.
%% @end

-module(exmpp_dns).
-author("Jack Moffitt <jack@metajack.im>").

-include_lib("kernel/include/inet.hrl").

% Exports
-export([get_servers/4,
         get_c2s/1,
         get_c2s/2,
         get_s2s/1,
         get_s2s/2
         ]).

srv_lookup(Name, Domain, DefaultPort, Retries) when Retries < 1 ->
      %% srv lookup failed, so create a fake result
      {ok, #hostent{h_name=Name,
                    h_aliases=[],
                    h_addrtype=srv,
                    h_length=1,
                    h_addr_list=[{0, 0, DefaultPort, Domain}]}};

srv_lookup(Name, Domain, DefaultPort, Retries) ->
    case inet_res:getbyname(Name, srv) of
      {error, _} ->
          %% srv lookup failed, retry.
          srv_lookup(Name, Domain, DefaultPort, Retries -1);
      {ok, _} = R ->
          R
    end.

%% @doc Do an SRV record query.
%% This returns an ordered list (by priority and then weight) of the SRV
%% results.  If no results are found or if no SRV record exists, it will
%% return the domain and the default port as an SRV result as a convenience.
%% This allows it to work with localhost.
%%
%% @spec do_query(Name::string(), Domain::string(),
%%                DefaultPort::integer()) -> List
%%       List = [Item]
%%       Item = {Priority, Weight, Host, Port}
%%       Priority = integer()
%%       Weight = integer()
%%       Host = string()
%%       Port = integer()
%%
%% @private

do_query(Name, Domain, DefaultPort) ->
    Res = srv_lookup(Name, Domain, DefaultPort, 2),
    case Res of
	{error, _} = E ->
	    E;
	{ok, HEnt} ->
	    case HEnt#hostent.h_addr_list of
		[] ->
		    %% we got no results, so make the fake one
		    [{0, 0, DefaultPort, Domain}];
		AddrList ->
		    %% sort by priority then by weight
		    lists:sort(fun ({PriorityA, WeightA, _, _}, 
				    {PriorityB, WeightB, _, _}) ->
				       if 
					   PriorityA < PriorityB ->
					       true;
					   WeightA < WeightB, 
					   PriorityA =:= PriorityB ->
					       true;
					   true ->
					       false
				       end
			       end, AddrList)
	    end
    end.

%% @doc Return the ordered SRV records for service 'xmpp-client'
%% This function returns a list of {Host, Port} tuples or an empty list
%% on an error.  They are ordered randomly based on priority and weight.
%% It is expected that a client try each record in order until a connection
%% can be made.
%%
%% @spec get_c2s(Domain::string(),
%%               DefaultPort::string()) -> List
%%       List = [Item]
%%       Item = {Host, Port}
%%       Host = string()
%%       Port = integer()

get_c2s(Domain, DefaultPort) ->		  
    get_servers("xmpp-client", "tcp", Domain, DefaultPort).

get_c2s(Domain) ->		  
    get_c2s(Domain, 5222).

%% @doc Return the ordered SRV records for service 'xmpp-server'
%% This function returns a list of {Host, Port} tuples or an empty list
%% on an error.  They are ordered randomly based on priority and weight.
%% It is expected that a server try each record in order until a connection
%% can be made.
%%
%% @spec get_s2s(Domain::string(),
%%               DefaultPort::string()) -> List
%%       List = [Item]
%%       Item = {Host, Port}
%%       Host = string()
%%       Port = integer()

get_s2s(Domain, DefaultPort) ->		  
    get_servers("xmpp-server", "tcp", Domain, DefaultPort).

get_s2s(Domain) ->		  
    get_s2s(Domain, 5269).

%% @doc Return the ordered SRV records for a service.
%% This function returns a list of {Host, Port} tuples or an empty list
%% on an error.  They are ordered randomly based on priority and weight.
%% It is expected that a client try each record in order until a connection
%% can be made.
%%
%% @spec get_servers(Server::string(),
%%                   Proto::string(),
%%                   Domain::string(),
%%                   DefaultPort::string()) -> List
%%       List = [Item]
%%       Item = {Host, Port}
%%       Host = string()
%%       Port = integer()

get_servers(Service, Proto, Domain, DefaultPort) ->		  
    {S1, S2, S3} = now(),
    random:seed(S1, S2, S3),
    Name = "_" ++ Service ++ "._" ++ Proto ++ "." ++ Domain,
    AddrList = case do_query(Name, Domain, DefaultPort) of
		   {error, _} ->
		       [];
		   L ->
		       L
	       end,
    do_sort(AddrList).


%% @doc Helper function to set up do_sort/2.
%%
%% @private

do_sort([]) ->
    [];
do_sort(AddrList) ->
    lists:reverse(do_sort(AddrList, [])).

%% @doc Sort the SRV record list randomly by priority and weight.
%%
%% @private

do_sort([], Acc) ->
    Acc;
do_sort(AddrList, Acc) ->
    {ok, Pick, Rest} = pick_server(AddrList),
    do_sort(Rest, [Pick | Acc]).

%% @doc Pick a record at random based on priority and weighting.
%% The picking algorithm is from RFC 2782.
%%
%% @spec pick_server(AddrList) -> {ok, {string(), integer()}, Rest}
%%       AddrList = [{Priority, Weight, Host, Port}]
%%       Priority = integer()
%%       Weight = integer()
%%       Host = strin()
%%       Port = integer()
%%       Rest = [{Host, Port}]
%%
%% @private

pick_server(AddrList) ->
    Priority = element(1, lists:nth(1, AddrList)),
    List = lists:takewhile(fun ({P, _, _, _}) -> P == Priority end, AddrList),
    Pick = case List of
	       [X] ->
		   X;
	       _ ->
		   {Weighted, Sum} = lists:mapfoldl(
				       fun ({P, W, Port, Host}, Sum) ->
					       {{P, W, Port, Host, Sum + W}, 
						Sum + W}
				       end, 0, List),
		   R = random:uniform(Sum + 1) - 1,
		   [Take | _]  = lists:dropwhile(fun ({_, _, _, _, S}) ->
							 S < R
						 end, Weighted),
		   {Pri, Wgt, Prt, Hst, _} = Take,
		   {Pri, Wgt, Prt, Hst}
	   
	   end,
    {_, _, Port, Addr} = Pick,
    {ok, {Addr, Port}, lists:delete(Pick, AddrList)}.
