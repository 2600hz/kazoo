%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Receive a callmgr CDR and augment it with Trunkstore-specific fields,
%%% storing it in the database.
%%% If the CDR is already stored (from another Trunkstore app), the save_doc/2
%%% should cause the process to crash, not storing it again.
%%% @end
%%% Created : 24 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_cdr).

-export([store_cdr/2]).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-include("ts.hrl").

-spec(store_cdr/2 :: (CDRProp :: proplist(), Flags :: #route_flags{}) -> no_return()).
store_cdr(CDRProp, #route_flags{routes_generated=RGs, direction=Dir, account_doc_id=DocID}=Flags) ->
    RateUsed = case Dir of
		   <<"inbound">> -> Flags#route_flags.inbound_rate_name;
		   <<"outbound">> -> Flags#route_flags.outbound_rate_name
	       end,
    TScdr = [{<<"_id">>, get_value(<<"Call-ID">>, CDRProp)}
	     ,{<<"Routes-Available">>, RGs}
	     ,{<<"Route-Used">>, find_route_used(Dir, get_value(<<"To-Uri">>, CDRProp), RGs)}
	     ,{<<"Rate-Used">>, RateUsed}
	     ,{<<"Customer-Account-ID">>, DocID}
	     | CDRProp],
    format_log(info, "TS_CDR: Saving ~p~n", [TScdr]),
    {ok, _} = couch_mgr:save_doc(?TS_CDR_DB, TScdr).

-spec(find_route_used/3 :: (Direction :: binary(), ToUri :: binary(), Routes :: list(tuple(struct, proplist()))) -> proplist()).
find_route_used(<<"outbound">>, ToUri, Routes) ->
    [ToUser, _ToDomain] = binary:split(ToUri, <<"@">>),
    lists:foldl(fun({struct, RouteData}=R, Acc) ->
			case get_value(<<"Route">>, RouteData) of
			    <<"sip:", DS/binary>> ->
				[DSUser, DSDomain] = binary:split(DS, <<"@">>),
				DS_IP = whistle_util:to_binary(ts_util:find_ip(DSDomain)),
				case binary:match(<<DSUser/bitstring, "@", DS_IP/bitstring>>, ToUri) =/= nomatch orelse
				    binary:match(DS, ToUri) of
				    true -> R; % Matched by IP
				    nomatch -> Acc; % neither matched
				    _ -> R % matched by hostname
				end;
			    [<<"user:", _U/binary>>, DID] ->
				case whistle_util:to_e164(ToUser) =:= whistle_util:to_e164(DID) of
				    true -> R;
				    false -> Acc
				end
			end
		end, [], Routes);
find_route_used(<<"inbound">>, _, _) -> [].

