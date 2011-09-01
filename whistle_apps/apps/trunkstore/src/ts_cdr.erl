%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Receive a callmgr CDR and augment it with Trunkstore-specific fields,
%%% storing it in the database.
%%% If the CDR is already stored (from another Trunkstore app), the save_doc/2
%%% should cause the process to crash, not storing it again.
%%% @end
%%% Created : 24 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_cdr).

-export([start_link/0, store_cdr/3, fetch_cdr/2, store/1]).

-include("ts.hrl").

start_link() ->
    {ok, proc_lib:spawn_link(fun cdr_init/0)}.

cdr_init() ->
    {_, {H,Min,S}} = calendar:universal_time(),
    MillisecsToMidnight = ?MILLISECS_PER_DAY - timer:hms(H,Min,S),

    _ = create_cdr_db(ts_util:todays_db_name(?TS_CDR_PREFIX)),

    cdr_loop(MillisecsToMidnight).

cdr_loop(Timeout) ->
    receive
    after
	Timeout ->
	    _ = create_cdr_db(ts_util:todays_db_name(?TS_CDR_PREFIX)),
	    cdr_loop(?MILLISECS_PER_DAY)
    end.

create_cdr_db(DB) ->
    couch_mgr:db_create(DB),
    case couch_mgr:load_doc_from_file(DB, trunkstore, <<"ts_cdr.json">>) of
	{ok, _} -> ok;
	{error, _} -> couch_mgr:update_doc_from_file(DB, trunkstore, <<"ts_cdr.json">>)
    end.

-spec store/1 :: (CDR) -> tuple(ok, json_object()) | tuple(error, atom()) when
      CDR :: json_object().
store(CDR) ->
    DB = ts_util:todays_db_name(?TS_CDR_PREFIX),
    CDR1 = wh_json:set_value(<<"_id">>, wh_json:get_value(<<"Call-ID">>, CDR), CDR),
    couch_mgr:save_doc(DB, CDR1).

-spec(store_cdr/3 :: (CDR :: json_object(), Flags :: #route_flags{}, DB :: binary()) -> no_return()).
store_cdr({struct, CDRProp}=CDRJObj, #route_flags{routes_generated=RGs, direction=Dir, account_doc_id=DocID, rate_name=RateName}, DB) ->
    TScdr = [{<<"_id">>, wh_json:get_value(<<"Call-ID">>, CDRJObj)}
	     ,{<<"Routes-Available">>, RGs}
	     ,{<<"Route-Used">>, find_route_used(Dir, wh_json:get_value(<<"To-Uri">>, CDRJObj), RGs)}
	     ,{<<"Rate-Used">>, RateName}
	     ,{<<"Customer-Account-ID">>, DocID}
	     | CDRProp],
    couch_mgr:save_doc(DB, TScdr).

-spec find_route_used/3 :: (Direction, ToUri, Routes) -> json_object() when
      Direction :: binary(),
      ToUri :: binary(),
      Routes :: json_object() | json_objects().
find_route_used(Dir, To, {struct,_}=Route) ->
    find_route_used(Dir, To, [Route]);
find_route_used(<<"outbound">>, ToUri, Routes) ->
    [ToUser, _ToDomain] = binary:split(ToUri, <<"@">>),
    lists:foldl(fun(RouteJObj, Acc) ->
			case wh_json:get_value(<<"Route">>, RouteJObj) of
			    <<"sip:", DS/binary>> ->
				[DSUser, DSDomain] = binary:split(DS, <<"@">>),
				DS_IP = wh_util:to_binary(ts_util:find_ip(DSDomain)),
				case binary:match(<<DSUser/bitstring, "@", DS_IP/bitstring>>, ToUri) =/= nomatch orelse
				    binary:match(DS, ToUri) of
				    true -> RouteJObj; % Matched by IP
				    nomatch -> Acc; % neither matched
				    _ -> RouteJObj % matched by hostname
				end;
			    [<<"user:", _U/binary>>, DID] ->
				case wh_util:to_e164(ToUser) =:= wh_util:to_e164(DID) of
				    true -> RouteJObj;
				    false -> Acc
				end
			end
		end, ?EMPTY_JSON_OBJECT, Routes);
find_route_used(<<"inbound">>, _, _) -> ?EMPTY_JSON_OBJECT.

-spec fetch_cdr/2 :: (CallID, DB) -> {'error', 'not_found'} | {'ok', json_object()} when
      CallID :: binary(),
      DB :: binary().
fetch_cdr(CallID, DB) ->
    case couch_mgr:open_doc(DB, CallID) of
	{error, _} ->
	    {error, not_found};
	{ok, ?EMPTY_JSON_OBJECT} ->
	    {error, not_found};
	{ok, _}=Resp ->
	    Resp
    end.
