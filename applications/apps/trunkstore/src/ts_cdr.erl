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

-export([start_link/0, store_cdr/3, fetch_cdr/2]).

-include("ts.hrl").

start_link() ->
    {ok, proc_lib:spawn_link(fun() -> cdr_init() end)}.

cdr_init() ->
    {_, {H,Min,S}} = calendar:universal_time(),
    MillisecsToMidnight = ?MILLISECS_PER_DAY - timer:hms(H,Min,S),
    {ok, _} = timer:send_after(MillisecsToMidnight, ?EOD),

    create_cdr_db(ts_util:todays_db_name(?TS_CDR_PREFIX)),

    cdr_loop().

cdr_loop() ->
    receive
	?EOD ->
	    create_cdr_db(ts_util:todays_db_name(?TS_CDR_PREFIX)),
	    {ok, _} = timer:send_after(?MILLISECS_PER_DAY, ?EOD),
	    cdr_loop()
    end.

create_cdr_db(DB) ->
    logger:format_log(info, "TS_CDR(~p): Creating new cdr db ~p~n", [self(), DB]),
    couch_mgr:db_create(DB),
    case couch_mgr:load_doc_from_file(DB, trunkstore, <<"ts_cdr.json">>) of
	{ok, _} -> ok;
	{error, _} -> couch_mgr:update_doc_from_file(DB, trunkstore, <<"ts_cdr.json">>)
    end.

-spec(store_cdr/3 :: (CDR :: json_object(), Flags :: #route_flags{}, DB :: binary()) -> no_return()).
store_cdr({struct, CDRProp}=CDRJObj, #route_flags{routes_generated=RGs, direction=Dir, account_doc_id=DocID, rate_name=RateName}, DB) ->
    TScdr = [{<<"_id">>, wh_json:get_value(<<"Call-ID">>, CDRJObj)}
	     ,{<<"Routes-Available">>, RGs}
	     ,{<<"Route-Used">>, find_route_used(Dir, wh_json:get_value(<<"To-Uri">>, CDRJObj), RGs)}
	     ,{<<"Rate-Used">>, RateName}
	     ,{<<"Customer-Account-ID">>, DocID}
	     | CDRProp],
    couch_mgr:save_doc(DB, TScdr).

-spec(find_route_used/3 :: (Direction :: binary(), ToUri :: binary(), Routes :: json_object() | json_objects()) -> json_object()).
find_route_used(Dir, To, {struct,_}=Route) ->
    find_route_used(Dir, To, [Route]);
find_route_used(<<"outbound">>, ToUri, Routes) ->
    [ToUser, _ToDomain] = binary:split(ToUri, <<"@">>),
    lists:foldl(fun(RouteJObj, Acc) ->
			case wh_json:get_value(<<"Route">>, RouteJObj) of
			    <<"sip:", DS/binary>> ->
				[DSUser, DSDomain] = binary:split(DS, <<"@">>),
				DS_IP = whistle_util:to_binary(ts_util:find_ip(DSDomain)),
				case binary:match(<<DSUser/bitstring, "@", DS_IP/bitstring>>, ToUri) =/= nomatch orelse
				    binary:match(DS, ToUri) of
				    true -> RouteJObj; % Matched by IP
				    nomatch -> Acc; % neither matched
				    _ -> RouteJObj % matched by hostname
				end;
			    [<<"user:", _U/binary>>, DID] ->
				case whistle_util:to_e164(ToUser) =:= whistle_util:to_e164(DID) of
				    true -> RouteJObj;
				    false -> Acc
				end
			end
		end, ?EMPTY_JSON_OBJECT, Routes);
find_route_used(<<"inbound">>, _, _) -> ?EMPTY_JSON_OBJECT.

-spec(fetch_cdr/2 :: (CallID :: binary(), DB :: binary()) -> {error, not_found} | {ok, json_object()}).
fetch_cdr(CallID, DB) ->
    case couch_mgr:open_doc(DB, CallID) of
	{error, _} ->
	    {error, not_found};
	{ok, ?EMPTY_JSON_OBJECT} ->
	    {error, not_found};
	{ok, Doc} ->
	    Doc
    end.
