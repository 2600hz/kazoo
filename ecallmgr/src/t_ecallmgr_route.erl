%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Pump route requests through the route handler
%%% @end
%%% Created : 30 Mar 2011 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(t_ecallmgr_route).

-export([test/1]).

test(Msgs) ->
    process_flag(trap_exit, true),
    {A, B, C} = erlang:now(),
    _ = random:seed(A, B, C),
    Prop = [{<<"Custom-Channel-Vars">>, {struct,[{<<"Tenant-ID">>,<<"ignore">>}, {<<"Access-Group">>,<<"ignore">>}, {<<"Direction">>,<<"outbound">>}, {<<"Username">>,<<"2600pbx">>}, {<<"Realm">>,<<"2600pbx.thinky64.d-man.org">>}]}}, {<<"Caller-ID-Number">>,<<"2600pbx">>}, {<<"Caller-ID-Name">>,<<"2600pbx">>}, {<<"From">>,<<"2600pbx@2600pbx.thinky64.d-man.org">>}, {<<"To">>,<<"4158867905@2600pbx.thinky64.d-man.org">>}, {<<"Msg-ID">>,<<"7fb4977e-5af1-11e0-8f0e-ffaa63003a4e">>}, {<<"App-Version">>,<<"0.5.0">>}, {<<"App-Name">>,<<"ecallmgr.route">>}, {<<"Event-Name">>,<<"route_req">>}, {<<"Event-Category">>,<<"dialplan">>}],
    Self = self(),
    Pids = [spawn_link(fun() -> send_prop(Prop, Self) end) || _ <- lists:seq(1,Msgs)],
    wait(Msgs, Pids, 0, 0).

wait(Msgs, [], Failed, Micro) ->
    io:format("TEST(~p): Total test for ~p msgs completed in ~p micros (~p r / sec). Failed: ~p~n", [self(), Msgs, Micro, (Msgs / (Micro / 1000000)), Failed]);
wait(Msgs, Left, Failed, Micro) ->
    receive
	{done, W, T} ->
	    wait(Msgs, lists:delete(W, Left), Failed, Micro + T);
	{'EXIT', P, normal} ->
	    wait(Msgs, lists:delete(P, Left), Failed, Micro);
	{'EXIT', P, _R} ->
	    io:format("~p went down ~p~n", [P, _R]),
	    wait(Msgs, lists:delete(P, Left), Failed + 1, Micro)
    end.

send_prop(Prop, Parent) ->
    Start = erlang:now(),
    CallID = list_to_binary(wh_util:to_hex(crypto:rand_bytes(16))),
    {ok, _Resp} = ecallmgr_amqp_pool:route_req([{<<"Call-ID">>,CallID} | Prop]),
    Parent ! {done, self(), timer:now_diff(erlang:now(), Start)}.

