-module(ecallmgr_test).

-export([test_reg/1, gc_all/0, gc_all/1]).

-include("ecallmgr.hrl").

-define(CHUNK_SENDS, 10).

test_reg(SendCnt) ->
    Prop = [ {<<"Method">>,<<"REGISTER">>},{<<"Auth-Realm">>,<<"james.thinky64.2600hz.com">>},{<<"Auth-User">>,<<"james_2">>},{<<"Orig-IP">>,<<"192.168.5.64">>},{<<"From">>,<<"james_2@james.thinky64.2600hz.com">>},{<<"To">>,<<"james_2@james.thinky64.2600hz.com">>},{<<"Msg-ID">>,<<"66ba5e9e-ede4-11e0-9636-036984be4498">>},{<<"App-Version">>,<<"0.8.0">>},{<<"App-Name">>,<<"ecallmgr">>},{<<"Event-Name">>,<<"authn_req">>},{<<"Event-Category">>,<<"directory">>}],

    Start = erlang:now(),

    send(Prop, SendCnt),

    timer:now_diff(erlang:now(), Start).

send(Prop, SendCnt) when SendCnt > 100 ->
    SendCnt1 = SendCnt div ?CHUNK_SENDS,

    ?LOG("I'm spawing 10 senders"),

    F = fun() -> [ send(Prop, SendCnt1) || _ <- lists:seq(1,?CHUNK_SENDS) ] end,
    Pids = [ spawn_monitor(F) || _ <- lists:seq(1,?CHUNK_SENDS)],
    wait_for_pids(Pids);
send(Prop, SendCnt) ->
    ?LOG("I'm sending ~b authn_req", [SendCnt]),
    [ ecallmgr_amqp_pool:authn_req(Prop) || _ <- lists:seq(1,SendCnt) ].

wait_for_pids([{P,Ref}|Ps]) ->
    receive
	{'DOWN', Ref, process, P, _Reason} ->
	    wait_for_pids(Ps)
    end;
wait_for_pids([]) ->
    ?LOG("End of send/2").


gc_all() ->
    gc_all(memory).

gc_all(Type) ->
    HSDelta = [ gc(P, Type) || P <- erlang:processes()],
    Sorted = lists:reverse(lists:keysort(2, HSDelta)),
    Top10 = lists:sublist(Sorted, 10),
    ?LOG_SYS("Top10 deltas:"),
    [ ?LOG_SYS("~p: ~b", [Pid, HS]) || {Pid, HS} <- Top10].

gc(P, Type) ->
    {Type, HS} = erlang:process_info(P, Type),
    erlang:garbage_collect(P),
    {Type, HS1} = erlang:process_info(P, Type),
    {P, HS1 - HS}.
