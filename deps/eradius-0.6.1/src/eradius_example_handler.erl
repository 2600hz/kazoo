-module(eradius_example_handler).

-behaviour(eradius_server).
-export([radius_request/3]).

-include("eradius_lib.hrl").
-include("dictionary.hrl").

radius_request(R = #radius_request{cmd = request}, _NasProp, {_Args, _NasIdIp}) ->
    io:format("~nGOT AUTH REQUEST:~n~p~n", [R]),
    Response = #radius_request{cmd = accept, attrs = [{?Realm, "foo"}]},
    {reply, Response};

radius_request(R = #radius_request{cmd = accreq}, _NasProp, {_Args, _NasIdIp}) ->
    io:format("~nGOT ACCT REQUEST:~n~p~n", [R]),
    Response = #radius_request{cmd = accresp, attrs = [{?Menu, <<"foo">>}]},
    {reply, Response}.
