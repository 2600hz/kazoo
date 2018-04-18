%% -*- coding: utf-8 -*-
-module(ci_parsers_tests).

%% ci_parsers_test: tests for module ci_chunk.

-include_lib("eunit/include/eunit.hrl").
-include("call_inspector.hrl").

-export([start_stop_parsers/0]).

%% API tests.

start_stop_parsers() ->
    {setup
    ,fun () -> {ok, _} = application:ensure_all_started(?APP), ?APP end
    ,fun application:stop/1
    ,fun (_ReturnOfSetup) ->
             [?_assertEqual(no_return, call_inspector_maintenance:start_hep_parser(<<"10.26.0.182">>, <<"9060">>))
             ,?_assertEqual(no_return, call_inspector_maintenance:start_kamailio_parser(<<"kamailio.log">>, <<"8.9.10.11">>, <<"1337">>))
             ,?_assertEqual(no_return, call_inspector_maintenance:start_freeswitch_parser(<<"freeswitch.log">>, <<"9.10.11.12">>, <<"11000">>))
             ]
     end
    }.

%% End of Module.
