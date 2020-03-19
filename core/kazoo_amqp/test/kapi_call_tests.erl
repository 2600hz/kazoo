%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_call_tests).

-include_lib("eunit/include/eunit.hrl").

call_routing_key_test_() ->
    Test = fun kapi_call:call_routing_key/2,
    Prefix = <<"call.">>,
    Event = kz_binary:rand_hex(4),
    CallId = kz_binary:rand_hex(4),

    [{"Event and CallId must be binary"
     ,[?_assertEqual(<<Prefix/binary, Event/binary, ".", CallId/binary>>, Test(Event, CallId))
      ,?_assertException('error', 'function_clause', Test(Event, binary_to_list(CallId)))
      ]
     }
    ,{"Event may be string"
     ,?_assertEqual(<<Prefix/binary, Event/binary, ".", CallId/binary>>
                   ,Test(binary_to_list(Event), CallId)
                   )
     }
    ,{"Event may be atom"
     ,?_assertEqual(<<Prefix/binary, Event/binary, ".", CallId/binary>>
                   ,Test(kz_term:to_atom(Event, 'true'), CallId)
                   )
     }
    ].

update_name_and_values_test_() ->
    Test = fun kapi_call:update_name_and_values/2,
    EventName = kz_binary:rand_hex(4),
    PList = [{<<"Event-Name">>, EventName}],
    JSON = kz_json:from_list(PList),
    Definition = kapi_call:api_definition(<<"event">>),

    [{"Request object must be proplist or kz_json:object/0"
     ,[?_assertEqual(EventName, kapi_definition:name(Test(Definition, PList)))
      ,?_assertEqual(EventName, kz_api:event_name(kapi_definition:values(Test(Definition, PList))))
      ,?_assertEqual(EventName, kapi_definition:name(Test(Definition, JSON)))
      ,?_assertEqual(EventName, kz_api:event_name(kapi_definition:values(Test(Definition, JSON))))
      ,?_assertException('error', 'function_clause', Test(Definition, EventName))
      ,?_assertException('error', 'function_clause', Test(Definition, binary_to_list(EventName)))
      ,?_assertException('error', 'function_clause', Test(Definition, kz_term:to_atom(EventName, 'true')))
      ]
     }
    ].
