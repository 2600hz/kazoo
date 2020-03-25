%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_presence_tests).

-include_lib("eunit/include/eunit.hrl").

search_req_routing_key_test_() ->
    Test = fun kapi_presence:search_req_routing_key/1,
    Prefix = <<"presence.search_req.">>,
    Realm = kz_binary:rand_hex(4),

    [{"Realm must be binary"
     ,[?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(Realm))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm)))
      ]
     }
    ].

subscribe_routing_key_test_() ->
    Test = fun kapi_presence:subscribe_routing_key/1,
    Prefix = <<"subscriptions.">>,
    User = kz_binary:rand_hex(4),

    [{"User must be binary"
     ,[?_assertEqual(<<Prefix/binary, User/binary>>, Test(User))
      ,?_assertEqual(<<Prefix/binary, User/binary>>
                    ,Test(<<(kz_binary:rand_hex(4))/binary, "@", User/binary>>)
                    )
      ,?_assertException('error', 'badarg', Test(binary_to_list(User)))
      ]
     }
    ].

dialog_routing_key_test_() ->
    Test = fun kapi_presence:dialog_routing_key/2,
    Prefix = <<"dialog.">>,
    CallId = kz_binary:rand_hex(4),
    PresenceId = kz_binary:rand_hex(4),

    [{"CallId and PresenceId must be binary"
     ,[?_assertEqual(<<Prefix/binary, PresenceId/binary, ".", CallId/binary>>
                    ,Test(CallId, PresenceId)
                    )
      ,?_assertEqual(<<Prefix/binary, PresenceId/binary, ".", CallId/binary>>
                    ,Test(CallId, <<(kz_binary:rand_hex(4))/binary, "@", PresenceId/binary>>)
                    )
      ,?_assertException('error', 'function_clause', Test(binary_to_list(CallId), PresenceId))
      ,?_assertException('error', 'badarg', Test(CallId, binary_to_list(PresenceId)))
      ]
     }
    ].

update_routing_key_test_() ->
    Test = fun kapi_presence:update_routing_key/2,
    Prefix = <<"update.">>,
    CallId = kz_binary:rand_hex(4),
    PresenceId = kz_binary:rand_hex(4),

    [{"CallId and PresenceId must be binary"
     ,[?_assertEqual(<<Prefix/binary, PresenceId/binary, ".", CallId/binary>>
                    ,Test(CallId, PresenceId)
                    )
      ,?_assertEqual(<<Prefix/binary, PresenceId/binary, ".", CallId/binary>>
                    ,Test(CallId, <<(kz_binary:rand_hex(4))/binary, "@", PresenceId/binary>>)
                    )
      ,?_assertException('error', 'function_clause', Test(binary_to_list(CallId), PresenceId))
      ,?_assertException('error', 'badarg', Test(CallId, binary_to_list(PresenceId)))
      ]
     }
    ].

probe_routing_key_test_() ->
    Test = fun kapi_presence:probe_routing_key/1,
    Prefix = <<"probes.">>,
    SubscriptionType = kz_binary:rand_hex(4),

    [{"SubscriptionType must be binary"
     ,[?_assertEqual(<<Prefix/binary, SubscriptionType/binary>>, Test(SubscriptionType))
      ,?_assertException('error', 'badarg', Test(binary_to_list(SubscriptionType)))
      ]
     }
    ].

mwi_update_routing_key_test_() ->
    Test = fun kapi_presence:mwi_update_routing_key/2,
    Prefix = <<"mwi_updates.">>,
    User = kz_binary:rand_hex(4),
    Realm = kz_binary:rand_hex(4),

    [{"User and Realm must be binary"
     ,[?_assertEqual(<<Prefix/binary, Realm/binary, ".", User/binary>>, Test(User, Realm))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(User), Realm))
      ,?_assertException('error', 'function_clause', Test(User, binary_to_list(Realm)))
      ]
     }
    ].

mwi_unsolicited_update_routing_key_test_() ->
    Test = fun kapi_presence:mwi_unsolicited_update_routing_key/1,
    Prefix = <<"mwi_unsolicited_updates.">>,
    To = kz_binary:rand_hex(4),

    [{"To must be binary"
     ,[?_assertEqual(<<Prefix/binary, To/binary>>, Test(To))
      ,?_assertEqual(<<Prefix/binary, To/binary>>
                    ,Test(<<(kz_binary:rand_hex(4))/binary, "@", To/binary>>)
                    )
      ,?_assertException('error', 'badarg', Test(binary_to_list(To)))
      ]
     }
    ].

mwi_query_routing_key_test_() ->
    Test = fun kapi_presence:mwi_query_routing_key/1,
    Prefix = <<"mwi_queries.">>,
    Realm = kz_binary:rand_hex(4),

    [{"Realm must be binary"
     ,[?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(Realm))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm)))
      ]
     }
    ].

register_overwrite_routing_key_test_() ->
    Test = fun kapi_presence:register_overwrite_routing_key/1,
    Prefix = <<"register_overwrites.">>,
    Realm = kz_binary:rand_hex(4),

    [{"Realm must be binary"
     ,[?_assertEqual(<<Prefix/binary, Realm/binary>>, Test(Realm))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm)))
      ]
     }
    ].

reset_routing_key_test_() ->
    Test = fun kapi_presence:reset_routing_key/2,
    Prefix = <<"presence.reset.">>,
    Realm = kz_binary:rand_hex(4),
    Username = kz_binary:rand_hex(4),

    [{"Realm and Username must be binary"
     ,[?_assertEqual(<<Prefix/binary, Realm/binary, ".", Username/binary>>, Test(Realm, Username))
      ,?_assertException('error', 'function_clause', Test(binary_to_list(Realm), Username))
      ,?_assertException('error', 'function_clause', Test(Realm, binary_to_list(Username)))
      ]
     }
    ].

get_value_test_() ->
    Test = fun kapi_presence:get_value/2,
    Key = kz_binary:rand_hex(4),
    Value = kz_binary:rand_hex(4),
    PList = [{Key, Value}],

    [{"Req may be proplist"
     ,?_assertEqual(Value, Test(Key, PList))
     }
    ,{"Req may be kz_json:object/0"
     ,?_assertEqual(Value, Test(Key, kz_json:from_list(PList)))
     }
    ].
