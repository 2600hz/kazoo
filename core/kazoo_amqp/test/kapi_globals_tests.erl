%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_globals_tests).

-include_lib("eunit/include/eunit.hrl").

routing_key_test_() ->
    Test = fun kapi_globals:routing_key/2,
    Prefix = <<"globals.">>,
    Event = kz_binary:rand_hex(4),
    Name = kz_binary:rand_hex(4),
    %%HexName = kz_term:to_hex_binary(base64:encode(term_to_binary(Name))),

    [{"Event and Name may be binary"
     ,?_assertEqual(<<Prefix/binary, Event/binary, ".", Name/binary>>, Test(Event, Name))
     }
    ,{"Event and Name may be atom"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_atom(Event, 'true'), kz_term:to_atom(Name, 'true'))
                   )
     }
    ,{"Event and Name may be string"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), kz_term:to_list(Name))
                   )
     }
    ,{"Name can be anything"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), 'undefined')
                   )
     }
    ,{"Name can be anything"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), {'hello', 'world'})
                   )
     }
    ,{"Name can be anything"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), ['hello', 'world'])
                   )
     }
    ,{"Name can be anything"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), 1234567890)
                   )
     }
    ,{"Name can be anything"
     ,?_assertMatch(<<Prefix:8/binary, Event:8/binary, ".", _/binary>>
                   ,Test(kz_term:to_list(Event), fun() -> ok end)
                   )
     }
    ].
