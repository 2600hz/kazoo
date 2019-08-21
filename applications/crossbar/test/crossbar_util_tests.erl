%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_util_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TS_DOC, <<"{\"account\":{\"credits\":{\"prepay\":\"0.00\"},\"trunks\":\"0\",\"inbound_trunks\":\"0\",\"auth_realm\":\"james.thinky64.2600hz.com\"},\"billing_account_id\":\"940755758b1ea64fed0bc9f9abda73ac\",\"DIDs_Unassigned\":{},\"servers\":[{\"DIDs\":{},\"options\":{\"enabled\":true,\"inbound_format\":\"e164\",\"international\":false,\"caller_id\":{},\"e911_info\":{},\"failover\":{},\"media_handling\":\"bypass\",\"ip\":\"5.6.7.8\"},\"permissions\":{\"users\":[]},\"monitor\":{\"monitor_enabled\":false},\"auth\":{\"auth_method\":\"Password\",\"ip\":\"5.6.7.8\",\"auth_user\":\"foobar\",\"auth_password\":\"bar\"},\"server_name\":\"testpbx\",\"server_type\":\"FreeSWITCH\"}],\"ui_metadata\":{\"ui\":\"kazoo-ui\"},\"id\":\"d8364feb61280fe9bd9873a82a284ed3\",\"pvt_request_id\":\"f0a00c9f001dc5db22e669692503e89f\",\"pvt_modified\":63598345727,\"pvt_created\":63598345578,\"pvt_account_db\":\"account%2F94%2F07%2F55758b1ea64fed0bc9f9abda73ac\",\"pvt_account_id\":\"940755758b1ea64fed0bc9f9abda73ac\",\"pvt_vsn\":\"1\",\"pvt_type\":\"sys_info\"}">>).

no_change_in_servers_test() ->
    JObj = kz_json:decode(?TS_DOC),
    Servers = kz_json:get_value(<<"servers">>, JObj, []),

    ?assertEqual('false', crossbar_util:trunkstore_servers_changed(Servers, Servers)).

change_in_servers_test_() ->
    JObj = kz_json:decode(?TS_DOC),
    Servers = kz_json:get_value(<<"servers">>, JObj, []),

    Keys = [[<<"auth">>, <<"auth_method">>]
           ,[<<"auth">>, <<"ip">>]
           ,[<<"auth">>, <<"auth_user">>]
           ,[<<"auth">>, <<"auth_password">>]
           ,[<<"options">>, <<"enabled">>]
           ],

    [{"Verifying multiple changes results in a change required"
     ,assert_changed(change_server_keys(Keys, Servers), Servers)
     }
     |
     [{iolist_to_binary(
         io_lib:format("Verifying changing ~s results in a change required", [kz_binary:join(K)])
        )
      ,assert_changed(change_server(K, Servers), Servers)
      }
      || K <- Keys
     ]
    ].

assert_changed(Changed, Servers) ->
    ?_assertEqual('true', crossbar_util:trunkstore_servers_changed(Changed, Servers)).

change_server_keys(Ks, Servers) ->
    lists:foldl(fun(K, Acc) ->
                        change_server(K, Acc)
                end
               ,Servers
               ,Ks
               ).

change_server([<<"options">>, <<"enabled">>] = Key, Servers) ->
    kz_json:set_value([1 | Key], 'false', Servers);
change_server(Key, Servers) ->
    New = kz_binary:rand_hex(5),
    kz_json:set_value([1 | Key], New, Servers).
