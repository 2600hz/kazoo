%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Kazoo HTTP client
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_web_maintenance).

-export([blacklist_client_ip/1
        ,blacklist_client_host/1
        ,show_client_blacklists/0
        ]).

-spec blacklist_client_ip(kz_term:ne_binary()) -> kz_term:ne_binaries() | 'error'.
blacklist_client_ip(CIDR) ->
    blacklist_client_ip(CIDR, kz_network_utils:is_cidr(CIDR)).

blacklist_client_ip(CIDR, 'false') ->
    io:format("Please use CIDR notation (maybe ~s/32)~n", [CIDR]),
    'error';
blacklist_client_ip(CIDR, 'true') ->
    CIDRs = kz_http_util:client_ip_blacklist(),
    kz_http_util:set_client_ip_blacklist([CIDR | CIDRs]).

-spec blacklist_client_host(kz_term:ne_binary()) -> kz_term:ne_binaries().
blacklist_client_host(Host) ->
    Hosts = kz_http_util:client_host_blacklist(),
    kz_http_util:set_client_host_blacklist([Host | Hosts]).

-spec show_client_blacklists() -> 'ok'.
show_client_blacklists() ->
    io:format("CLient Blacklists:~nCIDRS: ~p~nHosts: ~p~n"
             ,[kz_http_util:client_ip_blacklist()
              ,kz_http_util:client_host_blacklist()
              ]).
