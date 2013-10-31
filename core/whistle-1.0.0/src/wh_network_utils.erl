%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wh_network_utils).

-export([get_hostname/0]).
-export([is_ipv4/1]).
-export([is_ipv6/1]).
-export([to_cidr/1
         ,to_cidr/2
        ]).
-export([verify_cidr/2]).
-export([expand_cidr/1]).
-export([resolve/1]).
-export([is_rfc1918_ip/1]).
-export([iptuple_to_binary/1]).

-include_lib("kernel/include/inet.hrl").

-include("../include/wh_types.hrl").
-include("../include/wh_log.hrl").

-spec get_hostname() -> string().
get_hostname() ->
    {'ok', Host} = inet:gethostname(),
    {'ok', #hostent{h_name=Hostname}} = inet:gethostbyname(Host),
    Hostname.

-spec is_ipv4(text()) -> boolean().
is_ipv4(Address) when is_binary(Address) ->
    is_ipv4(wh_util:to_list(Address));
is_ipv4(Address) when is_list(Address) ->
    case inet_parse:ipv4_address(Address) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec is_ipv6(text()) -> boolean().
is_ipv6(Address) when is_binary(Address) ->
    is_ipv6(wh_util:to_list(Address));
is_ipv6(Address) when is_list(Address) ->
    case inet_parse:ipv6_address(Address) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec to_cidr(ne_binary()) -> ne_binary().
to_cidr(IP) -> to_cidr(IP, <<"32">>).

-spec to_cidr(ne_binary(), ne_binary()) -> ne_binary().
to_cidr(IP, Prefix) when not is_binary(IP) -> 
    to_cidr(wh_util:to_binary(IP), Prefix);
to_cidr(IP, Prefix) when not is_binary(Prefix) -> 
    to_cidr(IP, wh_util:to_binary(Prefix));
to_cidr(IP, Prefix) ->    
    case wh_network_utils:is_ipv4(IP) 
        andalso  wh_util:to_integer(Prefix) =< 32
    of
        'true' ->
            lager:debug("adjusting ip from ~s to ~s/~s~n", [IP, IP, Prefix]),
            <<IP/binary, "/", Prefix/binary>>;
        'false' ->
            IP
    end.

-spec verify_cidr(text(), text()) -> boolean().
verify_cidr(IP, CIDR) when is_binary(IP) ->
    verify_cidr(wh_util:to_list(IP), CIDR);
verify_cidr(IP, CIDR) when is_binary(CIDR) ->
    verify_cidr(IP, wh_util:to_list(CIDR));
verify_cidr(IP, CIDR) ->
    %% As per the docs... "This operation should only be used for test purposes"
    %% so, ummm ya, but probably cheaper then my expand bellow followed by a list
    %% test.  Just be aware this should only be used where performance is not
    %% critical
    case orber_acl:verify(IP, CIDR, 'inet') of
        'true' -> 'true';
        {'false', _, _} -> 'false';
        {'error', _} -> 'false'
    end.

-spec expand_cidr(text()) -> ne_binaries().
expand_cidr(CIDR) when is_binary(CIDR) ->
    expand_cidr(wh_util:to_list(CIDR));
expand_cidr(CIDR) ->
    %% EXTREMELY wastefull/naive approach, should never be used, but if you
    %% must we keep it in a class C
    case orber_acl:range(CIDR, 'inet') of
        {'error', _} -> [];
        {'ok', Start, End} ->
            [A1, B1, C1, D1] = lists:map(fun wh_util:to_integer/1, string:tokens(Start, ".")),
            [A2, B2, C2, D2] = lists:map(fun wh_util:to_integer/1, string:tokens(End, ".")),
            'true' = ((A2 + B2 + C2 + D2) - (A1 + B1 + C1 + D1)) =< 510,
            [iptuple_to_binary({A,B,C,D})
             || A <- lists:seq(A1, A2),
                B <- lists:seq(B1, B2),
                C <- lists:seq(C1, C2),
                D <- lists:seq(D1, D2)
            ]
    end.

-spec is_rfc1918_ip(text()) -> boolean().
is_rfc1918_ip(IP) ->
    verify_cidr(IP, "192.168.0.0/16")
        orelse verify_cidr(IP, "10.0.0.0/8")
        orelse verify_cidr(IP, "172.16.0.0/12").

-spec resolve(ne_binary()) -> wh_ip_list().
resolve(Address) ->
    case binary:split(Address, <<":">>) of
        [Addr|_Port] -> maybe_is_ip(Addr);
        _ -> maybe_is_ip(Address)
    end.

maybe_is_ip(Address) ->
    case is_ipv4(Address) of
        'true' -> [Address];
        'false' -> maybe_resolve_srv_records(Address)
    end.

-spec maybe_resolve_srv_records(ne_binary()) -> wh_ip_list().
maybe_resolve_srv_records(Address) ->
    Domain = <<"_sip._udp.", Address/binary>>,
    case inet_res:lookup(wh_util:to_list(Domain), 'in', 'srv') of
        [] -> maybe_resolve_a_records([Address]);
        SRVs -> maybe_resolve_a_records([D || {_, _, _, D} <- SRVs])
    end.

-spec maybe_resolve_a_records(ne_binary()) -> wh_ip_list().
maybe_resolve_a_records(Domains) ->
    lists:foldr(fun maybe_resolve_fold/2, [], Domains).

maybe_resolve_fold(Domain, IPs) ->
    case is_ipv4(Domain) of
        'true' -> [Domain];
        'false' -> resolve_a_record(wh_util:to_list(Domain), IPs)
    end.

-spec resolve_a_record(ne_binary(), wh_ip_list()) -> wh_ip_list().
resolve_a_record(Domain, IPs) ->
    case inet:getaddrs(Domain, 'inet') of
        {'error', _R} ->
            lager:info("unable to resolve ~s: ~p", [Domain, _R]),
            IPs;
        {'ok', Addresses} ->
            lists:foldr(fun(IPTuple, I) ->
                                [iptuple_to_binary(IPTuple)|I]
                        end, IPs, Addresses)
    end.

-spec iptuple_to_binary(inet:ip4_address()) -> ne_binary().
iptuple_to_binary({A,B,C,D}) ->
    <<(wh_util:to_binary(A))/binary, "."
      ,(wh_util:to_binary(B))/binary, "."
      ,(wh_util:to_binary(C))/binary, "."
      ,(wh_util:to_binary(D))/binary>>.
