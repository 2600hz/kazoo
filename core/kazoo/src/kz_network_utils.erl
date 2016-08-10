%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Various utilities - a veritable cornicopia
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_network_utils).

-export([get_hostname/0]).
-export([is_ipv4/1
        ,is_ipv6/1
        ,is_ip/1
        ]).
-export([to_cidr/1
        ,to_cidr/2
        ,verify_cidr/2
        ,expand_cidr/1
        ]).
-export([find_nameservers/1
        ,find_nameservers/2
        ]).
-export([resolve/1
        ,resolve/2
        ]).
-export([is_rfc1918_ip/1]).
-export([iptuple_to_binary/1
        ,srvtuple_to_binary/1
        ,naptrtuple_to_binary/1
        ,mxtuple_to_binary/1
        ]).
-export([pretty_print_bytes/1]).

-export([lookup_dns/2
        ,lookup_dns/3
        ]).

-export([lookup_timeout/0]).
-export([new_options/0
        ,default_options/0
        ,set_option_alt_nameservers/2
        ,set_option_edns/2
        ,set_option_inet6/2
        ,set_option_nameservers/2
        ,set_option_recurse/2
        ,set_option_retry/2
        ,set_option_timeout/2
        ,set_option_udp_payload_size/2
        ,set_option_usevc/2
        ]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-include("include/kz_types.hrl").
-include("include/kz_log.hrl").

-define(LOOKUP_TIMEOUT, 500).
-define(LOOKUP_OPTIONS, [{'timeout', ?LOOKUP_TIMEOUT}]).

-type srvtuple() :: {integer(), integer(), integer(), string()}.
-type naptrtuple() :: {integer(), integer(), string(), string(), string(), string()}.
-type mxtuple() :: {integer(), string()}.
-type options() :: [inet_res:req_option()].
-export_type([srvtuple/0
             ,naptrtuple/0
             ,mxtuple/0
             ,options/0
             ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_timeout() -> ?LOOKUP_TIMEOUT.
lookup_timeout() -> ?LOOKUP_TIMEOUT.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_hostname() -> string().
get_hostname() ->
    {'ok', Host} = inet:gethostname(),
    {'ok', #hostent{h_name=Hostname}} = inet:gethostbyname(Host),
    Hostname.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_ipv4(text()) -> boolean().
is_ipv4(Address) when is_binary(Address) ->
    is_ipv4(kz_util:to_list(Address));
is_ipv4(Address) when is_list(Address) ->
    case inet_parse:ipv4strict_address(Address) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec is_ipv6(text()) -> boolean().
is_ipv6(Address) when is_binary(Address) ->
    is_ipv6(kz_util:to_list(Address));
is_ipv6(Address) when is_list(Address) ->
    case inet_parse:ipv6strict_address(Address) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec is_ip(text()) -> boolean().
is_ip(Address) ->
    is_ipv4(Address)
        orelse is_ipv6(Address).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec to_cidr(ne_binary()) -> ne_binary().
to_cidr(IP) -> to_cidr(IP, <<"32">>).

-spec to_cidr(ne_binary(), ne_binary()) -> ne_binary().
to_cidr(IP, Prefix) when not is_binary(IP) ->
    to_cidr(kz_util:to_binary(IP), Prefix);
to_cidr(IP, Prefix) when not is_binary(Prefix) ->
    to_cidr(IP, kz_util:to_binary(Prefix));
to_cidr(IP, Prefix) ->
    case is_ipv4(IP)
        andalso kz_util:to_integer(Prefix) =< 32
    of
        'true' ->
            lager:debug("adjusting ip from ~s to ~s/~s~n", [IP, IP, Prefix]),
            <<IP/binary, "/", Prefix/binary>>;
        'false' ->
            IP
    end.

-spec verify_cidr(text(), text()) -> boolean().
verify_cidr(IP, CIDR) when is_binary(IP) ->
    verify_cidr(kz_util:to_list(IP), CIDR);
verify_cidr(IP, CIDR) when is_binary(CIDR) ->
    verify_cidr(IP, kz_util:to_list(CIDR));
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
    expand_cidr(kz_util:to_list(CIDR));
expand_cidr(CIDR) ->
    %% EXTREMELY wastefull/naive approach, should never be used, but if you
    %% must we keep it in a class C
    case orber_acl:range(CIDR, 'inet') of
        {'error', _} -> [];
        {'ok', Start, End} ->
            [A1, B1, C1, D1] = lists:map(fun kz_util:to_integer/1, string:tokens(Start, ".")),
            [A2, B2, C2, D2] = lists:map(fun kz_util:to_integer/1, string:tokens(End, ".")),
            'true' = ((A2 + B2 + C2 + D2) - (A1 + B1 + C1 + D1)) =< 510,
            [iptuple_to_binary({A,B,C,D})
             || A <- lists:seq(A1, A2),
                B <- lists:seq(B1, B2),
                C <- lists:seq(C1, C2),
                D <- lists:seq(D1, D2)
            ]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_rfc1918_ip(text()) -> boolean().
is_rfc1918_ip(IP) ->
    verify_cidr(IP, "192.168.0.0/16")
        orelse verify_cidr(IP, "10.0.0.0/8")
        orelse verify_cidr(IP, "172.16.0.0/12").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_nameservers(ne_binary()) -> [string()].
find_nameservers(Domain) ->
    find_nameservers(Domain, default_options()).

-spec find_nameservers(ne_binary(), options()) -> [string()].
find_nameservers(Domain, Options) ->
    case inet_res:lookup(kz_util:to_list(Domain), 'in', 'ns', Options) of
        [] ->
            find_nameservers_parent(
              binary:split(Domain, <<".">>, ['global'])
                                   ,Options
             );
        Nameservers -> Nameservers
    end.

-spec find_nameservers_parent(ne_binaries(), options()) -> [string()].
find_nameservers_parent([], _) -> [];
find_nameservers_parent([_, _]=Parts, Options) ->
    Domain =
        kz_util:to_list(
          kz_util:join_binary(Parts, <<".">>)
         ),
    inet_res:lookup(kz_util:to_list(Domain), 'in', 'ns', Options);
find_nameservers_parent([_|Parts], Options) ->
    Domain =
        kz_util:to_list(
          kz_util:join_binary(Parts, <<".">>)
         ),
    case inet_res:lookup(kz_util:to_list(Domain), 'in', 'ns', Options) of
        [] -> find_nameservers_parent(Parts, Options);
        Nameservers -> Nameservers
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec resolve(ne_binary()) -> kz_ip_list().
resolve(Address) ->
    resolve(Address, default_options()).

-spec resolve(ne_binary(), options()) -> kz_ip_list().
resolve(Address, Options) ->
    case binary:split(Address, <<":">>) of
        [Addr|_Port] -> maybe_is_ip(Addr, Options);
        _ -> maybe_is_ip(Address, Options)
    end.

-spec maybe_is_ip(ne_binary(), options()) -> ne_binaries().
maybe_is_ip(Address, Options) ->
    case is_ip(Address) of
        'true' -> [Address];
        'false' -> maybe_resolve_srv_records(Address, Options)
    end.

-spec maybe_resolve_srv_records(ne_binary(), options()) -> ne_binaries().
maybe_resolve_srv_records(Address, Options) ->
    Domain = <<"_sip._udp.", Address/binary>>,
    case inet_res:lookup(kz_util:to_list(Domain), 'in', 'srv', Options) of
        [] -> maybe_resolve_a_records([Address], Options);
        SRVs -> maybe_resolve_a_records([D || {_, _, _, D} <- SRVs], Options)
    end.

-spec maybe_resolve_a_records(ne_binaries(), options()) -> ne_binaries().
maybe_resolve_a_records(Domains, Options) ->
    lists:foldr(fun(Domain, IPs) ->
                        maybe_resolve_fold(Domain, IPs, Options)
                end, [], Domains).

-spec maybe_resolve_fold(ne_binary(), ne_binaries(), options()) -> ne_binaries().
maybe_resolve_fold(Domain, IPs, Options) ->
    case is_ip(Domain) of
        'true' -> [Domain];
        'false' -> resolve_a_record(kz_util:to_list(Domain), IPs, Options)
    end.

-spec resolve_a_record(string(), ne_binaries(), options()) -> ne_binaries().
resolve_a_record(Domain, IPs, Options) ->
    case inet_res:lookup(Domain, 'in', 'a', Options) of
        [] ->
            lager:info("unable to resolve ~s", [Domain]),
            IPs;
        Addresses ->
            lists:foldr(fun resolve_a_record_fold/2, IPs, Addresses)
    end.

-spec resolve_a_record_fold(inet:ip4_address(), ne_binaries()) -> ne_binaries().
resolve_a_record_fold(IPTuple, I) ->
    [iptuple_to_binary(IPTuple) | I].

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec iptuple_to_binary(inet:ip4_address() | inet:ipv6_address()) -> ne_binary().
iptuple_to_binary({A,B,C,D}) ->
    <<(kz_util:to_binary(A))/binary, "."
      ,(kz_util:to_binary(B))/binary, "."
      ,(kz_util:to_binary(C))/binary, "."
      ,(kz_util:to_binary(D))/binary
    >>;
iptuple_to_binary({I1, I2, I3, I4, I5, I6, I7, I8}) ->
    <<(kz_util:to_binary(I1))/binary, ":"
      ,(kz_util:to_binary(I2))/binary, ":"
      ,(kz_util:to_binary(I3))/binary, ":"
      ,(kz_util:to_binary(I4))/binary, ":"
      ,(kz_util:to_binary(I5))/binary, ":"
      ,(kz_util:to_binary(I6))/binary, ":"
      ,(kz_util:to_binary(I7))/binary, ":"
      ,(kz_util:to_binary(I8))/binary, ":"
    >>.

-spec srvtuple_to_binary(srvtuple()) -> ne_binary().
srvtuple_to_binary({Priority, Weight, Port, Domain}) ->
    <<(kz_util:to_binary(Priority))/binary, " "
      ,(kz_util:to_binary(Weight))/binary, " "
      ,(kz_util:to_binary(Port))/binary, " "
      ,(kz_util:strip_right_binary(kz_util:to_binary(Domain), $.))/binary
    >>.

-spec naptrtuple_to_binary(naptrtuple()) -> ne_binary().
naptrtuple_to_binary({Order, Preference, Flags, Services, Regexp, Domain}) ->
    <<(kz_util:to_binary(Order))/binary, " "
      ,(kz_util:to_binary(Preference))/binary, " "
      ,"\"", (kz_util:to_upper_binary(Flags))/binary, "\" "
      ,"\"", (kz_util:to_upper_binary(Services))/binary, "\" "
      ,"\"", (kz_util:to_binary(Regexp))/binary, "\" "
      ,(kz_util:strip_right_binary(kz_util:to_binary(Domain), $.))/binary
    >>.

-spec mxtuple_to_binary(mxtuple()) -> ne_binary().
mxtuple_to_binary({Priority, Domain}) ->
    <<(kz_util:to_binary(Priority))/binary, " "
      ,(kz_util:strip_right_binary(kz_util:to_binary(Domain), $.))/binary
    >>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec pretty_print_bytes(non_neg_integer()) -> iolist().
pretty_print_bytes(Bytes)
  when Bytes div 1073741824 > 0 ->
    io_lib:format("~.2fGB", [Bytes/1073741824]);
pretty_print_bytes(Bytes)
  when Bytes div 1048576 > 0 ->
    io_lib:format("~.2fMB", [Bytes/1048576]);
pretty_print_bytes(Bytes)
  when Bytes div 1024 > 0 ->
    io_lib:format("~.2fKB", [Bytes/1024]);
pretty_print_bytes(Bytes) ->
    io_lib:format("~BB", [Bytes]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup_dns(ne_binary(), atom()) ->
                        {'ok', [inet_res:dns_data()]}.
%% See kernel/src/inet_dns.hrl, the S_* macros for values for Type
lookup_dns(Hostname, Type) ->
    lookup_dns(Hostname, Type, default_options()).

-spec lookup_dns(ne_binary(), atom(), options()) ->
                        {'ok', [inet_res:dns_data()]}.
lookup_dns(Hostname, Type, Options) ->
    {'ok', inet_res:lookup(kz_util:to_list(Hostname), 'in', Type, Options)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new_options() -> options().
new_options() -> [].

-spec default_options() -> options().
default_options() -> ?LOOKUP_OPTIONS.

-type nameserver() :: {inet:ip_address(), Port :: 1..65535}.

-spec set_option_alt_nameservers(options(), [nameserver()]) -> options().
set_option_alt_nameservers(Options, Value) ->
    props:set_value('alt_nameservers', maybe_resolve_nameservers(Value, []), Options).

-spec set_option_edns(options(), 0 | 'false') -> options().
set_option_edns(Options, Value) ->
    props:set_value('edns', Value, Options).

-spec set_option_inet6(options(), boolean()) -> options().
set_option_inet6(Options, Value) ->
    props:set_value('inet6', Value, Options).

-spec set_option_nameservers(options(), [nameserver() | string()]) -> options().
set_option_nameservers(Options, Value) ->
    props:set_value('nameservers', maybe_resolve_nameservers(Value, []), Options).

-spec set_option_recurse(options(), boolean()) -> options().
set_option_recurse(Options, Value) ->
    props:set_value('recurse', Value, Options).

-spec set_option_retry(options(), integer()) -> options().
set_option_retry(Options, Value) ->
    props:set_value('retry', Value, Options).

-spec set_option_timeout(options(), integer()) -> options().
set_option_timeout(Options, Value) ->
    props:set_value('timeout', Value, Options).

-spec set_option_udp_payload_size(options(), integer()) -> options().
set_option_udp_payload_size(Options, Value) ->
    props:set_value('udp_payload_size', Value, Options).

-spec set_option_usevc(options(), boolean()) -> options().
set_option_usevc(Options, Value) ->
    props:set_value('usevc', Value, Options).

-spec maybe_resolve_nameservers([nameserver() | string()], [nameserver()]) ->
                                       [nameserver()].
maybe_resolve_nameservers([], Nameservers) -> Nameservers;
maybe_resolve_nameservers([{_, _}=Nameserver|Values], Nameservers) ->
    maybe_resolve_nameservers(Values, [Nameserver|Nameservers]);
maybe_resolve_nameservers([Domain|Values], Nameservers) ->
    case inet_res:lookup(Domain, 'in', 'a', default_options()) of
        [] -> maybe_resolve_nameservers(Values, Nameservers);
        Addresses ->
            maybe_resolve_nameservers(
              Values
                                     ,[{Address, 53} || Address <- Addresses] ++ Nameservers
             )
    end.
