%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Various utilities - a veritable cornucopia.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_network_utils).

-export([get_hostname/0]).
-export([default_binding_ip/0
        ,get_supported_binding_ip/0, get_supported_binding_ip/1, get_supported_binding_ip/2
        ,detect_ip_family/1
        ]).
-export([is_ipv4/1
        ,is_ipv6/1
        ,is_ip/1
        ,is_protocol_family_supported/1
        ,is_cidr/1
        ]).
-export([to_cidr/1
        ,to_cidr/2
        ,verify_cidr/2
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

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(LOOKUP_TIMEOUT, 500).
-define(LOOKUP_OPTIONS, [{'timeout', ?LOOKUP_TIMEOUT}]).

-define(BIND_ALL_INTERFACE_4, <<"0.0.0.0">>).
-define(BIND_ALL_INTERFACE_6, <<"::">>).

-type srvtuple() :: {integer(), integer(), integer(), string()}.
-type naptrtuple() :: {integer(), integer(), string(), string(), string(), string()}.
-type mxtuple() :: {integer(), string()}.
-type options() :: [inet_res:req_option()].

-type cidr_block() :: {inet:ip_address(), inet:ip_address(), pos_integer()}.

-export_type([srvtuple/0
             ,naptrtuple/0
             ,mxtuple/0
             ,options/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_timeout() -> ?LOOKUP_TIMEOUT.
lookup_timeout() -> ?LOOKUP_TIMEOUT.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_hostname() -> string().
get_hostname() ->
    {'ok', Host} = inet:gethostname(),
    {'ok', #hostent{h_name=Hostname}} = inet:gethostbyname(Host),
    Hostname.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_ipv4(kz_term:text()) -> boolean().
is_ipv4(Address) ->
    {Family, _} = detect_ip_family(Address),
    Family =:= 'inet'.

-spec is_ipv6(kz_term:text()) -> boolean().
is_ipv6(Address) ->
    {Family, _} = detect_ip_family(Address),
    Family =:= 'inet6'.

-spec is_ip(kz_term:text()) -> boolean().
is_ip(Address) ->
    {Family, _} = detect_ip_family(Address),
    Family =:= 'inet'
        orelse Family =:= 'inet6'.

-spec is_cidr(kz_term:text()) -> boolean().
is_cidr(Address) ->
    try inet_cidr:parse(Address) of
        {_Start, _End, _Len} -> 'true'
    catch
        'error':{'badmatch', _} -> 'false';
        'error':'invalid_cidr' -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Returns {@link inet:ip_address()} of {@link default_binding_ip/0}.
%% @see get_supported_binding_ip/2. See `get_supported_binding_ip/2' about
%% binding to all interfaces.
%% @throws {error, Reason::kz_term:ne_binary()}
%% @end
%%------------------------------------------------------------------------------
-spec get_supported_binding_ip() -> inet:ip_address().
get_supported_binding_ip() ->
    DefaultIP = default_binding_ip(),
    {'ok', DefaultIPAddress} = inet:parse_address(kz_term:to_list(DefaultIP)),
    DefaultIPAddress.

%% @equiv get_supported_binding_ip(IP, default_binding_ip())
-spec get_supported_binding_ip(kz_term:text() | 'undefined') -> inet:ip_address().
get_supported_binding_ip('undefined') ->
    get_supported_binding_ip();
get_supported_binding_ip(IP) ->
    DefaultIP = default_binding_ip(),
    get_supported_binding_ip(IP, DefaultIP).

%%------------------------------------------------------------------------------
%% @doc Returns {@link inet:ip_address()} of binding IP address if the `IP'
%% can be bound by underlying system, otherwise returns the default binding
%% address.
%%
%% This is useful to check for example the IPv6 or IPv4 protocol is supported
%% by the underlying system.
%%
%% The side effect is that the `IP' should be configured on an interface before
%% otherwise this test will fail.
%%
%% Also if you're trying to bind to all interfaces this actually returns success,
%% but it means the system is *capable* of binding to any interface (even if
%% there is no interface right now). In other words, if you're trying to bind
%% to all interfaces and want to make sure there is at least configured
%% interface don't rely on this function. This is not going to check if the
%% network is working or not.
%%
%% @throws {error, Reason::kz_term:ne_binary()}
%% @end
%%------------------------------------------------------------------------------
-spec get_supported_binding_ip(kz_term:text() | 'undefined', kz_term:text() | 'undefined') ->
                                      inet:ip_address().
get_supported_binding_ip('undefined', 'undefined') ->
    get_supported_binding_ip();
get_supported_binding_ip('undefined', DefaultIP) ->
    get_supported_binding_ip(DefaultIP, DefaultIP);
get_supported_binding_ip(IP, 'undefined') ->
    get_supported_binding_ip(IP, default_binding_ip());
get_supported_binding_ip(IP, DefaultIP) ->
    case detect_ip_is_bindable(IP) of
        {'ok', _, IPAdress} ->
            IPAdress;
        {'error', _, _ReasonIP} ->
            case detect_ip_is_bindable(DefaultIP) of
                {'ok', _, DefaultIPAddress} ->
                    ?SUP_LOG_WARNING("can not bind to address '~s' (~s), using default binding ip '~s'"
                                    ,[IP, _ReasonIP, DefaultIP]
                                    ),
                    DefaultIPAddress;
                {'error', _, _ReasonDefault} ->
                    Msg = kz_term:to_binary(io_lib:format("can not bind to ip '~s' (~s) and default binding ip '~s' (~s)"
                                                         ,[IP, _ReasonIP, DefaultIP, _ReasonDefault]
                                                         )
                                           ),
                    ?SUP_LOG_WARNING("~s", [Msg]),
                    throw({'error', Msg})
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Detects IP family of `IP' and returns its {@link inet:ip_address()}.
%% @end
%%------------------------------------------------------------------------------
-spec detect_ip_family(kz_term:ne_binary() | string()) -> {inet:address_family(), inet:ip_address()} | {'error', 'einval'}.
detect_ip_family(IP) when is_binary(IP) ->
    detect_ip_family(kz_term:to_list(IP));
detect_ip_family(IP) ->
    try
        case inet:parse_ipv6strict_address(IP) of
            {'ok', IPv6} -> {'inet6', IPv6};
            {'error', 'einval'} ->
                case inet:parse_ipv4_address(IP) of
                    {'ok', IPv4} -> {'inet', IPv4};
                    {'error', 'einval'}=Error -> Error
                end
        end
    catch
        'error':'function_clause' ->
            {'error', 'einval'}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns default IP address for binding on all interfaces based
%% on supported protocol family in the underlying system.
%%
%% If both IPv6 and IPv4 is available returns IPv4 by default.
%% @see get_supported_binding_ip/2. See `get_supported_binding_ip/2' about
%% binding to all interfaces.
%% @throws {error, Reason::kz_term:ne_binary()}
%% @end
%%------------------------------------------------------------------------------
-spec default_binding_ip() -> kz_term:ne_binary().
default_binding_ip() ->
    default_binding_ip(is_protocol_family_supported('inet')
                      ,is_protocol_family_supported('inet6')
                      ).

-spec default_binding_ip(boolean(), boolean()) -> kz_term:ne_binary().
default_binding_ip('true', _) -> ?BIND_ALL_INTERFACE_4;
default_binding_ip('false', 'true') -> ?BIND_ALL_INTERFACE_6;
default_binding_ip('false', 'false') ->
    throw({'error', <<"no network available for protocol family IPv6 or IPv4">>}).

%%------------------------------------------------------------------------------
%% @doc Detects if specified network protocol family is supported by system.
%% @see get_supported_binding_ip/2
%% @end
%%------------------------------------------------------------------------------
-spec is_protocol_family_supported('inet' | 'inet6') -> boolean().
is_protocol_family_supported(Family) ->
    case detect_ip_is_bindable(Family) of
        {'ok', _Family, _IPAddress} -> 'true';
        {'error', _, _} -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc Detects if Erlang can bind to the specified `IP'.
%% @see get_supported_binding_ip/2
%% @end
%%------------------------------------------------------------------------------
-spec detect_ip_is_bindable('inet' | 'inet6' | kz_term:text() | {'inet' | 'inet6', inet:ip_address()} | {'error', 'einval'}) ->
                                   {'ok', 'inet' | 'inet6', inet:ip_address()} |
                                   {'error', 'inet' | 'inet6'| 'einval', string()}.
detect_ip_is_bindable(IP) when is_binary(IP) ->
    detect_ip_is_bindable(detect_ip_family(IP));
detect_ip_is_bindable(IP) when is_list(IP) ->
    detect_ip_is_bindable(detect_ip_family(IP));
detect_ip_is_bindable('inet') ->
    detect_ip_is_bindable(?BIND_ALL_INTERFACE_4);
detect_ip_is_bindable('inet6') ->
    detect_ip_is_bindable(?BIND_ALL_INTERFACE_6);
detect_ip_is_bindable({'error', 'einval'}) ->
    {'error', 'einval', "einval: invalid ip address"};
detect_ip_is_bindable({Family, IPAdress}) ->
    Format = fun(R) -> io_lib:format("~p: ~s", [R, inet:format_error(R)]) end,
    try
        case gen_tcp:listen(6416, [Family, {'ifaddr', IPAdress}]) of
            {'ok', Port} ->
                gen_tcp:close(Port),
                {'ok', Family, IPAdress};
            {'error', 'eafnosupport'=Reason} ->
                {'error', Family, Format(Reason)};
            {'error', 'eaddrnotavail'=Reason} ->
                {'error', Family, Format(Reason)};
            {'error', 'einval'=Reason} ->
                %% can't bind to ip because its link-local or something reserved
                {'error', Family, Format(Reason)};
            {'error', 'eaddrinuse'} ->
                %% try again maybe? does this at least confirm that the ifaddr is exists?
                {'ok', Family, IPAdress};
            {'error', Reason} ->
                {'error', Family, Format(Reason)}
        end
    catch
        _E:_T ->
            {'error', io_lib:format("exception occurred when detecting ip family supportability/bindability: ~p:~p", [_E, _T])}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec to_cidr(kz_term:ne_binary()) -> kz_term:ne_binary().
to_cidr(IP) -> to_cidr(IP, <<"32">>).

-spec to_cidr(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
to_cidr(IP, Prefix) when not is_binary(IP) ->
    to_cidr(kz_term:to_binary(IP), Prefix);
to_cidr(IP, Prefix) when not is_binary(Prefix) ->
    to_cidr(IP, kz_term:to_binary(Prefix));
to_cidr(IP, Prefix) ->
    case is_ipv4(IP)
        andalso kz_term:to_integer(Prefix) =< 32
    of
        'true' ->
            lager:debug("adjusting ip from ~s to ~s/~s~n", [IP, IP, Prefix]),
            <<IP/binary, "/", Prefix/binary>>;
        'false' ->
            IP
    end.

-spec verify_cidr(kz_term:text(), kz_term:text()) -> boolean().
verify_cidr(IP, CIDR) when is_binary(IP) ->
    verify_cidr(kz_term:to_list(IP), CIDR);
verify_cidr(IP, CIDR) when is_binary(CIDR) ->
    verify_cidr(IP, kz_term:to_list(CIDR));
verify_cidr(IP, CIDR) ->
    try inet_cidr:parse(CIDR) of
        Block -> verify_block(IP, Block)
    catch
        'error':'invalid_cidr' -> 'false'
    end.

-spec verify_block(string(), cidr_block()) -> boolean().
verify_block(IP, Block) ->
    case inet:parse_address(IP) of
        {'ok', IPTuple} -> inet_cidr:contains(Block, IPTuple);
        {'error', _} -> 'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_rfc1918_ip(kz_term:text()) -> boolean().
is_rfc1918_ip(IP) ->
    verify_cidr(IP, "192.168.0.0/16")
        orelse verify_cidr(IP, "10.0.0.0/8")
        orelse verify_cidr(IP, "172.16.0.0/12").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_nameservers(kz_term:ne_binary()) -> [string()].
find_nameservers(Domain) ->
    find_nameservers(Domain, default_options()).

-spec find_nameservers(kz_term:ne_binary(), options()) -> [string()].
find_nameservers(Domain, Options) ->
    case inet_res:lookup(kz_term:to_list(Domain), 'in', 'ns', Options) of
        [] ->
            find_nameservers_parent(binary:split(Domain, <<".">>, ['global'])
                                   ,Options
                                   );
        Nameservers -> Nameservers
    end.

-spec find_nameservers_parent(kz_term:ne_binaries(), options()) -> [string()].
find_nameservers_parent([], _) -> [];
find_nameservers_parent([_, _]=Parts, Options) ->
    Domain = kz_term:to_list(kz_binary:join(Parts, <<".">>)),
    inet_res:lookup(kz_term:to_list(Domain), 'in', 'ns', Options);
find_nameservers_parent([_|Parts], Options) ->
    Domain = kz_term:to_list(kz_binary:join(Parts, <<".">>)),
    case inet_res:lookup(kz_term:to_list(Domain), 'in', 'ns', Options) of
        [] -> find_nameservers_parent(Parts, Options);
        Nameservers -> Nameservers
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resolve(kz_term:ne_binary()) -> kz_types:ip_list().
resolve(Address) ->
    resolve(Address, default_options()).

-spec resolve(kz_term:ne_binary(), options()) -> kz_types:ip_list().
resolve(Address, Options) ->
    case is_cidr(Address) of
        'true' -> [Address];
        'false' -> resolve_ip_or_hostname(Address, Options)
    end.

-spec resolve_ip_or_hostname(kz_term:ne_binary(), options()) -> kz_types:ip_list().
resolve_ip_or_hostname(Address, Options) ->
    Addr = maybe_strip_port(Address),
    case is_ip(Addr) of
        'true' -> [Addr];
        'false' -> maybe_resolve_srv_records(Addr, Options)
    end.

-spec maybe_strip_port(kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_strip_port(Address) ->
    case binary:split(Address, <<":">>) of
        [Address] -> Address;
        [Addr, _Port] -> Addr
    end.

-spec maybe_resolve_srv_records(kz_term:ne_binary(), options()) -> kz_term:ne_binaries().
maybe_resolve_srv_records(Address, Options) ->
    Domain = <<"_sip._udp.", Address/binary>>,
    case inet_res:lookup(kz_term:to_list(Domain), 'in', 'srv', Options) of
        [] -> maybe_resolve_a_records([Address], Options);
        SRVs -> maybe_resolve_a_records([D || {_, _, _, D} <- SRVs], Options)
    end.

-spec maybe_resolve_a_records(kz_term:ne_binaries(), options()) -> kz_term:ne_binaries().
maybe_resolve_a_records(Domains, Options) ->
    lists:foldr(fun(Domain, IPs) ->
                        maybe_resolve_fold(Domain, IPs, Options)
                end
               ,[]
               ,Domains
               ).

-spec maybe_resolve_fold(kz_term:ne_binary(), kz_term:ne_binaries(), options()) -> kz_term:ne_binaries().
maybe_resolve_fold(Domain, IPs, Options) ->
    case is_ip(Domain) of
        'true' -> [Domain];
        'false' -> resolve_a_record(kz_term:to_list(Domain), IPs, Options)
    end.

-spec resolve_a_record(string(), kz_term:ne_binaries(), options()) -> kz_term:ne_binaries().
resolve_a_record(Domain, IPs, Options) ->
    case inet_res:lookup(Domain, 'in', 'a', Options) of
        [] ->
            lager:info("unable to resolve ~s", [Domain]),
            IPs;
        Addresses ->
            lists:foldr(fun resolve_a_record_fold/2, IPs, Addresses)
    end.

-spec resolve_a_record_fold(inet:ip4_address(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
resolve_a_record_fold(IPTuple, I) ->
    [iptuple_to_binary(IPTuple) | I].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec iptuple_to_binary(inet:ip4_address() | inet:ipv6_address()) -> kz_term:ne_binary().
iptuple_to_binary({A,B,C,D}) ->
    <<(kz_term:to_binary(A))/binary, "."
     ,(kz_term:to_binary(B))/binary, "."
     ,(kz_term:to_binary(C))/binary, "."
     ,(kz_term:to_binary(D))/binary
    >>;

%% IPv4 mapped to IPv6
%% https://tools.ietf.org/html/rfc4038#section-4.2
iptuple_to_binary({0, 0, 0, 0, 0, 16#FFFF, AB, CD}) ->
    <<A:8, B:8>> = <<AB:16>>,
    <<C:8, D:8>> = <<CD:16>>,
    iptuple_to_binary({A,B,C,D});
iptuple_to_binary({_I1, _I2, _I3, _I4, _I5, _I6, _I7, _I8}=T) ->
    kz_binary:join([to_hex(I) || I <- tuple_to_list(T)], <<":">>).

-spec to_hex(integer()) -> binary().
to_hex(I) ->
    kz_term:to_lower_binary(integer_to_binary(I, 16)).

-spec srvtuple_to_binary(srvtuple()) -> kz_term:ne_binary().
srvtuple_to_binary({Priority, Weight, Port, Domain}) ->
    <<(kz_term:to_binary(Priority))/binary, " "
     ,(kz_term:to_binary(Weight))/binary, " "
     ,(kz_term:to_binary(Port))/binary, " "
     ,(kz_binary:strip_right(kz_term:to_binary(Domain), $.))/binary
    >>.

-spec naptrtuple_to_binary(naptrtuple()) -> kz_term:ne_binary().
naptrtuple_to_binary({Order, Preference, Flags, Services, Regexp, Domain}) ->
    <<(kz_term:to_binary(Order))/binary, " "
     ,(kz_term:to_binary(Preference))/binary, " "
     ,"\"", (kz_term:to_upper_binary(Flags))/binary, "\" "
     ,"\"", (kz_term:to_upper_binary(Services))/binary, "\" "
     ,"\"", (kz_term:to_binary(Regexp))/binary, "\" "
     ,(kz_binary:strip_right(kz_term:to_binary(Domain), $.))/binary
    >>.

-spec mxtuple_to_binary(mxtuple()) -> kz_term:ne_binary().
mxtuple_to_binary({_Priority, Domain}) ->
    <<(kz_binary:strip_right(kz_term:to_binary(Domain), $.))/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec lookup_dns(kz_term:ne_binary(), atom()) ->
                        {'ok', [inet_res:dns_data()]}.
%% See kernel/src/inet_dns.hrl, the S_* macros for values for Type
lookup_dns(Hostname, Type) ->
    lookup_dns(Hostname, Type, default_options()).

-spec lookup_dns(kz_term:ne_binary(), atom(), options()) ->
                        {'ok', [inet_res:dns_data()]}.
lookup_dns(Hostname, Type, Options) ->
    {'ok', inet_res:lookup(kz_term:to_list(Hostname), 'in', Type, Options)}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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
            maybe_resolve_nameservers(Values
                                     ,[{Address, 53} || Address <- Addresses] ++ Nameservers
                                     )
    end.
