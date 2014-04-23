%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc RFC4566 SDP parser and generator utilities
-module(nksip_sdp).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([new/2, new/0, empty/0, update/2, increment/1, parse/1, unparse/1]).
-export([is_sdp/1, is_new/2, update_ip/2]).

-export_type([sdp/0, sdp_a/0, sdp_m/0, sdp_t/0, address/0]).

%% ===================================================================
%% Types
%% ===================================================================

-type address() :: 
                    {
                        Type :: binary(),       % <<"IN">>
                        AddrType :: binary(),   % <<"IP4">> or <<"IP6">>
                        Addr :: binary()
                    }.

-type sdp_a() :: {Attribute::binary(), Values::[binary()]}. 

-type sdp_t() :: {Start::integer(), Stop::integer(), Reps::[binary()]}.

-type sdp_m() :: #sdp_m{}.

-type sdp() :: #sdp{}.

-type media_status() :: inactive | recvonly | sendonly | sendrecv.

-type media_spec() :: {
                        Media :: binary(),              % <<"audio">>, <<"video">>
                        Port :: inet:port_number(), 
                        Attributes :: 
                            [{rtpmap, Pos::integer(), Data::binary()} | binary()]
                    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Generates a simple base SDP record. 
%%
%  It will use the indicated `Host' and a `Medias' description
%% to generate a new `sdp()' record, having several 'm' sections,
%% one for each media. 
%%
%% Each media must define a `Media' (like `<<"audio">>' or `<<"video">>'), 
%% a `Port' and a list of `Attributes'. Each attributes can have the form
%% `{rtpmap, Pos, Data}' to define a codec (like `{rtpmap, 0, <<"PCMU/8000">>}')
%% or a standard SDP 'a' attribute (like `<<"inactive">>' or `<<"ptime:30">>').
%% The class will be `RTP/AVP'.
%%
%% If `Host' is `"auto.nksip"', it will be changed to the current local address
%% before sending.
%%
%% See sdp3_test/0 for an example.
%%
-spec new(string()|binary(), Medias::[media_spec()]) -> 
    sdp().
    
new(Host, MediaSpecList) ->
    Now = nksip_lib:timestamp(),
    Medias = [
        #sdp_m{
            media = nksip_lib:to_binary(Media), 
            port = Port,
            fmt = [nksip_lib:to_binary(Pos) || {rtpmap, Pos, _} <- Attributes],
            attributes = [
                    case Attr of
                        {rtpmap, Pos, Data} ->
                            {<<"rtpmap">>, 
                                [nksip_lib:to_binary(Pos), nksip_lib:to_binary(Data)]};
                        Other ->
                            parse_sdp_a(nksip_lib:to_binary(Other))
                    end
                    || Attr <- Attributes]
        }
        || {Media, Port, Attributes} <- MediaSpecList
    ],
    Address = {<<"IN">>, <<"IP4">>, nksip_lib:to_host(Host)},
    #sdp{
        id = Now, 
        vsn = Now, 
        address = Address,
        connect = Address,
        time = [{0, 0, []}],
        medias = Medias
    }.


%% @doc Generates a simple base SDP record (see {@link new/2}), 
%% using host `"auto.nksip"', port <i>1080</i>, codec <i>PCMU</i>, and <i>inactive</i>
-spec new() ->
    sdp().

new() ->
    Medias = [{<<"audio">>, 1080, [{rtpmap, 0, <<"PCMU/8000">>}, <<"inactive">>]}],
    new(<<"auto.nksip">>, Medias).


%% @doc Generates an empty SDP record, using host `"local.nksip"' (see {@link new/2}).
-spec empty() ->
    sdp().

empty() ->
    new(<<"auto.nksip">>, []).


%% @doc Increments the SDP version by one
-spec increment(sdp()) ->
    sdp().

increment(#sdp{vsn=Vsn}=SDP) ->
    SDP#sdp{vsn=Vsn+1}.


%% @doc Updates and SDP changing all medias to `inactive', `recvonly', 
%% `sendonly' or `sendrecv' and incrementing the SDP version.
-spec update(sdp(), media_status()) ->
    sdp().

update(#sdp{medias=M}=SDP, State) ->
    increment(SDP#sdp{medias=update_state(M, State, [])}).

update_state([], _State, Acc) ->
    lists:reverse(Acc);

update_state([#sdp_m{attributes=Attrs}=SDPM|Rest], State, Acc) ->
    Attrs1 = nksip_lib:delete(Attrs, [<<"inactive">>, <<"recvonly">>, <<"sendonly">>, 
                                        <<"sendrecv">>]),
    Attrs2 = Attrs1 ++ [{nksip_lib:to_binary(State), []}],
    update_state(Rest, State, [SDPM#sdp_m{attributes=Attrs2}|Acc]).


%% @doc Checks if term is an valid SDP.
-spec is_sdp(term()) ->
    boolean().

is_sdp(#sdp{}) -> true;
is_sdp(_) -> false.


%% @doc Checks if `SDP2' is newer than `SDP1'.
%% If any of them are `undefined', returns `false'.
-spec is_new(SDP2::undefined|sdp(), SDP1::undefined|sdp()) ->
    boolean().

is_new(#sdp{vsn=O2}, #sdp{vsn=O1}) when O2 =< O1 -> false;
is_new(#sdp{}, #sdp{}) -> true;
is_new(_, _) -> false.


%% @doc Parses a binary SDP packet into a `sdp()' record or `error'
-spec parse(binary()) -> 
    sdp() | error.

parse(Bin) ->
    try
        Data = [{K, V} 
            || <<K, $=, V/binary>> <- binary:split(Bin, <<"\r\n">>, [global])],
        parse_sdp(v, Data, #sdp{})
    catch
        _:_ -> error
    end.


%% @doc Generates a binary SDP packet from an `sdp()' record
-spec unparse(sdp()) -> 
    binary().

unparse(#sdp{}=SDP) ->
    {OType, OAddrType, OAddress} = SDP#sdp.address,
    list_to_binary([
        $v, $=, SDP#sdp.sdp_vsn, 13, 10,
        $o, $=, SDP#sdp.user, 32, nksip_lib:to_binary(SDP#sdp.id), 32, 
            nksip_lib:to_binary(SDP#sdp.vsn), 32, OType, 32, OAddrType, 32, 
            OAddress, 13, 10,
        $s, $=, SDP#sdp.session, 13, 10,
        case SDP#sdp.info of undefined -> []; I -> [$i, $=, I, 13, 10] end,
        case SDP#sdp.uri of undefined -> []; U -> [$u, $=, U, 13, 10] end,
        case SDP#sdp.email of undefined -> []; E -> [$e, $=, E, 13, 10] end,
        case SDP#sdp.phone of undefined -> []; P -> [$p, $=, P, 13, 10] end,
        case SDP#sdp.connect of 
            undefined -> 
                []; 
            {CType, CAddrType, CAddress} -> 
                [$c, $=, CType, 32, CAddrType, 32, CAddress, 13, 10]
        end,
        [[$b, $=, B, 13, 10] || B <- SDP#sdp.bandwidth],
        [
            [
                $t, $=, integer_to_list(Start), 32, integer_to_list(Stop), 13, 10, 
                [[$r, $=, R, 13, 10] || R <- Reps]
            ]
            || {Start, Stop, Reps} <- SDP#sdp.time
        ],
        case SDP#sdp.zone of undefined -> []; Z -> [$z, $=, Z, 13, 10] end,
        case SDP#sdp.key of undefined -> []; K -> [$k, $=, K, 13, 10] end,
        [
            case Values of
                [] -> [$a, $=, Attr, 13, 10];
                _ -> [$a, $=, Attr, $:, join(Values), 13, 10]
            end
            || {Attr, Values} <- SDP#sdp.attributes 
        ],
        [
            [
                $m, $=, Media#sdp_m.media, 32, integer_to_list(Media#sdp_m.port), 
                case Media#sdp_m.nports of 
                    1 -> []; 
                    NPorts -> [$/, integer_to_list(NPorts)] 
                end,
                32, Media#sdp_m.proto,
                case Media#sdp_m.fmt of
                    [] -> [];
                    Fmt -> [32, join(Fmt)]
                end,
                13, 10,
                case Media#sdp_m.info of 
                    undefined -> []; 
                    MI -> [$i, $=, MI, 13, 10] end,
                case Media#sdp_m.connect of 
                    undefined -> 
                        []; 
                    {MType, MAddrType, MAddress} -> 
                        [$c, $=, MType, 32, MAddrType, 32, MAddress, 13, 10]
                end,
                [[$b, $=, MB, 13, 10] || MB <- Media#sdp_m.bandwidth],
                case Media#sdp_m.key of 
                    undefined -> []; 
                    MK -> [$k, $=, MK, 13, 10] end,
                [
                    case MValues of
                        [] -> 
                            [$a, $=, MAttr, 13, 10];
                        _ -> 
                            [$a, $=, MAttr, $:, join(MValues), 13, 10]
                    end
                    || {MAttr, MValues} <- Media#sdp_m.attributes
                ]
            ]
            || Media <- SDP#sdp.medias
        ]
    ]);

unparse(_) ->
    undefined.
    


%% ===================================================================
%% Internal
%% ===================================================================

%% @private Updates the IP in SDP in case it is "auto.nksip" to the local IP
-spec update_ip(sdp(), inet:ip_address() | binary()) ->
    sdp().

update_ip(#sdp{connect = {_, _, <<"auto.nksip">>}} = SDP, ListenAddr) ->
    if
        is_tuple(ListenAddr), size(ListenAddr)==4 ->
            Ip = ListenAddr, Class = <<"IP4">>;
        is_tuple(ListenAddr), size(ListenAddr)==8 ->
            Ip = ListenAddr, Class = <<"IP6">>;
        true ->
            case nksip_dns:get_ips(ListenAddr) of
                [Ip|_] when size(Ip)==4 -> Class = <<"IP4">>;
                [Ip|_] when size(Ip)==8 -> Class = <<"IP6">>;
                _ -> Ip = {0,0,0,0}, Class = <<"IP4">>
            end
    end,
    Addr = {<<"IN">>, Class, nksip_lib:to_host(Ip, false)}, 
    SDP#sdp{address=Addr, connect=Addr};

update_ip(Any, _) ->
    Any.


%% @private
-spec parse_sdp(atom(), [{atom(), binary()}], sdp()) ->
    sdp().

parse_sdp(v, [{$v, V}|R], SDP) -> 
    parse_sdp(o, R, SDP#sdp{sdp_vsn=V});
parse_sdp(o, [{$o, O}|R], SDP) -> 
    [UserName, BinId, BinVsn, Type, AddrType, Address] 
        = binary:split(O, <<" ">>, [global]),
    SDP1 = SDP#sdp{
        user = UserName,
        id = list_to_integer(binary_to_list(BinId)),
        vsn = list_to_integer(binary_to_list(BinVsn)),
        address = {Type, AddrType, Address}
    },
    parse_sdp(s, R, SDP1);
parse_sdp(s, [{$s, S}|R], SDP) -> 
    parse_sdp(i, R, SDP#sdp{session=S});
parse_sdp(i, [{$i, I}|R], SDP) -> 
    parse_sdp(u, R, SDP#sdp{info=I});
parse_sdp(i, R, SDP) -> 
    parse_sdp(u, R, SDP);
parse_sdp(u, [{$u, U}|R], SDP) -> 
    parse_sdp(e, R, SDP#sdp{uri=U});
parse_sdp(u, R, SDP) -> 
    parse_sdp(e, R, SDP);
parse_sdp(e, [{$e, E}|R], SDP) -> 
    parse_sdp(p, R, SDP#sdp{email=E});
parse_sdp(e, R, SDP) -> 
    parse_sdp(p, R, SDP);
parse_sdp(p, [{$p, P}|R], SDP) -> 
    parse_sdp(c, R, SDP#sdp{phone=P});
parse_sdp(p, R, SDP) -> 
    parse_sdp(c, R, SDP);
parse_sdp(c, [{$c, C}|R], SDP) -> 
    [Type, AddrType, Address] = binary:split(C, <<" ">>, [global]),
    parse_sdp(b, R, SDP#sdp{connect={Type, AddrType, Address}});
parse_sdp(c, R, SDP) -> 
    parse_sdp(b, R, SDP);
parse_sdp(b, [{$b, B}|R], SDP) -> 
    parse_sdp(b, R, SDP#sdp{bandwidth=[B|SDP#sdp.bandwidth]});
parse_sdp(b, R, SDP) -> 
    parse_sdp(t, R, SDP#sdp{bandwidth=lists:reverse(SDP#sdp.bandwidth)});
parse_sdp(t, [{$t, T}|R], SDP) ->
    [BinStart, BinStop] = binary:split(T, <<" ">>),
    Start = nksip_lib:to_integer(BinStart),
    Stop = nksip_lib:to_integer(BinStop),
    {Reps, R1} = parse_sdp_t(R, []),
    parse_sdp(t, R1, SDP#sdp{time=[{Start, Stop, Reps}|SDP#sdp.time]});
parse_sdp(t, R, SDP) ->
    parse_sdp(z, R, SDP#sdp{time=lists:reverse(SDP#sdp.time)});
parse_sdp(z, [{$z, Z}|R], SDP) -> 
    parse_sdp(k, R, SDP#sdp{zone=Z});
parse_sdp(z, R, SDP) -> 
    parse_sdp(k, R, SDP);
parse_sdp(k, [{$k, K}|R], SDP) -> 
    parse_sdp(a, R, SDP#sdp{key=K});
parse_sdp(k, R, SDP) -> 
    parse_sdp(a, R, SDP);
parse_sdp(a, [{$a, A}|R], SDP) -> 
    Attr = parse_sdp_a(A),
    parse_sdp(a, R, SDP#sdp{attributes=[Attr|SDP#sdp.attributes]});
parse_sdp(a, R, SDP) -> 
    parse_sdp(m, R, SDP#sdp{attributes=lists:reverse(SDP#sdp.attributes)});
parse_sdp(m, [{$m, M}|R], SDP) ->
    [Media, Ports, Proto|Format] = binary:split(M, <<" ">>, [global]),
    case binary:split(Ports, <<"/">>) of
        [BinPort, BinNPorts] ->
            Port = list_to_integer(binary_to_list(BinPort)),
            NPorts = list_to_integer(binary_to_list(BinNPorts));
        [BinPort] ->
            Port = list_to_integer(binary_to_list(BinPort)),
            NPorts = 1
    end,
    M1 = #sdp_m{media=Media, port=Port,nports=NPorts, proto=Proto, fmt=Format},
    {M2, R1} = parse_sdp_m(i, R, M1),
    parse_sdp(m, R1, SDP#sdp{medias=[M2|SDP#sdp.medias]});
parse_sdp(m, [], SDP) ->
    SDP#sdp{medias=lists:reverse(SDP#sdp.medias)};
parse_sdp(K, R, SDP) -> 
    throw({K, R, SDP}).


%% @private
-spec parse_sdp_t([{atom(), binary()}], [binary()]) ->
    {[binary()], [{atom(), binary()}]}.

parse_sdp_t([{$r, Rep}|R], Acc) -> 
    parse_sdp_t(R, [Rep|Acc]);
parse_sdp_t(R, Acc) -> 
    {lists:reverse(Acc), R}.


%% @private
-spec parse_sdp_m(atom(), [{atom(), binary()}], #sdp_m{}) ->
    {sdp_m(), [{atom(), binary()}]}.

parse_sdp_m(i, [{$i, I}|R], SDPM) ->
    parse_sdp_m(c, R, SDPM#sdp_m{info=I});
parse_sdp_m(i, R, SDPM) ->
    parse_sdp_m(c, R, SDPM);
parse_sdp_m(c, [{$c, C}|R], SDPM) ->
    [Type, AddrType, Address] = binary:split(C, <<" ">>, [global]),
    parse_sdp_m(b, R, SDPM#sdp_m{connect={Type, AddrType, Address}});
parse_sdp_m(c, R, SDPM) -> 
    parse_sdp_m(b, R, SDPM);
parse_sdp_m(b, [{$b, B}|R], SDPM) ->
    parse_sdp_m(b, R, SDPM#sdp_m{bandwidth=[B|SDPM#sdp_m.bandwidth]});
parse_sdp_m(b, R, SDPM) ->
    parse_sdp_m(k, R, SDPM#sdp_m{bandwidth=lists:reverse(SDPM#sdp_m.bandwidth)});
parse_sdp_m(k, [{$k, K}|R], SDPM) ->
    parse_sdp_m(a, R, SDPM#sdp_m{key=K});
parse_sdp_m(k, R, SDPM) ->
    parse_sdp_m(a, R, SDPM);
parse_sdp_m(a, [{$a, A}|R], SDPM) ->
    Attr = parse_sdp_a(A),
    parse_sdp_m(a, R, SDPM#sdp_m{attributes=[Attr|SDPM#sdp_m.attributes]});
parse_sdp_m(a, R, SDPM) ->
    {SDPM#sdp_m{attributes=lists:reverse(SDPM#sdp_m.attributes)}, R}.

%% @private
-spec parse_sdp_a(binary()) ->
    sdp_a().

parse_sdp_a(V) ->
    case binary:split(V, <<":">>) of
        [Attribute] ->
            {Attribute, []};
        [Attribute, RawValues] ->
            {Attribute, binary:split(RawValues, <<" ">>, [global])}
    end.


%% @private
-spec join([binary()]) ->
    iolist().

join([Term]) -> Term;
join([First|Rest]) -> join(Rest, [First]).

%% @private
-spec join([binary()], iolist()) ->
    iolist().

join([Next|Rest], Acc) -> join(Rest, [Next, 32 | Acc]);
join([], Acc) -> lists:reverse(Acc).



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sdp1_test() ->
    Bin = <<
        "v=0\r\n"
        "o=- 31726 1 IN IP4 130.117.91.40\r\n"
        "s=Duocom\r\n"
        "c=IN IP4 130.117.91.40\r\n"
        "t=0 0\r\n"
        "m=audio 50370 RTP/AVP 0 101\r\n"
        "a=rtpmap:0 PCMU/8000\r\n"
        "a=rtpmap:101 telephone-event/8000\r\n"
        "a=rtcp:1500 IN IP4 1.2.3.4\r\n"
        "a=fmtp:101 0-15\r\n"
        "a=ptime:30\r\n"
        "a=sendrecv\r\n"
        "m=video 50371 RTP/AVP 0 101\r\n"
        "i=I2\r\n"
        "c=IN IP4 130.117.91.41\r\n"
        "a=rtcp:1500\r\n"
        "a=rtpmap:0\r\n"
        "a=rtpmap:101 telephone-event/8000\r\n"
        "a=fmtp:101 0-15\r\n"
        "a=ptime:30\r\n"
        "a=sendrecv\r\n"
        "a=otro\r\n"
        "a=uno:value otro:value\r\n"
    >>,
    SDP = parse(Bin),
    ?assertMatch(
        #sdp{
        sdp_vsn = <<"0">>,
        user = <<"-">>, 
        id = 31726,
        vsn = 1,
        address = {<<"IN">>, <<"IP4">>, <<"130.117.91.40">>},
        session = <<"Duocom">>,
        connect = {<<"IN">>, <<"IP4">>, <<"130.117.91.40">>},
        time = [{0, 0, []}],
        medias = [
            #sdp_m{
                media = <<"audio">>,
                port = 50370,
                proto = <<"RTP/AVP">>,
                fmt = [<<"0">>, <<"101">>],
                attributes = [
                    {<<"rtpmap">>, [<<"0">>, <<"PCMU/8000">>]},
                    {<<"rtpmap">>, [<<"101">>, <<"telephone-event/8000">>]},
                    {<<"rtcp">>, [<<"1500">>, <<"IN">>, <<"IP4">>, <<"1.2.3.4">>]},
                    {<<"fmtp">>, [<<"101">>, <<"0-15">>]},
                    {<<"ptime">>, [<<"30">>]},
                    {<<"sendrecv">>, []}
                ]
            },
            #sdp_m{
                media = <<"video">>,
                port = 50371,
                proto = <<"RTP/AVP">>,
                fmt = [<<"0">>, <<"101">>],
                info = <<"I2">>,
                connect = {<<"IN">>, <<"IP4">> ,<<"130.117.91.41">>},
                attributes = [
                    {<<"rtcp">>, [<<"1500">>]},
                    {<<"rtpmap">>, [<<"0">>]},
                    {<<"rtpmap">>, [<<"101">>, <<"telephone-event/8000">>]},
                    {<<"fmtp">>,[<<"101">>,<<"0-15">>]},
                    {<<"ptime">>,[<<"30">>]},
                    {<<"sendrecv">>,[]},
                    {<<"otro">>,[]},
                    {<<"uno">>,[<<"value">>,<<"otro:value">>]}
                ]
            }
        ]}, SDP),
    ?assertMatch(Bin, nksip_sdp:unparse(SDP)).

sdp2_test() ->
    Bin = <<
        "v=0\r\n",
        "o=Name 1 2 IN IP4 1.2.3.4\r\n",
        "s=S\r\n",
        "i=I\r\n",
        "u=U\r\n",
        "e=E\r\n",
        "p=P\r\n",
        "c=IN IP4 4.3.2.1\r\n",
        "b=B1\r\n",
        "b=B2\r\n",
        "t=1 2\r\n",
        "r=T1R1\r\n",
        "r=T1R2\r\n",
        "t=3 4\r\n"
        "r=T2R1\r\n",
        "r=T2R2\r\n",
        "z=Z\r\n",
        "k=K\r\n",
        "a=prea1\r\n",
        "a=prea2:value a2\r\n",
        "m=class0 1234 transp0 1 2 3\r\n",
        "i=M1I\r\n",
        "c=IN IP4 5.6.7.8\r\n",
        "b=M1B1\r\n",
        "b=M1B2\r\n",
        "k=M1K\r\n",
        "a=M1A1\r\n",
        "a=M1A2\r\n"
        "m=class2 4321/2 transp1\r\n"
    >>,
    SDP = parse(Bin),
    ?assertMatch(
        #sdp{
            sdp_vsn = <<"0">>,
            user = <<"Name">>,
            id = 1,
            vsn = 2,
            address = {<<"IN">>, <<"IP4">>, <<"1.2.3.4">>},
            session = <<"S">>,
            info = <<"I">>,
            uri = <<"U">>,
            email = <<"E">>,
            phone = <<"P">>,
            connect = {<<"IN">>, <<"IP4">>, <<"4.3.2.1">>},
            bandwidth = [<<"B1">>, <<"B2">>],
            time = [
                        {1, 2, [<<"T1R1">>, <<"T1R2">>]}, 
                        {3, 4, [<<"T2R1">>, <<"T2R2">>]}
                    ],
            zone = <<"Z">>,
            key = <<"K">>,
            attributes = [{<<"prea1">>, []}, {<<"prea2">>, [<<"value">>, <<"a2">>]}],
            medias = [
                #sdp_m{
                    media = <<"class0">>, 
                    port = 1234, 
                    nports = 1, 
                    proto = <<"transp0">>,
                    fmt = [<<"1">>, <<"2">>, <<"3">>],
                    info = <<"M1I">>,
                    connect = {<<"IN">>, <<"IP4">>, <<"5.6.7.8">>},
                    bandwidth = [<<"M1B1">>,<<"M1B2">>],
                    key = <<"M1K">>,
                    attributes = [{<<"M1A1">>, []}, {<<"M1A2">>, []}]
                },
                #sdp_m{
                    media = <<"class2">>, 
                    port = 4321,
                    nports = 2,
                    proto = <<"transp1">>,
                    fmt = []
                }
            ]
        },
        SDP),
    ?assert(Bin == nksip_sdp:unparse(SDP)).

sdp3_test() ->
    Media = [
        {<<"audio">>, 10000, [{rtpmap, 0, "Params0"}, {rtpmap, 1, "Params1"}, sendrecv]},
        {<<"video">>, 10001, [{rtpmap, 2, "Params2"}, {rtpmap, 3, "Params3"}, sendrecv]}
    ],
    #sdp{id=Id, vsn=Vsn} = SDP = new("local", Media),
    Bin = list_to_binary([
        "v=0\r\n"
        "o=- ", integer_to_list(Id), " ", integer_to_list(Vsn), " IN IP4 local\r\n"
        "s=nksip\r\n"
        "c=IN IP4 local\r\n"
        "t=0 0\r\n"
        "m=audio 10000 RTP/AVP 0 1\r\n"
        "a=rtpmap:0 Params0\r\n"
        "a=rtpmap:1 Params1\r\n"
        "a=sendrecv\r\n"
        "m=video 10001 RTP/AVP 2 3\r\n"
        "a=rtpmap:2 Params2\r\n"
        "a=rtpmap:3 Params3\r\n"
        "a=sendrecv\r\n"
    ]),
    ?assertMatch(Bin, unparse(SDP)).

sdp4_test() ->
    SDP = #sdp{connect={<<"IN">>, <<"IP4">>, <<"0.0.0.0">>}, time=[{0,0,[]}]},
    Bin = <<
        "v=0\r\n"
        "o=- 0 0 IN IP4 0.0.0.0\r\n"
        "s=nksip\r\n"
        "c=IN IP4 0.0.0.0\r\n"
        "t=0 0\r\n"
    >>,
    ?assertMatch(Bin, unparse(SDP)),
    ?assertMatch(SDP, parse(Bin)).


-endif.


