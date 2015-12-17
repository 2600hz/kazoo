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

%% @doc RFC5389 STUN utility functions.
%% This module implements several functions to help sending and receiving
%% STUN requests and responses. Only <i>Binding</i> method is supported.

-module(nksip_stun).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([decode/1, binding_request/0, binding_response/3]).

-export_type([class/0, method/0, attribute/0]).

-include("nksip.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type class() :: request | indication | response | error.

-type method() :: binding | unknown.

-type attribute() :: 
        {mapped_address, {inet:ip_address(), inet:port_number()}} | {username, binary()} |
        {message_integrity, binary()} | {error_code, binary()} |
        {unknown_attributes, binary()} | {realm, binary()} | {nonce, binary()} |
        {xor_mapped_address, {inet:ip4_address(), inet:port_number()}} | {software, binary()} |
        {alternate_server, binary()} | {fingerprint, binary()} |
        {{unknown, integer()}, binary()} | {{error, integer()}, binary()}.



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Decodes a STUN packet
-spec decode(Packet::binary()) ->
    {Class::class(), Method::method(), Id::binary(), 
        Attributes::[attribute()]} | error.

decode(<<0:2, M1:5, C1:1, M2:3, C2:1, M3:4, Length:16, 16#2112A442:32, 
        Id:96, Msg/binary>>) 
        when byte_size(Msg)==Length ->
    Class = case <<C1:1, C2:1>> of
        <<2#00:2>> -> request;
        <<2#01:2>> -> indication;
        <<2#10:2>> -> response;
        <<2#11:2>> -> error
    end,
    Method = case <<0:4, M1:5, M2:3, M3:4>> of
        <<16#0001:16>> -> binding;
        _ -> unknown
    end,
    {Class, Method, <<Id:96>>, attributes(Msg)};

decode(_) ->
    error.


%% @doc Generates a <i>Binding</i> request.
-spec binding_request() ->
    {Id::binary(), Msg::binary()}.

binding_request() ->
    Id = crypto:rand_bytes(12),
    {Id, <<16#0001:16, 0:16, 16#2112A442:32, Id/binary>>}.


%% @doc Generates a <i>Binding</i> response.
-spec binding_response(Id::binary(), Ip::inet:ip4_address(), Port::inet:port_number()) ->
    Packet::binary().

binding_response(Id, {I1, I2, I3, I4}, P) ->
    case binary:encode_unsigned(P) of
        <<P1, P2>> -> ok;
        <<P2>> -> P1 = 0
    end,
    <<XP1, XP2>> = crypto:exor(<<P1, P2>>, <<33, 18>>),
    <<XI1:8, XI2:8, XI3:8, XI4:8>> = crypto:exor(<<I1, I2, I3, I4>>, <<33, 18, 164, 66>>),
    % Binding, 12 bytes, Cookie, Id, xor_mapping_address, 8 bytes, ipv4 (1), port, ip
    <<16#0101:16, 12:16, 16#2112A442:32, Id/binary,  
      16#0020:16, 8:16, 1:16, XP1, XP2, XI1, XI2, XI3, XI4>>.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private Process a list of STUN attributes
-spec attributes(binary()) -> 
    [attribute()].

attributes(Msg) ->
    attributes(Msg, []).

attributes(<<Type:16, Length:16, Rest/binary>>, Acc) ->
    Total = Length + case Length rem 4 of 0 -> 0; 1 ->3; 2->2; 3->1 end,
    case byte_size(Rest) >= Total of
        true ->
            {Data, _} = split_binary(Rest, Length),
            {_, Rest1} = split_binary(Rest, Total),
            {Class, Value} = case Type of
                % Must-process attributes
                16#0001 ->
                    case Data of
                        <<1:16, Port:16, A:8, B:8, C:8, D:8>> -> 
                            {mapped_address, {{A,B,C,D}, Port}};
                        <<2:16, Port:16, A:16, B:16, C:16, D:16, 
                                         E:16, F:16, G:16, H:16>> ->  
                            {mapped_address, {{A,B,C,D,E,F,G,H}, Port}};
                        _ -> 
                            {{error, Type}, Data}
                    end;
                16#0006 -> {username, Data};
                16#0008 -> {message_integrity, Data};
                16#0009 -> {error_code, Data};
                16#000A -> {unknown_attributes, Data};
                16#0014 -> {realm, Data};
                16#0015 -> {nonce, Data};
                16#0020 -> 
                    case Data of
                        <<1:16, PA:8, PB:8, IA:8, IB:8, IC:8, ID:8>> -> 
                            P1 = crypto:exor(<<PA, PB>>, <<33,18>>),
                            P2 = binary:decode_unsigned(P1),
                            <<A:8, B:8, C:8, D:8>> 
                                = crypto:exor(<<IA, IB, IC, ID>>, <<33,18,164,66>>),
                            {xor_mapped_address, {{A,B,C,D}, P2}};
                        % <<2:16, Port:16, Ip:128>> ->  
                        %     % IPv6 NOT DONE YET
                        %     {xor_mapped_address, {Ip, Port}};
                        _ -> 
                            {{error, Type}, Data}
                    end;
                % Optional attributes
                16#8022 -> {software, Data};                
                16#8023 -> {alternate_server, Data};
                16#8028 -> {fingerprint, Data};
                _ -> {{unknown, Type}, Data}
            end,
            attributes(Rest1, [{Class, Value}|Acc]);
        false ->
            lists:reverse(Acc)
    end;

attributes(_, Acc)   ->
    lists:reverse(Acc).



%%====================================================================
%% Eunit tests
%%====================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stun_test() ->
    M = <<
        128,40,0,4,97,98,99,100,
        16#0001:16, 8:16, 1:16, 1234:16, 127, 0, 0, 1,
        16#0006:16, 5:16, 1,2,3,4,5,6,7,8
    >>,
    ?assertMatch(
        [
            {fingerprint, <<"abcd">>},
            {mapped_address, {{127,0,0,1}, 1234}},
            {username, <<1,2,3,4,5>>}
        ],
        attributes(M)),
    {Id, Request} = binding_request(),
    ?assertMatch({request, binding, Id, []}, decode(Request)),
    Response = binding_response(Id, {1,2,3,4}, 5),
    ?assertMatch({response, binding, Id, [{xor_mapped_address,{{1,2,3,4},5}}]}, 
                    decode(Response)).

client_test() ->
    ServerIp = {216,93,246,18}, % stun.counterpath.net
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    {ok, {_LocalIp, LocalPort}} = inet:sockname(Socket),
    {Id, Request} = binding_request(),
    ?debugFmt("Sending STUN binding request to ~s", [inet_parse:ntoa(ServerIp)]),
    ok = gen_udp:send(Socket, ServerIp, 3478, Request),
    case gen_udp:recv(Socket, 0, 5000) of
        {ok, {_, _, Raw}} ->
            case decode(Raw) of
                {response, binding, Id, Data} ->
                    case proplists:get_value(mapped_address, Data) of
                        {RemoteIp, RemotePort} ->
                            ?debugFmt("STUN OK (Local: ~p, Remote: ~p, ~p)",
                                        [LocalPort, RemoteIp, RemotePort]);
                        _ ->
                            ?debugMsg("STUN: Incorrect response\n")
                    end;
                _ ->
                    ?debugMsg("STUN: Incorrect response\n")
            end;
        _ ->
            ?debugMsg("STUN: Timeout\n")
    end,
    gen_udp:close(Socket).

-endif.


