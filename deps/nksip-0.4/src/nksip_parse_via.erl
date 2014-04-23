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

%% @private SIP Via Parser
-module(nksip_parse_via).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-export([vias/1]).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parse a serie of VIAs in a string
-spec vias(binary() | string() | #via{}) ->
    [#via{}] | error.

vias(#via{}=Via) ->
    [Via];

vias(Bin) when is_binary(Bin) ->
    vias(binary_to_list(Bin));

vias(String) when is_list(String) ->
    vias(strip(String), []).


%% ===================================================================
%% Private
%% ===================================================================

%% @private
vias(String, Acc) ->
    case header(strip(String), #via{}) of
        {#via{}=Via, []} when Acc==[]-> [Via];
        {#via{}=Via, []} -> lists:reverse([Via|Acc]);
        {#via{}=Via, Rest} -> vias(Rest, [Via|Acc]);
        {error, Type, Line} -> 
            lager:debug("Error parsing via ~s: ~p (~p)", [String, Type, Line]),
            error
    end.


%% @private VIA header
header("SIP"++Rest1, Via) ->
    case strip(Rest1) of
        [$/|Rest2] -> 
            case strip(Rest2) of
                "2.0"++Rest3 ->
                    case strip(Rest3) of
                        [$/|Rest4] -> proto(strip(Rest4), [], Via);
                        _ -> {error, header, ?LINE}
                    end;
                _ ->
                    {error, header, ?LINE}
            end;
        _ ->
            {error, header, ?LINE}
    end;

header(_, _Via) ->
    {error, header, ?LINE}.


%% @private VIA Proto
proto([], _Acc, _Via) ->
    {error, proto, ?LINE};

proto([Ch|Rest], Acc, Via) when Ch==32; Ch==9; Ch==13 ->
    case Acc of
        [] ->
            {error, proto, ?LINE};
        _ ->
            Raw = lists:reverse(Acc),
            Proto = case string:to_lower(Raw) of
                "udp" -> udp;
                "tcp" -> tcp;
                "tls" -> tls;
                "sctp" -> sctp;
                "ws" -> ws;
                "wss" -> wss;
                _ -> list_to_binary(Raw)
            end,
            domain(strip(Rest), [], false, Via#via{proto=Proto})
    end;

proto([Ch|Rest], Acc, Via) ->
    proto(Rest, [Ch|Acc], Via).


%% @private VIA Domain
domain([], Acc, Ip6, Via) ->
    case Acc==[] orelse Ip6 of
        true ->
            {error, domain, ?LINE};
        false -> 
            Via1 = Via#via{domain=list_to_binary(lists:reverse(Acc))},
            {Via1, []}
    end;

domain([Ch|_]=Rest, Acc, Ip6, Via) when Ch==$;; Ch==$?; Ch==$, ->
    case Acc==[] orelse Ip6 of
        true ->
            {error, domain, ?LINE};
        false ->
            Via1 = Via#via{domain=list_to_binary(lists:reverse(Acc))},
            opts(Rest, Via1)
    end;

domain([$[|Rest], Acc, Ip6, Via) ->
    case Acc==[] andalso not Ip6 of
        true -> domain(Rest, [$[|Acc], true, Via);
        false -> {error, domain, ?LINE}
    end;

domain([$]|Rest], Acc, Ip6, Via) ->
    case Acc/=[] andalso Ip6 of
        true -> domain(Rest, [$]|Acc], false, Via);
        false -> {error, domain, ?LINE}
    end;

domain([$:|Rest], Acc, false, Via) ->
    case Acc==[] of
        true ->
            {error, domain, ?LINE};
        false -> 
            Via1 = Via#via{domain=list_to_binary(lists:reverse(Acc))},
            port(strip(Rest), [], Via1)
    end;

domain([Ch|_]=Rest, Acc, Ip6, Via) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            domain([], Acc, Ip6, Via);
        [Ch1|_]=Rest1 when Ch1==$:; Ch1==$;; Ch1==$,; Ch1==$[; Ch1==$] ->
            domain(Rest1, Acc, Ip6, Via);
        _ -> 
            {error, domain, ?LINE}
    end;

domain([Ch|Rest], Acc, Ip6, Via) ->
    domain(Rest, [Ch|Acc], Ip6, Via).


%% @private VIA Port
port([], Acc, Via) ->
    case Acc==[] of
        true ->
            {error, port, ?LINE};
        false ->
            case catch list_to_integer(lists:reverse(Acc)) of
                Port when is_integer(Port), Port>=0, Port=<65535 -> 
                    {Via#via{port = Port}, []};
                _ -> 
                    {error, port, ?LINE}
            end
    end;

port([Ch|_]=Rest, Acc, Via) when Ch==$;; Ch==$, ->
    case Acc of
        [] -> 
            {error, port, ?LINE};
        _ ->
            case catch list_to_integer(lists:reverse(Acc)) of
                Port when is_integer(Port), Port >= 0, Port =< 65535 ->
                    Via1 = Via#via{port = Port},
                    opts(Rest, Via1);
                _ ->
                  {error, port, ?LINE}
            end
    end;

port([Ch|_]=Rest, Acc, Via) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            port([], Acc, Via);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$, ->
            port(Rest1, Acc, Via);
        _ -> 
            {error, port, ?LINE}
    end;

port([Ch|Rest], Acc, Via) ->
    port(Rest, [Ch|Acc], Via).


%% @private VIA Opts
opts([], Via) -> 
    {Via, []};

opts([Ch|Rest], Via) ->
    case Ch of
        $; -> 
            opts_key(strip(Rest), [], Via);
        $, -> 
            case strip(Rest) of 
                [] -> {error, opts, ?LINE};
                Rest1 -> {Via, Rest1}
            end;
        _ when Ch==32; Ch==9; Ch==13 -> 
            opts(strip(Rest), Via);
        _ -> 
            {error, opts, ?LINE}
    end.


%% @private URI Opts Keys
opts_key([], Acc, Via) ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Via1 = Via#via{opts = Via#via.opts++[Opt]},
            opts([], Via1)
    end;

opts_key([Ch|_]=Rest, Acc, Via) when Ch==$;; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Via1 = Via#via{opts = Via#via.opts++[Opt]},
            opts(Rest, Via1)
    end;

opts_key([$=|Rest], Acc, Via) ->
    case Acc of
        [] -> {error, opts_key, ?LINE};
        _ -> opts_value(strip(Rest), lists:reverse(Acc), [], Via)
    end;

opts_key([Ch|_]=Rest, Acc, Via) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> opts_key([], Acc, Via);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$,; Ch1==$= -> opts_key(Rest1, Acc, Via);
        _ -> {error, opts_key, ?LINE}
    end;

opts_key([Ch|Rest], Acc, Via) ->
    opts_key(Rest, [Ch|Acc], Via).


%% @private URI Opts Values
opts_value([], Key, Acc, Via) ->
    case Acc of
        [] ->
            {error, opts_value, ?LINE};
        _ ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Via1 = Via#via{opts = Via#via.opts++[Opt]},
            opts([], Via1)
    end;

opts_value([Ch|_]=Rest, Key, Acc, Via) when Ch==$;; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_value, ?LINE};
        _ ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Via1 = Via#via{opts = Via#via.opts++[Opt]},
            opts(Rest, Via1)
    end;

opts_value([Ch|_]=Rest, Key, Acc, Via) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> opts_value([], Key, Acc, Via);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$, -> opts_value(Rest1, Key, Acc, Via);
        _ -> {error, opts_value, ?LINE}
    end;

opts_value([Ch|Rest], Key, Acc, Via) ->
    opts_value(Rest, Key, [Ch|Acc], Via).


%% @private VIA Strip white space
strip([32|Rest]) -> strip(Rest);
strip([13|Rest]) -> strip(Rest);
strip([10|Rest]) -> strip(Rest);
strip([9|Rest]) -> strip(Rest);
strip(Rest) -> Rest.



%% ===================================================================
%% EUnit tests
%% ===================================================================


% -ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

via_test() ->
    error = vias("SIP/3.0/udp host"),
    error = vias("SIP/2.0/udp "),
    error = vias("SIP/2.0/udp a, "),
    [#via{proto=udp, port=0, opts=[<<"rport">>, {<<"received">>, <<"1.2.3.4">>}, <<"c">>]}] =
        vias("SIP/2.0/udp host;rport;received=1.2.3.4 ; c"),
    [
        #via{proto = <<"kkk">>, domain = <<"host">>, port=1500, opts=[]},
        #via{proto = udp, domain = <<"[1:2::3]">>, port=25, opts = [<<"d">>]}
    ] = 
        vias("  SIP  / 2.0  / kkk     host : 1500  ,  SIP/2.0/UdP [1:2::3]:25;d"),
    [#via{domain= <<"host">>, port=12}] = vias("  SIP / 2.0/TCP host:12"),
    [
        #via{proto=tls, domain= <<"host">>, port=0},
        #via{domain= <<"host2">>, port=5061, 
            opts=[<<"maddr">>, {<<"received">>, <<"1.2.3.4">>}, <<"a">>]}
    ] = 
        vias("SIP/2.0/TLS  host  ,  SIP / 2.0 / UDP host2 : 5061  "
                "; maddr; received = 1.2.3.4 ; a"). 


% -endif.

