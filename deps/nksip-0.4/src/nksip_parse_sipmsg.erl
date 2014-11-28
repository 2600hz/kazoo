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

%% @private SIP message parsing functions
%%
%% This module implements several functions to parse sip requests, responses
%% headers, uris, vias, etc.

-module(nksip_parse_sipmsg).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([parse/2, parse/1]).

%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parses a SIP packet. Message MUST have a \r\n\r\n.
-spec parse(binary()) ->
    {ok, Class, [nksip:header()], binary()} | error
    when Class :: {req, nksip:method(), binary()} | {resp, string(), binary()}.

parse(Bin) ->
    try
        Class = case first(Bin) of
            {req, Method, Uri, Rest1} -> {req, Method, Uri};
            {resp, Code, Reason, Rest1} -> {resp, Code, Reason}
        end,
        {Headers, Rest2} = headers(Rest1, []),
        {ok, Class, Headers, Rest2}
    catch
        throw:{line, _Line} -> 
            % lager:error("LINE: ~p", [_Line]),
            error
    end.




%% @doc Parses a SIP packet. Message MUST have a \r\n\r\n.
-spec parse(nksip:protocol(), binary()) ->
    {ok, Class, [nksip:header()], binary(), binary()} | partial | error |
    {reply, Class, [nksip:header()], binary()}
    when Class :: {req, nksip:method(), binary()} | {resp, string(), binary()}.

parse(Proto, Bin) ->
    try
        Class = case first(Bin) of
            {req, Method, Uri, Rest1} -> {req, Method, Uri};
            {resp, Code, Reason, Rest1} -> {resp, Code, Reason}
        end,
        {Headers, Rest2} = headers(Rest1, []),
        case proplists:get_all_values(<<"content-length">>, Headers) of
            [] when Proto==tcp; Proto==tls -> 
                {reply, Class, Headers, <<"Content-Length">>};
            [] -> 
                {ok, Class, Headers, Rest2, <<>>};
            [CL0] ->
                case nksip_lib:to_integer(CL0) of
                    error -> 
                        {reply, Class, Headers, <<"Content-Length">>};
                    CL when CL<0 ->
                        {reply, Class, Headers, <<"Content-Length">>};
                    CL -> 
                        case byte_size(Rest2) of
                            CL -> 
                                {ok, Class, Headers, Rest2, <<>>};
                            BS when CL>BS andalso (Proto==tcp orelse Proto==tls) ->
                                partial;
                            BS when CL>BS ->
                                {reply, Class, Headers, <<"Content-Length">>};
                            _ ->
                                {Body, Rest3} = split_binary(Rest2, CL),
                                {ok, Class, Headers, Body, Rest3}
                        end
                end;
            _ ->
                {reply, Class, Headers, <<"Content-Length">>}
        end
    catch
        throw:{line, _} -> error
    end.





%% ===================================================================
%% Internal
%% ===================================================================


%% @private
-spec first(binary()) ->
    {req, nksip:method(), binary(), binary()} |
    {resp, string(), binary(), binary()}.

first(<<"SIP/2.0 ", Rest/binary>>) -> 
    {Code, Rest1} = until_sp(Rest, []),
    {Reason, Rest2} = until_rn(Rest1, []),
    {resp, Code, list_to_binary(Reason), Rest2};

first(Bin) -> 
    {Method, Rest1} = until_sp(Bin, []),
    case until_sp(Rest1, []) of
        {Uri, <<"SIP/2.0\r\n", Rest2/binary>>} ->
            RUri = <<$<, (list_to_binary(Uri))/binary, $>>>,
            {req, nksip_parse:method(Method), RUri, Rest2};
        _ ->
            throw({line, ?LINE})
    end.


%% @private
headers(<<>>, _Acc) ->
    throw({line, ?LINE});

headers(<<"\r\n", Body/binary>>, Acc) ->
    {lists:reverse(Acc), Body};

headers(Bin, Acc) ->
    {Name, Value, Rest} = name(Bin, []),
    Name1 = case Name of
        [Single] -> long_name(Single);
        _ -> list_to_binary(Name)
    end,
    headers(Rest, [{Name1, list_to_binary(Value)}|Acc]).


%% @private
name(<<$\s, Rest/binary>>, Acc) -> colon(remove_ws(Rest), lists:reverse(Acc));
name(<<$\t, Rest/binary>>, Acc) -> colon(remove_ws(Rest), lists:reverse(Acc));
name(<<$\r, $\n, _/binary>>, _Acc) -> throw({line, ?LINE});
name(<<$:, Rest/binary>>, Acc) -> value(remove_ws(Rest), lists:reverse(Acc), []);
name(<<Ch, Rest/binary>>, Acc) when Ch>=$A, Ch=<$Z -> name(Rest, [Ch+32|Acc]);
name(<<Ch, Rest/binary>>, Acc) -> name(Rest, [Ch|Acc]);
name(<<>>, _Acc) -> throw({line, ?LINE}).


%% @private
colon(<<$:, Rest/binary>>,  N) -> value(remove_ws(Rest), N, []);
colon(_, _N) -> throw({line, ?LINE}).


%% @private
value(<<$\r, $\n, $\s, Rest/binary>>, N, Acc) -> value(remove_ws(Rest), N, [$\s|Acc]);
value(<<$\r, $\n, $\t, Rest/binary>>, N, Acc) -> value(remove_ws(Rest), N, [$\s|Acc]);
value(<<$\r, $\n, Rest/binary>>, N, Acc) -> {N, lists:reverse(Acc), Rest};
value(<<Ch, Rest/binary>>, N, Acc) -> value(Rest, N, [Ch|Acc]);
value(<<>>, _N, _Acc) -> throw({line, ?LINE}).


%% @private
until_sp(<<32, _/binary>>, []) -> throw({line, ?LINE});
until_sp(<<32, Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
until_sp(<<>>, _Acc) -> throw({line, ?LINE});
until_sp(<<Ch, Rest/binary>>, Acc) -> until_sp(Rest, [Ch|Acc]).


%% @private
until_rn(<<$\r, $\n, Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
until_rn(<<>>, _Acc) -> throw({line, ?LINE});
until_rn(<<Ch, Rest/binary>>, Acc) -> until_rn(Rest, [Ch|Acc]).


%% @private
remove_ws(<<$\s, Rest/binary>>) -> remove_ws(Rest);
remove_ws(<<$\t, Rest/binary>>) -> remove_ws(Rest);
remove_ws(Other) -> Other.


%% @private
long_name($a) -> <<"accept-contact">>;
long_name($b) -> <<"referred-by">>;
long_name($c) -> <<"content-type">>;
long_name($d) -> <<"request-disposition">>;
long_name($e) -> <<"content-encoding">>;
long_name($f) -> <<"from">>;
long_name($i) -> <<"call-id">>;
long_name($j) -> <<"reject-contact">>;
long_name($k) -> <<"supported">>;
long_name($l) -> <<"content-length">>;
long_name($m) -> <<"contact">>;
long_name($n) -> <<"identity-info">>;
long_name($o) -> <<"event">>;
long_name($r) -> <<"refer-to">>;
long_name($s) -> <<"subject">>;
long_name($t) -> <<"to">>;
long_name($u) -> <<"allow-events">>;
long_name($v) -> <<"via">>;
long_name($x) -> <<"session-expires">>;
long_name($y) -> <<"identity">>;
long_name(Other) -> <<Other>>.




