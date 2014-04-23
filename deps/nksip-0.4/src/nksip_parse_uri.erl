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

%% @private SIP URI Parser
-module(nksip_parse_uri).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-export([uris/1]).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parse a series of uris in a string
%% Can parse > 100.000 uris/sec (`<sip:user@host.com;transport=tcp;lr>') on i7
-spec uris(binary() | string() | #uri{}) ->
    [#uri{}] | error.

uris(#uri{}=Uri) ->
    [Uri];

uris(Bin) when is_binary(Bin) ->
    uris(binary_to_list(Bin));

uris(String) when is_list(String) ->
    uris(strip(String), []);

uris(_) ->
    error.


%% ===================================================================
%% Private
%% ===================================================================

%% @private
uris(String, Acc) ->
    String1 = strip(String),
    case disp(String1, [], false, #uri{}, String1) of
        {#uri{}=Uri, []} when Acc==[]-> [Uri];
        {#uri{}=Uri, []} -> lists:reverse([Uri|Acc]);
        {#uri{}=Uri, Rest} -> uris(Rest, [Uri|Acc]);
        {error, Type, Line} -> 
            lager:info("Error parsing uri ~p: ~p (~p)", [String, Type, Line]),
            error
    end.


%% @private URI Display
disp([], _Acc, _Quoted, _Uri, _Full) -> 
    {error, disp, ?LINE};

%% Case for "*"
disp([$*|Rest], [], false, Uri, Full) ->
    Uri1 = Uri#uri{domain = <<"*">>},
    case strip(Rest) of
        [] -> {Uri1, []};
        [Ch1|_]=Rest1 when Ch1==$,; Ch1==$; -> opts(Rest1, false, Uri1);
         _ -> disp(Rest, [$*], false, Uri, Full)
    end;

disp([$<|Rest], Acc, false, Uri, _Full) -> 
    Uri1 = Uri#uri{disp=list_to_binary(lists:reverse(Acc))},
    scheme(strip(Rest), [], true, Uri1);

disp([$:|_], _Acc, false, Uri, Full) -> 
    scheme(strip(Full), [], false, Uri);

disp([$,|_Rest], _Acc, false, _Uri, _Full) ->
    {error, disp, ?LINE};

disp([92, $"|Rest], Acc, true, Uri, Full) -> 
    disp(Rest, [$", 92|Acc], true, Uri, Full);

disp([$"|Rest], Acc, Quoted, Uri, Full) -> 
    disp(Rest, [$"|Acc], not Quoted, Uri, Full);

disp([Ch|Rest], Acc, Quoted, Uri, Full) -> 
    disp(Rest, [Ch|Acc], Quoted, Uri, Full).


%% @private URI Scheme
scheme([], _Acc, _Block, _Uri) ->
    {error, scheme, ?LINE};

scheme([$:|Rest], Acc, Block, Uri) ->
    case Acc of
        [] ->
            {error, scheme, ?LINE};
        _ ->
            Uri1 = Uri#uri{scheme=nksip_parse:scheme(lists:reverse(Acc))},
            Rest1 = strip(Rest),
            case user(Rest1, [], Block, Uri1) of
                {error, _Error, _Line} -> 
                    case domain(Rest1, [], false, Block, Uri1) of
                        {error, Error1, Line1} -> {error, Error1, Line1};
                        {Uri2, Rest2} -> {Uri2, Rest2}
                    end;
                {Uri2, Rest2} -> 
                    {Uri2, Rest2}
            end
    end;

scheme([Ch|Rest], Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [$:|_]=Rest1 -> scheme(Rest1, Acc, Block, Uri);
        _ -> {error, scheme, ?LINE}
    end;

scheme([$,|_Rest], _Acc, _Block, _Uri) ->
    {error, scheme, ?LINE};

scheme([Ch|Rest], Acc, Block, Uri) ->
    scheme(Rest, [Ch|Acc], Block, Uri).


%% @private URI User
user([], _Acc, _Block, _Uri) ->
    {error, user, ?LINE};

user([$:|Rest], Acc, Block, Uri) ->
    case Acc==[] of
        true ->
            {error, user, ?LINE};
        false -> 
            Uri1 = Uri#uri{user=list_to_binary(lists:reverse(Acc))},
            pass(strip(Rest), [], Block, Uri1)
    end;

user([$@|Rest], Acc, Block, Uri) ->
    case Acc==[] of
        true ->
            {error, user, ?LINE};
        false ->
            Uri1 = Uri#uri{user=list_to_binary(lists:reverse(Acc))},
            domain(strip(Rest), [], false, Block, Uri1)
    end;

user([$>|_], _Acc, true, _Uri) ->
    {error, user, ?LINE};

user([Ch|_]=Rest, Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> user([], Acc, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$:; Ch1==$@ -> user(Rest1, Acc, Block, Uri);
        _ -> {error, user, ?LINE}
    end;

user([Ch|Rest], Acc, Block, Uri) ->
    user(Rest, [Ch|Acc], Block, Uri).


%% @private URI User
pass([], _Acc, _Block, _Uri) ->
    {error, pass, ?LINE};

pass([$@|Rest], Acc, Block, Uri) ->
    case Acc==[] of
        true ->
            {error, pass, ?LINE};
        false ->
            Uri1 = Uri#uri{pass=list_to_binary(lists:reverse(Acc))},
            domain(strip(Rest), [], false, Block, Uri1)
    end;

pass([Ch|_]=Rest, Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> pass([], Acc, Block, Uri);
        [$@|_]=Rest1 -> pass(Rest1, Acc, Block, Uri);
        _ -> {error, pass, ?LINE}
    end;

pass([Ch|Rest], Acc, Block, Uri) ->
    pass(Rest, [Ch|Acc], Block, Uri).



%% @private URI Domain
domain([], Acc, Ip6, Block, Uri) ->
    case Acc==[] orelse Ip6 orelse Block of
        true ->
            {error, domain, ?LINE};
        false -> 
            Uri1 = Uri#uri{domain=list_to_binary(lists:reverse(Acc))},
            {Uri1, []}
    end;

domain([$/|Rest], [], Ip6, Block, Uri) ->
    domain(Rest, [], Ip6, Block, Uri);

domain([Ch|_]=Rest, Acc, Ip6, Block, Uri) when Ch==$;; Ch==$?; Ch==$>; Ch==$,; Ch==$/ ->
    case Acc==[] orelse Ip6 of
        true ->
            {error, domain, ?LINE};
        false ->
            Uri1 = Uri#uri{domain=list_to_binary(lists:reverse(Acc))},
            case Ch of
                $/ -> path(Rest, Block, Uri1, []);
                _ -> opts(Rest, Block, Uri1)
            end
    end;

domain([$[|Rest], Acc, Ip6, Block, Uri) ->
    case Acc==[] andalso not Ip6 of
        true -> domain(Rest, [$[|Acc], true, Block, Uri);
        false -> {error, domain, ?LINE}
    end;

domain([$]|Rest], Acc, Ip6, Block, Uri) ->
    case Acc/=[] andalso Ip6 of
        true -> domain(Rest, [$]|Acc], false, Block, Uri);
        false -> {error, domain, ?LINE}
    end;

domain([$:|Rest], Acc, false, Block, Uri) ->
    case Acc==[] of
        true ->
            {error, domain, ?LINE};
        false -> 
            Uri1 = Uri#uri{domain=list_to_binary(lists:reverse(Acc))},
            port(strip(Rest), [], Block, Uri1)
    end;

domain([$@|_], _Acc, _Ip6, _Block, _Uri) ->
    {error, domain, ?LINE};

domain([Ch|_]=Rest, Acc, Ip6, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            domain([], Acc, Ip6, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$:; Ch1==$@; Ch1==$;; Ch1==$?; Ch1==$>; Ch1==$,;
                           Ch1==$[; Ch1==$] ->
            domain(Rest1, Acc, Ip6, Block, Uri);
        _ -> 
            {error, domain, ?LINE}
    end;

domain([Ch|Rest], Acc, Ip6, Block, Uri) ->
    domain(Rest, [Ch|Acc], Ip6, Block, Uri).


%% @private URI Port
port([], Acc, Block, Uri) ->
    case Acc==[] orelse Block of
        true ->
            {error, port, ?LINE};
        false ->
            case catch list_to_integer(lists:reverse(Acc)) of
                Port when is_integer(Port), Port>=0, Port=<65535 -> 
                    {Uri#uri{port = Port}, []};
                _ -> 
                    {error, port, ?LINE}
            end
    end;

port([$@|_], _Acc, _Block, _Uri) ->
    {error, port, ?LINE};

port([Ch|_]=Rest, Acc, Block, Uri) when Ch==$;; Ch==$?; Ch==$>; Ch==$,; Ch==$/ ->
    case Acc of
        [] -> 
            {error, port, ?LINE};
        _ ->
            case catch list_to_integer(lists:reverse(Acc)) of
                Port when is_integer(Port), Port >= 0, Port =< 65535 ->
                    Uri1 = Uri#uri{port = Port},
                    case Ch of
                        $/ -> path(Rest, Block, Uri1, []);
                        _ -> opts(Rest, Block, Uri1)
                    end;
                _ ->
                  {error, port, ?LINE}
            end
    end;

port([Ch|_]=Rest, Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            port([], Acc, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$@; Ch1==$;; Ch1==$?; Ch1==$>; Ch1==$, ->
            port(Rest1, Acc, Block, Uri);
        _ -> 
            {error, port, ?LINE}
    end;

port([Ch|Rest], Acc, Block, Uri) ->
    port(Rest, [Ch|Acc], Block, Uri).


%% @private URI Opts
opts([], Block, Uri) ->
    case Block of
        false -> {Uri, []};
        true -> {error, opts, ?LINE}
    end;

opts([Ch|Rest], Block, Uri) ->
    case Ch of
        $; -> 
            opts_key(strip(Rest), [], Block, Uri);
        $? -> 
            headers_key(strip(Rest), [], Block, Uri);
        $& -> 
            headers_key(strip(Rest), [], Block, Uri);
        $> when Block -> 
            opts(strip(Rest), false, Uri);
        $> -> 
            {error, opts, ?LINE};
        $, -> 
            case strip(Rest) of
                [] -> {error, opts, ?LINE};
                Rest1 -> {Uri, Rest1}
            end;
        _ when Ch==32; Ch==9; Ch==13 -> 
            opts(strip(Rest), Block, Uri);
        _ -> 
            {error, opts, ?LINE}
    end.


%% @private URI Opts Keys
opts_key([], Acc, Block, Uri) ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Uri1 = case Block of
                true -> Uri#uri{opts = Uri#uri.opts++[Opt]};
                false -> Uri#uri{ext_opts = Uri#uri.ext_opts++[Opt]}
            end,
            opts([], Block, Uri1)
    end;

opts_key([Ch|_]=Rest, Acc, Block, Uri) when Ch==$;; Ch==$?; Ch==$>; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Uri1 = case Block of
                true -> Uri#uri{opts = Uri#uri.opts++[Opt]};
                false -> Uri#uri{ext_opts = Uri#uri.ext_opts++[Opt]}
            end,
            opts(Rest, Block, Uri1)
    end;

opts_key([$=|Rest], Acc, Block, Uri) ->
    case Acc of
        [] -> 
            {error, opts_key, ?LINE};
        _ ->
            opts_value(strip(Rest), lists:reverse(Acc), [], false, Block, Uri)
    end;

opts_key([Ch|_]=Rest, Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            opts_key([], Acc, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$?; Ch1==$>; Ch1==$,; Ch1==$= ->
            opts_key(Rest1, Acc, Block, Uri);
        _ ->
            {error, opts_key, ?LINE}
    end;

opts_key([Ch|Rest], Acc, Block, Uri) ->
    opts_key(Rest, [Ch|Acc], Block, Uri).


%% @private URI Opts Values
opts_value([], Key, Acc, Quoted, Block, Uri) ->
    case Acc==[] orelse Quoted of
        true ->
            {error, opts_value, ?LINE};
        false ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Uri1 = case Block of
                true -> Uri#uri{opts = Uri#uri.opts++[Opt]};
                false -> Uri#uri{ext_opts = Uri#uri.ext_opts++[Opt]}
            end,
            opts([], Block, Uri1)
    end;

opts_value([92, $"|Rest], Key, Acc, true, Block, Uri) ->
    opts_value(Rest, Key, [$", 92|Acc], true, Block, Uri);

opts_value([$"|Rest], Key, Acc, Quoted, Block, Uri) ->
    opts_value(Rest, Key, [$"|Acc], not Quoted, Block, Uri);

opts_value([Ch|_]=Rest, Key, Acc, false, Block, Uri) 
            when Ch==$;; Ch==$?; Ch==$>; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_value, ?LINE};
        _ ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Uri1 = case Block of
                true -> Uri#uri{opts = Uri#uri.opts++[Opt]};
                false -> Uri#uri{ext_opts = Uri#uri.ext_opts++[Opt]}
            end,
            opts(Rest, Block, Uri1)
    end;

opts_value([Ch|_]=Rest, Key, Acc, false, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            opts_value([], Key, Acc, false, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$?; Ch1==$>; Ch1==$, ->
            opts_value(Rest1, Key, Acc, false, Block, Uri);
        _ ->
            {error, opts_value, ?LINE}
    end;

opts_value([Ch|Rest], Key, Acc, Quoted, Block, Uri) ->
    opts_value(Rest, Key, [Ch|Acc], Quoted, Block, Uri).


%% @private URI Header Keys
headers_key([], Acc, Block, Uri) ->
    case Acc of
        [] ->
            {error, headers_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Uri1 = case Block of
                true -> Uri#uri{headers = Uri#uri.headers++[Opt]};
                false -> Uri#uri{ext_headers = Uri#uri.ext_headers++[Opt]}
            end,
            opts([], Block, Uri1)
    end;

headers_key([Ch|_]=Rest, Acc, Block, Uri) when Ch==$&; Ch==$>; Ch==$, ->
    case Acc of
        [] ->
            {error, headers_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Uri1 = case Block of
                true -> Uri#uri{headers = Uri#uri.headers++[Opt]};
                false -> Uri#uri{ext_headers = Uri#uri.ext_headers++[Opt]}
            end,
            opts(Rest, Block, Uri1)
    end;

headers_key([$=|Rest], Acc, Block, Uri) ->
    case Acc of
        [] -> 
            {error, headers_key, ?LINE};
        _ ->
            headers_value(strip(Rest), lists:reverse(Acc), [], false, Block, Uri)
    end;

headers_key([Ch|_]=Rest, Acc, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            headers_key([], Acc, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$&; Ch1==$>; Ch1==$,; Ch1==$= ->
            headers_key(Rest1, Acc, Block, Uri);
        _ ->
            {error, headers_key, ?LINE}
    end;

headers_key([Ch|Rest], Acc, Block, Uri) when Ch>=$A, Ch=<$Z ->
    headers_key(Rest, [Ch+32|Acc], Block, Uri);

headers_key([Ch|Rest], Acc, Block, Uri) ->
    headers_key(Rest, [Ch|Acc], Block, Uri).


%% @private URI Opts Values
headers_value([], Key, Acc, Quoted, Block, Uri) ->
    case Acc==[] orelse Quoted of
        true ->
            {error, headers_value, ?LINE};
        false ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Uri1 = case Block of
                true -> Uri#uri{headers = Uri#uri.headers++[Opt]};
                false -> Uri#uri{ext_headers = Uri#uri.ext_headers++[Opt]}
            end,
            opts([], Block, Uri1)
    end;

headers_value([92, $"|Rest], Key, Acc, true, Block, Uri) ->
    headers_value(Rest, Key, [$", 92|Acc], true, Block, Uri);

headers_value([$"|Rest], Key, Acc, Quoted, Block, Uri) ->
    headers_value(Rest, Key, [$"|Acc], not Quoted, Block, Uri);

headers_value([Ch|_]=Rest, Key, Acc, false, Block, Uri) when Ch==$&; Ch==$>; Ch==$, ->
    case Acc of
        [] ->
            {error, headers_value, ?LINE};
        _ ->
            Opt = {list_to_binary(Key), list_to_binary(lists:reverse(Acc))},
            Uri1 = case Block of
                true -> Uri#uri{headers = Uri#uri.headers++[Opt]};
                false -> Uri#uri{ext_headers = Uri#uri.ext_headers++[Opt]}
            end,
            opts(Rest, Block, Uri1)
    end;

headers_value([Ch|_]=Rest, Key, Acc, false, Block, Uri) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] ->
            headers_value([], Key, Acc, false, Block, Uri);
        [Ch1|_]=Rest1 when Ch1==$&; Ch1==$>; Ch1==$, ->
            headers_value(Rest1, Key, Acc, false, Block, Uri);
        _ ->
            {error, headers_key, ?LINE}
    end;

headers_value([Ch|Rest], Key, Acc, Quoted, Block, Uri) ->
    headers_value(Rest, Key, [Ch|Acc], Quoted, Block, Uri).



%% @private URI Opts
path([], Block, Uri, Acc) ->
    case Block of
        false -> 
            Path = list_to_binary(lists:reverse(Acc)),
            {Uri#uri{path=Path}, []};
        true -> 
            {error, path, ?LINE}
    end;

path([Ch|_]=Rest, Block, Uri, Acc) when Ch==$;; Ch==$?; Ch==$>; Ch==$, ->
    Path = list_to_binary(lists:reverse(Acc)),
    opts(Rest, Block, Uri#uri{path=Path});

path([Ch|Rest], Block, Uri, Acc) ->
    path(Rest, Block, Uri, [Ch|Acc]).


%% @private URI Strip white space
strip([32|Rest]) -> strip(Rest);
strip([13|Rest]) -> strip(Rest);
strip([10|Rest]) -> strip(Rest);
strip([9|Rest]) -> strip(Rest);
strip(Rest) -> Rest.


%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

uri1_test() ->
    error = uris(" one two sip  :  a  "),
    error = uris("abc"),
    error = uris(":a"),
    error = uris("<  : a>"),
    error = uris("sip: "),
    error = uris("<sip:a"),
    error = uris("<sip: a;"),
    error = uris("sip: a:a"),
    error = uris("<sip:u@a"),

    [#uri{disp = <<>>, scheme = sip, domain = <<"a">>}] = uris("sip:a"),
    [#uri{disp = <<>>, scheme = sip, domain = <<"a">>}] = uris("<sip:a>"),
    [#uri{disp = <<>>, scheme = sip, domain = <<"a">>}] = uris(" \x09  sip  \r\n :  a  "),
    [#uri{scheme = sip, domain = <<"b">>}] = uris("   <  sip  :  b  >  "),
    [#uri{disp = <<"Mi \"disp<,:\" ">>, scheme = sip, domain = <<"a">>}] = 
        uris(" Mi \"disp<,:\" <sip:a>"),
    [#uri{domain = <<"a">>, port = 20}] = uris("sip:a : 20"),
    [#uri{domain = <<"a">>, port = 20}] = uris("<sip:a:20>"),
    [#uri{user = <<"u">>, domain = <<"a">>}] = uris("sip:u@a"),
    [#uri{user = <<"u">>, domain = <<"a">>}] = uris("<sip:u@a >"),
    [#uri{user = <<"u">>, domain = <<"a">>, port=20}] = uris("<sip:u@a:20>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=20}] = uris("<sip:u:p@a:20>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=0}] = uris(" sip:u: p @ a"),
    [#uri{domain = <<"a">>, port = 0, ext_opts = [<<"b">>]}] = uris("sip: a;b"),
    [#uri{ext_opts = [<<"b">>, {<<"c">>, <<"2">>}]}] = uris("sip: a  ;  b ; c =  2"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=20, 
         opts = [<<"b">>, {<<"c">>, <<"2">>}]}] = uris("<sip:u:p@a:20;b;c=2>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=20, 
         ext_opts = [<<"b">>, {<<"c">>, <<"2">>}]}] = uris("sip:u:p@a:20;b;c=2 "),
    [#uri{ext_headers = [{<<"d">>, <<"2">>}, <<"f">>]}] = uris("sip:a?d=2&f"),
    [#uri{headers = [{<<"dd">>, <<"2">>}, <<"ff">>]}] = uris("<sip:a?dd=2&ff>"),

    [#uri{opts = [<<"b">>,{<<"c">>, <<"2">>}], headers = [{<<"dd">>,<<"2">>},<<"ff">>],
         ext_opts = [{<<"g">>, <<"3">>}], ext_headers = [<<"h">>,<<"i">>]}] = 
         uris("<sip:a;b;c=2?dd=2&ff>;g=3?h&i"),

    [#uri{domain = <<"*">>}] = uris("*"),
    [#uri{domain = <<"*">>}] = uris("  *  "),
    [#uri{domain = <<"*">>, ext_opts = [<<"a">>]}] = uris("*;a"),
    [#uri{domain = <<"*">>}, #uri{domain = <<"b">>}] = uris("*, sip:b"),
    ok.


uri2_test() ->
    error = uris("sip : as[1::2]"),
    error = uris("sip :[1::2"),
    [#uri{domain = <<"[1:2::3]">>, port=20}] = uris("sip:[1:2::3]:20"),
    [#uri{domain = <<"[1:2::3]">>, port=20}] = uris("<sip:  [1:2::3]  : 20  >"),
    [#uri{user = <<"u">>, domain = <<"[1:2::3]">>}] = uris("<sip:u@ [1:2::3]>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"[1:2::3]">>}] = 
        uris("<sip:u : p @ [1:2::3]>"),
    ok.

uri3_test() ->
    [#uri{domain= <<"host">>}] = uris("sip:host"),
    [#uri{scheme=sips, domain= <<"host">>, port=5061}] = 
        uris("  sips  :  host  :  5061  "),
    [#uri{
        disp=(<<"\"My name\" ">>), user=(<<"user">>), pass=(<<"pass">>), 
        domain= <<"host">>, port=5061, opts=[{<<"transport">>,<<"tcp">>}],
        headers=[<<"head1">>], ext_opts=[{<<"op1">>,<<"\"1\"">>}]
    }] = uris(" \"My name\" <sip:user:pass@host:5061;transport=tcp?head1> ; op1=\"1\""),
    [#uri{
        disp=(<<"Name   ">>), domain= <<"host">>, port=5061,
        ext_headers=[{<<"hd2">>,<<"2">>},{<<"hd3">>,<<"a">>}]
    }] = uris(" Name   < sips : host:  5061 > ?hd2=2&hd3=a"),
    [#uri{user=(<<"user">>), domain= <<"host">>, opts=[<<"lr">>,{<<"t">>,<<"1">>},<<"d">>]}] = 
        uris(" < sip : user@host ;lr; t=1 ;d ? a=1 >"),
    [#uri{ext_opts = [{<<"tag">>, <<"a48s">>}]}] = 
        uris("\"A. G. Bell\" <sip:agb@bell-telephone.com> ;tag=a48s"),
    [#uri{
        scheme = sip,
        user = <<"+12125551212">>,
        domain = <<"server.phone2net.com">>,
        ext_opts = [{<<"tag">>, <<"887s">>}]
    }] = uris("sip:+12125551212@server.phone2net.com;tag=887s"),
    [#uri{
        scheme = sip,user = <<"xxxx">>,
        pass = <<>>,domain = <<"192.168.1.2">>,port = 5060,
        opts = [{<<"transport">>,<<"TCP">>},<<"ob">>],
        headers = [],
        ext_opts = [
            {<<"reg-id">>,<<"1">>},
            {<<"+sip.instance">>,
                <<"\"<urn:uuid:00000000-0000-0000-0000-000097ee887e>\"">>}]
    }] =
        uris("<sip:xxxx@192.168.1.2:5060;transport=TCP;ob>;reg-id=1;"
             "+sip.instance=\"<urn:uuid:00000000-0000-0000-0000-000097ee887e>\""),
    [#uri{
        domain = <<"host">>, opts = [{<<"key">>,<<"\"my \\\"\"">>}],
        headers = [{<<"hd">>,<<"\"your \\\"\"">>}]
    }] = uris("<sip:host;key=\"my \\\"\"?hd=\"your \\\"\">").


uri4_test() ->
    [#uri{domain = <<"a">>}, #uri{domain = <<"b">>}] = uris("sip:a,sip:b"),
    [#uri{domain = <<"a">>}, #uri{domain = <<"b">>}] = uris("<sip:a>  ,  sip:b  "),
    [#uri{domain = <<"a">>}, #uri{domain = <<"b">>}, #uri{domain = <<"c">>}] = 
        uris(<<"<sip:a>,<sip:b@b>,<sip:c>">>),
    [
        #uri{user = <<"u1">>, pass = <<"p1">>, domain = <<"a">>, ext_headers = [<<"a1">>]}, 
        #uri{user = <<"u2">>, pass = <<"p2">>, domain = <<"b">>, opts = [<<"b1">>]}
    ] = 
        uris("sip:u1:p1@a?a1,<sip:u2:p2@b;b1>"),
    [
        #uri{user = <<"u1">>, pass = <<"p1">>, domain = <<"a">>, ext_headers = [<<"a1">>]}, 
        #uri{user = <<"u2">>, pass = <<"p2">>, domain = <<"b">>, opts = [<<"b1">>]}
    ] = 
        uris("sip:u1:p1@a?a1,<sip:u2:p2@b;b1>").

uri5_test() ->
    [#uri{headers=[{<<"route">>, <<"a">>}], ext_headers=[{<<"route">>, <<"b">>}]}] = 
        uris("<sip:a?routE=a>?route=b"),
    [#uri{headers=[], ext_headers=[{<<"route">>, <<"a">>}]}] = 
        uris("sip:a?routE=a").

uri6_test() ->    [#uri{domain = <<"a">>, port = 0, path = <<"/ws/1">>}] = uris("sip:a/ws/1"),
    [#uri{domain = <<"a">>, port = 0, path = <<"/ws">>}] = uris("<sip:a/ws>"),
    error = uris(" \x09  sip  \r\n :  a /ws/1  "),
    [#uri{domain = <<"a">>, port = 20, path= <<"/ws">>}] = uris("sip:a : 20/ws"),
    [#uri{domain = <<"a">>, port = 20, path= <<"/ws">>}] = uris("<sip:a:20/ws>"),
    [#uri{user = <<"u">>, domain = <<"a">>, port = 0, path= <<"/ws">>}] = uris("sip:u@a/ws"),
    [#uri{user = <<"u">>, domain = <<"a">>, port = 0, path= <<"/ws ">>}] = uris("<sip:u@a/ws >"),
    [#uri{user = <<"u">>, domain = <<"a">>, port = 20, path= <<"/ws">>}] = uris("<sip:u@a:20/ws>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port = 20, path= <<"/ws">>}] = uris("<sip:u:p@a:20/ws>"),
    [#uri{domain = <<"a">>, port = 0, path = <<"/ws">>, ext_opts = [<<"b">>]}] = uris("sip: a/ws;b"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=20, path = <<"/ws">>,
         opts = [<<"b">>, {<<"c">>, <<"2">>}]}] = uris("<sip:u:p@a:20/ws;b;c=2>"),
    [#uri{user = <<"u">>, pass = <<"p">>, domain = <<"a">>, port=20, path = <<"/ws">>,
         ext_opts = [<<"b">>, {<<"c">>, <<"2">>}]}] = uris("sip:u:p@a:20/ws;b;c=2 "),
    [#uri{path = <<"/ws">>, ext_headers = [{<<"d">>, <<"2">>}, <<"f">>]}] = uris("sip:a/ws?d=2&f"),
    [#uri{path = <<"/ws">> , opts = [<<"b">>,{<<"c">>, <<"2">>}], headers = [{<<"dd">>,<<"2">>},<<"ff">>],
         ext_opts = [{<<"g">>, <<"3">>}], ext_headers = [<<"h">>,<<"i">>]}] = 
         uris("<sip:a/ws;b;c=2?dd=2&ff>;g=3?h&i"),
    [#uri{path = <<"/ws">>}, #uri{path = <<"/ws2">>}] = uris("sip:a/ws,sip:b/ws2"),
    [#uri{path = <<"/ws">>}, #uri{path = <<"/ws2">>}] = uris("<sip:a/ws>  ,  sip:b/ws2"),
    [#uri{domain = <<"[1:2::3]">>, port=0, path = <<"/ws">>}] = uris("sip:[1:2::3]/ws"),
    [#uri{domain = <<"[1:2::3]">>, port=20, path = <<"/ws">>}] = uris("sip:[1:2::3]:20/ws"),
    ok.

-endif.

