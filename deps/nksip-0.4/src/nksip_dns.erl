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

%% @doc NkSIP DNS cache and utilities with RFC3263 support, including NAPTR and SRV
-module(nksip_dns).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-compile([export_all]).

-include("nksip.hrl").

-export([resolve/1, get_ips/1, get_srvs/1, get_naptr/1, clear/1, clear/0]).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).

-define(CHECK_INTERVAL, 60).    % secs

-type connection() :: 
    {nksip:protocol(), inet:ip_address(), inet:port_number(), binary()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Finds the ips, transports and ports to try for `Uri', following RFC3263
-spec resolve(nksip:user_uri()) -> 
    [connection()].

resolve(#uri{path=Path}=Uri) ->
    case resolve_uri(Uri) of
        {ok, TranspInfo} ->
            TranspInfo;
        {naptr, Scheme, Domain} ->
            case get_naptr(Domain) of
                [] ->
                    Naptr = [
                        {sip, tls, "_sips._tcp." ++ Domain},
                        {sip, tcp, "_sip._tcp." ++ Domain},
                        {sip, udp, "_sip._udp." ++ Domain}
                    ];
                Naptr ->
                    ok
            end,
            case resolve_srvs(Scheme, Naptr, []) of
                [] when Scheme==sips -> 
                    [{tls, Addr, 5061, <<>>} || Addr <- get_ips(Domain)];
                [] -> 
                    [{udp, Addr, 5060, <<>>} || Addr <- get_ips(Domain)];
                Srvs ->
                    [
                        case Proto==ws orelse Proto==wss of
                            true  -> {Proto, Addr, Port, Path};
                            false -> {Proto, Addr, Port, <<>>}
                        end
                        ||
                        {Proto, Addr, Port} <- Srvs
                    ]
            end
    end;

resolve(Uri) ->
    case nksip_parse:uris(Uri) of
        [PUri] -> resolve(PUri);
        _ -> []
    end.


%% @private
resolve_srvs(sips, [{Scheme, _, _}|Rest], Acc) when Scheme/=sips -> 
    resolve_srvs(sips, Rest, Acc);

resolve_srvs(Scheme, [{_, NProto, NDomain}|Rest], Acc) -> 
    case get_srvs(NDomain) of
        [] -> 
            resolve_srvs(Scheme, Rest, Acc);
        Srvs -> 
            Addrs = [
                [{NProto, Addr, SPort} || Addr <- get_ips(SHost)]
                || {SHost, SPort} <- Srvs
            ],
            resolve_srvs(Scheme, Rest, [Addrs|Acc])
    end;

resolve_srvs(_, [], Acc) ->
    lists:flatten(lists:reverse(Acc)).


%% @private
-spec resolve_uri(nksip:uri()) -> 
    {ok, [{nksip:protocol(), inet:ip_address(), inet:port_number()}]} | 
    {naptr, sip|sips, string()}.

resolve_uri(#uri{scheme=Scheme, domain=Host, opts=Opts, port=Port, path=Path}) ->
    Target = nksip_lib:get_list(<<"maddr">>, Opts, Host),
    case nksip_lib:to_ip(Target) of 
        {ok, TargetIp} -> IsNumeric = true;
        _ -> TargetIp = IsNumeric = false
    end,
    Proto = case nksip_lib:get_value(<<"transport">>, Opts) of
        Atom when is_atom(Atom) -> 
            Atom;
        Other -> 
            LcTransp = string:to_lower(nksip_lib:to_list(Other)),
            case catch list_to_existing_atom(LcTransp) of
                {'EXIT', _} -> nksip_lib:to_binary(Other);
                Atom -> Atom
            end
    end,
    Resolve = fun(FProto) -> 
        Port1 = case Port > 0 of
            true -> Port;
            false -> nksip_transport:default_port(FProto)
        end,
        Addrs = case IsNumeric of
            true -> [TargetIp];
            false -> get_ips(Target)
        end,
        {ok, [{FProto, Addr, Port1, Path} || Addr <- Addrs]} 
    end,
    case {Scheme, Proto} of
        {sip, udp} -> Resolve(udp);
        {sip, tcp} -> Resolve(tcp);
        {sip, tls} -> Resolve(tls);
        {sip, sctp} -> Resolve(sctp);
        {sip, ws} -> Resolve(ws);
        {sip, wss} -> Resolve(wss);
        {sip, undefined} when IsNumeric; Port>0 -> Resolve(udp);
        {sip, undefined} -> {naptr, sip, Target};
        {sips, udp} -> {ok, []};
        {sips, tcp} -> Resolve(tls);
        {sips, tls} -> Resolve(tls);
        {sips, sctp} -> {ok, []};
        {sips, ws} -> Resolve(wss);
        {sips, wss} -> Resolve(wss);
        {sips, undefined} when IsNumeric; Port>0 -> Resolve(tls);
        {sips, undefined} -> {naptr, sips, Target};
        _ -> {ok, []}
    end.


%% @doc Gets all IPs for this host, or `[]' if not found.
%% It will first try to get it form the cache.
%% Each new invocation rotates the list of IPs.
-spec get_ips(string()|binary()) ->
    [inet:ip_address()].

get_ips(Host) ->
    Host1 = nksip_lib:to_list(Host),
    case ets:lookup(?MODULE, {ips, Host1}) of
        [{_, Ips, _Time}] ->
            random(Ips);
        [] ->
            case inet:getaddrs(Host1, inet) of
                {ok, Ips} -> 
                    ok;
                {error, _} -> 
                    case inet:getaddrs(Host1, inet6) of
                        {ok, Ips} -> 
                            ok;
                        {error, _} -> 
                            Ips = []
                    end
            end,
            Now = nksip_lib:timestamp(),
            ets:insert(?MODULE, {{ips, Host1}, Ips, Now}),
            random(Ips)
    end.


%% @doc Gets all hosts for a SRV domain, sorting the result
%% according to RFC2782
-spec get_srvs(string()|binary()) ->
    [{string(), inet:port_number()}].

get_srvs(Domain) ->
    Domain1 = nksip_lib:to_list(Domain),
    case ets:lookup(?MODULE, {srvs, Domain1}) of
        [{_, Srvs, _Time}] ->
            rfc2782_sort(Srvs);
        [] ->
            Srvs = case inet_res:lookup(Domain1, in, srv) of
                [] -> [];
                Res -> [{O, W, {D, P}} || {O, W, P, D} <- Res]
            end,
            Now = nksip_lib:timestamp(),
            ets:insert(?MODULE, {{srvs, Domain1}, Srvs, Now}),
            rfc2782_sort(Srvs)
    end.


%% @doc Finds published services using DNS NAPTR search.
-spec get_naptr(string()|binary()) -> 
    [{sip|sips, nksip:protocol(), string()}].

%% TODO: Check site certificates in case of tls
get_naptr(Domain) ->
    Domain1 = nksip_lib:to_list(Domain),
    case ets:lookup(?MODULE, {naptr, Domain1}) of
        [{_, Naptr, _Time}] ->
            Naptr;
        [] ->
            Naptr = case inet_res:lookup(Domain1, in, naptr) of
                [] ->
                    [];
                Res ->
                    [Value || Term <- lists:sort(Res), 
                              (Value = naptr_filter(Term)) /= false]
            end,
            Now = nksip_lib:timestamp(),
            ets:insert(?MODULE, {{naptr, Domain1}, Naptr, Now}),
            Naptr
    end.


%% @doc Clear all info about `Domain' in the cache.
-spec clear(string()|binary()) ->
    ok.

clear(Domain) ->
    Domain1 = nksip_lib:to_list(Domain),
    ets:delete(?MODULE, {ips, Domain1}),
    ets:delete(?MODULE, {srvs, Domain1}),
    ets:delete(?MODULE, {naptr, Domain1}),
    ok.


%% @doc Clear all stored information in the cache.
-spec clear() ->
    ok.

clear() ->
    ets:delete_all_objects(?MODULE),
    ok.


%% ===================================================================
%% gen_server
%% ===================================================================

-record(state, {
    ttl :: integer()
}).


%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
        

%% @private 
-spec init(term()) ->
    gen_server_init(#state{}).

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table, public]),   
    erlang:start_timer(1000*?CHECK_INTERVAL, self(), check_ttl), 
    TTL = nksip_config:get(dns_cache_ttl),
    {ok, #state{ttl=TTL}}.


%% @private
-spec handle_call(term(), from(), #state{}) ->
    gen_server_call(#state{}).

handle_call(Msg, _From, State) -> 
    lager:error("Module ~p received unexpected call ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_cast(term(), #state{}) ->
    gen_server_cast(#state{}).

handle_cast(Msg, State) -> 
    lager:error("Module ~p received unexpected cast ~p", [?MODULE, Msg]),
    {noreply, State}.


%% @private
-spec handle_info(term(), #state{}) ->
    gen_server_info(#state{}).

handle_info({timeout, _, check_ttl}, #state{ttl=TTL}=State) ->
    Last = nksip_lib:timestamp() - TTL,
    Spec = [{{'_', '_', '$1'}, [{'<', '$1', Last}], [true]}],
    ets:select_delete(?MODULE, Spec),
    erlang:start_timer(1000*?CHECK_INTERVAL, self(), check_ttl), 
    {noreply, State};

handle_info(Info, State) -> 
    lager:warning("Module ~p received unexpected info: ~p", [?MODULE, Info]),
    {noreply, State}.


%% @private
-spec code_change(term(), #state{}, term()) ->
    gen_server_code_change(#state{}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
-spec terminate(term(), #state{}) ->
    gen_server_terminate().

terminate(_Reason, _State) ->  
    ok.



%% ===================================================================
%% Weight algorithm
%% ===================================================================


%% @private Applies RFC2782 weight sorting algorithm
-spec rfc2782_sort([{Prio, Weight, Target}]) ->
    [Target]
    when Prio::integer(), Weight::integer(), Target::term().

rfc2782_sort([]) ->
    [];

rfc2782_sort(List) ->
    Groups = groups(List),
    lists:flatten([do_sort(Group, []) || Group <- Groups]).

%% @private
do_sort([], Acc) ->
    lists:reverse(Acc);

do_sort(List, Acc) ->
    {Pos, Sum} = sort_sum(List),
    % ?P("Pos: ~p, Sum: ~p", [Pos, Sum]),
    {Current, Rest} = sort_select(Pos, Sum, []),
    % ?P("Current: ~p", [Current]),
    do_sort(Rest, [Current|Acc]).


%% @private 
-spec sort_sum([{Weight, Target}]) ->
    {Pos, [{AccWeight, Target}]}
    when Weight::integer(), Target::term(), Pos::integer(), AccWeight::integer().

sort_sum(List) ->
    Total = lists:foldl(
        fun({W, _}, Acc) -> W+Acc end, 
        0, 
        List),
    Sum = lists:foldl(
        fun({W, T}, Acc) -> 
            case Acc of
                [{OldW, _}|_] -> [{OldW+W, T}|Acc];
                [] -> [{W, T}]
            end
        end,
        [],
        lists:sort(List)),
    Pos = case Total >= 1 of 
        true -> crypto:rand_uniform(0, Total);
        false -> 0
    end,
    {Pos, lists:reverse(Sum)}.


%% @private
-spec sort_select(Pos, [{AccWeight, Target}], [{AccWeight, Target}]) ->
    Target
    when Pos::integer(), AccWeight::integer(), Target::term().

sort_select(Pos, [{W, T}|Rest], Acc) when Pos =< W ->
    {T, Rest++Acc};

sort_select(Pos, [C|Rest], Acc) -> 
    sort_select(Pos, Rest, [C|Acc]).



%% ===================================================================
%% Utils
%% ===================================================================


%% @private Extracts and groups records with the same priority
-spec groups([{Prio::integer(), Weight::integer(), Target::term()}]) ->
    [Group]
    when Group :: [{Weight::integer(), Target::term()}].

groups(Srvs) ->
    groups(lists:sort(Srvs), [], []).


%% @private
groups([{Prio, _, _}=N|Rest], [{Prio, _, _}|_]=Acc1, Acc2) ->
    groups(Rest, [N|Acc1], Acc2);

groups([N|Rest], [], Acc2) ->
    groups(Rest, [N], Acc2);

groups([N|Rest], Acc1, Acc2) ->
    LAcc1 = [{W, T} || {_, W, T} <- Acc1],
    groups(Rest, [N], [LAcc1|Acc2]);

groups([], [], Acc2) ->
    lists:reverse(Acc2);

groups([], Acc1, Acc2) ->
    LAcc1 = [{W, T} || {_, W, T} <- Acc1],
    lists:reverse([LAcc1|Acc2]).


%% @private
-spec random(list()) ->
    list().

random([]) ->
    [];
random([A]) ->
    [A];
random([A, B]) ->
    case crypto:rand_uniform(0, 2) of
        0 -> [A, B];
        1 -> [B, A]
    end;
random([A, B, C]) ->
    case crypto:rand_uniform(0, 3) of
        0 -> [A, B, C];
        1 -> [B, C, A];
        2 -> [C, A, B]
    end;
random(List) ->
    Size = length(List),
    List1 = [{crypto:rand_uniform(0, Size), Term} || Term <- List],
    [Term || {_, Term} <- lists:sort(List1)].


%% @private
naptr_filter({_, _, "s", "sips+d2t", "", Domain}) -> {sips, tls, Domain};
naptr_filter({_, _, "s", "sip+d2u", "", Domain}) -> {sip, udp, Domain};
naptr_filter({_, _, "s", "sip+d2t", "", Domain}) -> {sip, tcp, Domain};
naptr_filter({_, _, "s", "sip+d2s", "", Domain}) -> {sip, sctp, Domain};
naptr_filter({_, _, "s", "sips+d2w", "", Domain}) -> {sips, wss, Domain};
naptr_filter({_, _, "s", "sip+d2w", "", Domain}) -> {sips, ws, Domain};
naptr_filter(_) -> false.



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

weigth_test() ->
    []= groups([]),
    [[{1,a}]] = groups([{1,1,a}]),
    [[{1,a}],[{2,b}]] = groups([{2,2,b}, {1,1,a}]),
    [[{2,b},{1,a}],[{3,c}]] = groups([{1,1,a}, {1,2,b}, {2,3,c}]),
    [[{1,a}],[{3,c},{2,b}]] = groups([{1,1,a}, {2,2,b}, {2,3,c}]),
    [[{2,b},{1,a}],[{4,d},{3,c}],[{5,e}]] = 
        groups([{1,1,a}, {1,2,b}, {2,3,c}, {2,4,d}, {3,5,e}]),

    {_, [{0,c},{0,f},{5,a},{15,b},{25,e},{50,d}]} = 
        sort_sum([{5,a}, {10,b}, {0,c}, {25,d}, {10,e}, {0,f}]),

    {b,[{4,c},{1,a}]} = 
        sort_select(3, [{1,a}, {3,b}, {4,c}], []),

    Total = [
        begin
            [A, B, C] = do_sort([{1,a}, {9,b}, {90,c}], []),
            false = A==B,
            false = A==C,
            false = B==C,
            A
        end
        || _ <- lists:seq(0,1000)
    ],
    As = length([true || a <-Total]),
    Bs = length([true || b <-Total]),
    Cs = length([true || c <-Total]),
    % ?P("As: ~p vs 1%, Bs: ~p vs 9%, Cs: ~p vs 90%", [As/10, Bs/10, Cs/10]),
    true = Cs > Bs andalso Bs > As,

    [] = rfc2782_sort([]),
    [a] = rfc2782_sort([{1,1,a}]),
    [b, a, c] = rfc2782_sort([{2,2,a}, {1,1,b}, {3,3,c}]),

    [b, A1, A2, c] = 
        rfc2782_sort([{2,10,a}, {1,1,b}, {3,3,c}, {2,10,d}]),
    true = A1==a orelse A1==d,
    true = A1/=A2,
    ok.


uri_test() ->
    Test = [
        {"<sip:1.2.3.4;transport=udp>",  {ok, [{udp, {1,2,3,4}, 5060, <<>>}]}},
        {"<sip:1.2.3.4;transport=tcp>",  {ok, [{tcp, {1,2,3,4}, 5060, <<>>}]}},
        {"<sip:1.2.3.4;transport=tls>",  {ok, [{tls, {1,2,3,4}, 5061, <<>>}]}},
        {"<sip:1.2.3.4;transport=sctp>", {ok, [{sctp, {1,2,3,4}, 5060, <<>>}]}},
        {"<sip:1.2.3.4;transport=ws>",   {ok, [{ws, {1,2,3,4}, 80, <<>>}]}},
        {"<sip:1.2.3.4;transport=wss>",  {ok, [{wss, {1,2,3,4}, 443, <<>>}]}},
        {"<sip:1.2.3.4;transport=other>",  {ok, []}},

        {"<sips:1.2.3.4;transport=udp>",  {ok, []}},
        {"<sips:1.2.3.4;transport=tcp>",  {ok, [{tls, {1,2,3,4}, 5061, <<>>}]}},
        {"<sips:1.2.3.4;transport=tls>",  {ok, [{tls, {1,2,3,4}, 5061, <<>>}]}},
        {"<sips:1.2.3.4;transport=sctp>", {ok, []}},
        {"<sips:1.2.3.4;transport=ws>",   {ok, [{wss, {1,2,3,4}, 443, <<>>}]}},
        {"<sips:1.2.3.4;transport=wss>",  {ok, [{wss, {1,2,3,4}, 443, <<>>}]}},
        {"<sip:1.2.3.4;transport=other>",  {ok, []}},

        {"<sip:1.2.3.4:4321;transport=tcp>",  {ok, [{tcp, {1,2,3,4}, 4321, <<>>}]}},
        {"<sips:127.0.0.1:4321;transport=tls>",  {ok, [{tls, {127,0,0,1}, 4321, <<>>}]}},

        {"<sip:1.2.3.4>",  {ok, [{udp, {1,2,3,4}, 5060, <<>>}]}},
        {"<sip:1.2.3.4:4321>",  {ok, [{udp, {1,2,3,4}, 4321, <<>>}]}},
        {"<sips:1.2.3.4>",  {ok, [{tls, {1,2,3,4}, 5061, <<>>}]}},
        {"<sips:1.2.3.4:4321>",  {ok, [{tls, {1,2,3,4}, 4321, <<>>}]}},

        {"<sip:127.0.0.1:1234>",  {ok, [{udp, {127,0,0,1}, 1234, <<>>}]}},
        {"<sips:127.0.0.1:1234>",  {ok, [{tls, {127,0,0,1}, 1234, <<>>}]}},

        {"<sip:anyhost>",  {naptr, sip, "anyhost"}},
        {"<sips:anyhost>",  {naptr, sips, "anyhost"}}
    ],
    lists:foreach(
        fun({Uri, Result}) -> 
            [PUri] = nksip_parse:uris(Uri),
            ?assertMatch(Result, resolve_uri(PUri)) 
        end,
        Test).

resolv_test() ->
    EtsStarted = case ets:info(?MODULE) of
        undefined ->
            ?MODULE = ets:new(?MODULE, [named_table, public]),
            true;
        _ ->
            false
    end,
    Now = nksip_lib:timestamp(),
    
    Naptr = [
        {sips, tls, "_sips._tcp.test1.local"},
        {sip, tcp, "_sip._tcp.test2.local"},
        {sip, tcp, "_sip._tcp.test3.local"},
        {sip, udp, "_sip._udp.test4.local"}
    ],
    ets:insert(?MODULE, {{naptr, "test.local"}, Naptr, Now}),
    
    Srvs1 = [{1, 1, {"test100.local", 100}}],
    ets:insert(?MODULE, {{srvs, "_sips._tcp.test1.local"}, Srvs1, Now}),
    
    Srvs2 = [{1, 1, {"test200.local", 200}}, 
             {2, 1, {"test201.local", 201}}, {2, 5, {"test202.local", 202}}, 
             {3, 1, {"test300.local", 300}}],
    ets:insert(?MODULE, {{srvs, "_sip._tcp.test2.local"}, Srvs2, Now}),
    
    Srvs3 = [{1, 1, {"test400.local", 400}}],
    ets:insert(?MODULE, {{srvs, "_sip._tcp.test3.local"}, Srvs3, Now}),
    Srvs4 = [{1, 1, {"test500.local", 500}}],
    ets:insert(?MODULE, {{srvs, "_sip._udp.test4.local"}, Srvs4, Now}),

    ets:insert(?MODULE, {{ips, "test100.local"}, [{1,1,100,1}, {1,1,100,2}], Now}),
    ets:insert(?MODULE, {{ips, "test200.local"}, [{1,1,200,1}], Now}),
    ets:insert(?MODULE, {{ips, "test201.local"}, [{1,1,201,1}], Now}),
    ets:insert(?MODULE, {{ips, "test202.local"}, [{1,1,202,1}], Now}),
    ets:insert(?MODULE, {{ips, "test300.local"}, [{1,1,300,1}], Now}),
    ets:insert(?MODULE, {{ips, "test400.local"}, [], Now}),
    ets:insert(?MODULE, {{ips, "test500.local"}, [{1,1,500,1}], Now}),

     %% Travis test machine returns two hosts...
    [{udp, {127,0,0,1}, 5060, <<>>}|_] = resolve("sip:localhost"),
    [{tls, {127,0,0,1}, 5061, <<>>}|_] = resolve("sips:localhost"),

    [A, B, C, D, E, F, G] = resolve("sip:test.local"),

    true = (A=={tls, {1,1,100,1}, 100, <<>>} orelse A=={tls, {1,1,100,2}, 100, <<>>}),
    true = (B=={tls, {1,1,100,1}, 100, <<>>} orelse B=={tls, {1,1,100,2}, 100, <<>>}),
    true = A/=B,

    C = {tcp, {1,1,200,1}, 200, <<>>},
    true = (D=={tcp, {1,1,201,1}, 201, <<>>} orelse D=={tcp, {1,1,202,1}, 202, <<>>}),
    true = (E=={tcp, {1,1,201,1}, 201, <<>>} orelse E=={tcp, {1,1,202,1}, 202, <<>>}),
    true = D/=E,

    F = {tcp, {1,1,300,1}, 300, <<>>},
    G = {udp, {1,1,500,1}, 500, <<>>},

    [H, I] = resolve("sips:test.local"),

    true = (H=={tls, {1,1,100,1}, 100, <<>>} orelse H=={tls, {1,1,100,2}, 100, <<>>}),
    true = (I=={tls, {1,1,100,1}, 100, <<>>} orelse I=={tls, {1,1,100,2}, 100, <<>>}),
    true = H/=I,

    case EtsStarted of
        true -> ets:delete(?MODULE);
        false -> ok
    end,
    ok.

path_test() ->
    % Travis CI environment returns several entries for "localhost"
    [{udp, {127,0,0,1}, 5060, <<>>}|_] = 
        nksip_dns:resolve("sip://localhost/base"),
    [{ws, {127,0,0,1}, 80, <<"/base">>}|_] = 
        nksip_dns:resolve("<sip://localhost/base;transport=ws>"),
    [{wss, {127,0,0,1}, 1234, <<"/base">>}|_] = 
        nksip_dns:resolve("<sips://localhost:1234/base;transport=ws>"),
    ok.

-endif.



