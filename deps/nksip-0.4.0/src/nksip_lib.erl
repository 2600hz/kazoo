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

%% @doc Common library utility funcions
-module(nksip_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([ensure_all_started/2]).
-export([cseq/0, luid/0, lhash/1, uid/0, uuid_4122/0, hash/1, hash36/1]).
-export([get_local_ips/0, find_main_ip/0, find_main_ip/2]).
-export([timestamp/0, l_timestamp/0, l_timestamp_to_float/1]).
-export([timestamp_to_local/1, timestamp_to_gmt/1]).
-export([local_to_timestamp/1, gmt_to_timestamp/1]).
-export([get_value/2, get_value/3, get_binary/2, get_binary/3, get_list/2, get_list/3]).
-export([get_integer/2, get_integer/3, store_value/2, store_value/3]).
-export([to_binary/1, to_list/1, to_integer/1, to_ip/1, to_host/1, to_host/2]).
-export([to_lower/1, to_upper/1, strip/1, unquote/1, is_string/1]).
-export([bjoin/1, bjoin/2, tokens/1, hex/1, extract/2, delete/2, defaults/2, bin_last/2]).
-export([cancel_timer/1, msg/2]).

-export_type([optslist/0, timestamp/0, l_timestamp/0]).

-include("nksip.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% Standard Proplist
-type optslist() :: [atom() | binary() | {atom()|binary(), term()}].

%% System timestamp
-type timestamp() :: non_neg_integer().
-type l_timestamp() :: non_neg_integer().



%% ===================================================================
%% Public
%% =================================================================

%% @doc Ensure that an application and all of its transitive
%% dependencies are started.
ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
        {ok, Started} ->
            {ok, lists:reverse(Started)};
        {error, Reason, Started} ->
            [ application:stop(App) || App <- Started ],
            {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
        ok ->
            {ok, [Application | Started]};
        {error, {already_started, Application}} ->
            {ok, Started};
        {error, {not_started, Dependency}} ->
            case ensure_all_started(Dependency, Type, Started) of
                {ok, NewStarted} ->
                    ensure_all_started(Application, Type, NewStarted);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason, Started}
    end.


%% @doc Generates an incrementing-each-second 31 bit integer.
%% It will not wrap around until until {{2080,1,19},{3,14,7}} GMT.
-spec cseq() -> 
    non_neg_integer().

cseq() ->
    case binary:encode_unsigned(timestamp()-1325376000) of  % Base is 1/1/2012
        <<_:1, CSeq:31>> -> ok;
        <<_:9, CSeq:31>> -> ok
    end,
    CSeq.   


%% @doc Generates a new printable random UUID.
-spec luid() -> 
    binary().

luid() ->
    lhash({make_ref(), os:timestamp()}).


%% @doc Generates a RFC4122 compatible uuid
-spec uuid_4122() ->
    binary().

uuid_4122() ->
    Rand = hex(crypto:rand_bytes(4)),
    <<A:16/bitstring, B:16/bitstring, C:16/bitstring>> = <<(nksip_lib:l_timestamp()):48>>,
    Hw = get_hwaddr(),
    <<Rand/binary, $-, (hex(A))/binary, $-, (hex(B))/binary, $-, 
      (hex(C))/binary, $-, Hw/binary>>.


%% @doc Generates a new printable SHA hash binary over `Base' (using 160 bits, 27 chars).
-spec lhash(term()) -> 
    binary().

lhash(Base) -> 
    <<I:160/integer>> = crypto:hash(sha, term_to_binary(Base)),
    case encode_integer(I) of
        Hash when byte_size(Hash) == 27 -> Hash;
        Hash -> <<(binary:copy(<<"a">>, 27-byte_size(Hash)))/binary, Hash/binary>>
    end.


%% @doc Generates a new random tag of 6 chars
-spec uid() -> 
    binary().

uid() ->
    hash({make_ref(), os:timestamp()}).

%% @doc Generates a new tag of 6 chars based on a value.
-spec hash(term()) -> 
    binary().

hash(Base) ->
    case encode_integer(erlang:phash2([Base], 4294967296)) of
        Hash when byte_size(Hash)==6 -> Hash;
        Hash -> <<(binary:copy(<<"a">>, 6-byte_size(Hash)))/binary, Hash/binary>>
    end.


%% @doc Generates a new tag based on a value (only numbers and uppercase) of 7 chars
-spec hash36(term()) -> 
    binary().

hash36(Base) ->
    case encode_integer_36(erlang:phash2([Base], 4294967296)) of
        Hash when byte_size(Hash)==7 -> Hash;
        Hash -> <<(binary:copy(<<"A">>, 7-byte_size(Hash)))/binary, Hash/binary>>
    end.


%% @doc Get all local network ips.
-spec get_local_ips() -> 
    [inet:ip_address()].

get_local_ips() ->
    {ok, All} = inet:getifaddrs(),
    lists:flatten([proplists:get_all_values(addr, Data) || {_, Data} <- All]).


%% @doc Equivalent to `find_main_ip(auto, ipv4)'.
-spec find_main_ip() -> 
    inet:ip_address().

find_main_ip() ->
    find_main_ip(auto, ipv4).


%% @doc Finds the <i>best</i> local IP.
%% If a network interface is supplied (as "en0") it returns its ip.
%% If `auto' is used, probes `ethX' and `enX' interfaces. If none is available returns 
%% localhost
-spec find_main_ip(auto|string(), ipv4|ipv6) -> 
    inet:ip_address().

find_main_ip(NetInterface, Type) ->
    {ok, All} = inet:getifaddrs(),
    case NetInterface of
        auto ->
            IFaces = lists:filter(
                fun(Name) ->
                    case Name of
                        "eth" ++ _ -> true;
                        "en" ++ _ -> true;
                        _ -> false
                    end
                end,
                proplists:get_keys(All)),
            find_main_ip(lists:sort(IFaces), All, Type);
        _ ->
            find_main_ip([NetInterface], All, Type)   
    end.


%% @private
find_main_ip([], _, ipv4) ->
    {127,0,0,1};

find_main_ip([], _, ipv6) ->
    {0,0,0,0,0,0,0,1};

find_main_ip([IFace|R], All, Type) ->
    Data = get_value(IFace, All, []),
    Flags = get_value(flags, Data, []),
    case lists:member(up, Flags) andalso lists:member(running, Flags) of
        true ->
            Addrs = lists:zip(
                proplists:get_all_values(addr, Data),
                proplists:get_all_values(netmask, Data)),
            case find_real_ip(Addrs, Type) of
                error -> find_main_ip(R, All, Type);
                Ip -> Ip
            end;
        false ->
            find_main_ip(R, All, Type)
    end.

%% @private
find_real_ip([], _Type) ->
    error;

% Skip link-local addresses
find_real_ip([{{65152,_,_,_,_,_,_,_}, _Netmask}|R], Type) ->
    find_real_ip(R, Type);

find_real_ip([{{A,B,C,D}, Netmask}|_], ipv4) 
             when Netmask /= {255,255,255,255} ->
    {A,B,C,D};

find_real_ip([{{A,B,C,D,E,F,G,H}, Netmask}|_], ipv6) 
             when Netmask /= {65535,65535,65535,65535,65535,65535,65535,65535} ->
    {A,B,C,D,E,F,G,H};

find_real_ip([_|R], Type) ->
    find_real_ip(R, Type).


%% @private Finds the MAC addr for enX or ethX, or a random one if none is found
get_hwaddr() ->
    {ok, Addrs} = inet:getifaddrs(),
    get_hwaddrs(Addrs).


%% @private
get_hwaddrs([{Name, Data}|Rest]) ->
    case Name of
        "en"++_ ->
            case nksip_lib:get_value(hwaddr, Data) of
                Hw when is_list(Hw), length(Hw)==6 -> hex(Hw);
                _ -> get_hwaddrs(Rest)
            end;
        "eth"++_ ->
            case nksip_lib:get_value(hwaddr, Data) of
                Hw when is_list(Hw), length(Hw)==6 -> hex(Hw);
                _ -> get_hwaddrs(Rest)
            end;
        _ ->
            get_hwaddrs(Rest)
    end;

get_hwaddrs([]) ->
    hex(crypto:rand_bytes(6)).




% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(SECONDS_FROM_GREGORIAN_BASE_TO_EPOCH, (1970*365+478)*24*60*60).


%% @doc Gets an second-resolution timestamp
-spec timestamp() -> timestamp().

timestamp() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds*1000000 + Seconds.


%% @doc Gets an microsecond-resolution timestamp
-spec l_timestamp() -> l_timestamp().

l_timestamp() ->
    {N1, N2, N3} = os:timestamp(),
    (N1 * 1000000 + N2) * 1000000 + N3.


%% @doc Converts a `timestamp()' to a local `datetime()'.
-spec timestamp_to_local(timestamp()) -> 
    calendar:datetime().

timestamp_to_local(Secs) ->
    calendar:now_to_local_time({0, Secs, 0}).


%% @doc Converts a `timestamp()' to a gmt `datetime()'.
-spec timestamp_to_gmt(timestamp()) -> 
    calendar:datetime().

timestamp_to_gmt(Secs) ->
    calendar:now_to_universal_time({0, Secs, 0}).

%% @doc Generates a float representing `HHMMSS.MicroSecs' for a high resolution timer.
-spec l_timestamp_to_float(l_timestamp()) -> 
    float().

l_timestamp_to_float(LStamp) ->
    Timestamp = trunc(LStamp/1000000),
    {_, {H,Mi,S}} = nksip_lib:timestamp_to_local(Timestamp),
    Micro = LStamp-Timestamp*1000000,
    H*10000+Mi*100+S+(Micro/1000000).


%% @doc Converts a local `datetime()' to a `timestamp()',
-spec gmt_to_timestamp(calendar:datetime()) -> 
    timestamp().

gmt_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200.


%% @doc Converts a gmt `datetime()' to a `timestamp()'.
-spec local_to_timestamp(calendar:datetime()) -> 
    timestamp().

local_to_timestamp(DateTime) ->
    case calendar:local_time_to_universal_time_dst(DateTime) of
        [First, _] -> gmt_to_timestamp(First);
        [Time] -> gmt_to_timestamp(Time);
        [] -> 0
    end.



%% @doc Equivalent to `proplists:get_value/2' but faster.
-spec get_value(term(), list()) -> 
    term().

get_value(Key, List) ->
    get_value(Key, List, undefined).


%% @doc Requivalent to `proplists:get_value/3' but faster.
-spec get_value(term(), list(), term()) -> 
    term().

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _ -> Default
    end.


%% @doc Similar to `get_value(Key, List, <<>>)' but converting the result into
%% a `binary()'.
-spec get_binary(term(), list()) -> 
    binary().

get_binary(Key, List) ->
    to_binary(get_value(Key, List, <<>>)).


%% @doc Similar to `get_value(Key, List, Default)' but converting the result into
%% a `binary()'.
-spec get_binary(term(), list(), term()) -> 
    binary().

get_binary(Key, List, Default) ->
    to_binary(get_value(Key, List, Default)).


%% @doc Similar to `get_value(Key, List, [])' but converting the result into a `list()'.
-spec get_list(term(), list()) -> 
    list().

get_list(Key, List) ->
    to_list(get_value(Key, List, [])).


%% @doc Similar to `get_value(Key, List, Default)' but converting the result
%% into a `list()'.
-spec get_list(term(), list(), term()) -> 
    list().

get_list(Key, List, Default) ->
    to_list(get_value(Key, List, Default)).


%% @doc Similar to `get_value(Key, List, 0)' but converting the result into 
%% an `integer()' or `error'.
-spec get_integer(term(), list()) -> 
    integer() | error. 

get_integer(Key, List) ->
    to_integer(get_value(Key, List, 0)).


%% @doc Similar to `get_value(Key, List, Default)' but converting the result into
%% a `integer()' or `error'.
-spec get_integer(term(), list(), term()) -> 
    integer() | error.

get_integer(Key, List, Default) ->
    to_integer(get_value(Key, List, Default)).



%% @doc Stores a value in a list
-spec store_value(term(), list()) ->
    list().
 
store_value(Term, List) ->
    case lists:member(Term, List) of
        true -> List;
        false -> [Term|List]
    end.

    
%% @doc Stores a value in a proplist
-spec store_value(term(), term(), nksip:optslist()) ->
    nksip:optslist().
 
store_value(Key, Val, List) ->
    lists:keystore(Key, 1, List, {Key, Val}).


%% @doc Converts anything into a `binary()'. Can convert ip addresses also.
-spec to_binary(term()) -> 
    binary().

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(undefined) -> <<>>;
to_binary(A) when is_atom(A) -> atom_to_binary(A, latin1);
to_binary(I) when is_integer(I) -> list_to_binary(erlang:integer_to_list(I));
to_binary(#uri{}=Uri) -> nksip_unparse:uri(Uri);
to_binary(#via{}=Via) -> nksip_unparse:via(Via);
to_binary(N) -> msg("~p", [N]).


%% @doc Converts anything into a `string()'.
-spec to_list(string()|binary()|atom()|integer()) -> 
    string().

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(I) when is_integer(I) -> erlang:integer_to_list(I).


%% @doc Converts anything into a `integer()' or `error'.
-spec to_integer(integer()|binary()|string()) ->
    integer() | error.

to_integer(I) when is_integer(I) -> 
    I;
to_integer(B) when is_binary(B) -> 
    to_integer(binary_to_list(B));
to_integer(L) when is_list(L) -> 
    case catch list_to_integer(L) of
        I when is_integer(I) -> I;
        _ -> error
    end;
to_integer(_) ->
    error.


%% @doc Converts a `list()' or `binary()' into a `inet:ip_address()' or `error'.
-spec to_ip(string() | binary()) ->
    {ok, inet:ip_address()} | error.

to_ip(Address) when is_binary(Address) ->
    to_ip(binary_to_list(Address));

% For IPv6
to_ip([$[ | Address1]) ->
    case lists:reverse(Address1) of
        [$] | Address2] -> to_ip(lists:reverse(Address2));
        _ -> error
    end;

to_ip(Address) when is_list(Address) ->
    case inet_parse:address(Address) of
        {ok, Ip} -> {ok, Ip};
        _ -> error
    end.


%% @doc Converts an IP or host to a binary host value
-spec to_host(inet:ip_address() | string() | binary()) ->
    binary().

to_host(IpOrHost) ->
    to_host(IpOrHost, false).


%% @doc Converts an IP or host to a binary host value. 
% If `IsUri' and it is an IPv6 address, it will be enclosed in `[' and `]'
-spec to_host(inet:ip_address() | string() | binary(), boolean()) ->
    binary().

to_host({A,B,C,D}=Address, _IsUri) 
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    list_to_binary(inet_parse:ntoa(Address));
to_host({A,B,C,D,E,F,G,H}=Address, IsUri) 
    when is_integer(A), is_integer(B), is_integer(C), is_integer(D),
    is_integer(E), is_integer(F), is_integer(G), is_integer(H) ->
    case IsUri of
        true -> list_to_binary([$[, inet_parse:ntoa(Address), $]]);
        false -> list_to_binary(inet_parse:ntoa(Address))
    end;
to_host(Host, _IsUri) ->
    to_binary(Host).


%% @doc converts a `string()' or `binary()' to a lower `binary()'.
-spec to_lower(string()|binary()|atom()) ->
    binary().

to_lower(List) when is_list(List) -> 
    list_to_binary(string:to_lower(List));
to_lower(Other) -> 
    to_lower(to_list(Other)).


%% @doc converts a `string()' or `binary()' to an upper `binary()'.
-spec to_upper(string()|binary()|atom()) ->
    binary().
    
to_upper(List) when is_list(List) -> 
    list_to_binary(string:to_upper(List));
to_upper(Other) -> 
    to_upper(to_list(Other)).


%% @doc URI Strips trailing white space
-spec strip(list()|binary()) ->
    list().

strip(Bin) when is_binary(Bin) -> strip(binary_to_list(Bin));
strip([32|Rest]) -> strip(Rest);
strip([13|Rest]) -> strip(Rest);
strip([10|Rest]) -> strip(Rest);
strip([9|Rest]) -> strip(Rest);
strip(Rest) -> Rest.


%% @doc Removes doble quotes
-spec unquote(list()|binary()) ->
    list().

unquote(Bin) when is_binary(Bin) -> 
    unquote(binary_to_list(Bin));

unquote(List) -> 
    case strip(List) of
        [$"|Rest] -> 
            case strip(lists:reverse(Rest)) of
                [$"|Rest1] -> strip(lists:reverse(Rest1));
                _ -> []
            end;
        Other -> 
            Other
    end.


%% @doc Generates a printable string from a big number using base 62.
-spec encode_integer(integer()) ->
    binary().

encode_integer(Int) ->
    list_to_binary(integer_to_list(Int, 62, [])).



%% @doc Generates a printable string from a big number using base 36
%% (only numbers and uppercase)
-spec encode_integer_36(integer()) ->
    binary().

encode_integer_36(Int) ->
    list_to_binary(integer_to_list(Int, 36, [])).



%% @private
-spec integer_to_list(integer(), integer(), string()) -> 
    string().

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if 
        D >= 36 -> [D-36+$a|R0];
        D >= 10 -> [D-10+$A|R0];
        true -> [D+$0|R0]
    end,
    if 
        I1 == 0 -> R1;
       true -> integer_to_list(I1, Base, R1)
    end.


%% @doc Extracts all elements in `Proplist' having key `KeyOrKeys' or having key in 
%% `KeyOrKeys' if `KeyOrKeys' is a list.
-spec extract([term()], term() | [term()]) ->
    [term()].

extract(PropList, KeyOrKeys) ->
    Fun = fun(Term) ->
        if
            is_tuple(Term), is_list(KeyOrKeys) -> 
                lists:member(element(1, Term), KeyOrKeys);
            is_tuple(Term) ->
                element(1, Term) == KeyOrKeys;
            is_list(KeyOrKeys) -> 
                lists:member(Term, KeyOrKeys);
            Term == KeyOrKeys ->
                true;
            true ->
                false
        end
    end,
    lists:filter(Fun, PropList).


%% @doc Deletes all elements in `Proplist' having key `KeyOrKeys' or having key in 
%% `KeyOrKeys' if `KeyOrKeys' is a list.
-spec delete([term()], term() | [term()]) ->
    [term()].

delete(PropList, KeyOrKeys) ->
    Fun = fun(Term) ->
        if
            is_tuple(Term), is_list(KeyOrKeys) -> 
                not lists:member(element(1, Term), KeyOrKeys);
            is_tuple(Term) ->
                element(1, Term) /= KeyOrKeys;
            is_list(KeyOrKeys) -> 
                not lists:member(Term, KeyOrKeys);
            Term /= KeyOrKeys ->
                true;
            true ->
                false
        end
    end,
    lists:filter(Fun, PropList).

%% @doc Inserts defaults in a proplist
-spec defaults([{term(), term()}], [{term(), term()}]) ->
    [{term(), term()}].

defaults(List, []) ->
    List;

defaults(List, [{Key, Val}|Rest]) ->
    case lists:keymember(Key, 1, List) of
        true -> defaults(List, Rest);
        false -> defaults([{Key, Val}|List], Rest)
    end.


%% @doc Checks if `Term' is a `string()' or `[]'.
-spec is_string(Term::term()) -> 
    boolean().

is_string([]) -> true;
is_string([F|R]) when is_integer(F) -> is_string(R);
is_string(_) -> false.


%% @doc Joins each element in `List' into a `binary()' using `<<",">>' as separator.
-spec bjoin(List::[term()]) ->
    binary().

bjoin(List) ->
    bjoin(List, <<",">>).

%% @doc Join each element in `List' into a `binary()', using the indicated `Separator'.
-spec bjoin(List::[term()], Separator::binary()) -> 
    binary().

bjoin([], _J) ->
    <<>>;
bjoin([Term], _J) ->
    to_binary(Term);
bjoin([First|Rest], J) ->
    bjoin2(Rest, to_binary(First), J).

bjoin2([[]|Rest], Acc, J) ->
    bjoin2(Rest, Acc, J);
bjoin2([Next|Rest], Acc, J) ->
    bjoin2(Rest, <<Acc/binary, J/binary, (to_binary(Next))/binary>>, J);
bjoin2([], Acc, _J) ->
    Acc.


%% @doc Splits a `string()' or `binary()' into a list of tokens
-spec tokens(string() | binary()) ->
    [string()] | error.

tokens(Bin) when is_binary(Bin) ->
    tokens(binary_to_list(Bin));

tokens(List) when is_list(List) ->
    tokens(List, [], []);

tokens(_) ->
    error.


tokens([], [], Tokens) ->
    lists:reverse(Tokens);

tokens([], Chs, Tokens) ->
    lists:reverse([lists:reverse(Chs)|Tokens]);

tokens([Ch|Rest], Chs, Tokens) when Ch==32; Ch==9; Ch==13; Ch==10 ->
    case Chs of
        [] -> tokens(Rest, [], Tokens);
        _ -> tokens(Rest, [], [lists:reverse(Chs)|Tokens])
    end;

tokens([Ch|Rest], Chs, Tokens) ->
    tokens(Rest, [Ch|Chs], Tokens).


%% @private
-spec hex(binary()|string()) -> 
    binary().

hex(B) when is_binary(B) -> hex(binary_to_list(B), []);
hex(S) -> hex(S, []).


%% @private
hex([], Res) -> list_to_binary(lists:reverse(Res));
hex([N|Ns], Acc) -> hex(Ns, [digit(N rem 16), digit(N div 16)|Acc]).


%% @private
digit(D) when (D >= 0) and (D < 10) -> D + 48;
digit(D) -> D + 87.


%% @doc Gets the subbinary after `Char'.
-spec bin_last(char(), binary()) ->
    binary().

bin_last(Char, Bin) ->
    case binary:match(Bin, <<Char>>) of
        {First, 1} -> binary:part(Bin, First+1, byte_size(Bin)-First-1);
        _ -> <<>>
    end.



%% @doc Cancels and existig timer.
-spec cancel_timer(reference()|undefined) ->
    false | integer().

cancel_timer(Ref) when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            receive {timeout, Ref, _} -> 0
            after 0 -> false 
            end;
        RemainingTime ->
            RemainingTime
    end;

cancel_timer(_) ->
    false.


%% @private
-spec msg(string(), [term()]) -> 
    binary().

msg(Msg, Vars) ->
    case catch list_to_binary(io_lib:format(Msg, Vars)) of
        {'EXIT', _} -> 
            lager:warning("MSG PARSE ERROR: ~p, ~p", [Msg, Vars]),
            <<"Msg parser error">>;
        Result -> 
            Result
    end.



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bjoin_test() ->
    ?assertMatch(<<"hi">>, bjoin([<<"hi">>], <<"any">>)),
    ?assertMatch(<<"hianyhi">>, bjoin([<<"hi">>, <<"hi">>], <<"any">>)),
    ?assertMatch(<<"hi1_hi2_hi3">>, bjoin([<<"hi1">>, <<"hi2">>, <<"hi3">>], <<"_">>)).

-endif.





