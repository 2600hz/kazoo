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

%% @doc General SIP header processing functions.
-module(nksip_headers).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([header/2, new/1, update/2]).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Generates a single header. If `Value' is a list, all values will be joined
%% using comma as separator.
-spec header(Name::binary(), Value::nksip:header_value()|[nksip:header_value()]) -> 
    nksip:header().

header(Name, #uri{}=Uri) ->
    {Name, nksip_unparse:uri(Uri)};
header(Name, {Token, Opts}) when is_list(Opts)->
    {Name, nksip_unparse:token({Token, Opts})};
header(Name, [{_Token, Opts}|_]=List) when is_list(Opts) ->
    {Name, nksip_unparse:token(List)};
header(Name, Binary) when is_binary(Binary) ->
    {Name, Binary};
header(Name, Integer) when is_integer(Integer) ->
    {Name, nksip_lib:to_binary(Integer)};
header(Name, [F|_]=List) when is_integer(F) ->
    {Name, nksip_lib:to_binary(List)};
header(Name, List) when is_list(List) ->
    Values = [
        case Term of
            #uri{} -> nksip_unparse:uri(Term);
            _ -> nksip_lib:to_binary(Term)
        end
        || Term <- List
    ], 
    {Name, nksip_lib:bjoin(Values)};
header(Name, Raw) ->
    {Name, nksip_lib:to_binary(Raw)}.


%% @doc Generates a header list from a list of specifications.
%% See {@link update/2} for options description
-spec new([{Operation, Name, Value} | none]) -> [nksip:header()] 
    when Operation :: single|multi|after_single|before_single|after_multi|
                      before_multi|default_single|default_multi,
         Name :: binary(), Value :: nksip:header_value() | [nksip:header_value()].

new(Operations) ->
    update([], Operations).


%% @doc Performs a serie of modifications to a headers list (or the header list of
%% a request or response). 
%% Each modification is applied serially, and has the from `{Op, Name, Value}'.
%% `Value' can be a single value or a list of values, affecting the behaviour of
%% `single' and `multi' version of the operations (see {@link header/2}).
%% Valid values for `Op' are:
%% <ul>
%%  <li>`single': Deletes old `Name' headers and adds a single new one. 
%%      If `Value' is `[]' headers will be deleted.</li> 
%%  <li>`multi': Deletes old `Name' headers and adds one or more new ones.
%%      If `Value' is `[]' headers will be deleted.</li> 
%%  <li>`add_after_single': Combine previous headers into a single new one appending
%%       value or values in `Value' after old values.</li>
%%  <li>`add_before_single': Combine previous headers into a single new one appending
%%       value or values in `Value' before old values.</li>
%%  <li>`add_after_multi': Generates multiple headers, one for each value or values in
%%       `Value', after any existing header/s.</li>
%%  <li>`add_before_multi': Generates multiple headers, one for each value or values in 
%%       `Value', before any existing header/s.</li>
%%  <li>`default_single': adds a single header only if it was not already present
%%       and `Value' is not `[]'</li>
%%  <li>`default_multi': adds multiple headers only if it was not already present
%%       and `Value' is not `[]'</li>
%% </ul> 
%%
-spec update(Input, [{Operation, Name, Value} | list() | none]) -> 
    [nksip:header()] 
    when Input :: nksip:request() | nksip:response() | [nksip:header()],
         Operation :: single|multi|after_single|before_single|after_multi|
                      before_multi|default_single|default_multi,
         Name :: binary(), Value :: nksip:header_value() | [nksip:header_value()].

update(#sipmsg{headers=Headers}, Operations) ->
    update(Headers, Operations);

update(Headers, []) ->
    Headers;

update(Headers, [{default_single, Name, ValueOrValues}|R]) ->
    case lists:keymember(Name, 1, Headers) of
        false when ValueOrValues==[]-> update(Headers, R);
        false -> update([header(Name, ValueOrValues)|Headers], R);
        true  -> update(Headers, R)
    end;

update(Headers, [{default_multi, Name, ValueOrValues}|R]) ->
    case lists:keymember(Name, 1, Headers) of
        false ->
            case ValueOrValues of
                [] -> 
                    update(Headers, R);
                [I|_] when is_integer(I) -> 
                    update([header(Name, list_to_binary(ValueOrValues))|Headers], R);
                [_|_] ->    
                    update([header(Name, V) || V <- ValueOrValues] ++ Headers, R);
                _ ->
                    update([header(Name, ValueOrValues)|Headers], R)
            end;
        true -> 
            update(Headers, R)
    end;

update(Headers, [{single, Name, ValueOrValues}|R]) ->
    Headers1 = nksip_lib:delete(Headers, Name),
    case ValueOrValues of
        [] -> update(Headers1, R);
        _ -> update([header(Name, ValueOrValues)|Headers1], R)
    end;

update(Headers, [{multi, Name, ValueOrValues}|R]) ->
    Headers1 = nksip_lib:delete(Headers, Name),
    case ValueOrValues of
        [] -> 
            update(Headers1, R);
        [I|_] when is_integer(I) ->
            update([header(Name, list_to_binary(ValueOrValues))|Headers1], R);
        [_|_] ->
            update([header(Name, V) || V <- ValueOrValues] ++ Headers1, R);
        _ -> 
            update([header(Name, ValueOrValues)|Headers1], R)
    end;

update(Headers, [{_, _, []}|R]) ->
    update(Headers, R);

update(Headers, [{after_single, Name, ValueOrValues}|R]) ->
    {OldValues, Headers1} = extract(Name, Headers),
    Values1 = case is_list(ValueOrValues) of
        true -> OldValues ++ ValueOrValues;
        false -> OldValues ++ [ValueOrValues]
    end,
    update([header(Name, Values1) | Headers1], R);

update(Headers, [{after_multi, Name, ValueOrValues}|R]) ->
    {OldValues, Headers1} = extract(Name, Headers),
    % If OldValues is like "a,b,c" this will not be splitted in multiple headers
    Values1 = case is_list(ValueOrValues) of
        true -> OldValues ++ ValueOrValues;
        false -> OldValues ++ [ValueOrValues]
    end,
    update([header(Name, V) || V <- Values1] ++ Headers1, R);

update(Headers, [{before_single, Name, ValueOrValues}|R]) ->
    {OldValues, Headers1} = extract(Name, Headers),
    Values1 = case is_list(ValueOrValues) of
        true -> ValueOrValues ++ OldValues;
        false -> [ValueOrValues | OldValues]
    end,
    update([header(Name, Values1) | Headers1], R);

update(Headers, [{before_multi, Name, ValueOrValues}|R]) ->
    {OldValues, Headers1} = extract(Name, Headers),
    % If OldValues is like "a,b,c" this will not be splitted in multiple headers
    Values1 = case is_list(ValueOrValues) of
        true -> ValueOrValues ++ OldValues;
        false -> [ValueOrValues | OldValues]
    end,
    update([header(Name, V) || V <- Values1] ++ Headers1, R);

update(Headers, [List|R]) when is_list(List) ->
    Headers1 = update(Headers, List),
    update(Headers1, R);

update(Headers, [_|R]) ->
    update(Headers, R).


%% ===================================================================
%% Private
%% ===================================================================

%% @private Extract all the properties named `Name' from `List', converting them to 
%% binary. Returns the extracted values and the remaining list
-spec extract(term(), list()) -> 
    {[binary()], [term()]}.

extract(Name, List) ->
    extract(Name, List, [], []).

extract(Name, [{Name, Value}|Rest], Acc1, Acc2) when is_binary(Value)->
    extract(Name, Rest, [Value|Acc1], Acc2);

extract(Name, [{Name, Value}|Rest], Acc1, Acc2) ->
    extract(Name, Rest, [nksip_lib:to_binary(Value)|Acc1], Acc2);

extract(Name, [Term|Rest], Acc1, Acc2) ->
    extract(Name, Rest, Acc1, [Term|Acc2]);

extract(_Name, [], Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)}.



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

headers_test() ->

    H1 = [{<<"a">>,<<"a1,a2">>}],
    ?assertMatch(H1, 
        new([
            {default_single, <<"a">>, []},
            {default_single, <<"a">>, [a1, "a2"]}
        ])),
    ?assertMatch(H1, 
        update(H1, [{default_single, <<"a">>, otro}])),

    H2 = [
        {<<"d">>,<<"d1">>},
        {<<"c">>,<<"c1">>},
        {<<"c">>,<<"c2">>},
        {<<"b">>,<<"b1">>},
        {<<"a">>,<<"a1,a2">>}],
    ?assertMatch(H2, 
        update(H1, [
            {default_multi, <<"b">>, []},
            {default_multi, <<"b">>, "b1"},
            {default_multi, <<"c">>, ["c1", c2]},
            {default_multi, <<"d">>, <<"d1">>}
        ])),
    ?assertMatch(H2, 
        update(H2, [{default_multi, <<"b">>, otro}])),

    H3 = [
        {<<"a">>,<<"a3,a4">>},
        {<<"b">>,<<"b2">>},
        {<<"e">>,<<"e1,e2">>},
        {<<"d">>,<<"d1">>}],
    ?assertMatch(H3, 
        update(H2, [
            {single, <<"e">>, [e1, e2]},
            {single, <<"c">>, []},
            {single, <<"b">>, <<"b2">>},
            {single, <<"a">>, [a3, "a4"]}
        ])),

    H4 = [
        {<<"d">>,<<"d2">>},
        {<<"c">>,<<"c3">>},
        {<<"c">>,<<"c4">>},
        {<<"b">>,<<"b3">>},
        {<<"e">>,<<"e1,e2">>}],
    ?assertMatch(H4, 
        update(H3, [
            {multi, <<"a">>, []},
            {multi, <<"b">>, "b3"},
            {multi, <<"c">>, ["c3", <<"c4">>]},
            {multi, <<"d">>, d2}
        ])),

    H5 = [
        {<<"f">>,<<"f1">>},
        {<<"e">>,<<"e1,e2,e3,e4">>},
        {<<"c">>,<<"c3,c4,c5">>},
        {<<"d">>,<<"d2">>},
        {<<"b">>,<<"b3">>}],
    ?assertMatch(H5, 
        update(H4, [
            {after_single, <<"c">>, c5},
            {after_single, <<"e">>, [e3, e4]},
            {after_single, <<"f">>, f1}
        ])),
        
    H6 = [
        {<<"d">>,<<"d2">>},
        {<<"d">>,<<"d3">>},
        {<<"c">>,<<"c3,c4,c5">>},
        {<<"c">>,<<"c6">>}],
    ?assertMatch(H6, 
        update(H5, [
            {single, <<"f">>, []},
            {multi, <<"e">>, []},
            {single, <<"b">>, []},
            {after_multi, <<"c">>, c6},
            {after_multi, <<"d">>, [d3]}
        ])),

    H7 = [
        {<<"c">>,<<"c7">>},
        {<<"c">>,<<"c3,c4,c5">>},
        {<<"c">>,<<"c6">>},
        {<<"d">>,<<"d4">>},
        {<<"d">>,<<"d2">>},
        {<<"d">>,<<"d3">>},
        {<<"b">>,<<"b4">>},
        {<<"b">>,<<"b5">>},
        {<<"a">>,<<"a5">>}],
    ?assertMatch(H7, 
            update(H6, [
                {before_multi, <<"a">>, a5},
                {before_multi, <<"b">>, [b4, b5]},
                {before_multi, <<"d">>, d4},
                {before_multi, <<"c">>, c7}
            ])),

    H8= [
        {<<"a">>,<<"a6,a7,a5">>},
        {<<"b">>,<<"b6,b4,b5">>},
        {<<"c">>,<<"c8,c7,c3,c4,c5,c6">>}],
    ?assertMatch(H8, 
        update(H7, [
            {before_single, <<"c">>, c8},
            {single, <<"d">>, []},
            {before_single, <<"b">>, b6},
            {before_single, <<"a">>, [a6, a7]}
        ])).


-endif.

