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

%% @private SIP Tokens Parser
-module(nksip_parse_tokens).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").
-export([tokens/1]).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Parse a serie of tokens in a string
-spec tokens(nksip:token() | binary() | string()) ->
    [Token] | error 
    when Token :: {binary(), [binary() | {binary(), binary()}]}.

tokens({Token, Opts}) when is_binary(Token), is_list(Opts) ->
    [{Token, Opts}];

tokens(Bin) when is_binary(Bin) ->
    tokens(binary_to_list(Bin));

tokens(String) when is_list(String) ->
    tokens(strip(String), []);

tokens(_) ->
    error.


%% ===================================================================
%% Private
%% ===================================================================

%% @private
tokens(String, Acc) ->
    case name(strip(String), []) of
        {error, _Type, _Line} -> 
            % lager:debug("Error parsing token ~s: ~p (~p)", [String, _Type, _Line]),
            error;
        {Name, Opts, []} when Acc==[]-> 
            [{Name, Opts}];
        {Name, Opts, []} -> 
            lists:reverse([{Name, Opts}|Acc]);
        {Name, Opts, Rest} -> 
            tokens(Rest, [{Name, Opts}|Acc])
    end.


%% @private Token name
name([], Acc) ->
    case Acc of
        [] -> {error, name, ?LINE};
        _ -> {list_to_binary(lists:reverse(Acc)), [], []}
    end;


name([Ch|_]=Rest, Acc) when Ch==$;; Ch==$, ->
    case Acc of
        [] ->
            {error, name, ?LINE};
        _ ->
            Name = list_to_binary(lists:reverse(Acc)), 
            opts(Rest, Name, [])
    end;

name([Ch|Rest], Acc) when Ch==32; Ch==9; Ch==13 ->
    case Acc of
        [] ->
            {error, name, ?LINE};
        _ ->
            Name = list_to_binary(lists:reverse(Acc)), 
            opts(strip(Rest), Name, [])
    end;

name([Ch|Rest], Acc) ->
    name(Rest, [Ch|Acc]).


%% @private Token opts
opts([], Name, Opts) -> 
    {Name, Opts, []};

opts([Ch|Rest], Name, Opts) ->
    case Ch of
        $; -> 
            opts_key(strip(Rest), [], Opts, Name);
        $, -> 
            case strip(Rest) of 
                [] -> {error, opts, ?LINE};
                Rest1 -> {Name, Opts, Rest1}
            end;
        _ when Ch==32; Ch==9; Ch==13 -> 
            opts(strip(Rest), Name, Opts);
        _ -> 
            {error, opts, ?LINE}
    end.


%% @private Token Opts Keys
opts_key([], Acc, Opts, Name) ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Opts1 = Opts ++ [Opt],
            opts([], Name, Opts1)
    end;

opts_key([Ch|_]=Rest, Acc, Opts, Name) when Ch==$;; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_key, ?LINE};
        _ ->
            Opt = list_to_binary(lists:reverse(Acc)),
            Opts1 = Opts ++ [Opt],
            opts(Rest, Name, Opts1)
    end;

opts_key([$=|Rest], Acc, Opts, Name) ->
    case Acc of
        [] -> 
            {error, opts_key, ?LINE};
        _ -> 
            Key = list_to_binary(lists:reverse(Acc)),
            opts_value(strip(Rest), Key, [], Opts, Name)
    end;

opts_key([Ch|_]=Rest, Acc, Opts, Name) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> opts_key([], Acc, Opts, Name);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$,; Ch1==$= -> opts_key(Rest1, Acc, Opts, Name);
        _ -> {error, opts_key, ?LINE}
    end;

opts_key([Ch|Rest], Acc, Opts, Name) ->
    opts_key(Rest, [Ch|Acc], Opts, Name).


%% @private Token Opts Values
opts_value([], Key, Acc, Opts, Name) ->
    case Acc of
        [] ->
            {error, opts_value, ?LINE};
        _ ->
            Opt = {Key, list_to_binary(lists:reverse(Acc))},
            Opts1 = Opts ++ [Opt],
            opts([], Name, Opts1)
    end;

opts_value([Ch|_]=Rest, Key, Acc, Opts, Name) when Ch==$;; Ch==$, ->
    case Acc of
        [] ->
            {error, opts_value, ?LINE};
        _ ->
            Opt = {Key, list_to_binary(lists:reverse(Acc))},
            Opts1 = Opts ++ [Opt],
            opts(Rest, Name, Opts1)
    end;

opts_value([Ch|_]=Rest, Key, Acc, Opts, Name) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> opts_value([], Key, Acc, Opts, Name);
        [Ch1|_]=Rest1 when Ch1==$;; Ch1==$, -> opts_value(Rest1, Key, Acc, Opts, Name);
        _ -> {error, opts_value, ?LINE}
    end;

opts_value([Ch|Rest], Key, Acc, Opts, Name) ->
    opts_value(Rest, Key, [Ch|Acc], Opts, Name).


%% @private VIA Strip white space
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

token_test() ->
    error = tokens("   aaa  , "),
    error = tokens("   aaa; "),
    error = tokens("   aaa;b= , c"),
    [{<<"aaa">>, []}] = tokens("aaa"),
    [{<<"aaa">>, []}] = tokens("   aaa   "),
    [
        {<<"aaa">>,[]},
        {<<"bbb">>,[]},
        {<<"ccc">>,[]},
        {<<"eee">>, [<<"c">>, <<"d">>, {<<"e">>,<<"1">>}, {<<"g">>,<<"2">>}]},
        {<<"fff">>,[{<<"h">>, <<"\"3\"">>}]}
    ] = 
        tokens("aaa ,  bbb,ccc,eee;c  ;  d  ; e = 1  ; g=2, fff;h=\"3\"").

-endif.

