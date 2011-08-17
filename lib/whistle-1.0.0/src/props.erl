%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Replaces proplists get_value/2 and get_value/3 with a faster version
%%% using lists:keyfind
%%% @end
%%% Created :  6 Oct 2010 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(props).

-export([get_value/2, get_value/3, delete/2, is_defined/2]).

-include_lib("whistle/include/wh_types.hrl").

-spec get_value/2 :: (Key, Prop) -> term() when
      Key :: binary() | atom(),
      Prop :: proplist().
-spec get_value/3 :: (Key, Prop, Default) -> term() when
      Key :: binary() | atom(),
      Prop :: proplist(),
      Default :: term().
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

get_value(_Key, [], Def) -> Def;
get_value(Key, {struct, Prop}, Def) ->
    get_value(Key, Prop, Def);
get_value(Key, Prop, Default) ->
    case lists:keyfind(Key, 1, Prop) of
	false ->
	    case lists:member(Key, Prop) of
		true -> true;
		false -> Default
	    end;
	{Key, V} -> % only return V if a two-tuple is found
	    V;
	Other when is_tuple(Other) -> % otherwise return the default
	    Default
    end.

-spec delete/2 :: (Key, Prop) -> proplist() when
      Key :: binary() | atom(),
      Prop :: proplist().
delete(K, Prop) ->
    lists:keydelete(K, 1, Prop).

-spec is_defined/2 :: (Key, Prop) -> boolean() when
      Key :: term(),
      Prop :: proplist().
is_defined(Key, Prop) ->
    case lists:keyfind(Key, 1, Prop) of
	{Key,_} -> true;
	_ -> false
    end.
