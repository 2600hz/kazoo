%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Replaces proplists get_value/2 and get_value/3 with a faster version
%%% using lists:keyfind
%%% @end
%%% Created :  6 Oct 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(props).

-export([get_value/2, get_value/3, delete/2, is_defined/2]).
-export([get_integer_value/2, get_integer_value/3]).
-export([get_keys/1]).
-export([unique/1]).

-include_lib("whistle/include/wh_types.hrl").

-spec get_value/2 :: (ne_binary() | atom(), wh_proplist()) -> term().
-spec get_value/3 :: (ne_binary() | atom(), wh_proplist(), Default) -> Default | term().
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

get_value(_Key, [], Def) -> Def;
get_value(Key, Prop, Default) when is_list(Prop) ->
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

-spec get_integer_value/2 :: (ne_binary() | atom(), wh_proplist()) -> integer() | 'undefined'.
-spec get_integer_value/3 :: (ne_binary() | atom(), wh_proplist(), Default) -> integer() | Default.
get_integer_value(Key, Prop) ->
    get_integer_value(Key, Prop, undefined).
get_integer_value(Key, Prop, Default) ->
    case ?MODULE:get_value(Key, Prop, Default) of
        Default -> Default;
        Val -> wh_util:to_integer(Val)
    end.

-spec get_keys/1 :: (wh_proplist()) -> [term(),...] | [].
get_keys(Prop) ->
    [ K || {K,_} <- Prop].

-spec delete/2 :: (ne_binary() | atom(), wh_proplist()) -> wh_proplist().
delete(K, Prop) ->
    lists:keydelete(K, 1, Prop).

-spec is_defined/2 :: (ne_binary() | atom(), wh_proplist()) -> boolean().
is_defined(Key, Prop) ->
    case lists:keyfind(Key, 1, Prop) of
        {Key,_} -> true;
        _ -> false
    end.

-spec unique/1 :: (wh_proplist()) -> wh_proplist().
unique(List) ->
    unique(List, []).

-spec unique/2 :: (wh_proplist(), wh_proplist()) -> wh_proplist().
unique([], Uniques) ->
    lists:reverse(Uniques);
unique([{Key, _}=H|T], Uniques) ->
    unique(lists:filter(fun({K, _}) when K =:= Key -> false; (_) -> true end, T), [H|Uniques]).
