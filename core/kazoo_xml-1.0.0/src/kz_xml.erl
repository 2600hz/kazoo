%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_xml).

-export([elements/1, elements/2
         ,texts_to_binary/1, texts_to_binary/2
         ,attributes_to_proplist/1
        ]).

-include_lib("whistle/include/wh_types.hrl").

-spec elements(list()) -> xml_els().
-spec elements(list(), atom()) -> xml_els().
elements(Els) -> [El || #xmlElement{}=El <- Els].
elements(Els, Name) -> [El || #xmlElement{name=N}=El <- Els, N =:= Name].

-spec texts_to_binary(list()) -> binary().
-spec texts_to_binary(list(), pos_integer()) -> binary().
texts_to_binary([]) -> <<>>;
texts_to_binary([_|_]=Vs) ->
    lists:foldl(fun(C, B) ->
                        wh_util:strip_binary(B, C)
                end
                ,iolist_to_binary([wh_util:to_binary(V) || #xmlText{value=V, type='text'} <- Vs])
                ,[$\n, $\s, $\n, $\s]
               ).

texts_to_binary(Vs, Size) when is_list(Vs), is_integer(Size), Size > 0 ->
    B = texts_to_binary(Vs),
    case byte_size(B) > Size of
        'true' -> erlang:binary_part(B, 0, Size);
        'false' -> B
    end.

-spec attributes_to_proplist(xml_attribs()) -> wh_proplist().
attributes_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].
