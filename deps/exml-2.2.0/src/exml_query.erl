%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml.hrl").

-export([path/2, path/3]).
-export([paths/2]).
-export([subelement/2, subelement/3]).
-export([subelements/2]).
-export([attr/2, attr/3]).
-export([cdata/1]).

-type path() :: [cdata | {element, binary()} | {attr, binary()}].
-export_type([path/0]).

%% @doc gets the element/attr/cdata contained in the leftmost path
-spec path(exml:element(), path()) -> exml:element() | binary() | undefined.
path(Element, Path) ->
    path(Element, Path, undefined).

%% @doc gets the element/attr/cdata in the leftmost possible described path
-spec path(exml:element(), path(), Other) -> exml:element() | binary() | Other.
path(#xmlel{} = Element, [], _) ->
    Element;
path(#xmlel{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name), % may return undefined
    path(Child, Rest, Default);
path(#xmlel{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlel{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

%% @doc gets the elements/attrs/cdatas reachable by the described path
-spec paths(exml:element(), path()) -> [exml:element() | binary()].
paths(#xmlel{} = Element, []) ->
    [Element];
paths(#xmlel{} = Element, [{element, Name} | Rest]) ->
    Children = subelements(Element, Name),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [cdata]) ->
    [cdata(Element)];
paths(#xmlel{attrs = Attrs}, [{attr, Name}]) ->
    lists:sublist([V || {N, V} <- Attrs, N =:= Name], 1);
paths(#xmlel{} = El, Path) when is_list(Path) ->
    erlang:error(invalid_path, [El, Path]).

-spec subelement(exml:element(), binary()) -> exml:element() | undefined.
subelement(Element, Name) ->
    subelement(Element, Name, undefined).

-spec subelement(exml:element(), binary(), Other) -> exml:element() | Other.
subelement(#xmlel{children = Children}, Name, Default) ->
    case lists:keyfind(Name, #xmlel.name, Children) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec subelements(exml:element(), binary()) -> [exml:element()].
subelements(#xmlel{children = Children}, Name) ->
    lists:filter(fun(#xmlel{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end, Children).

-spec cdata(exml:element()) -> binary().
cdata(#xmlel{children = Children}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{} = C <- Children]).

-spec attr(exml:element(), binary()) -> binary() | undefined.
attr(Element, Name) ->
    attr(Element, Name, undefined).

-spec attr(exml:element(), binary(), Other) -> binary() | Other.
attr(#xmlel{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.
