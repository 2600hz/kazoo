%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
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

-export([value/2]).

-include_lib("kazoo/include/kz_types.hrl").

-spec elements(list()) -> xml_els().
-spec elements(list(), atom()) -> xml_els().
elements(Els) -> [El || #xmlElement{}=El <- Els].
elements(Els, Name) -> [El || #xmlElement{name=N}=El <- Els, N =:= Name].

-spec texts_to_binary(xml_texts()) -> binary().
-spec texts_to_binary(xml_texts(), pos_integer()) -> binary().
texts_to_binary([]) -> <<>>;
texts_to_binary([_|_]=Vs) ->
    lists:foldl(fun(C, B) ->
                        kz_term:strip_binary(B, C)
                end
                ,iolist_to_binary([kz_term:to_binary(V) || #xmlText{value=V, type='text'} <- Vs])
                ,[$\n, $\s, $\n, $\s]
               ).

texts_to_binary(Vs, Size) when is_list(Vs), is_integer(Size), Size > 0 ->
    B = texts_to_binary(Vs),
    case byte_size(B) > Size of
        'true' -> erlang:binary_part(B, 0, Size);
        'false' -> B
    end.

-spec attributes_to_proplist(xml_attribs()) -> kz_proplist().
attributes_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generic helper to get the text value of a XML path
%% @end
%%--------------------------------------------------------------------
-spec value(kz_deeplist(), xml_el() | string()) -> api_binary().
value(Paths, Xml) ->
    Path = lists:flatten(Paths),
    try xmerl_xpath:string(Path, Xml) of
        Elements when is_list(Elements) -> extract_xml_values(Elements);
        _Else -> 'undefined'
    catch
        _E:_R ->
            lager:debug("~s getting value of '~s': ~p", [_E, Path, _R]),
            'undefined'
    end.

%% @private
-spec extract_xml_values(xml_els()) -> api_binary().
extract_xml_values([]) -> 'undefined';
extract_xml_values(Elements) ->
    Values = [case Element of
                  #xmlText{value = Value} -> Value;
                  #xmlAttribute{value = Value} -> Value;
                  _ -> <<>> %% Important as xmerl only handles strings
              end
              || Element <- Elements],
    case iolist_to_binary(Values) of
        <<>> ->
            %% Note: here we make sure that Values were all either xmlText
            %%  or xmlAttribute. Thus, if Values is a list of only empty binaries
            %%  it means that no value field was extracted.
            %% On the flip side, a "" present in Values means
            %%  that at least one extracted value was ""
            %%  and we should return <<>> instead of 'undefined'.
            IsEmptyBinary = fun (<<>>) -> 'true'; (_) -> 'false' end,
            case lists:all(IsEmptyBinary, Values) of
                'true' -> 'undefined';
                'false' -> <<>>
            end;
        Bin -> Bin
    end.
