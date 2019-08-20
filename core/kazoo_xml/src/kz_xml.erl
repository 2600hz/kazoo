%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_xml).

-export([elements/1, elements/2
        ,texts_to_binary/1, texts_to_binary/2
        ,attributes_to_proplist/1
        ,filter_empty_text/1

        ,get_value/2
        ,extract_values/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec elements(list()) -> kz_types:xml_els().
elements(Els) -> [El || #xmlElement{}=El <- Els].

-spec elements(list(), atom()) -> kz_types:xml_els().
elements(Els, Name) -> [El || #xmlElement{name=N}=El <- Els, N =:= Name].

-spec texts_to_binary(kz_types:xml_texts()) -> binary().
texts_to_binary([]) -> <<>>;
texts_to_binary([_|_]=Vs) ->
    lists:foldl(fun(C, B) ->
                        kz_binary:strip(B, C)
                end
               ,unicode:characters_to_binary([V || #xmlText{value=V, type='text'} <- Vs])
               ,[$\n, $\s, $\n, $\s]
               ).

-spec texts_to_binary(kz_types:xml_texts(), pos_integer()) -> binary().
texts_to_binary(Vs, Size) when is_list(Vs), is_integer(Size), Size > 0 ->
    B = texts_to_binary(Vs),
    case byte_size(B) > Size of
        'true' -> erlang:binary_part(B, 0, Size);
        'false' -> B
    end.

-spec attributes_to_proplist(kz_types:xml_attribs()) -> kz_term:proplist().
attributes_to_proplist(L) ->
    [{K, V} || #xmlAttribute{name=K, value=V} <- L].

-spec filter_empty_text(kz_types:xml_els() | kz_types:xml_texts()) -> kz_types:xml_els() | kz_types:xml_texts().
filter_empty_text([_|_]=Els) ->
    [El || El <- Els,
           not is_empty_text(El)
    ].

-spec is_empty_text(kz_types:xml_text() | kz_types:xml_el()) -> boolean().
is_empty_text(#xmlText{value=" "}) -> 'true';
is_empty_text(_El) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Generic helper to get the text value of a XML path
%% @end
%%------------------------------------------------------------------------------
-spec get_value(kz_term:deeplist(), kz_types:xml_el() | string()) -> kz_term:api_binary().
get_value(Paths, Xml) ->
    Path = lists:flatten(Paths),
    try xmerl_xpath:string(Path, Xml) of
        Elements when is_list(Elements) -> extract_values(Elements);
        _Else -> 'undefined'
    catch
        _E:_R ->
            lager:debug("~s getting value of '~s': ~p", [_E, Path, _R]),
            'undefined'
    end.


-spec extract_values(kz_types:xml_els()) -> kz_term:api_binary().
extract_values([]) -> 'undefined';
extract_values(Elements) ->
    Values = [case Element of
                  #xmlText{value = Value} -> Value;
                  #xmlAttribute{value = Value} -> Value;
                  _ -> <<>> %% Important as xmerl only handles strings
              end
              || Element <- Elements
             ],
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
