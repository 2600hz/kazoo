%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_xml).

-export([elements/1
        ,get_elem/2
        ,get_attr/2 ,get_attrs/1
        ,get_attrval/1, get_attrval/2
        ,get_content/1, get_content/2
        ,get_textval/1, get_textval/2
        ,texts_to_binary/1, texts_to_binary/2
        ,attributes_to_proplist/1
        ,filter_empty_text/1

        ,get_value/2
        ,extract_values/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%%------------------------------------------------------------------------------
%% @doc Get all `#xmlElement{}' from list `Els'.
%% @end
%%------------------------------------------------------------------------------
-spec elements(list()) -> kz_types:xml_els().
elements(Els) -> [El || #xmlElement{}=El <- Els].

%%------------------------------------------------------------------------------
%% @doc Get `#xmlElement{}' by name from a list of elements.
%% @end
%%------------------------------------------------------------------------------
-spec get_elem(atom(), kz_types:xml_els()) -> kz_types:xml_els().
get_elem(Name, Els) ->
    [El || #xmlElement{name = N} = El <- Els, N =:= Name].

%%------------------------------------------------------------------------------
%% @doc Extract element's attributes from `#xmlElement{}' record.
%% @end
%%------------------------------------------------------------------------------
-spec get_attrs(kz_types:xml_el()) -> kz_types:xml_attribs().
get_attrs(#xmlElement{attributes = Attribs}) ->
    Attribs.

%%------------------------------------------------------------------------------
%% @doc Get `#xmlAttribute{}' by name from a list of attributes or element's
%% list of attributes.
%% @end
%%------------------------------------------------------------------------------
-spec get_attr(atom(), kz_types:xml_attribs() | kz_types:xml_el()) -> kz_types:xml_attribs().
get_attr(_, []) ->
    [];
get_attr(Name, [#xmlAttribute{}|_]=Attributes) ->
    [Atr || #xmlAttribute{name = N} = Atr <- Attributes, N =:= Name];
get_attr(Name, #xmlElement{attributes = Attribs}) ->
    get_attr(Name, Attribs).

%%------------------------------------------------------------------------------
%% @doc Extract attribute value from `#xmlAttribute{}' record.
%% @end
%%------------------------------------------------------------------------------
-spec get_attrval(kz_types:xml_attrib()) -> kz_types:xml_attrib_value().
get_attrval(#xmlAttribute{value = Value}) ->
    Value.

%%------------------------------------------------------------------------------
%% @doc Get value of the attribute with name `Name' from element's attributes.
%% @end
%%------------------------------------------------------------------------------
-spec get_attrval(atom(), kz_types:xml_el()) -> kz_types:xml_attrib_value().
get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
        [#xmlAttribute{value = Value}] -> Value;
        [] -> ""
    end.

%%------------------------------------------------------------------------------
%% @doc Extract content of element from `#xmlElement{}' record.
%% @end
%%------------------------------------------------------------------------------
-spec get_content(kz_types:xml_el()) -> kz_types:xml_els().
get_content(#xmlElement{content = Els}) ->
    Els.

%%------------------------------------------------------------------------------
%% @doc Get first element by name from list and return its content.
%% @end
%%------------------------------------------------------------------------------
-spec get_content(atom(), kz_types:xml_els()) -> [#xmlElement{}| #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlDecl{} | any()].
get_content(Name, Els) ->
    case get_elem(Name, Els) of
        [#xmlElement{content = Els1} | _] ->
            Els1;
        [] -> []
    end.

%%------------------------------------------------------------------------------
%% @doc Extract value of `#xmlText{}'.
%% @end
%%------------------------------------------------------------------------------
-spec get_textval(kz_types:xml_text()) -> string() | iolist().
get_textval(#xmlText{value = Text}) ->
    Text.

%%------------------------------------------------------------------------------
%% @doc Get element by name from list and extract its first `#xmlText{}' value.
%% @end
%%------------------------------------------------------------------------------
-spec get_textval(atom(), kz_types:xml_els()) -> string() | iolist().
get_textval(Name, Els) ->
    case get_content(Name, Els) of
        [#xmlText{value = Text} | _] ->
            Text;
        [] -> ""
    end.

-spec texts_to_binary(kz_types:xml_texts()) -> binary().
texts_to_binary([]) -> <<>>;
texts_to_binary([_|_]=Vs) ->
    lists:foldl(fun(C, B) ->
                        kz_binary:strip(B, C)
                end
               ,iolist_to_binary([kz_term:to_binary(V) || #xmlText{value=V, type='text'} <- Vs])
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
