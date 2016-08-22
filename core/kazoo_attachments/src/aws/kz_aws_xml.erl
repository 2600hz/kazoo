%% @author root
%% @doc @todo Add description to kz_aws_xml.
-module(kz_aws_xml).

-export([decode/2, decode/3
        ,get_bool/2
        ,get_float/2
        ,get_integer/2
        ,get_list/2
        ,get_text/1, get_text/2, get_text/3
        ,get_time/2
        ]).

-include("kz_aws.hrl").

-type xml() :: tuple().
-type xpath() :: {string(), string()}.
-type attribute() :: {atom(), string(), fun()}.

-spec decode([attribute()], xml()) -> [{atom(), any()}].
decode(Values, Node) ->
    lists:reverse(
      lists:foldl(fun ({Name, XPath, Type}, Output) ->
                          case get_value(XPath, Type, Node) of
                              'undefined' -> Output;
                              Value ->
                                  [{Name, Value}|Output]
                          end
                  end
                 ,[]
                 ,Values
                 )
     ).

-spec decode([attribute()], xml(), A) -> A.
decode(Values, Node, Record) ->
    lists:foldl(fun ({Index, XPath, Type}, Output) ->
                        case get_value(XPath, Type, Node) of
                            'undefined' -> Output;
                            Value -> setelement(Index, Output, Value)
                        end
                end
               ,Record
               ,Values
               ).

get_value(XPath, Type, Node) ->
    case Type of
        'text' -> get_text(XPath, Node);
        'optional_text' -> get_text(XPath, Node, 'undefined');
        'integer' -> get_integer(XPath, Node);
        'optional_integer' ->
            case get_text(XPath, Node, 'undefined') of
                'undefined' -> 'undefined';
                Text -> list_to_integer(Text)
            end;
        'float' -> get_float(XPath, Node);
        'time' -> get_time(XPath, Node);
        'list' -> get_list(XPath, Node);
        'boolean' -> get_bool(XPath, Node);
        'optional_boolean' ->
            case get_text(XPath, Node, 'undefined') of
                'undefined' -> 'undefined';
                "true" -> 'true';
                _ -> 'false'
            end;
        'present' -> xmerl_xpath:string(XPath, Node) =/= [];
        'xml' -> Node;
        Fun when is_function(Fun, 1) ->
            Fun(xmerl_xpath:string(XPath, Node));
        {'single', Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> 'undefined';
                [SubNode] -> Fun(SubNode)
            end;
        {'map', Fun} when is_function(Fun, 1) ->
            lists:map(Fun, xmerl_xpath:string(XPath, Node));
        {'optional_map', Fun} when is_function(Fun, 1) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> 'undefined';
                List  -> lists:map(Fun, List)
            end;
        {'single', List} when is_list(List) ->
            case xmerl_xpath:string(XPath, Node) of
                [] -> 'undefined';
                [SubNode] -> decode(List, SubNode)
            end;
        {'value', Fun} when is_function(Fun, 1) ->
            Fun(get_text(XPath, Node));
        List when is_list(List) ->
            [decode(List, SubNode) || SubNode <- xmerl_xpath:string(XPath, Node)]
    end.

-spec get_float(xpath(), xml()) -> float().
get_float(XPath, Node) ->
    list_to_float(get_text(XPath, Node)).

-spec get_text(xml()) -> string().
get_text(#xmlText{value=Value}) -> Value;
get_text(#xmlElement{content=Content}) ->
    lists:flatten([get_text(Node) || Node <- Content]).

-spec get_text(xpath(), xml()) -> string().
-spec get_text(xpath(), xml(), Default) -> string() | Default.
get_text(XPath, Doc) -> get_text(XPath, Doc, "").
get_text({XPath, AttrName}, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/@" ++ AttrName, Doc) of
        [] -> Default;
        [#xmlAttribute{value=Value}|_] -> Value
    end;
get_text(XPath, Doc, Default) ->
    case xmerl_xpath:string(XPath ++ "/text()", Doc) of
        [] -> Default;
        TextNodes ->
            lists:flatten([Node#xmlText.value || Node <- TextNodes])
    end.

-spec get_list(xpath(), xml()) -> [string()].
get_list(XPath, Doc) ->
    [get_text(Node) || Node <- xmerl_xpath:string(XPath, Doc)].

-spec get_integer(xpath(), xml()) -> integer().
-spec get_integer(xpath(), xml(), Default) -> integer() | Default.
get_integer(XPath, Doc) -> get_integer(XPath, Doc, 0).
get_integer(XPath, Doc, Default) ->
    case get_text(XPath, Doc) of
        "" -> Default;
        Text -> list_to_integer(Text)
    end.

-spec get_bool(xpath(), xml()) -> boolean().
get_bool(XPath, Doc) ->
    get_text(XPath, Doc, "false") =:= "true".

-spec get_time(xpath(), xml()) -> calendar:datetime() |
                                  'undefined' |
                                  'error'.
get_time(XPath, Doc) ->
    case get_text(XPath, Doc, 'undefined') of
        'undefined' -> 'undefined';
        Time -> parse_time(Time)
    end.

parse_time(String) ->
    case re:run(String
               ,"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})(?:\\.\\d+)?Z"
               ,[{'capture', 'all_but_first', 'list'}]
               )
    of
        {'match', [Yr, Mo, Da, H, M, S]} ->
            {{list_to_integer(Yr), list_to_integer(Mo), list_to_integer(Da)},
             {list_to_integer(H), list_to_integer(M), list_to_integer(S)}};
        'nomatch' ->
            'error'
    end.
