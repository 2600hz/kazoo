%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(lineman_util).

-export([add_dynamic_var/2]).
-export([replace_dynamic_vars/1]).
-export([try_connect_to_target/2, try_connect_to_target/3]).
-export([try_get_cookie_from_vmargs/1]).
-export([xml_content/1, xml_content/2]).
-export([xml_attribute/2, xml_attribute/3]).
-export([xml_string_attribute/2, xml_string_attribute/3]).
-export([xml_atom_attribute/2, xml_atom_attribute/3]).
-export([xml_integer_attribute/2, xml_integer_attribute/3]).
-export([xml_boolean_attribute/2, xml_boolean_attribute/3]).
-export([xml_value/2, xml_value/3]).
-export([xml_binary_value/2, xml_binary_value/3]).
-export([xml_string_value/2, xml_string_value/3]).
-export([xml_atom_value/2, xml_atom_value/3]).
-export([xml_integer_value/2, xml_integer_value/3]).
-export([xml_boolean_value/2, xml_boolean_value/3]).

-include("lineman.hrl").

-type xml_return_types() :: 'undefined' | 'binary' | 'string' | 'atom' | 'integer' | 'boolean'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_dynamic_var/2 :: (term(), term()) -> term().
add_dynamic_var(Name, Value) ->
    lager:debug("added dynamic var '~s': ~s", [Name, Value]),
    case get(dynamic_vars) of
        undefined -> 
            put(dynamic_vars, [{Name, Value}]);
        Vars -> 
            put(dynamic_vars, [{Name, Value} | proplists:delete(Name, Vars)])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replace_dynamic_vars/1 :: (binary()) -> binary().
replace_dynamic_vars(Subject) when not is_binary(Subject) ->
    replace_dynamic_vars(wh_util:to_binary(Subject));
replace_dynamic_vars(Subject) ->
    case get(dynamic_vars) of
        undefined -> Subject;
        Vars ->
            lists:foldl(fun({K, V}, S) ->
                                binary:replace(S, <<"{{", (wh_util:to_binary(K))/binary, "}}">>, V, [global])
                        end, Subject, Vars)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to connect to another erlang node
%% @end
%%--------------------------------------------------------------------
-spec try_connect_to_target/2 :: (text(), text()) -> {'ok', atom()} | 
                                                     {'error', term()}.
-spec try_connect_to_target/3 :: (text(), text(), text()) -> {'ok', atom()} | 
                                                             {'error', term()}.

try_connect_to_target(Node, Cookie) ->
    try_connect_to_target(Node, Cookie, net_adm:localhost()).

try_connect_to_target(Node, Cookie, Host) when not is_list(Node) ->
    try_connect_to_target(wh_util:to_list(Node), Cookie, Host);
try_connect_to_target(Node, Cookie, Host) when not is_atom(Cookie) ->
    try_connect_to_target(Node, wh_util:to_atom(Cookie, true), Host);
try_connect_to_target(Node, Cookie, Host) when not is_list(Host) ->
    try_connect_to_target(Node, Cookie, wh_util:to_list(Host));
try_connect_to_target(Node, Cookie, Host) ->
    case inet:gethostbyname(Host) of
        {error, nxdomain} ->
            {error, host_unresolvable};
        {error, _Reason} ->
            lager:debug("unable to resolve host '~s': ~p", [Host, _Reason]),
            {error, unknown_host};
        {ok, _} -> 
            Target = list_to_atom(Node ++ "@" ++ Host),
            erlang:set_cookie(Target, Cookie),
            case net_adm:ping(Target) of
                pong ->
                    lager:debug("connected to service '~s' with cookie '~s'~n", [Target, Cookie]),
                    {ok, Target};
                pang ->
                    lager:debug("connection to service '~s' rejected~n", [Target]),            
                    {error, pang};
                Else ->
                    lager:debug("connection to service '~s' failed: ~p~n", [Target, Else]),            
                    {error, Else}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Looks in the default location for vm.args and tries to find the
%% setcookie value
%% @end
%%--------------------------------------------------------------------
-spec try_get_cookie_from_vmargs/1 :: (string()) -> atom().
try_get_cookie_from_vmargs(File) ->
    case file:read_file(File) of
        {error, _} -> erlang:get_cookie();
        {ok, Bin} ->
            case re:run(Bin, <<"-setcookie (.*)\\n">>, [{capture, [1], list}]) of
                {match, [Cookie]} -> list_to_atom(Cookie);
                _Else -> erlang:get_cookie()
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts build the content of a xml node either into a proplist
%% or string, optionally cleaning whitespace, tabs, newlines that make
%% the xml file easier to read.
%% @end
%%--------------------------------------------------------------------
-spec xml_content/1 :: (xml_el() | xml_els()) -> binary() | wh_proplist().
xml_content(Content) ->
    xml_content(Content, true).

-spec xml_content/2 :: (xml_el() | xml_els(), boolean()) -> binary() | wh_proplist().
xml_content(Content, Clean) ->
    xml_content(Content, Clean, <<>>).

-spec xml_content/3 :: (xml_el() | xml_els(), boolean(), binary() | list()) -> binary() | wh_proplist().
xml_content(#xmlElement{content=Content}, Clean, Acc) ->
    xml_content(Content, Clean, Acc);
xml_content([#xmlElement{content=Content}], Clean, Acc) ->
    xml_content(Content, Clean, Acc);
xml_content([#xmlElement{content=SubContent}=Element|Content], Clean, Acc) ->
    NewAcc = case xml_attribute("name", Element) of
                 undefined -> Acc;
                 Key when is_list(Acc) ->
                     [{replace_dynamic_vars(Key), xml_content(SubContent, Clean, <<>>)}|Acc];
                 Key ->
                     [{replace_dynamic_vars(Key), xml_content(SubContent, Clean, <<>>)}]
             end,
    xml_content(Content, Clean, NewAcc);
xml_content([#xmlText{value=Value}|Content], true, Acc) when is_binary(Acc) ->
    case re:run(Value, "^[\n\t\\s]*$") =/= nomatch of
        true -> xml_content(Content, true, Acc);
        false -> 
            Text = replace_dynamic_vars(clean_xml_value(Value)),
            xml_content(Content, true, <<Acc/binary, Text/binary>>)
    end;
xml_content([#xmlText{value=Value}|Content], Clean, Acc) when is_binary(Acc) ->
    xml_content(Content, Clean, <<Acc/binary, (replace_dynamic_vars(Value))/binary>>);
xml_content([_|Content], Clean, Acc) ->
    xml_content(Content, Clean, Acc);
xml_content([], _, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to find the text value of a xml element attribute, returning
%% a default value (undefined unless specified) if not found, or if
%% multiples are found.
%% @end
%%--------------------------------------------------------------------
-spec xml_attribute/2 :: (string(), xml_el() | xml_els()) -> binary() | 'undefined'.
xml_attribute(Attribute, Xml) ->
    xml_attribute(Attribute, Xml, undefined, binary).

-spec xml_string_attribute/2 :: (string(), xml_el() | xml_els()) -> string() | 'undefined'.
xml_string_attribute(Attribute, Xml) ->
    xml_attribute(Attribute, Xml, undefined, string).

-spec xml_atom_attribute/2 :: (string(), xml_el() | xml_els()) -> atom() | 'undefined'.
xml_atom_attribute(Attribute, Xml) ->
    xml_attribute(Attribute, Xml, undefined, atom).

-spec xml_integer_attribute/2 :: (string(), xml_el() | xml_els()) -> integer() | 'undefined'.
xml_integer_attribute(Attribute, Xml) ->
    xml_attribute(Attribute, Xml, undefined, integer).

-spec xml_boolean_attribute/2 :: (string(), xml_el() | xml_els()) -> boolean() | 'undefined'.
xml_boolean_attribute(Attribute, Xml) ->
    xml_attribute(Attribute, Xml, undefined, boolean).

-spec xml_attribute/3 :: (string(), xml_el() | xml_els(), Default) -> binary() | Default.
xml_attribute(Attribute, Xml, Default) ->
    xml_attribute(Attribute, Xml, Default, binary).

-spec xml_string_attribute/3 :: (string(), xml_el() | xml_els(), Default) -> string() | Default.
xml_string_attribute(Attribute, Xml, Default) ->
    xml_attribute(Attribute, Xml, Default, string).

-spec xml_atom_attribute/3 :: (string(), xml_el() | xml_els(), Default) -> atom() | Default.
xml_atom_attribute(Attribute, Xml, Default) ->
    xml_attribute(Attribute, Xml, Default, atom).

-spec xml_integer_attribute/3 :: (string(), xml_el() | xml_els(), Default) -> integer() | Default.
xml_integer_attribute(Attribute, Xml, Default) ->
    xml_attribute(Attribute, Xml, Default, integer).

-spec xml_boolean_attribute/3 :: (string(), xml_el() | xml_els(), Default) -> boolean() | Default.
xml_boolean_attribute(Attribute, Xml, Default) ->
    xml_attribute(Attribute, Xml, Default, boolean).

-spec xml_attribute/4 :: (string(), xml_el() | xml_els(), term(), xml_return_types()) -> term().
xml_attribute(Attribute, Xml, Default, Type) ->
    case get_xml_attribute(Attribute, Xml) of
        undefined -> Default;
        Value when Type =:= binary -> 
            wh_util:to_binary(Value);
        Value when Type =:= string -> 
            wh_util:to_list(Value);
        Value when Type =:= atom -> 
            wh_util:to_atom(Value, true);
        Value when Type =:= integer -> 
            wh_util:to_integer(Value);
        Value when Type =:= boolean -> 
            wh_util:is_true(Value);
        Value -> Value
    end.

-spec get_xml_attribute/2 :: (string(), xml_el() | xml_els()) -> 'undefined' | ne_binary().
get_xml_attribute(Attribute, Xml) ->
    case xmerl_xpath:string("@" ++ Attribute, Xml) of
        [#xmlAttribute{value=""}] -> undefined;
        [#xmlAttribute{value=Value}] ->
            replace_dynamic_vars(Value);
        _Else -> undefined
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% cleaning whitespace, tabs, newlines that make the xml file easier 
%% to read but still maintains newlines that are intentional
%% @end
%%-------------------------------------------------------------------
-spec xml_value/2 :: (string(), xml_el() | xml_els()) -> term().
xml_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, undefined).    

-spec xml_binary_value/2 :: (string(), xml_el() | xml_els()) -> binary() | 'undefined'.
xml_binary_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, binary).  

-spec xml_string_value/2 :: (string(), xml_el() | xml_els()) -> string() | 'undefined'.
xml_string_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, string).

-spec xml_atom_value/2 :: (string(), xml_el() | xml_els()) -> atom() | 'undefined'.
xml_atom_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, atom).

-spec xml_integer_value/2 :: (string(), xml_el() | xml_els()) -> integer() | 'undefined'.
xml_integer_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, integer).

-spec xml_boolean_value/2 :: (string(), xml_el() | xml_els()) -> boolean() | 'undefined'.
xml_boolean_value(Path, Xml) ->
    xml_value(Path, Xml, undefined, boolean).

-spec xml_value/3 :: (string(), xml_el() | xml_els(), term()) -> term().
xml_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, undefined).    

-spec xml_binary_value/3 :: (string(), xml_el() | xml_els(), Default) -> binary() | Default.
xml_binary_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, binary).    

-spec xml_string_value/3 :: (string(), xml_el() | xml_els(), Default) -> string() | Default.
xml_string_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, string).

-spec xml_atom_value/3 :: (string(), xml_el() | xml_els(), Default) -> atom() | Default.
xml_atom_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, atom).

-spec xml_integer_value/3 :: (string(), xml_el() | xml_els(), Default) -> integer() | Default.
xml_integer_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, integer).

-spec xml_boolean_value/3 :: (string(), xml_el() | xml_els(), Default) -> boolean() | Default.
xml_boolean_value(Path, Xml, Default) ->
    xml_value(Path, Xml, Default, boolean).

-spec xml_value/4 :: (string(), xml_el() | xml_els(), Default, xml_return_types()) -> binary() | Default.
xml_value(Path, Xml, Default, Type) ->
    case xml_content(xmerl_xpath:string(Path, Xml)) of
        <<>> -> Default;
        Value when Type =:= binary -> wh_util:to_binary(Value);
        Value when Type =:= string -> wh_util:to_list(Value);
        Value when Type =:= atom -> wh_util:to_atom(Value, true);
        Value when Type =:= integer -> wh_util:to_integer(Value);
        Value when Type =:= boolean -> wh_util:is_true(Value);
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% cleaning whitespace, tabs, newlines that make the xml file easier 
%% to read but still maintains newlines that are intentional
%% @end
%%-------------------------------------------------------------------
-spec clean_xml_value/1 :: (string() | binary()) -> string() | binary().
clean_xml_value(Value) ->
    lists:foldr(fun({RE, Replacement}, Subject) -> 
                        re:replace(Subject, RE, Replacement, [global]) 
                end
                ,Value
                ,[{" {2,}", ""}
                  ,{"\\t", ""}
                  ,{"\\n[ \\t]*$", ""}
                  ,{"^\\n?[ \\t]*", ""}
                 ]).
