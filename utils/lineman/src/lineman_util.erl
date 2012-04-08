%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(lineman_util).

-export([try_connect_to_target/2, try_connect_to_target/3]).
-export([try_get_cookie_from_vmargs/1]).
-export([get_xml_element_content/1, get_xml_element_content/2]).
-export([get_xml_attribute_value/2, get_xml_attribute_value/3]).

-include_lib("lineman/src/lineman.hrl").

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
                    lager:info("connected to service '~s' with cookie '~s'~n", [Target, Cookie]),
                    {ok, Target};
                pang ->
                    lager:info("connection to service '~s' rejected~n", [Target]),            
                    {error, pang};
                Else ->
                    lager:info("connection to service '~s' failed: ~p~n", [Target, Else]),            
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
-type xml_collection() :: [term(),...].

-spec get_xml_element_content/1 :: (xml_collection()) -> binary() | proplist().
get_xml_element_content(Content) ->
    get_xml_element_content(Content, true).

-spec get_xml_element_content/2 :: (xml_collection(), boolean()) -> binary() | proplist().
get_xml_element_content(Content, Clean) ->
    get_xml_element_content(Content, Clean, <<>>).

-spec get_xml_element_content/3 :: (xml_collection(), boolean(), binary() | list()) -> binary() | proplist().
get_xml_element_content(#xmlElement{content=Content}, Clean, Acc) ->
    get_xml_element_content(Content, Clean, Acc);
get_xml_element_content([#xmlElement{content=SubContent}=Element|Content], Clean, Acc) ->
    NewAcc = case get_xml_attribute_value("name", Element) of
                 undefined -> Acc;
                 Key when is_list(Acc) ->
                     [{wh_util:to_binary(Key), get_xml_element_content(SubContent, Clean, <<>>)}|Acc];
                 Key ->
                     [{wh_util:to_binary(Key), get_xml_element_content(SubContent, Clean, <<>>)}]
             end,
    get_xml_element_content(Content, Clean, NewAcc);
get_xml_element_content([#xmlText{value=Value}|Content], true, Acc) when is_binary(Acc) ->
    case re:run(Value, "^[\n\t\\s]*$") =/= nomatch of
        true -> get_xml_element_content(Content, true, Acc);
        false -> 
            Text = wh_util:to_binary(clean_xml_value(Value)),
            get_xml_element_content(Content, true, <<Acc/binary, Text/binary>>)
    end;
get_xml_element_content([#xmlText{value=Value}|Content], Clean, Acc) when is_binary(Acc) ->
    get_xml_element_content(Content, Clean, <<Acc/binary, (wh_util:to_binary(Value))/binary>>);
get_xml_element_content([_|Content], Clean, Acc) ->
    get_xml_element_content(Content, Clean, Acc);
get_xml_element_content([], _, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to find the text value of a xml element attribute, returning
%% a default value (undefined unless specified) if not found, or if
%% multiples are found.
%% @end
%%--------------------------------------------------------------------
-spec get_xml_attribute_value/2 :: (string(), #xmlElement{}) -> string() | 'undefined'.
get_xml_attribute_value(Attribute, Xml) ->
    get_xml_attribute_value(Attribute, Xml, undefined).

-spec get_xml_attribute_value/3 :: (string(), #xmlElement{}, Default) -> string() | Default.
get_xml_attribute_value(Attribute, Xml, Default) ->
    case xmerl_xpath:string("@" ++ Attribute, Xml) of
        [#xmlAttribute{value=Value}] -> Value;
        _Else -> Default
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% cleaning whitespace, tabs, newlines that make the xml file easier 
%% to read but still maintains newlines that are intentional
%% @end
%%-------------------------------------------------------------------
-spec clean_xml_value/1 :: (string()) -> string().
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
