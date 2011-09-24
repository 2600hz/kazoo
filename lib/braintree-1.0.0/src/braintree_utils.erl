%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_utils).

-include_lib("xmerl/include/xmerl.hrl").
-include("braintree.hrl").

-export([get_xml_value/2, make_doc_xml/2]).

get_xml_value(Path, Xml) ->
    try
        [#xmlText{value=Value}] = xmerl_xpath:string(Path, Xml),
        Value
    catch
        _:_ ->
            undefined
    end.

make_doc_xml(Props, Root) ->
    Xml = xmerl:export_simple([doc_xml_simple(Props, Root)], xmerl_xml
                              ,[{prolog, ?BT_XML_PROLOG}]),
    unicode:characters_to_binary(Xml).

doc_xml_simple(Props, Root) ->
    {Root, props_to_xml(Props, [])}.

props_to_xml([], Xml) ->
    Xml;
props_to_xml([{_, undefined}|T], Xml) ->
    props_to_xml(T, Xml);
props_to_xml([{_, []}|T], Xml) ->
    props_to_xml(T, Xml);

props_to_xml([{K, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, V}|T], Xml) ->
    props_to_xml(T, [{K, [wh_util:to_list(V)]}|Xml]);

props_to_xml([{K, Attr, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}|Attr], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, [wh_util:to_list(V)]}|Xml]).
