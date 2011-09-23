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

-export([get_xml_value/2, make_doc_xml/1]).

get_xml_value(Path, Xml) ->
    try
        [#xmlText{value=Value}] = xmerl_xpath:string(Path, Xml),
        Value
    catch
        _:_ ->
            undefined
    end.

make_doc_xml(Fields) ->
    Xml = xmerl:export_simple([doc_xml_simple(Fields)], xmerl_xml,
                              [{prolog, ?BT_XML_PROLOG}]),
    unicode:characters_to_binary(Xml).

doc_xml_simple(Fields) ->
    {customer, fields_to_xml_simple(Fields)}.

fields_to_xml_simple(Fields) ->
    [ {K, [V]} || {K, V} <- Fields, V =/= undefined ].
