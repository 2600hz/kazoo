%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_util).

-include_lib("xmerl/include/xmerl.hrl").
-include("braintree.hrl").

-export([get_xml_value/2, make_doc_xml/2]).
-export([props_to_json/1]).
-export([bt_error_to_json/1, bt_verification_to_json/1, bt_api_error_to_json/1]).

%% from stdlib/src/unicode.erl
-type char_to_bin_res() :: binary() |
			   {'error', binary(), unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()} |
			   {'incomplete', binary(), binary()}.
-export_type([char_to_bin_res/0]).


get_xml_value(Path, Xml) ->
    try
        [#xmlText{value=Value}] = xmerl_xpath:string(Path, Xml),
        Value
    catch
        _:_ ->
            undefined
    end.

-spec make_doc_xml/2 :: (proplist(), atom()) -> char_to_bin_res().
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

props_to_xml([{K, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, [{_, _, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}|Attr], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, [wh_util:to_list(V)]}|Xml]);

props_to_xml([{K, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{type, "boolean"}], [wh_util:to_list(V)]}|Xml]);
props_to_xml([{K, V}|T], Xml) ->
    props_to_xml(T, [{K, [wh_util:to_list(V)]}|Xml]).

-spec props_to_json/1 :: (proplist()) -> json_object().
props_to_json(Props) ->
    wh_json:from_list([begin
			   case V of
			       {struct, _} -> {K, V};
			       [{struct, _}|_] -> {K, V};
			       [] -> {K, V};
			       _ when is_boolean(V) -> {K, V};
			       _ -> {K, wh_util:to_binary(V)}
			   end
		       end || {K, V} <- Props, V =/= undefined]).

bt_error_to_json(BtError) ->
    Props = [{<<"code">>, BtError#bt_error.code}
             ,{<<"message">>, BtError#bt_error.message}
             ,{<<"attribute">>, BtError#bt_error.attribute}],
    props_to_json(Props).

bt_verification_to_json(BtVerification) ->
    Props = [{<<"verification_status">>, BtVerification#bt_verification.verification_status}
             ,{<<"processor_response_code">>, BtVerification#bt_verification.processor_response_code}
             ,{<<"processor_response_text">>, BtVerification#bt_verification.processor_response_text}
             ,{<<"cvv_response_code">>, BtVerification#bt_verification.cvv_response_code}
             ,{<<"avs_response_code">>, BtVerification#bt_verification.avs_response_code}
             ,{<<"postal_response_code">>, BtVerification#bt_verification.postal_response_code}
             ,{<<"street_response_code">>, BtVerification#bt_verification.street_response_code}
             ,{<<"gateway_rejection_reason">>, BtVerification#bt_verification.gateway_rejection_reason}],
    props_to_json(Props).

bt_api_error_to_json(BtApiError) ->
    Props = [{<<"errors">>, [bt_error_to_json(Error) || Error <- BtApiError#bt_api_error.errors]}
             ,{<<"verification">>, bt_verification_to_json(BtApiError#bt_api_error.verification)}
             ,{<<"message">>, BtApiError#bt_api_error.message}],
    props_to_json(Props).
