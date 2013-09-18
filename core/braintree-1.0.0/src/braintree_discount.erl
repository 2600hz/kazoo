%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_discount).

-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([record_to_json/1]).

-import(braintree_util, [make_doc_xml/2]).
-import(wh_util, [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (bt_xml()) -> #bt_discount{}.
-spec xml_to_record/2 :: (bt_xml(), wh_deeplist()) -> #bt_discount{}.

xml_to_record(Xml) ->
    xml_to_record(Xml, "/discount").

xml_to_record(Xml, Base) ->
    #bt_discount{id = get_xml_value([Base, "/id/text()"], Xml)
              ,amount = get_xml_value([Base, "/amount/text()"], Xml)
              ,never_expires = wh_util:is_true(get_xml_value([Base, "/never-expires/text()"], Xml))
              ,billing_cycle = get_xml_value([Base, "/current-billing-cycle/text()"], Xml)
              ,number_of_cycles = get_xml_value([Base, "/number-of-billing-cycles/text()"], Xml)
              ,quantity = wh_util:to_integer(get_xml_value([Base, "/quantity/text()"], Xml))}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (#bt_discount{}) -> proplist() | bt_xml().
-spec record_to_xml/2 :: (#bt_discount{}, boolean()) -> proplist() | bt_xml().

record_to_xml(Discount) ->
    record_to_xml(Discount, false).

record_to_xml(Discount, ToString) ->
    Props = [{'id', Discount#bt_discount.id}
             ,{'amount', Discount#bt_discount.amount}
             ,{'never-expires', Discount#bt_discount.never_expires}
             ,{'number-of-billing-cycles', Discount#bt_discount.number_of_cycles}
             ,{'quantity', Discount#bt_discount.quantity}
             ,{'inherited-from-id', Discount#bt_discount.inherited_from}
             ,{'existing-id', Discount#bt_discount.id}
            ],
    case ToString of
        true -> make_doc_xml(Props, 'discount');
        false -> Props
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (#bt_discount{}) -> wh_json:object().
record_to_json(#bt_discount{id=Id, amount=Amount, quantity=Q}) ->
    Props = [{<<"id">>, Id}
             ,{<<"amount">>, Amount}
             ,{<<"quantity">>, wh_util:to_integer(Q)}
            ],
    wh_json:from_list([KV || {_, V}=KV <- Props, V =/= undefined]).
