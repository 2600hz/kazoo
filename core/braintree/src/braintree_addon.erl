%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_addon).

-export([get_quantity/1]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([record_to_json/1]).
-export([json_to_record/1]).

-import(braintree_util, [make_doc_xml/2]).
-import(wh_util, [get_xml_value/2]).

-include_lib("braintree/include/braintree.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_quantity/1 :: (bt_addon()) -> integer().
get_quantity(#bt_addon{quantity=Quantity}) ->
    Quantity.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (bt_xml()) -> bt_addon().
-spec xml_to_record/2 :: (bt_xml(), wh_deeplist()) -> bt_addon().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/add-on").

xml_to_record(Xml, Base) ->
    #bt_addon{id = get_xml_value([Base, "/id/text()"], Xml)
              ,amount = get_xml_value([Base, "/amount/text()"], Xml)
              ,never_expires = wh_util:is_true(get_xml_value([Base, "/never-expires/text()"], Xml))
              ,billing_cycle = get_xml_value([Base, "/current-billing-cycle/text()"], Xml)
              ,number_of_cycles = get_xml_value([Base, "/number-of-billing-cycles/text()"], Xml)
              ,quantity = wh_util:to_integer(get_xml_value([Base, "/quantity/text()"], Xml))
             }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (bt_addon()) -> wh_proplist() | bt_xml().
-spec record_to_xml/2 :: (bt_addon(), boolean()) -> wh_proplist() | bt_xml().

record_to_xml(Addon) ->
    record_to_xml(Addon, false).

record_to_xml(Addon, ToString) ->
    Props = [{'id', Addon#bt_addon.id}
             ,{'amount', Addon#bt_addon.amount}
             ,{'never-expires', Addon#bt_addon.never_expires}
             ,{'number-of-billing-cycles', Addon#bt_addon.number_of_cycles}
             ,{'quantity', Addon#bt_addon.quantity}
             ,{'inherited-from-id', Addon#bt_addon.inherited_from}
             ,{'existing-id', Addon#bt_addon.id}
            ],
    case ToString of
        true -> make_doc_xml(Props, 'add-on');
        false -> Props
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (bt_addon()) -> wh_json:object().
record_to_json(#bt_addon{id=Id, amount=Amount, quantity=Q}) ->
    Props = [{<<"id">>, Id}
             ,{<<"amount">>, Amount}
             ,{<<"quantity">>, wh_util:to_integer(Q)}
            ],
    wh_json:from_list([KV || {_, V}=KV <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json obj into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(api_object()) -> bt_addon() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_addon{id = wh_doc:id(JObj)
              ,amount = wh_json:get_binary_value(<<"amount">>, JObj)
              ,never_expires = wh_json:get_value(<<"never_expires">>, JObj, 'true')
              ,billing_cycle = wh_json:get_binary_value(<<"billing_cycle">>, JObj)
              ,number_of_cycles = wh_json:get_binary_value(<<"number_of_cycles">>, JObj)
              ,quantity = wh_json:get_integer_value(<<"quantity">>, JObj)
              ,inherited_from = wh_json:get_binary_value(<<"inherited_from">>, JObj)
              ,existing_id = wh_json:get_binary_value(<<"existing_id">>, JObj)
    }.

