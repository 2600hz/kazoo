%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson <karl@2600hz.org>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_addon).

-export([get_quantity/1]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([record_to_json/1]).
-export([json_to_record/1]).

-include("braintree.hrl").

%%------------------------------------------------------------------------------
%% @doc Accessor to get add-on quantity from the given add-on record.
%% @end
%%------------------------------------------------------------------------------

-spec get_quantity(bt_addon()) -> integer().
get_quantity(#bt_addon{quantity=Quantity}) ->
    Quantity.

%% equiv xml_to_record(Xml, "/add-on")

-spec xml_to_record(bt_xml()) -> bt_addon().
xml_to_record(Xml) ->
    xml_to_record(Xml, "/add-on").

%%------------------------------------------------------------------------------
%% @doc Converts the given XML to a add-on record. Uses `Base' as base path
%% to get values from XML.
%% @end
%%------------------------------------------------------------------------------

-spec xml_to_record(bt_xml(), kz_term:deeplist()) -> bt_addon().
xml_to_record(Xml, Base) ->
    #bt_addon{id = kz_xml:get_value([Base, "/id/text()"], Xml)
             ,amount = kz_xml:get_value([Base, "/amount/text()"], Xml)
             ,never_expires = kz_term:is_true(kz_xml:get_value([Base, "/never-expires/text()"], Xml))
             ,billing_cycle = kz_xml:get_value([Base, "/current-billing-cycle/text()"], Xml)
             ,number_of_cycles = kz_xml:get_value([Base, "/number-of-billing-cycles/text()"], Xml)
             ,quantity = kz_term:to_integer(kz_xml:get_value([Base, "/quantity/text()"], Xml))
             }.

%% @equiv record_to_xml(Addon, false)

-spec record_to_xml(bt_addon()) -> kz_term:proplist() | bt_xml().
record_to_xml(Addon) ->
    record_to_xml(Addon, false).

%%------------------------------------------------------------------------------
%% @doc Converts the given add-on record to a XML document. If `ToString' is
%% `true' returns exported XML as string binary.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_xml(bt_addon(), boolean()) -> kz_term:proplist() | bt_xml().
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
        true -> braintree_util:make_doc_xml(Props, 'add-on');
        false -> Props
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a given record into a JSON object.
%% @end
%%------------------------------------------------------------------------------

-spec record_to_json(bt_addon()) -> kz_json:object().
record_to_json(#bt_addon{id=Id, amount=Amount, quantity=Q}) ->
    Props = [{<<"id">>, Id}
            ,{<<"amount">>, Amount}
            ,{<<"quantity">>, kz_term:to_integer(Q)}
            ],
    kz_json:from_list([KV || {_, V}=KV <- Props, V =/= undefined]).

%%------------------------------------------------------------------------------
%% @doc Convert a given JSON object into a record.
%% @end
%%------------------------------------------------------------------------------

-spec json_to_record(kz_term:api_object()) -> bt_addon() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_addon{id = kz_doc:id(JObj)
             ,amount = kz_json:get_binary_value(<<"amount">>, JObj)
             ,never_expires = kz_json:get_value(<<"never_expires">>, JObj, 'true')
             ,billing_cycle = kz_json:get_binary_value(<<"billing_cycle">>, JObj)
             ,number_of_cycles = kz_json:get_binary_value(<<"number_of_cycles">>, JObj)
             ,quantity = kz_json:get_integer_value(<<"quantity">>, JObj)
             ,inherited_from = kz_json:get_binary_value(<<"inherited_from">>, JObj)
             ,existing_id = kz_json:get_binary_value(<<"existing_id">>, JObj)
             }.
