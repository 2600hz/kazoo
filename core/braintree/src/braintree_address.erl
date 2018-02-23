%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%%
%%% @author Karl Anderson
%%% @end
%%%-------------------------------------------------------------------
-module(braintree_address).

-export([url/1, url/2]).
-export([find/2]).
-export([create/1, create/2]).
-export([update/1]).
-export([delete/1, delete/2]).
-export([xml_to_record/1, xml_to_record/2]).
-export([record_to_xml/1, record_to_xml/2]).
-export([json_to_record/1]).
-export([record_to_json/1]).

-include("bt.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create the partial url for this module
%% @end
%%--------------------------------------------------------------------

-spec url(kz_term:ne_binary()) -> string().
url(CustomerId) ->
    lists:append(["/customers/", kz_term:to_list(CustomerId), "/addresses"]).

-spec url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().
url(CustomerId, AddressId) ->
    lists:append(["/customers/", kz_term:to_list(CustomerId), "/addresses/", kz_term:to_list(AddressId)]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find(kz_term:ne_binary() | nonempty_string(), kz_term:ne_binary() | nonempty_string()) -> bt_address().
find(CustomerId, AddressId) ->
    Url = url(CustomerId, AddressId),
    Xml = braintree_request:get(Url),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------

-spec create(bt_address()) -> bt_address().
create(#bt_address{customer_id=CustomerId}=Address) ->
    Url = url(CustomerId),
    Request = record_to_xml(Address, 'true'),
    Xml = braintree_request:post(Url, Request),
    xml_to_record(Xml).

-spec create(nonempty_string() | kz_term:ne_binary(), bt_address()) -> bt_address().
create(CustomerId, Address) ->
    create(Address#bt_address{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update(bt_address()) -> bt_address().
update(#bt_address{id=AddressId
                  ,customer_id=CustomerId
                  }=Address) ->
    Url = url(CustomerId, AddressId),
    Request = record_to_xml(Address#bt_address{id='undefined'}, 'true'),
    Xml = braintree_request:put(Url, Request),
    xml_to_record(Xml).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------

-spec delete(bt_address()) -> bt_address().
delete(#bt_address{customer_id=CustomerId
                  ,id=AddressId
                  }) ->
    delete(CustomerId, AddressId).

-spec delete(kz_term:ne_binary() | nonempty_string(), kz_term:ne_binary() | nonempty_string()) ->  bt_address().
delete(CustomerId, AddressId) ->
    Url = url(CustomerId, AddressId),
    _ = braintree_request:delete(Url),
    #bt_address{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------

-spec xml_to_record(bt_xml()) -> bt_address().
xml_to_record(Xml) ->
    xml_to_record(Xml, "/address").

-spec xml_to_record(bt_xml(), kz_term:deeplist()) -> bt_address().
xml_to_record(Xml, Base) ->
    #bt_address{id = kz_xml:get_value([Base, "/id/text()"], Xml)
               ,customer_id = kz_xml:get_value([Base, "/customer-id/text()"], Xml)
               ,first_name = kz_xml:get_value([Base, "/first-name/text()"], Xml)
               ,last_name = kz_xml:get_value([Base, "/last-name/text()"], Xml)
               ,company = kz_xml:get_value([Base, "/company/text()"], Xml)
               ,street_address = kz_xml:get_value([Base, "/street-address/text()"], Xml)
               ,extended_address = kz_xml:get_value([Base, "/extended-address/text()"], Xml)
               ,locality = kz_xml:get_value([Base, "/locality/text()"], Xml)
               ,region = kz_xml:get_value([Base, "/region/text()"], Xml)
               ,postal_code = kz_xml:get_value([Base, "/postal-code/text()"], Xml)
               ,country_code_two = kz_xml:get_value([Base, "/country-code-alpha2/text()"], Xml)
               ,country_code_three = kz_xml:get_value([Base, "/country-code-alpha3/text()"], Xml)
               ,country_code = kz_xml:get_value([Base, "/country-code-numeric/text()"], Xml)
               ,country_name = kz_xml:get_value([Base, "/country-name/text()"], Xml)
               ,created_at = kz_xml:get_value([Base, "/created-at/text()"], Xml)
               ,updated_at = kz_xml:get_value([Base, "/updated-at/text()"], Xml)
               }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------

-spec record_to_xml(bt_address()) -> kz_term:proplist() | bt_xml() | 'undefined'.
record_to_xml(Address) ->
    record_to_xml(Address, 'false').

-spec record_to_xml(bt_address(), boolean()) -> kz_term:proplist() | bt_xml() | 'undefined'.
record_to_xml('undefined', _ToString) -> 'undefined';
record_to_xml(Address, ToString) ->
    Props = [{'first-name', Address#bt_address.first_name}
            ,{'last-name', Address#bt_address.last_name}
            ,{'company', Address#bt_address.company}
            ,{'street-address', Address#bt_address.street_address}
            ,{'extended-address', Address#bt_address.extended_address}
            ,{'locality', Address#bt_address.locality}
            ,{'region', Address#bt_address.region}
            ,{'postal-code', Address#bt_address.postal_code}
            ,{'country-code-alpha2', Address#bt_address.country_code_two}
            ,{'country-code-alpha3', Address#bt_address.country_code_three}
            ,{'country-code-numeric', Address#bt_address.country_code}
            ,{'country-name', Address#bt_address.country_name}
            ],
    Props1 = case Address#bt_address.update_existing of
                 'true' -> [{'options', [{'update-existing', 'true'}]}|Props];
                 _ -> Props
             end,
    case ToString of
        'true' -> braintree_util:make_doc_xml(Props1, 'address');
        'false' -> Props1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record(kz_term:api_object()) -> bt_address() | 'undefined'.
json_to_record('undefined') -> 'undefined';
json_to_record(JObj) ->
    #bt_address{id = create_or_get_json_id(JObj)
               ,first_name = kz_json:get_value(<<"first_name">>, JObj)
               ,last_name = kz_json:get_value(<<"last_name">>, JObj)
               ,company = kz_json:get_value(<<"company">>, JObj)
               ,street_address = kz_json:get_value(<<"street_address">>, JObj)
               ,extended_address = kz_json:get_value(<<"extended_address">>, JObj)
               ,locality = kz_json:get_value(<<"locality">>, JObj)
               ,region = kz_json:get_value(<<"region">>, JObj)
               ,postal_code = kz_json:get_value(<<"postal_code">>, JObj)
               ,country_code_two = kz_json:get_value(<<"country_code_two">>, JObj)
               ,country_code_three = kz_json:get_value(<<"country_code_three">>, JObj)
               ,country_code = kz_json:get_value(<<"country_code">>, JObj)
               ,country_name = kz_json:get_value(<<"country_name">>, JObj)
               ,update_existing = kz_json:is_true(<<"update_existing">>, JObj)
               }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json(bt_address() | 'undefined') -> kz_term:api_object().
record_to_json('undefined') -> 'undefined';
record_to_json(#bt_address{}=Address) ->
    kz_json:from_list(
      [{<<"id">>, Address#bt_address.id}
      ,{<<"customer_id">>, Address#bt_address.customer_id}
      ,{<<"first_name">>, Address#bt_address.first_name}
      ,{<<"last_name">>, Address#bt_address.last_name}
      ,{<<"company">>, Address#bt_address.company}
      ,{<<"street_address">>, Address#bt_address.street_address}
      ,{<<"extended_address">>, Address#bt_address.extended_address}
      ,{<<"locality">>, Address#bt_address.locality}
      ,{<<"region">>, Address#bt_address.region}
      ,{<<"postal_code">>, Address#bt_address.postal_code}
      ,{<<"country_code_two">>, Address#bt_address.country_code_two}
      ,{<<"country_code_three">>, Address#bt_address.country_code_three}
      ,{<<"country_code">>, Address#bt_address.country_code}
      ,{<<"country_name">>, Address#bt_address.country_name}
      ,{<<"created_at">>, Address#bt_address.created_at}
      ,{<<"updated_at">>, Address#bt_address.updated_at}
      ]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the object exists in but no id has been provided then generate
%% a uuid to use during creation.
%% @end
%%--------------------------------------------------------------------
-spec create_or_get_json_id(kz_json:object()) ->  kz_term:api_binary().
create_or_get_json_id(JObj) ->
    case kz_json:get_value(<<"street_address">>, JObj) of
        'undefined' -> kz_doc:id(JObj);
        _Address -> kz_doc:id(JObj, kz_binary:rand_hex(16))
    end.
