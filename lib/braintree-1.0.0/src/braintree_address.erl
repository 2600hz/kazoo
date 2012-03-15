%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_address).

-include("braintree.hrl").

-export([create/1, create/2, update/1, delete/1, delete/2]).
-export([find/2]).
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1, record_to_xml/2]).
-export([json_to_record/1, record_to_json/1]).

-import(braintree_util, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#bt_address{}) -> {'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'customer_id_invalid'}.
-spec create/2 :: (nonempty_string() | ne_binary(), #bt_address{}) -> {'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'customer_id_invalid'}.

create(#bt_address{customer_id=CustomerId}=Address) ->
    try
        true = validate_id(CustomerId, true),
        Request = record_to_xml(Address, true),
        case braintree_request:post("/customers/"
                                    ++ wh_util:to_list(CustomerId)
                                    ++ "/addresses", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

create(CustomerId, Address) ->
    create(Address#bt_address{customer_id=CustomerId}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (#bt_address{}) -> {'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'address_id_invalid'}.
update(#bt_address{id=Id, customer_id=CustomerId}=Address) ->
    try
        true = validate_id(Id),
        Request = record_to_xml(Address#bt_address{id=undefined}, true),
        case braintree_request:put("/customers/"
                                   ++ wh_util:to_list(CustomerId)
                                   ++ "/addresses/"
                                   ++ wh_util:to_list(Id), Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, address_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (#bt_address{}) -> {'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'address_id_invalid'}.
-spec delete/2 :: (ne_binary() | nonempty_string(), ne_binary() | nonempty_string()) ->
			  {'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'address_id_invalid'}.
delete(#bt_address{customer_id=CustomerId, id=Id}) ->
    delete(CustomerId, Id).

delete(CustomerId, Id) ->
    try
        true = validate_id(Id),
        case braintree_request:delete("/customers/"
                                      ++ wh_util:to_list(CustomerId)
                                      ++ "/addresses/"
                                      ++ wh_util:to_list(Id)) of
            {ok, _} ->
                {ok, #bt_address{}};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, address_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/2 :: (ne_binary() | nonempty_string(), ne_binary() | nonempty_string()) ->
			{'ok', #bt_address{}} | {'error', braintree_request:error_types() | 'address_id_invalid'}.
find(CustomerId, Id) ->
        try
            true = validate_id(Id),
            case braintree_request:get("/customers/"
                                      ++ wh_util:to_list(CustomerId)
                                      ++ "/addresses/"
                                      ++ wh_util:to_list(Id)) of
                {ok, Xml} ->
                    {ok, xml_to_record(Xml)};
                {error, _}=E ->
                    E
            end
        catch
            error:{badmatch, _} ->
                {error, address_id_invalid}
        end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Verifies that the id being used is valid
%% @end
%%--------------------------------------------------------------------
-spec validate_id/1 :: (nonempty_string() | ne_binary()) -> boolean().
-spec validate_id/2 :: (nonempty_string() | ne_binary(), boolean()) -> boolean().

validate_id(Id) ->
    validate_id(Id, false).
validate_id(Id, _) ->
    (Id =/= <<>> andalso Id =/= "")
        andalso (re:run(Id, "^[0-9A-Za-z_-]+$") =/= nomatch).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_address{} when
      Xml :: bt_xml().
-spec xml_to_record/2 :: (Xml, Base) -> #bt_address{} when
      Xml :: bt_xml(),
      Base :: string().

xml_to_record(Xml) ->
    xml_to_record(Xml, "/address").

xml_to_record(Xml, Base) ->
    #bt_address{id = get_xml_value(Base ++ "/id/text()", Xml)
                ,customer_id = get_xml_value(Base ++ "/customer-id/text()", Xml)
                ,first_name = get_xml_value(Base ++ "/first-name/text()", Xml)
                ,last_name = get_xml_value(Base ++ "/last-name/text()", Xml)
                ,company = get_xml_value(Base ++ "/company/text()", Xml)
                ,street_address = get_xml_value(Base ++ "/street-address/text()", Xml)
                ,extended_address = get_xml_value(Base ++ "/extended-address/text()", Xml)
                ,locality = get_xml_value(Base ++ "/locality/text()", Xml)
                ,region = get_xml_value(Base ++ "/region/text()", Xml)
                ,postal_code = get_xml_value(Base ++ "/postal-code/text()", Xml)
                ,country_code_two = get_xml_value(Base ++ "/country-code-alpha2/text()", Xml)
                ,country_code_three = get_xml_value(Base ++ "/country-code-alpha3/text()", Xml)
                ,country_code = get_xml_value(Base ++ "/country-code-numeric/text()", Xml)
                ,country_name = get_xml_value(Base ++ "/country-name/text()", Xml)
                ,created_at = get_xml_value(Base ++ "/created-at/text()", Xml)
                ,updated_at = get_xml_value(Base ++ "/updated-at/text()", Xml)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-type record_proplist() :: [{'first-name' | 'last-name' | 'company' | 'street-address'
			     | 'extended-address' | 'locality' | 'region' | 'postal-code'
			     | 'country-code-alpha2' | 'country-code-alpha3' | 'country-code-numeric'
			     | 'country-name'
			     ,'undefined' | [char() | {_,_}]},...].
-spec record_to_xml/1 :: (#bt_address{}) -> record_proplist() | braintree_util:char_to_bin_res().
-spec record_to_xml/2 :: (#bt_address{}, boolean()) -> record_proplist() | braintree_util:char_to_bin_res().

record_to_xml(Address) ->
    record_to_xml(Address, false).

record_to_xml(Address, ToString) ->
    Props = [{'first-name', Address#bt_address.first_name}
             ,{'last-name', Address#bt_address.last_name}
%%             ,{'id', Address#bt_address.id}
             ,{'company', Address#bt_address.company}
             ,{'street-address', Address#bt_address.street_address}
             ,{'extended-address', Address#bt_address.extended_address}
             ,{'locality', Address#bt_address.locality}
             ,{'region', Address#bt_address.region}
             ,{'postal-code', Address#bt_address.postal_code}
             ,{'country-code-alpha2', Address#bt_address.country_code_two}
             ,{'country-code-alpha3', Address#bt_address.country_code_three}
             ,{'country-code-numeric', Address#bt_address.country_code}
             ,{'country-name', Address#bt_address.country_name}],
    Props1 = case Address#bt_address.update_existing of
                 true -> [{'options', [{'update-existing', true}]}|Props];
                 _ -> Props
             end,
    case ToString of
        true -> make_doc_xml(Props1, 'address');
        false -> Props1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given json object into a record
%% @end
%%--------------------------------------------------------------------
-spec json_to_record/1 :: (JObj) -> #bt_address{} when
      JObj :: undefined | wh_json:json_object().
json_to_record(undefined) ->
    undefined;
json_to_record(JObj) ->
    #bt_address{id = create_or_get_json_id(JObj)
                ,first_name = wh_json:get_value(<<"first_name">>, JObj)
                ,last_name = wh_json:get_value(<<"last_name">>, JObj)
                ,company = wh_json:get_value(<<"company">>, JObj)
                ,street_address = wh_json:get_value(<<"street_address">>, JObj)
                ,extended_address = wh_json:get_value(<<"extended_address">>, JObj)
                ,locality = wh_json:get_value(<<"locality">>, JObj)
                ,region = wh_json:get_value(<<"region">>, JObj)
                ,postal_code = wh_json:get_value(<<"postal_code">>, JObj)
                ,country_code_two = wh_json:get_value(<<"country_code_two">>, JObj)
                ,country_code_three = wh_json:get_value(<<"country_code_three">>, JObj)
                ,country_code = wh_json:get_value(<<"country_code">>, JObj)
                ,country_name = wh_json:get_value(<<"country_name">>, JObj)
                ,update_existing = wh_json:is_true(<<"update_existing">>, JObj)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Convert a given record into a json object
%% @end
%%--------------------------------------------------------------------
-spec record_to_json/1 :: (Address) -> wh_json:json_object() when
      Address :: #bt_address{}.
record_to_json(Address) ->
    Props = [{<<"id">>, Address#bt_address.id}
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
             ,{<<"updated_at">>, Address#bt_address.updated_at}],
    braintree_util:props_to_json(Props).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the object exists in but no id has been provided then generate
%% a uuid to use during creation.
%% @end
%%--------------------------------------------------------------------
-spec create_or_get_json_id/1 :: (JObj) ->  undefined | string() when
      JObj :: wh_json:json_object().
create_or_get_json_id(JObj) ->
    case wh_json:get_value(<<"street_address">>, JObj) of
        undefined ->
            wh_json:get_value(<<"id">>, JObj);
         _ ->
            wh_json:get_value(<<"id">>, JObj, braintree_uuid:to_string())
    end.
