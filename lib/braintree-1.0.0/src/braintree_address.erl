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
-export([xml_to_record/1, xml_to_record/2, record_to_xml/1]).

-import(braintree_utils, [get_xml_value/2, make_doc_xml/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Address) -> bt_result() when
      Address :: #bt_address{}.
-spec create/2 :: (CustomerId, Address) -> bt_result() when
      CustomerId :: string() | binary(),
      Address :: #bt_address{}.

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
-spec update/1 :: (Address) -> bt_result() when
      Address :: #bt_address{}.
update(#bt_address{id=Id, customer_id=CustomerId}=Address) ->
    try
        true = validate_id(Id),
        Request = record_to_xml(Address, true),
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
-spec delete/1 :: (Address) -> bt_result() when
      Address :: #bt_address{}.
-spec delete/2 :: (CustomerId, Id) -> bt_result() when
      CustomerId :: binary() | string(),
      Id :: binary() | string().

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
-spec find/2 :: (CustomerId, Id) -> bt_result() when
      CustomerId :: binary() | string(),
      Id :: binary() | string().
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
-spec validate_id/1 :: (Id) -> boolean() when
      Id :: string() | binary().
-spec validate_id/2 :: (Id, AllowUndefined) -> boolean() when
      Id :: string() | binary(),
      AllowUndefined :: boolean().

validate_id(Id) ->
    validate_id(Id, false).

validate_id(undefined, false) ->
    false;
validate_id(Id, _) ->
    (Id =/= <<>> andalso Id =/= "")
        andalso (re:run(Id, "^[0-9A-Za-z_-]+$") =/= nomatch).

%%--------------------------------------------------------------------
%% @private
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
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Address) -> bt_xml() when
      Address :: #bt_address{}.
-spec record_to_xml/2 :: (Address, ToString) -> bt_xml() when
      Address :: #bt_address{},
      ToString :: boolean().

record_to_xml(Address) ->
    record_to_xml(Address, false).

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
             ,{'country-name', Address#bt_address.country_name}],
    Props1 = case Address#bt_address.update_existing of
                 true -> [{'options', [{'update-existing', true}]}|Props];
                 _ -> Props
             end,
    case ToString of
        true -> make_doc_xml(Props1, 'address');
        false -> Props1
    end.
