%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Sep 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(braintree_customer).

-include("braintree.hrl").

-export([create/1, update/1, delete/1]).
-export([all/0, find/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.
create(Customer) ->
    try
        true = validate_id(Customer),
        Request = record_to_xml(Customer),
        case braintree_request:post("/customers", Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.
update(Customer) ->
    try
        true = validate_id(Customer),
        Request = record_to_xml(Customer),
        case braintree_request:put("/customers/" ++ wh_util:to_list(Customer#bt_customer.id), Request) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (CustomerId) -> bt_result() when
      CustomerId :: binary() | string().
delete(CustomerId) ->
    try
        true = validate_id(CustomerId),
        case braintree_request:delete("/customers/" ++ wh_util:to_list(CustomerId)) of
            {ok, Xml} ->
                {ok, xml_to_record(Xml)};
            {error, _}=E ->
                E
        end
    catch
        error:{badmatch, _} ->
            {error, customer_id_invalid}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec all/0 :: () -> list(#bt_customer{}) | tuple(error, term()).
all() ->
    case braintree_request:get("/customers/") of
        {ok, Xml} ->
            Customers = [xml_to_record(Customer)
                         || Customer <- xmerl_xpath:string("//customers/customer", Xml)],
            {ok, [Customers]};
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find a customer by id
%% @end
%%--------------------------------------------------------------------
-spec find/1 :: (CustomerId) -> bt_result() when
      CustomerId :: binary() | string().
find(CustomerId) ->
        try
            true = validate_id(CustomerId),
            case braintree_request:get("/customers/" ++ wh_util:to_list(CustomerId)) of
                {ok, Xml} ->
                    {ok, xml_to_record(Xml)};
                {error, _}=E ->
                    E
            end
        catch
            error:{badmatch, _} ->
                {error, customer_id_invalid}
        end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%Verifies that a valid customer id is being used
%% @end
%%--------------------------------------------------------------------
-spec validate_id/1 :: (Customer) -> boolean() when
      Customer :: #bt_customer{} | string() | binary().
validate_id(#bt_customer{id=Id}) ->
    validate_id(Id);
validate_id(Id) ->
    (Id =/= <<>> andalso Id =/= "")
        andalso (re:run(Id, "^[0-9A-Za-z_-]+$") =/= nomatch)
        andalso Id =/= undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_customer{} when
      Xml :: bt_xml().
xml_to_record(Xml) ->
    #bt_customer{id = braintree_utils:get_xml_value("//customer/id/text()", Xml)
                 ,first_name = braintree_utils:get_xml_value("//customer/first-name/text()", Xml)
                 ,last_name = braintree_utils:get_xml_value("//customer/last-name/text()", Xml)
                 ,company = braintree_utils:get_xml_value("//customer/company/text()", Xml)
                 ,email = braintree_utils:get_xml_value("//customer/email/text()", Xml)
                 ,phone = braintree_utils:get_xml_value("//customer/phone/text()", Xml)
                 ,fax = braintree_utils:get_xml_value("//customer/fax/text()", Xml)
                 ,website = braintree_utils:get_xml_value("//customer/website/text()", Xml)
                 ,created_at = braintree_utils:get_xml_value("//customer/created-at/text()", Xml)
                 ,updated_at = braintree_utils:get_xml_value("//customer/updated-at/text()", Xml)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec record_to_xml/1 :: (Customer) -> bt_xml() when
      Customer :: #bt_customer{}.
record_to_xml(Customer) ->
    Props = [{'id', Customer#bt_customer.id}
             ,{'first-name', Customer#bt_customer.first_name}
             ,{'last-name', Customer#bt_customer.last_name}
             ,{'company', Customer#bt_customer.company}
             ,{'email', Customer#bt_customer.email}
             ,{'phone', Customer#bt_customer.phone}
             ,{'fax', Customer#bt_customer.fax}
             ,{'website', Customer#bt_customer.website}],
    braintree_utils:make_doc_xml(Props).
