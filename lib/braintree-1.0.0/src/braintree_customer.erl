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
-export([find/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Creates a new customer using the given record
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.

create(Customer) ->
    {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Updates a customer with the given record
%% @end
%%--------------------------------------------------------------------
-spec update/1 :: (Customer) -> bt_result() when
      Customer :: #bt_customer{}.

update(Customer) ->
    {error, not_implemented}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Deletes a customer id from braintree's system
%% @end
%%--------------------------------------------------------------------
-spec delete/1 :: (CustomerId) -> bt_result() when
      CustomerId :: binary() | string().

delete(CustomerId) ->
    {error, not_implemented}.

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
                {ok, Response} ->
                    {ok, xml_to_record(Response)};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Contert the given XML to a customer record
%% @end
%%--------------------------------------------------------------------
-spec xml_to_record/1 :: (Xml) -> #bt_customer{} when
      Xml :: term().

xml_to_record(Xml) ->
    #bt_customer{}.
