%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_util).

-export([make_doc_xml/2]).
-export([bt_error_to_json/1]).
-export([bt_verification_to_json/1]).
-export([bt_api_error_to_json/1]).

-export([update_services_card/2
        ,update_services_cards/2
        ,delete_services_card/2
        ]).

-export([error_to_props/1]).
-export([error_no_payment_token/0]).
-export([error_authentication/0]).
-export([error_authorization/0]).
-export([error_not_found/1]).
-export([error_upgrade_required/0]).
-export([error_server_error/0]).
-export([error_maintenance/0]).
-export([error_api/1]).
-export([error_io_fault/0]).
-export([error_min_amount/1]).
-export([error_max_amount/1]).

-include("braintree.hrl").

%% from stdlib/src/unicode.erl
-type char_to_bin_res() :: binary() |
                           {'error', binary(), unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()} |
                           {'incomplete', binary(), binary()}.
-export_type([char_to_bin_res/0]).

%% https://developers.braintreepayments.com/reference/general/validation-errors/all/php#code-91511
%% https://developers.braintreepayments.com/reference/general/validation-errors/all/php#code-91510
%% https://developers.braintreepayments.com/reference/general/validation-errors/all/php#code-81571
-define(NO_PAYMENT_TOKEN_CODES, [91511, 91510, 81571]).

%%------------------------------------------------------------------------------
%% @doc Export XMerl object to XML and convert it to a binary.
%% @end
%%------------------------------------------------------------------------------

-spec make_doc_xml(kz_term:proplist(), atom()) -> char_to_bin_res().
make_doc_xml(Props, Root) ->
    Xml = xmerl:export_simple([doc_xml_simple(Props, Root)]
                             ,'xmerl_xml'
                             ,[{'prolog', ?BT_XML_PROLOG}]
                             ),
    unicode:characters_to_binary(Xml).

doc_xml_simple(Props, Root) ->
    {Root, props_to_xml(Props, [])}.

props_to_xml([], Xml) ->
    Xml;
props_to_xml([{_, 'undefined'}|T], Xml) ->
    props_to_xml(T, Xml);
props_to_xml([{_, _, 'undefined'}|T], Xml) ->
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
    props_to_xml(T, [{K, [{'type', "boolean"}|Attr], [kz_term:to_list(V)]}|Xml]);
props_to_xml([{K, Attr, V}|T], Xml) ->
    props_to_xml(T, [{K, Attr, [kz_term:to_list(V)]}|Xml]);

props_to_xml([{K, [{_, _}|_]=V}|T], Xml) ->
    props_to_xml(T, [{K, props_to_xml(V, [])}|Xml]);
props_to_xml([{K, V}|T], Xml) when is_boolean(V) ->
    props_to_xml(T, [{K, [{'type', "boolean"}], [kz_term:to_list(V)]}|Xml]);
props_to_xml([{K, V}|T], Xml) ->
    props_to_xml(T, [{K, [kz_term:to_list(V)]}|Xml]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_services_card(kz_term:ne_binary() | kz_services:services(), bt_card()) -> {'ok' | 'error', kz_services:services()}.
update_services_card(CustomerIdOrServices, Card) ->
    Token = braintree_card:record_to_payment_token(Card),
    save_services(kz_services_payment_tokens:update(CustomerIdOrServices, <<"braintree">>, Token)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_services_cards(kz_term:ne_binary() | kz_services:services(), bt_cards()) -> {'ok' | 'error', kz_services:services()}.
update_services_cards(CustomerIdOrServices, Cards) ->
    Tokens = [braintree_card:record_to_payment_token(Card) || Card <- Cards],
    save_services(kz_services_payment_tokens:updates(CustomerIdOrServices, <<"braintree">>, Tokens)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_services_card(kz_term:ne_binary() | kz_services:services(), bt_card() | kz_term:ne_binary()) -> {'ok' | 'error', kz_services:services()}.
delete_services_card(CustomerIdOrServices, #bt_card{}=Card) ->
    Token = braintree_card:record_to_payment_token(Card),
    save_services(kz_services_payment_tokens:delete(CustomerIdOrServices, <<"braintree">>, Token));
delete_services_card(CustomerIdOrServices, ?NE_BINARY = CardId) ->
    save_services(kz_services_payment_tokens:delete(CustomerIdOrServices, <<"braintree">>, CardId)).

-spec save_services(kz_services:services()) -> {'ok' | 'error', kz_services:services()}.
save_services(Services) ->
    case kz_services:is_dirty(
           kz_services:save_services_jobj(Services)
          )
    of
        'false' -> {'error', Services};
        'true' -> {'ok', Services}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bt_error_to_json(bt_error()) -> kz_json:object().
bt_error_to_json(BtError) ->
    kz_json:from_list(
      [{<<"code">>, BtError#bt_error.code}
      ,{<<"message">>, BtError#bt_error.message}
      ,{<<"attribute">>, BtError#bt_error.attribute}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec bt_verification_to_json(bt_verification()) -> kz_json:object().
bt_verification_to_json(BtVerification) ->
    kz_json:from_list(
      [{<<"verification_status">>, BtVerification#bt_verification.verification_status}
      ,{<<"processor_response_code">>, BtVerification#bt_verification.processor_response_code}
      ,{<<"processor_response_text">>, BtVerification#bt_verification.processor_response_text}
      ,{<<"cvv_response_code">>, BtVerification#bt_verification.cvv_response_code}
      ,{<<"avs_response_code">>, BtVerification#bt_verification.avs_response_code}
      ,{<<"postal_response_code">>, BtVerification#bt_verification.postal_response_code}
      ,{<<"street_response_code">>, BtVerification#bt_verification.street_response_code}
      ,{<<"gateway_rejection_reason">>, BtVerification#bt_verification.gateway_rejection_reason}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec bt_api_error_to_json(bt_api_error()) -> kz_json:object().
bt_api_error_to_json(BtApiError) ->
    kz_json:from_list(
      [{<<"errors">>, [bt_error_to_json(Error) || Error <- BtApiError#bt_api_error.errors]}
      ,{<<"verification">>, bt_verification_to_json(BtApiError#bt_api_error.verification)}
      ,{<<"message">>, BtApiError#bt_api_error.message}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec error_to_props({atom(), any()}) -> kz_term:proplist().
error_to_props({'no_payment_token'=Reason, Details}) ->
    Key = kz_term:to_binary(Reason),
    Message = kz_json:get_ne_binary_value(Key, Details, ?DEFAULT_ERROR_MESSAGE),
    [{<<"Message">>, Message}
    ,{<<"Status">>, <<"error">>}
    ,{<<"Reason">>, Key}
    ,{<<"Details">>, Details}
    ];
error_to_props({'api_error'=Reason, Error}) ->
    Key = kz_term:to_binary(Reason),
    Details = kz_json:get_json_value(Key, Error, kz_json:new()),
    BraintreeErrors = kz_json:get_value(<<"errors">>, Details, []),
    case is_missing_payment_token(BraintreeErrors) of
        'true' ->
            [{<<"Message">>, <<"No payment method available">>}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, <<"no_payment_token">>}
            ,{<<"Details">>, Details}
            ];
        'false' ->
            Message = kz_json:get_ne_binary_value(<<"message">>
                                                 ,Details
                                                 ,?DEFAULT_ERROR_MESSAGE
                                                 ),
            [{<<"Message">>, Message}
            ,{<<"Status">>, <<"error">>}
            ,{<<"Reason">>, Key}
            ,{<<"Details">>, Details}
            ]
    end;
error_to_props({'min_amount'=Reason, Error}) ->
    Key = kz_term:to_binary(Reason),
    Message = kz_json:get_ne_binary_value(Key, Error, ?DEFAULT_ERROR_MESSAGE),
    [{<<"Message">>, Message}
    ,{<<"Status">>, <<"error">>}
    ,{<<"Reason">>, Key}
    ];
error_to_props({'max_amount'=Reason, Error}) ->
    Key = kz_term:to_binary(Reason),
    Message = kz_json:get_ne_binary_value(Key, Error, ?DEFAULT_ERROR_MESSAGE),
    [{<<"Message">>, Message}
    ,{<<"Status">>, <<"error">>}
    ,{<<"Reason">>, Key}
    ];
error_to_props({Reason, Error}) ->
    Key = kz_term:to_binary(Reason),
    Message = kz_json:get_ne_binary_value(Key, Error, ?DEFAULT_ERROR_MESSAGE),
    [{<<"Message">>, Message}
    ,{<<"Status">>, <<"fatal">>}
    ,{<<"Reason">>, Key}
    ].

-spec is_missing_payment_token(kz_json:objects()) -> boolean().
is_missing_payment_token(BraintreeErrors) ->
    lists:any(fun(JObj) ->
                      Code = kz_json:get_integer_value(<<"code">>, JObj, 0),
                      lists:member(Code, ?NO_PAYMENT_TOKEN_CODES)
              end
             ,BraintreeErrors
             ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_no_payment_token() -> no_return().
error_no_payment_token() ->
    Error = <<"No credit card found">>,
    lager:debug("~s", [Error]),
    throw({'no_payment_token', kz_json:from_list([{<<"no_payment_token">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_authentication() -> no_return().
error_authentication() ->
    Error = <<"Failed to authenticate with the card processor">>,
    lager:debug("~s", [Error]),
    throw({'authentication', kz_json:from_list([{<<"authentication">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_authorization() -> no_return().
error_authorization() ->
    Error = <<"Failed to authorize with the card processor">>,
    lager:debug("~s", [Error]),
    throw({'authorization', kz_json:from_list([{<<"authorization">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_not_found(binary()) -> no_return().
error_not_found(<<>>) ->
    error_not_found(<<"object">>);
error_not_found(Object) ->
    Error = <<Object/binary, " not found">>,
    lager:debug("~s", [Error]),
    throw({'not_found', kz_json:from_list([{<<"not_found">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_upgrade_required() -> no_return().
error_upgrade_required() ->
    Error = <<"Card processor requires API library upgrade">>,
    lager:warning("~s", [Error]),
    throw({'upgrade_required', kz_json:from_list([{<<"upgrade_required">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_server_error() -> no_return().
error_server_error() ->
    Error = <<"Card processor server error">>,
    lager:debug("~s", [Error]),
    throw({'server_error', kz_json:from_list([{<<"server_error">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_maintenance() -> no_return().
error_maintenance() ->
    Error = <<"Card processor currently down for maintenance">>,
    lager:debug("~s", [Error]),
    throw({'maintenance', kz_json:from_list([{<<"maintenance">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_api(bt_api_error()) -> no_return().
error_api(ApiError) ->
    JObj = bt_api_error_to_json(ApiError),
    lager:debug("~s", [kz_json:encode(JObj)]),
    throw({'api_error', kz_json:from_list([{<<"api_error">>, JObj}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_io_fault() -> no_return().
error_io_fault() ->
    Error = <<"Unable to establish communication with card processor">>,
    lager:debug("~s", [Error]),
    throw({'io_fault', kz_json:from_list([{<<"io_fault">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-define(DOLLAR_SIGN, 36).

-spec error_min_amount(number() | kz_term:ne_binary()) -> no_return().
error_min_amount(Amount) ->
    Error = <<"Unable to process a transaction for less than ", ?DOLLAR_SIGN, (kz_term:to_binary(Amount))/binary>>,
    lager:debug("~s", [Error]),
    throw({'min_amount', kz_json:from_list([{<<"min_amount">>, Error}])}).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec error_max_amount(number() | kz_term:ne_binary()) -> no_return().
error_max_amount(Amount) ->
    Error = <<"Unable to process a transaction for more than ", ?DOLLAR_SIGN, (kz_term:to_binary(Amount))/binary>>,
    lager:debug("~s", [Error]),
    throw({'max_amount', kz_json:from_list([{<<"max_amount">>, Error}])}).
