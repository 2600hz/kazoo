%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

-define(KNM_BW_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".bandwidth">>).

-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE
       ,[{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
        ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
        ,{'xmlns', "http://www.bandwidth.com/api/"}
        ]).
-define(BW_NUMBER_URL
       ,kapps_config:get_string(?KNM_BW_CONFIG_CAT
                               ,<<"numbers_api_url">>
                               ,"https://api.bandwidth.com/public/v2/numbers.api"
                               )
       ).

-define(BW_CDR_URL
       ,kapps_config:get_string(?KNM_BW_CONFIG_CAT
                               ,<<"cdrs_api_url">>
                               ,"https://api.bandwidth.com/api/public/v2/cdrs.api"
                               )
       ).

-ifdef(TEST).
-define(NPAN_RESPONSE, knm_util:fixture("bandwidth_numbersearch_response.xml")).
-define(AREACODE_RESPONSE, knm_util:fixture("bandwidth_areacode_response.xml")).

-define(BW_DEBUG(_Format, _Args), ok).
-else.
-define(BW_DEBUG, kapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"debug">>, 'false')).
-define(BW_DEBUG_FILE, "/tmp/bandwidth.com.xml").
-define(BW_DEBUG(Format, Args),
        _ = ?BW_DEBUG
        andalso file:write_file(?BW_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).
-endif.

-define(IS_SANDBOX_PROVISIONING_TRUE,
        kapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')).
-define(IS_PROVISIONING_ENABLED,
        kapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"enable_provisioning">>, 'true')).
-define(BW_ORDER_NAME_PREFIX,
        kapps_config:get_string(?KNM_BW_CONFIG_CAT, <<"order_name_prefix">>, "Kazoo")).

-define(BW_ENDPOINTS, kapps_config:get(?KNM_BW_CONFIG_CAT, <<"endpoints">>)).
-define(BW_DEVELOPER_KEY, kapps_config:get_string(?KNM_BW_CONFIG_CAT, <<"developer_key">>, "")).


%%% API

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query the Bandwidth.com system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) -> knm_search:mod_response().
find_numbers(<<"+", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<"1", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<NPA:3/binary>>, Quantity, Options) ->
    Props = [{'areaCode', [kz_term:to_list(NPA)]}
            ,{'maxQuantity', [kz_term:to_list(Quantity)]}
            ],
    case make_numbers_request('areaCodeNumberSearch', Props) of
        {'error', _}=E -> E;
        {'ok', Xml} -> process_numbers_search_resp(Xml, Options)
    end;
find_numbers(Search, Quantity, Options) ->
    NpaNxx = kz_binary:truncate_right(Search, 6),
    Props = [{'npaNxx', [kz_term:to_list(NpaNxx)]}
            ,{'maxQuantity', [kz_term:to_list(Quantity)]}
            ],
    case make_numbers_request('npaNxxNumberSearch', Props) of
        {'error', _}=E -> E;
        {'ok', Xml} -> process_numbers_search_resp(Xml, Options)
    end.

-spec process_numbers_search_resp(kz_types:xml_el(), knm_search:options()) ->
          {'ok', knm_search:results()}.
process_numbers_search_resp(Xml, Options) ->
    TelephoneNumbers = "/numberSearchResponse/telephoneNumbers/telephoneNumber",
    QID = knm_search:query_id(Options),
    {'ok', [SearchResp
            || Number <- xmerl_xpath:string(TelephoneNumbers, Xml),
               SearchResp <- [found_number_to_search_response(Number, QID)]
           ]
    }.

-spec found_number_to_search_response(kz_types:xml_el() | kz_types:xml_els(), kz_term:ne_binary()) ->
          knm_search:result().
found_number_to_search_response(Found, QID) ->
    JObj = number_search_response_to_json(Found),
    Num = kz_json:get_value(<<"e164">>, JObj),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) ->
          knm_phone_number:record().
acquire_number(PN) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PN;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PN);
        'true' ->
            acquire_and_provision_number(PN)
    end.

-spec acquire_and_provision_number(knm_phone_number:record()) ->
          knm_phone_number:record().
acquire_and_provision_number(PN) ->
    AuthBy = knm_phone_number:auth_by(PN),
    AssignedTo = knm_phone_number:assigned_to(PN),
    Id = kz_json:get_string_value(<<"number_id">>, knm_phone_number:carrier_data(PN)),
    Hosts = case ?BW_ENDPOINTS of
                'undefined' -> [];
                Endpoint when is_binary(Endpoint) ->
                    [{'endPoints', [{'host', [kz_term:to_list(Endpoint)]}]}];
                Endpoints ->
                    [{'endPoints', [{'host', [kz_term:to_list(E)]} || E <- Endpoints]}]
            end,
    OrderName = lists:flatten([?BW_ORDER_NAME_PREFIX, "-", integer_to_list(kz_time:now_s())]),
    AcquireFor = case kz_term:is_empty(AssignedTo) of
                     'true' -> "no_assigned_account";
                     'false' -> binary_to_list(AssignedTo)
                 end,
    Props = [{'orderName', [OrderName]}
            ,{'extRefID', [binary_to_list(AuthBy)]}
            ,{'numberIDs', [{'id', [Id]}]}
            ,{'subscriber', [kz_term:to_list(AcquireFor)]}
             | Hosts
            ],
    case make_numbers_request('basicNumberOrder', Props) of
        {'error', Error} ->
            knm_errors:by_carrier(?MODULE, Error, PN);
        {'ok', Xml} ->
            Response = xmerl_xpath:string("/numberOrderResponse/numberOrder", Xml),
            Data = number_order_response_to_json(Response),
            knm_phone_number:set_carrier_data(PN, Data)
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(PN) -> PN.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_PN) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Make a REST request to Bandwidth.com Numbers API to preform the
%% given verb (purchase, search, provision, etc).
%% @end
%%------------------------------------------------------------------------------
-spec make_numbers_request(atom(), kz_term:proplist()) ->
          {'ok', kz_types:xml_el() | atom()} |
          {'error', kz_term:api_binary()}.

-ifdef(TEST).
make_numbers_request('npaNxxNumberSearch', _Props) ->
    {Xml, _} = xmerl_scan:string(?NPAN_RESPONSE),
    verify_response(Xml);
make_numbers_request('areaCodeNumberSearch', _Props) ->
    {Xml, _} = xmerl_scan:string(?AREACODE_RESPONSE),
    verify_response(Xml).
-else.
make_numbers_request(Verb, Props) ->
    lager:debug("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    Request = [{'developerKey', [?BW_DEVELOPER_KEY]}
               | Props
              ],
    Body = unicode:characters_to_binary(
             xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                                ,'xmerl_xml'
                                ,[{'prolog', ?BW_XML_PROLOG}]
                                )
            ),
    Headers = [{"Accept", "*/*"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"X-BWC-IN-Control-Processing-Type", "process"}
              ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  , {'body_format', 'string'}
                  ],
    ?BW_DEBUG("Request:~n~s ~s~n~s~n", ['post', ?BW_NUMBER_URL, Body]),
    case kz_http:post(?BW_NUMBER_URL, Headers, Body, HTTPOptions) of
        {'ok', 401, _, _Response} ->
            ?BW_DEBUG("Response:~n401~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', 403, _, _Response} ->
            ?BW_DEBUG("Response:~n403~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', 404, _, _Response} ->
            ?BW_DEBUG("Response:~n404~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', 500, _, _Response} ->
            ?BW_DEBUG("Response:~n500~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', 503, _, _Response} ->
            ?BW_DEBUG("Response:~n503~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 503"),
            {'error', 'server_error'};
        {'ok', Code, _, "<?xml"++_=Response} ->
            ?BW_DEBUG("Response:~n~p~n~s~n", [Code, Response]),
            lager:debug("received response from bandwidth.com"),
            try
                {Xml, _} = xmerl_scan:string(Response),
                verify_response(Xml)
            catch
                _:R ->
                    lager:debug("failed to decode xml: ~p", [R]),
                    {'error', 'empty_response'}
            end;
        {'ok', Code, _, _Response} ->
            ?BW_DEBUG("Response:~n~p~n~s~n", [Code, _Response]),
            lager:debug("bandwidth.com empty response: ~p", [Code]),
            {'error', 'empty_response'};
        {'error', _}=E ->
            lager:debug("bandwidth.com request error: ~p", [E]),
            E
    end.
-endif.

%%------------------------------------------------------------------------------
%% @doc Convert a number order response to json.
%% @end
%%------------------------------------------------------------------------------
-spec number_order_response_to_json(any()) -> kz_json:object().
number_order_response_to_json([]) ->
    kz_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    kz_json:from_list(
      [{<<"order_id">>, get_cleaned("orderID/text()", Xml)}
      ,{<<"order_number">>, get_cleaned("orderNumber/text()", Xml)}
      ,{<<"order_name">>, get_cleaned("orderName/text()", Xml)}
      ,{<<"ext_ref_id">>, get_cleaned("extRefID/text()", Xml)}
      ,{<<"accountID">>, get_cleaned("accountID/text()", Xml)}
      ,{<<"accountName">>, get_cleaned("accountName/text()", Xml)}
      ,{<<"quantity">>, get_cleaned("quantity/text()", Xml)}
      ,{<<"number">>, number_search_response_to_json(
                        xmerl_xpath:string("telephoneNumbers/telephoneNumber", Xml)
                       )
       }
      ]).

%%------------------------------------------------------------------------------
%% @doc Convert a number search response XML entity to json
%% @end
%%------------------------------------------------------------------------------
-spec number_search_response_to_json(kz_types:xml_el() | kz_types:xml_els()) -> kz_json:object().
number_search_response_to_json([]) ->
    kz_json:new();
number_search_response_to_json([Xml]) ->
    number_search_response_to_json(Xml);
number_search_response_to_json(Xml) ->
    kz_json:from_list(
      [{<<"number_id">>, get_cleaned("numberID/text()", Xml)}
      ,{<<"ten_digit">>, get_cleaned("tenDigit/text()", Xml)}
      ,{<<"formatted_number">>, get_cleaned("formattedNumber/text()", Xml)}
      ,{<<"e164">>, get_cleaned("e164/text()", Xml)}
      ,{<<"npa_nxx">>, get_cleaned("npaNxx/text()", Xml)}
      ,{<<"status">>, get_cleaned("status/text()", Xml)}
      ,{<<"rate_center">>, rate_center_to_json(xmerl_xpath:string("rateCenter", Xml))}
      ]).

-spec get_cleaned(kz_term:deeplist(), kz_types:xml_el()) -> kz_term:api_binary().
get_cleaned(Path, Xml) ->
    case kz_xml:get_value(Path, Xml) of
        'undefined' -> 'undefined';
        V -> kz_binary:strip(V, [$\s, $\n])
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a rate center XML entity to json
%% @end
%%------------------------------------------------------------------------------
-spec rate_center_to_json(list()) -> kz_json:object().
rate_center_to_json([]) ->
    kz_json:new();
rate_center_to_json([Xml]) ->
    rate_center_to_json(Xml);
rate_center_to_json(Xml) ->
    kz_json:from_list(
      [{<<"name">>, get_cleaned("name/text()", Xml)}
      ,{<<"lata">>, get_cleaned("lata/text()", Xml)}
      ,{<<"state">>, get_cleaned("state/text()", Xml)}
      ]).

%%------------------------------------------------------------------------------
%% @doc Determine if the request was successful, and if not extract any
%% error text
%% @end
%%------------------------------------------------------------------------------
-spec verify_response(kz_types:xml_el()) ->
          {'ok', kz_types:xml_el()} |
          {'error', kz_term:api_binary()}.
verify_response(Xml) ->
    case get_cleaned("/*/status/text()", Xml) of
        <<"success">> ->
            lager:debug("request was successful"),
            {'ok', Xml};
        _ ->
            lager:debug("request failed"),
            {'error', get_cleaned("/*/errors/error/message/text()", Xml)}
    end.
