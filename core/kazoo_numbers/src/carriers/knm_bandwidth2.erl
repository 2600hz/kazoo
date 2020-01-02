%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Handle client requests for phone_number documents using new bandwidth api
%%% @author Karl Anderson
%%% @author Mark Magnusson
%%% @author Pierre Fenoll
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_bandwidth2).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

%% Maintenance commands
-export([sites/0, peers/1]).

-include("knm.hrl").

-define(KNM_BW2_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".bandwidth2">>).

-define(BW2_DEBUG, kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"debug">>, 'false')).
-define(BW2_DEBUG_FILE, "/tmp/bandwidth2.com.xml").
-define(DEBUG_WRITE(Format, Args),
        _ = ?BW2_DEBUG
        andalso file:write_file(?BW2_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?BW2_DEBUG
        andalso file:write_file(?BW2_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).

-define(BW2_BASE_URL, "https://api.inetwork.com/v1.0").

-define(BW2_ACCOUNT_ID
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"account_id">>, "")
       ).

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?KNM_BW2_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).
-define(BW2_ORDER_NAME_PREFIX
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"order_name_prefix">>, "Kazoo")
       ).

-define(BW2_API_USERNAME
       ,kapps_config:get_binary(?KNM_BW2_CONFIG_CAT, <<"api_username">>, <<>>)
       ).
-define(BW2_API_PASSWORD
       ,kapps_config:get_binary(?KNM_BW2_CONFIG_CAT, <<"api_password">>, <<>>)
       ).
-define(BW2_SIP_PEER
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"sip_peer">>, "")
       ).
-define(BW2_SITE_ID
       ,kapps_config:get_string(?KNM_BW2_CONFIG_CAT, <<"site_id">>, "")
       ).

-define(MAX_SEARCH_QUANTITY
       ,kapps_config:get_pos_integer(?KNM_BW2_CONFIG_CAT, <<"max_search_quantity">>, 500)
       ).

-define(BW2_ORDER_POLL_INTERVAL, 2000).

-define(ORDER_NUMBER_XPATH, "ExistingTelephoneNumberOrderType/TelephoneNumberList/TelephoneNumber/text()").
-define(CUSTOMER_ORDER_ID_XPATH, "CustomerOrderId/text()").
-define(ORDER_NAME_XPATH, "Name/text()").
-define(BW_ORDER_ID_XPATH, "Order/id/text()").
-define(BW_ORDER_STATUS_XPATH, "OrderStatus/text()").

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

-spec is_number_billable(knm_phone_number:record()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {ok, kz_json:object()} |
          {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.

%%------------------------------------------------------------------------------
%% @doc Query the Bandwidth.com system for a quantity of available numbers
%% in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) -> knm_search:mod_response().
find_numbers(<<"+", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);

find_numbers(<<"1", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);

find_numbers(<<Prefix:3/binary, _/binary>>=Num, Quantity, Options) when ?IS_US_TOLLFREE(Prefix) ->
    <<_:1/binary, Wildcard/binary>> = Prefix,
    Params = ["tollFreeWildCardPattern=", binary_to_list(Wildcard), "*"
              "&enableTNDetail=true&quantity=", quantity_uri_param(Quantity)
             ],
    Result = search(Num, Params),
    process_tollfree_search_response(Result, Options);

find_numbers(<<Prefix:3/binary, _/binary>>=Num, Quantity, Options) when ?IS_US_TOLLFREE_WILDCARD(Prefix) ->
    Params = ["tollFreeWildCardPattern=", binary_to_list(Prefix),
              "&enableTNDetail=false&quantity=", quantity_uri_param(Quantity)
             ],
    Result = search(Num, Params),
    process_tollfree_search_response(Result, Options);

find_numbers(<<NPA:3/binary>>, Quantity, Options) ->
    Params = ["areaCode=", binary_to_list(NPA)
             ,"&enableTNDetail=false&quantity=", quantity_uri_param(Quantity)
             ],
    Result = search(NPA, Params),
    process_search_response(Result, Options);

find_numbers(Search, Quantity, Options) ->
    NpaNxx = kz_binary:truncate_right(Search, 6),
    Params = ["npaNxx=", binary_to_list(NpaNxx)
             ,"&enableTNDetail=false&quantity=", quantity_uri_param(Quantity)
             ],
    Result = search(Search, Params),
    process_search_response(Result, Options).

-spec process_tollfree_search_response(kz_types:xml_el(), knm_search:options()) -> {'ok', knm_search:results()}.
process_tollfree_search_response(Result, Options) ->
    QID = knm_search:query_id(Options),
    Found = [N
             || X <- xmerl_xpath:string("TelephoneNumberList/TelephoneNumber", Result),
                N <- [build_tollfree_number_search_result(X, QID)]
            ],
    {'ok', Found}.

-spec process_search_response(kz_types:xml_el(), knm_search:options()) -> {'ok', knm_search:results()}.
process_search_response(Result, Options) ->
    QID = knm_search:query_id(Options),
    Found = [N
             || X <- xmerl_xpath:string("TelephoneNumberList/TelephoneNumber", Result),
                N <- [build_number_search_result(X, QID)]
            ],
    {'ok', Found}.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PhoneNumber) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PhoneNumber;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PhoneNumber);
        'true' ->
            Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
            ON = lists:flatten([?BW2_ORDER_NAME_PREFIX, "-", integer_to_list(kz_time:now_s())]),
            AuthBy = knm_phone_number:auth_by(PhoneNumber),

            Props = [{'Name', [ON]}
                    ,{'CustomerOrderId', [kz_term:to_list(AuthBy)]}
                    ,{'SiteId', [?BW2_SITE_ID]}
                    ,{'PeerId', [?BW2_SIP_PEER]}
                    ,{'ExistingTelephoneNumberOrderType',
                      [{'TelephoneNumberList', [{'TelephoneNumber', [binary_to_list(Num)]}]}
                      ]}
                    ],
            Body = xmerl:export_simple([{'Order', Props}], 'xmerl_xml'),

            case api_post(url(["orders"]), Body) of
                {'error', Reason} ->
                    Error = <<"Unable to acquire number: ", (kz_term:to_binary(Reason))/binary>>,
                    knm_errors:by_carrier(?MODULE, Error, PhoneNumber);
                {'ok', Xml} ->
                    Response = xmerl_xpath:string("Order", Xml),
                    OrderId = kz_xml:get_value(?BW_ORDER_ID_XPATH, Xml),
                    OrderStatus = kz_xml:get_value(?BW_ORDER_STATUS_XPATH, Xml),
                    check_order(OrderId, OrderStatus, Response, PhoneNumber)
            end
    end.

-spec check_order(kz_term:api_binary(), kz_term:ne_binary(), kz_types:xml_el(), knm_phone_number:record()) ->
          knm_phone_number:record().
check_order(OrderId, <<"RECEIVED">>, _Response, PhoneNumber) ->
    timer:sleep(?BW2_ORDER_POLL_INTERVAL),
    Url = ["orders/", kz_term:to_list(OrderId)],
    case api_get(url(Url)) of
        {'error', Reason} ->
            Error = <<"Unable to acquire number: ", (kz_term:to_binary(Reason))/binary>>,
            Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
            knm_errors:by_carrier(?MODULE, Error, Num);
        {'ok', Xml} ->
            Response = xmerl_xpath:string("Order", Xml),
            OrderStatus = kz_xml:get_value(?BW_ORDER_STATUS_XPATH, Xml),
            check_order(OrderId, OrderStatus, Response, PhoneNumber)
    end;

check_order(_OrderId, <<"COMPLETE">>, Response, PhoneNumber) ->
    OrderData = number_order_response_to_json(Response),
    knm_phone_number:update_carrier_data(PhoneNumber, OrderData);

check_order(_OrderId, <<"FAILED">>, _Response, PhoneNumber) ->
    Reason = <<"FAILED">>,
    Error = <<"Unable to acquire number: ", (kz_term:to_binary(Reason))/binary>>,
    Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num);

check_order(_OrderId, OrderStatus, _Response, PhoneNumber) ->
    Error = <<"Unable to acquire number: ", (kz_term:to_binary(OrderStatus))/binary>>,
    Num = to_bandwidth2(knm_phone_number:number(PhoneNumber)),
    knm_errors:by_carrier(?MODULE, Error, Num).

-spec to_bandwidth2(kz_term:ne_binary()) -> kz_term:ne_binary().
to_bandwidth2(<<"+1", Number/binary>>) -> Number;
to_bandwidth2(Number) -> Number.

-spec from_bandwidth2(kz_term:ne_binary()) -> kz_term:ne_binary().
from_bandwidth2(<<"+", _/binary>> = Number) -> Number;
from_bandwidth2(Number) -> <<"+1", Number/binary>>.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) -> knm_phone_number:record().
disconnect_number(_Number) -> _Number.


-spec sites() -> 'ok'.
sites() ->
    {'ok', Xml} = api_get(url(["sites"])),
    io:format("listing all sites for account ~p~n", [?BW2_ACCOUNT_ID]),
    Sites = xmerl_xpath:string("Sites/Site", Xml),
    lists:foreach(fun process_site/1, Sites),
    io:format("done.~n").

-spec process_site(kz_types:xml_el()) -> 'ok'.
process_site(Site) ->
    Id   = kz_xml:get_value("Id/text()", Site),
    Name = kz_xml:get_value("Name/text()", Site),
    io:format("Id: ~p Name: ~p~n", [Id, Name]).

-spec peers(binary()) -> 'ok'.
peers(SiteId) ->
    {'ok', Xml} = api_get(url(["sippeers"])),
    io:format("listing all peers for account ~p, site ~p~n", [?BW2_ACCOUNT_ID, SiteId]),
    Peers = xmerl_xpath:string("SipPeers/SipPeer", Xml),
    lists:foreach(fun process_peer/1, Peers),
    io:format("done.~n").

-spec process_peer(kz_types:xml_el()) -> 'ok'.
process_peer(Peer) ->
    Id   = kz_xml:get_value("PeerId/text()", Peer),
    Name = kz_xml:get_value("PeerName/text()", Peer),
    io:format("Id: ~p Name: ~p~n", [Id, Name]).

%%% Internals

-spec url([nonempty_string()]) -> nonempty_string().
url(RelativePath) ->
    lists:flatten(
      [io_lib:format("~s/accounts/~s/", [?BW2_BASE_URL, ?BW2_ACCOUNT_ID])
       | RelativePath
      ]).

-type api_res() :: {'ok', kz_types:xml_el()} | {'error', atom()}.

-spec search(kz_term:ne_binary(), [nonempty_string()]) -> kz_types:xml_el().
search(Num, Params) ->
    case api_get(url(["availableNumbers?" | Params])) of
        {'ok', Results} -> Results;
        {'error', Reason} -> knm_errors:by_carrier(?MODULE, Reason, Num)
    end.

-ifndef(TEST).
-spec auth() -> {'basic_auth', {kz_term:ne_binary(), kz_term:ne_binary()}}.
auth() ->
    {'basic_auth', {?BW2_API_USERNAME, ?BW2_API_PASSWORD}}.

-spec api_get(nonempty_string()) -> api_res().
api_get(Url) ->
    HTTPOptions = [auth()
                  ],
    ?DEBUG_WRITE("Request:~n~s ~s~n~p~n", ['get', Url, HTTPOptions]),
    Response = kz_http:get(Url, [], HTTPOptions),
    handle_response(Response).

-spec api_post(nonempty_string(), binary()) -> api_res().
api_post(Url, Body) ->
    UnicodeBody = unicode:characters_to_binary(Body),
    Headers = [{"Accept", "*/*"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"X-BWC-IN-Control-Processing-Type", "process"}
              ,{"Content-Type", "application/xml"}
              ],
    HTTPOptions = [auth()
                  ,{'ssl', [{'verify', 'verify_none'}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'body_format', 'string'}
                  ],
    ?DEBUG_WRITE("Request:~n~s ~s~n~p~n~p~n~p~p~n", ['post', Url, Headers, HTTPOptions, Body, UnicodeBody]),
    Response = kz_http:post(Url, Headers, UnicodeBody, HTTPOptions),
    handle_response(Response).
-else.
-spec api_get(nonempty_string()) -> api_res().
api_get("https://api.inetwork.com/v1.0/accounts//availableNumbers?areaCode="++_) ->
    Resp = knm_util:fixture("bandwidth2_find_by_npa_no_detail.xml"),
    handle_response({'ok', 200, [], Resp});
api_get("https://api.inetwork.com/v1.0/accounts//availableNumbers?tollFreeWildCardPattern="++_) ->
    Resp = knm_util:fixture("bandwidth2_find_tollfree.xml"),
    handle_response({'ok', 200, [], Resp});
api_get("https://api.inetwork.com/v1.0/accounts//orders/" ++ _) ->
    Resp = knm_util:fixture("bandwidth2_check_order.xml"),
    handle_response({'ok', 200, [], Resp}).

-spec api_post(nonempty_string(), binary()) -> api_res().
api_post("https://api.inetwork.com/v1.0/accounts//orders", Body) ->
    _UnicodeBody = unicode:characters_to_binary(Body),
    Resp = knm_util:fixture("bandwidth2_buy_a_number.xml"),
    handle_response({'ok', 200, [], Resp}).
-endif.

%%------------------------------------------------------------------------------
%% @doc Make a REST request to Bandwidth.com Numbers API to perform the
%% given verb (purchase, search, provision, etc).
%% @end
%%------------------------------------------------------------------------------
-spec handle_response(kz_http:ret()) -> api_res().
handle_response({Result, Code, Props, Response})
  when is_binary(Response) ->
    handle_response({Result, Code, Props, kz_term:to_list(Response)});
handle_response({'ok', 401, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n401~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
    {'error', 'authentication'};

handle_response({'ok', 403, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n403~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 403 (unauthorized)"),
    {'error', 'authorization'};

handle_response({'ok', 404, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n404~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 404 (not found)"),
    {'error', 'not_found'};

handle_response({'ok', 500, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n500~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 500 (server error)"),
    {'error', 'server_error'};

handle_response({'ok', 503, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n503~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 503"),
    {'error', 'server_error'};

handle_response({'ok', _Code, _Headers, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~nCode : ~p~nBody : ~s~nHeaders : ~p~n", [_Code, Response, _Headers]),
    lager:debug("received response from bandwidth.com"),
    try
        {Xml, _} = xmerl_scan:string(Response),
        verify_response(Xml)
    catch
        _:R ->
            lager:debug("failed to decode xml: ~p", [R]),
            {'error', 'empty_response'}
    end;

handle_response({'ok', Code, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~s~n", [Code, _Response]),
    lager:debug("bandwidth.com empty response: ~p", [Code]),
    {'error', 'empty_response'};

handle_response({'error', _}=E) ->
    lager:debug("bandwidth.com request error: ~p", [E]),
    E.

%%------------------------------------------------------------------------------
%% @doc Convert a number order response to json
%% @end
%%------------------------------------------------------------------------------
-spec number_order_response_to_json(kz_types:xml_els() | kz_types:xml_el()) -> kz_json:object().
number_order_response_to_json([]) ->
    kz_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    Num = from_bandwidth2(kz_xml:get_value(?ORDER_NUMBER_XPATH, Xml)),
    kz_json:from_list(
      props:filter_empty(
        [{<<"order_id">>, kz_xml:get_value(?CUSTOMER_ORDER_ID_XPATH, Xml)}
        ,{<<"order_name">>, kz_xml:get_value(?ORDER_NAME_XPATH, Xml)}
        ,{<<"number">>, Num}
        ]
       )
     ).

-spec build_number_search_result(kz_types:xml_els() | kz_types:xml_el(), kz_term:ne_binary()) ->
          knm_search:result().
build_number_search_result([Xml], QID) ->
    build_number_search_result(Xml, QID);
build_number_search_result(Xml, QID) ->
    Num = from_bandwidth2(kz_xml:get_value("//TelephoneNumber/text()", Xml)),
    JObj = kz_json:from_list(
             props:filter_empty(
               [{<<"rate_center">>, rate_center_to_json(Xml)}]
              )
            ),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}.

-spec build_tollfree_number_search_result(kz_types:xml_el(), kz_term:ne_binary()) ->
          knm_search:result().
build_tollfree_number_search_result(Xml, QID) ->
    Num = from_bandwidth2(kz_xml:get_value("//TelephoneNumber/text()", Xml)),
    {QID, {Num, ?MODULE, ?NUMBER_STATE_DISCOVERY, kz_json:new()}}.

%%------------------------------------------------------------------------------
%% @doc Convert a rate center XML entity to json
%% @end
%%------------------------------------------------------------------------------
-spec rate_center_to_json(kz_types:xml_els() | kz_types:xml_el()) -> kz_json:object().
rate_center_to_json([]) ->
    kz_json:new();
rate_center_to_json(Xml) ->
    kz_json:from_list(
      props:filter_empty(
        [{<<"name">>, kz_xml:get_value("//RateCenter/text()", Xml)}
        ,{<<"lata">>, kz_xml:get_value("//LATA/text()", Xml)}
        ,{<<"state">>, kz_xml:get_value("//State/text()", Xml)}
        ]
       )
     ).

%%------------------------------------------------------------------------------
%% @doc Determine if the request was successful, and if not extract any
%% error text
%% @end
%%------------------------------------------------------------------------------
-spec verify_response(kz_types:xml_el()) -> {'ok', kz_types:xml_el()} |
          {'error', any()}.
verify_response(Xml) ->
    NPAPath = "count(//TelephoneNumberDetailList/TelephoneNumberDetail)",
    TollFreePath = "count(//TelephoneNumberList/TelephoneNumber)",
    SitesPath = "count(//SitesResponse/Sites/Site)",
    PeersPath = "count(//TNSipPeersResponse/SipPeers/SipPeer)",
    case validate_xpath_value(xmerl_xpath:string(NPAPath, Xml))
        orelse validate_xpath_value(xmerl_xpath:string(TollFreePath, Xml))
        orelse validate_xpath_value(xmerl_xpath:string(SitesPath, Xml))
        orelse validate_xpath_value(xmerl_xpath:string(PeersPath, Xml))
        orelse validate_xpath_value(kz_xml:get_value("//OrderStatus/text()", Xml))
    of
        'true' ->
            lager:debug("request was successful"),
            {'ok', Xml};
        'false' ->
            ErrorPath = "//ErrorList/Error/Description/text()",
            Reason = case kz_xml:get_value(ErrorPath, Xml) of
                         'undefined' -> <<"Number not found">>;
                         R -> R
                     end,
            lager:debug("request failed: ~s", [Reason]),
            {'error', Reason}
    end.

-spec validate_xpath_value(kz_term:api_binary() | {atom(), atom(), non_neg_integer()}) -> boolean().
validate_xpath_value('undefined') -> 'false';
validate_xpath_value(<<>>) -> 'false';
validate_xpath_value({'xmlObj', 'number', Num}) -> Num > 0;
validate_xpath_value(_) -> 'true'.

-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

-spec quantity_uri_param(integer()) -> string().
quantity_uri_param(Q) -> integer_to_list(min(Q, ?MAX_SEARCH_QUANTITY)).
