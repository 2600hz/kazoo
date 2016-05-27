%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_bandwidth).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../wnm.hrl").

-define(WNM_BW_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".bandwidth">>).

-define(BW_DEBUG, whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"debug">>, 'false')).
-define(BW_DEBUG_FILE, "/tmp/bandwidth.com.xml").
-define(DEBUG_WRITE(Format, Args),
        _ = ?BW_DEBUG andalso
            file:write_file(?BW_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?BW_DEBUG andalso
            file:write_file(?BW_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).

-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE, [{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
                           ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
                           ,{'xmlns', "http://www.bandwidth.com/api/"}
                          ]).

-define(BW_DEVELOPER_KEY, whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"developer_key">>, "")).
-define(BW_NUMBER_URL, whapps_config:get_string(?WNM_BW_CONFIG_CAT
                                                ,<<"numbers_api_url">>
                                                ,"https://api.bandwidth.com/public/v2/numbers.api")).
-define(BW_CDR_URL, whapps_config:get_string(?WNM_BW_CONFIG_CAT
                                             ,<<"cdrs_api_url">>
                                             ,"https://api.bandwidth.com/api/public/v2/cdrs.api")).

-define(IS_SANDBOX_PROVISIONING_TRUE,
        whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')).
-define(IS_PROVISIONING_ENABLED,
        whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"enable_provisioning">>, 'true')).
-define(GET_BW_ENDPOINTS, whapps_config:get(?WNM_BW_CONFIG_CAT, <<"endpoints">>, [])).
-define(BW_ORDER_NAME_PREFIX,
        whapps_config:get_binary(?WNM_BW_CONFIG_CAT, <<"order_name_prefix">>, <<"Kazoo">>)).

-define(IS_US_TOLLFREE(Prefix),
        Prefix == <<"800">> orelse
        Prefix == <<"822">> orelse
        Prefix == <<"833">> orelse
        Prefix == <<"844">> orelse
        Prefix == <<"855">> orelse
        Prefix == <<"866">> orelse
        Prefix == <<"877">> orelse
        Prefix == <<"880">> orelse
        Prefix == <<"881">> orelse
        Prefix == <<"882">> orelse
        Prefix == <<"883">> orelse
        Prefix == <<"884">> orelse
        Prefix == <<"885">> orelse
        Prefix == <<"886">> orelse
        Prefix == <<"887">> orelse
        Prefix == <<"888">> orelse
        Prefix == <<"889">>
       ).
-define(TOLLFREE_LOGIN,
        whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"tollfree_login">>, <<"tollfree-username">>)).
-define(TOLLFREE_PASSWORD,
        whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"tollfree_password">>, <<"tollfree-password">>)).
-define(BW_URL_V1, "https://api.bandwidth.com/public/voip/v1_1/NumberManagementService.asmx").

%% @public
-spec is_number_billable(wnm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-type search_ret() :: {'ok', wh_json:object()} | {'error', any()}.
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) -> search_ret().
find_numbers(<<"+", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<"1", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<Prefix:3/binary,_/binary>>=Num, Quantity, _) when ?IS_US_TOLLFREE(Prefix) ->
    ToSearch = case wnm_util:is_reconcilable(Num) of
                   'true' -> Num;
                   'false' -> Prefix
               end,
    do_search_tollfree(wh_util:to_list(ToSearch), Quantity);
find_numbers(<<NPA:3/binary>>, Quantity, _) ->
    Props = [{'areaCode', [wh_util:to_list(NPA)]}
             ,{'maxQuantity', [wh_util:to_list(Quantity)]}
            ],
    do_search_by('areaCodeNumberSearch', Props);
find_numbers(Search, Quantity, _) ->
    NpaNxx = wh_util:truncate_right_binary(Search, 6),
    Props = [{'npaNxx', [NpaNxx]}
             ,{'maxQuantity', [wh_util:to_list(Quantity)]}
            ],
    do_search_by('npaNxxNumberSearch', Props).

-spec do_search_by(search_by(), wh_proplist()) -> search_ret().
do_search_by(ByType, Props) ->
    case make_numbers_request(ByType, Props) of
        {'error', _}=E -> E;
        {'ok', Xml} ->
            TelephoneNumbers = "/numberSearchResponse/telephoneNumbers/telephoneNumber",
            Resp = [begin
                        JObj = number_search_response_to_json(Number),
                        Num = wh_json:get_ne_binary_value(<<"e164">>, JObj),
                        {Num, JObj}
                    end
                    || Number <- xmerl_xpath:string(TelephoneNumbers, Xml)
                   ],
            {'ok', wh_json:from_list(Resp)}
    end.

-spec do_search_tollfree(ne_binary(), pos_integer()) -> search_ret().
do_search_tollfree(TollFreePrefix, Quantity) ->
    case maybe_tollfree_login() of
        {'error', _}=E -> E;
        {'ok', SessionID} ->
            maybe_tollfree_search(SessionID, TollFreePrefix, Quantity)
    end.

-spec maybe_tollfree_login() -> {'ok', ne_binary()} | {'error', any()}.
maybe_tollfree_login() ->
    Body = tollfree_login_body(),
    AdditionalHeaders = [{"SOAPAction", "http://www.bandwidth.com/Login"}],
    case post_xml(?BW_URL_V1, Body, AdditionalHeaders) of
        {'error', _}=E -> E;
        {'ok', Xml} ->
            case wh_util:get_xml_value("//LoginResponse/LoginResult/text()", Xml) of
                'undefined' -> {'error', 'bw_login_failed'};
                SessionId -> {'ok', SessionId}
            end
    end.

-spec maybe_tollfree_search(ne_binary(), ne_binary(), pos_integer()) -> search_ret().
maybe_tollfree_search(SessionId, Prefix, Quantity) ->
    Body = tollfree_search_body(SessionId, Prefix),
    case post_xml(?BW_URL_V1, Body, []) of
        {'error', _}=E -> E;
        {'ok', Xml} ->
            Resp = [begin
                        JObj = tollfree_search_response_to_json(Number),
                        Num = wh_json:get_ne_binary_value(<<"number">>, JObj),
                        {Num, JObj}
                    end
                    || Number <- lists:sublist(xmerl_xpath:string("//TelephoneNumber", Xml), Quantity)
                   ],
            {'ok', wh_json:from_list(Resp)}
    end.

-spec tollfree_login_body() -> iolist().
tollfree_login_body() ->
    ["<?xml version='1.0' encoding='utf-8'?>"
     "<soap:Envelope xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/2001/XMLSchema' xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'>"
     "<soap:Body>"
     "<Login xmlns='http://www.bandwidth.com'>"
     "<UserName>", ?TOLLFREE_LOGIN, "</UserName>"
     "<Password>", ?TOLLFREE_PASSWORD, "</Password>"
     "</Login>"
     "</soap:Body>"
     "</soap:Envelope>"].

-spec tollfree_search_body(ne_binary(), ne_binary()) -> iolist().
tollfree_search_body(SessionId, Prefix) ->
    ["<?xml version='1.0' encoding='utf-8'?>"
     "<soap:Envelope xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/2001/XMLSchema' xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'>"
     "<soap:Body>"
     "<SearchTelephoneNumbers xmlns='http://www.bandwidth.com'>"
     "<SessionID>", xmerl_lib:export_text(SessionId), "</SessionID>"
     "<SearchType>FullNumber</SearchType>"
     "<SearchKey>", Prefix, "</SearchKey>"
     "<LoadSubscriber>false</LoadSubscriber>"
     "<LoadAddresses>false</LoadAddresses>"
     "<LoadNumberOptions>false</LoadNumberOptions>"
     "</SearchTelephoneNumbers>"
     "</soap:Body>"
     "</soap:Envelope>"].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().

acquire_number(#number{dry_run = 'true'}=Number) -> Number;
acquire_number(#number{auth_by = AuthBy
                       ,assigned_to = AssignedTo
                       ,module_data = Data
                      } = N) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            N;
        'false' ->
            Error = <<"Unable to acquire numbers on this system, carrier provisioning is disabled">>,
            wnm_number:error_carrier_fault(Error, N);
        'true' ->
            Id = wh_json:get_string_value(<<"number_id">>, Data),
            Hosts = case ?GET_BW_ENDPOINTS of
                        Endpoint when is_binary(Endpoint) ->
                            [{'endPoints', [{'host', [wh_util:to_list(Endpoint)]}]}];
                        Endpoints ->
                            [{'endPoints', [{'host', [wh_util:to_list(E)]} || E <- Endpoints]}]
                    end,
            ON = list_to_binary([?BW_ORDER_NAME_PREFIX, "-", wh_util:to_binary(wh_util:current_tstamp())]),
            ExtRef = case wh_util:is_empty(AuthBy) of
                         'true' -> "no_authorizing_account";
                         'false' -> wh_util:to_list(AuthBy)
                     end,
            AcquireFor = case wh_util:is_empty(AuthBy) of
                             'true' -> "no_assigned_account";
                             'false' -> wh_util:to_list(AssignedTo)
                         end,
            Props = [{'orderName', [wh_util:to_list(ON)]}
                     ,{'extRefID', [wh_util:to_list(ExtRef)]}
                     ,{'numberIDs', [{'id', [Id]}]}
                     ,{'subscriber', [wh_util:to_list(AcquireFor)]}
                     | Hosts
                    ],
            case make_numbers_request('basicNumberOrder', Props) of
                {'error', Reason} ->
                    Error = <<"Unable to acquire number: ", (wh_util:to_binary(Reason))/binary>>,
                    wnm_number:error_carrier_fault(Error, N);
                {'ok', Xml} ->
                    Response = xmerl_xpath:string("/numberOrderResponse/numberOrder", Xml),
                    N#number{module_data = number_order_response_to_json(Response)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to Bandwidth.com Numbers API to perform the
%% given verb (purchase, search, provision, etc).
%% @end
%%--------------------------------------------------------------------
-type search_by() :: 'npaNxxNumberSearch' |
                     'areaCodeNumberSearch' |
                     %%'tollfreeNumberSearch' | <- does not work! (use do_search_tollfree/1)
                     'getTelephoneNumber' |
                     'basicNumberOrder'.
-spec make_numbers_request(search_by(), wh_proplist()) -> bw_ret().

make_numbers_request(Verb, Props) ->
    lager:debug("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    Request = [{'developerKey', [?BW_DEVELOPER_KEY]}
               | Props],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,'xmerl_xml'
                               ,[{'prolog', ?BW_XML_PROLOG}]),
    post_xml(?BW_NUMBER_URL, Body, []).

-spec post_xml(string(), iolist(), wh_proplist()) -> bw_ret().
post_xml(Url, Body, AdditionalHeaders) ->
    Headers = AdditionalHeaders
        ++ [{"Accept", "*/*"}
            ,{"User-Agent", ?WNM_USER_AGENT}
            ,{"X-BWC-IN-Control-Processing-Type", "process"}
            ,{"Content-Type", "text/xml"}
           ],
    HTTPOptions = [{'ssl', [{'verify', 0}]}
                   ,{'inactivity_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ],
    Timeout = 180 * ?MILLISECONDS_IN_SECOND,
    ?DEBUG_WRITE("Request:~n~s ~s~n~s~n", ['post', Url, Body]),
    UnicodeBody = unicode:characters_to_binary(Body),
    Resp = ibrowse:send_req(Url, Headers, 'post', UnicodeBody, HTTPOptions, Timeout),
    handle_ibrowse_response(Resp).

-type bw_ret() :: {'ok', ne_binary()} | {'error', any()}.
-spec handle_ibrowse_response(any()) -> bw_ret().
handle_ibrowse_response({'ok', "401", _, _Response}) ->
    ?DEBUG_APPEND("Response:~n401~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
    {'error', 'authentication'};
handle_ibrowse_response({'ok', "403", _, _Response}) ->
    ?DEBUG_APPEND("Response:~n403~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 403 (unauthorized)"),
    {'error', 'authorization'};
handle_ibrowse_response({'ok', "404", _, _Response}) ->
    ?DEBUG_APPEND("Response:~n404~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 404 (not found)"),
    {'error', 'not_found'};
handle_ibrowse_response({'ok', "500", _, _Response}) ->
    ?DEBUG_APPEND("Response:~n500~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 500 (server error)"),
    {'error', 'server_error'};
handle_ibrowse_response({'ok', "503", _, _Response}) ->
    ?DEBUG_APPEND("Response:~n503~n~s~n", [_Response]),
    lager:debug("bandwidth.com request error: 503"),
    {'error', 'server_error'};
handle_ibrowse_response({'ok', Code, _, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~s~n", [Code, Response]),
    lager:debug("received response from bandwidth.com"),
    try
        {Xml, _} = xmerl_scan:string(Response),
        verify_response(Xml)
    catch
        _:R ->
            lager:debug("failed to decode xml: ~p", [R]),
            {'error', 'empty_response'}
    end;
handle_ibrowse_response({'ok', Code, _, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~s~n", [Code, _Response]),
    lager:debug("bandwidth.com empty response: ~p", [Code]),
    {'error', 'empty_response'};
handle_ibrowse_response({'error', _}=E) ->
    lager:debug("bandwidth.com request error: ~p", [E]),
    E.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number order response to json
%% @end
%%--------------------------------------------------------------------
-spec number_order_response_to_json(xml_el() | xml_els()) -> wh_json:object().
number_order_response_to_json([]) ->
    wh_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    Props = [{<<"order_id">>, wh_util:get_xml_value("orderID/text()", Xml)}
             ,{<<"order_number">>, wh_util:get_xml_value("orderNumber/text()", Xml)}
             ,{<<"order_name">>, wh_util:get_xml_value("orderName/text()", Xml)}
             ,{<<"ext_ref_id">>, wh_util:get_xml_value("extRefID/text()", Xml)}
             ,{<<"accountID">>, wh_util:get_xml_value("accountID/text()", Xml)}
             ,{<<"accountName">>, wh_util:get_xml_value("accountName/text()", Xml)}
             ,{<<"quantity">>, wh_util:get_xml_value("quantity/text()", Xml)}
             ,{<<"number">>, number_search_response_to_json(
                               xmerl_xpath:string("telephoneNumbers/telephoneNumber", Xml))}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number search response XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec number_search_response_to_json(xml_el() | xml_els()) -> wh_json:object().
number_search_response_to_json([]) ->
    wh_json:new();
number_search_response_to_json([Xml]) ->
    number_search_response_to_json(Xml);
number_search_response_to_json(Xml) ->
    Props = [{<<"number_id">>, wh_util:get_xml_value("numberID/text()", Xml)}
             ,{<<"ten_digit">>, wh_util:get_xml_value("tenDigit/text()", Xml)}
             ,{<<"formatted_number">>, wh_util:get_xml_value("formattedNumber/text()", Xml)}
             ,{<<"e164">>, wh_util:get_xml_value("e164/text()", Xml)}
             ,{<<"npa_nxx">>, wh_util:get_xml_value("npaNxx/text()", Xml)}
             ,{<<"status">>, wh_util:get_xml_value("status/text()", Xml)}
             ,{<<"rate_center">>, rate_center_to_json(xmerl_xpath:string("rateCenter", Xml))}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%% @private
-spec tollfree_search_response_to_json(xml_el()) -> wh_json:object().
tollfree_search_response_to_json(Xml) ->
    P = [{<<"number_id">>, wh_util:get_xml_value("//TelephoneNumberID/text()", Xml)}
         ,{<<"status">>, wh_util:get_xml_value("//Status/text()", Xml)}
         ,{<<"area_code">>, wh_util:get_xml_value("//AreaCode/text()", Xml)}
         ,{<<"prefix">>, wh_util:get_xml_value("//Prefix/text()", Xml)}
         ,{<<"line">>, wh_util:get_xml_value("//Line/text()", Xml)}
        ],
    Props = [{<<"number">>, tollfree_number(P)} | P],
    wh_json:from_list(props:filter_undefined(Props)).

%% @private
-spec tollfree_number(wh_proplist()) -> api_binary().
tollfree_number(Props) ->
    AreaCode = props:get_value(<<"area_code">>, Props),
    Prefix   = props:get_value(<<"prefix">>, Props),
    Line     = props:get_value(<<"line">>, Props),
    case is_binary(AreaCode) and is_binary(Prefix) and is_binary(Line) of
        'false' -> 'undefined';
        'true' -> <<AreaCode/binary, Prefix/binary, Line/binary>>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a rate center XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec rate_center_to_json(list()) -> wh_json:object().
rate_center_to_json([]) ->
    wh_json:new();
rate_center_to_json([Xml]) ->
    rate_center_to_json(Xml);
rate_center_to_json(Xml) ->
    Props = [{<<"name">>, wh_util:get_xml_value("name/text()", Xml)}
             ,{<<"lata">>, wh_util:get_xml_value("lata/text()", Xml)}
             ,{<<"state">>, wh_util:get_xml_value("state/text()", Xml)}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the request was successful, and if not extract any
%% error text
%% @end
%%--------------------------------------------------------------------
-spec verify_response(xml_el()) -> {'ok', ne_binary()} |
                                   {'error', api_binary() | ne_binaries()}.
verify_response(Xml) ->
    case wh_util:get_xml_value("/*/status/text()", Xml) =:= <<"success">>
        orelse wh_util:get_xml_value("//LoginResponse/LoginResult/text()", Xml) =/= 'undefined'
        orelse wh_util:get_xml_value("//SearchTelephoneNumbersResponse/SearchTelephoneNumbersResult", Xml) =/= []
    of
        'true' ->
            lager:debug("request was successful"),
            {'ok', Xml};
        'false' ->
            lager:debug("request failed"),
            {'error', wh_util:get_xml_value("/*/errors/error/message/text()", Xml)}
    end.

-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
