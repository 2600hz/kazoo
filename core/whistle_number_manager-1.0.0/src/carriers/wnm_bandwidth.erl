%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_bandwidth).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([get_number_data/1]).

-include("../wnm.hrl").

-define(WNM_BW_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".bandwidth">>).

-define(SERVER, ?MODULE).
-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE, [{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
                           ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
                           ,{'xmlns', "http://www.bandwidth.com/api/"}]).
-define(BW_NUMBER_URL, whapps_config:get_string(?WNM_BW_CONFIG_CAT
                                                   ,<<"numbers_api_url">>
                                                   ,<<"https://api.bandwidth.com/public/v2/numbers.api">>)).
-define(BW_CDR_URL, whapps_config:get_string(?WNM_BW_CONFIG_CAT
                                                ,<<"cdrs_api_url">>
                                                ,<<"https://api.bandwidth.com/api/public/v2/cdrs.api">>)).
-define(BW_DEBUG, whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"debug">>, false)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec get_number_data/1 :: (ne_binary()) -> wh_json:object().
get_number_data(<<"+", Rest/binary>>) ->
    get_number_data(Rest);
get_number_data(<<"1", Rest/binary>>) ->
    get_number_data(Rest);
get_number_data(Number) ->
    Props = [{'getType', ["10digit"]}
             ,{'getValue', [wh_util:to_list(Number)]}
            ],
    case make_numbers_request('getTelephoneNumber', Props) of
        {error, _} -> wh_json:new(); 
        {ok, Xml} ->
            Response = xmerl_xpath:string("/getResponse/telephoneNumber", Xml),
            number_search_response_to_json(Response)
    end.
                
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/3 :: (ne_binary(), pos_integer(), wh_proplist()) -> {'ok', wh_json:object()} |
                                                        {'error', term()}.
find_numbers(<<"+", Rest/binary>>, Quanity, Opts) ->
    find_numbers(Rest, Quanity, Opts);
find_numbers(<<"1", Rest/binary>>, Quanity, Opts) ->
    find_numbers(Rest, Quanity, Opts);
find_numbers(<<NPA:3/binary>>, Quanity, _) ->
    Props = [{'areaCode', [wh_util:to_list(NPA)]}
             ,{'maxQuantity', [wh_util:to_list(Quanity)]}], 
    case make_numbers_request('areaCodeNumberSearch', Props) of
        {error, _}=E -> E;
        {ok, Xml} ->
            TelephoneNumbers = "/numberSearchResponse/telephoneNumbers/telephoneNumber",
            Resp = [begin
                        JObj = number_search_response_to_json(Number),
                        Num = wh_json:get_value(<<"e164">>, JObj),
                        {Num, JObj}
                    end
                    || Number <- xmerl_xpath:string(TelephoneNumbers, Xml)],
            {ok, wh_json:from_list(Resp)}
    end;
find_numbers(Search, Quanity, _) ->
    NpaNxx = binary:part(Search, 0, (case size(Search) of L when L < 6 -> L; _ -> 6 end)),
    Props = [{'npaNxx', [wh_util:to_list(NpaNxx)]}
             ,{'maxQuantity', [wh_util:to_list(Quanity)]}], 
    case make_numbers_request('npaNxxNumberSearch', Props) of
        {error, _}=E -> E;
        {ok, Xml} ->
            TelephoneNumbers = "/numberSearchResponse/telephoneNumbers/telephoneNumber",
            Resp = [begin
                        JObj = number_search_response_to_json(Number),
                        Num = wh_json:get_value(<<"e164">>, JObj),
                        {Num, JObj}
                    end
                    || Number <- xmerl_xpath:string(TelephoneNumbers, Xml)],
            {ok, wh_json:from_list(Resp)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(#number{auth_by=AuthBy, assigned_to=AssignedTo, module_data=Data}=N) ->
    Debug = whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"sandbox_provisioning">>, true),
    case whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"enable_provisioning">>, true) of
        false when Debug -> 
            lager:debug("allowing sandbox provisioning", []),
            N;
        false -> 
            Error = <<"Unable to acquire numbers on this system, carrier provisioning is disabled">>,
            wnm_number:error_carrier_fault(Error, N);
        true ->
            Id = wh_json:get_string_value(<<"number_id">>, Data),
            Hosts = case whapps_config:get(?WNM_BW_CONFIG_CAT, <<"endpoints">>) of
                        undefined -> [];
                        Endpoint when is_binary(Endpoint) ->
                            [{'endPoints', [{'host', [wh_util:to_list(Endpoint)]}]}];
                        Endpoints ->
                            [{'endPoints', [{'host', [wh_util:to_list(E)]} || E <- Endpoints]}]
                    end,
            OrderNamePrefix = whapps_config:get_binary(?WNM_BW_CONFIG_CAT, <<"order_name_prefix">>, <<"Kazoo">>),
            OrderName = list_to_binary([OrderNamePrefix, "-", wh_util:to_binary(wh_util:current_tstamp())]),
            ExtRef = case wh_util:is_empty(AuthBy) of true -> "no_authorizing_account"; false -> wh_util:to_list(AuthBy) end, 
            AcquireFor = case wh_util:is_empty(AuthBy) of true -> "no_assigned_account"; false -> wh_util:to_list(AssignedTo) end, 
            Props = [{'orderName', [wh_util:to_list(OrderName)]}
                     ,{'extRefID', [wh_util:to_list(ExtRef)]}
                     ,{'numberIDs', [{'id', [Id]}]}
                     ,{'subscriber', [wh_util:to_list(AcquireFor)]}
                     | Hosts
                    ],
            case make_numbers_request('basicNumberOrder', Props) of
                {error, Reason} ->
                    Error = <<"Unable to acquire number: ", (wh_util:to_binary(Reason))/binary>>,
                    wnm_number:error_carrier_fault(Error, N);
                {ok, Xml} ->
                    Response = xmerl_xpath:string("/numberOrderResponse/numberOrder", Xml),
                    N#number{module_data=number_order_response_to_json(Response)}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number/1 :: (wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to Bandwidth.com Numbers API to preform the
%% given verb (purchase, search, provision, ect).
%% @end
%%--------------------------------------------------------------------
-spec make_numbers_request/2 :: (atom(), proplist()) -> {ok, term()} | {error, term()}.
make_numbers_request(Verb, Props) ->
    lager:debug("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    DevKey = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"developer_key">>, <<>>),
    Request = [{'developerKey', [DevKey]}
               | Props],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,xmerl_xml
                               ,[{prolog, ?BW_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"X-BWC-IN-Control-Processing-Type", "process"}
               ,{"Content-Type", "text/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}
                   ,{inactivity_timeout, 180000}
                   ,{connect_timeout, 180000}
                  ],
    ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                      ,io_lib:format("Request:~n~s ~s~n~s~n", [post, ?BW_NUMBER_URL, Body])),
    case ibrowse:send_req(?BW_NUMBER_URL, Headers, post, unicode:characters_to_binary(Body), HTTPOptions, 180000) of
        {ok, "401", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n401~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 403 (unauthorized)"),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 404 (not found)"),
            {error, not_found};
        {ok, "500", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 500 (server error)"),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                              ,[append]),
            lager:debug("bandwidth.com request error: 503"),
            {error, server_error};
        {ok, Code, _, [$<,$?,$x,$m,$l|_]=Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            lager:debug("received response from bandwidth.com"),
            try
                {Xml, _} = xmerl_scan:string(Response),
                verify_response(Xml)
            catch
                _:R ->
                    lager:debug("failed to decode xml: ~p", [R]),
                    {error, empty_response}
            end;
        {ok, Code, _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, _Response])
                                              ,[append]),
            lager:debug("bandwidth.com empty response: ~p", [Code]),
            {error, empty_response};
        {error, _}=E ->
            lager:debug("bandwidth.com request error: ~p", [E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number order response to json
%% @end
%%--------------------------------------------------------------------
-spec number_order_response_to_json/1 :: (term()) -> wh_json:object().
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
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number search response XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec number_search_response_to_json/1 :: (term()) -> wh_json:object().
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
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a rate center XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec rate_center_to_json/1 :: (list()) -> wh_json:object().
rate_center_to_json([]) ->
    wh_json:new();
rate_center_to_json([Xml]) ->
    rate_center_to_json(Xml);
rate_center_to_json(Xml) ->
    Props = [{<<"name">>, wh_util:get_xml_value("name/text()", Xml)}
             ,{<<"lata">>, wh_util:get_xml_value("lata/text()", Xml)}
             ,{<<"state">>, wh_util:get_xml_value("state/text()", Xml)}
            ],
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the request was successful, and if not extract any
%% error text
%% @end
%%--------------------------------------------------------------------
-spec verify_response/1 :: (term()) -> {ok, term()} | {error, undefined | binary() | [binary(),...]}.
verify_response(Xml) ->
    case wh_util:get_xml_value("/*/status/text()", Xml) of
        <<"success">> -> 
            lager:debug("request was successful"),
            {ok, Xml};
        _ ->
            lager:debug("request failed"),
            {error, wh_util:get_xml_value("/*/errors/error/message/text()", Xml)}
    end.
