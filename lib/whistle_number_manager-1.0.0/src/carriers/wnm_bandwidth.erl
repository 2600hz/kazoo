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

-export([find_numbers/2]).
-export([acquire_number/3]).
-export([release_number/2]).

-include("../../include/wh_number_manager.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(SERVER, ?MODULE).
-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE, [{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
                           ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
                           ,{'xmlns', "http://www.bandwidth.com/api/"}]).
-define(BW_NUMBER_URL, whapps_config:get_string(?WNM_CONFIG_CAT
                                                   ,<<"bandwidth.numbers_api_url">>
                                                   ,<<"https://api.bandwidth.com/public/v2/numbers.api">>)).
-define(BW_CDR_URL, whapps_config:get_string(?WNM_CONFIG_CAT
                                                ,<<"bandwidth.cdrs_api_url">>
                                                ,<<"https://api.bandwidth.com/api/public/v2/cdrs.api">>)).
-define(BW_DEBUG, whapps_config:get_is_true(?WNM_CONFIG_CAT, <<"bandwidth.debug">>, false)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of avaliable numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/2 :: (ne_binary(), pos_integer()) -> {ok, json_object} | {error, term()}.
find_numbers(<<"+", Rest/binary>>, Quanity) ->
    find_numbers(Rest, Quanity);
find_numbers(<<"1", Rest/binary>>, Quanity) ->
    find_numbers(Rest, Quanity);
find_numbers(<<NPA:3/binary>>, Quanity) ->
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
find_numbers(Search, Quanity) ->
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
-spec acquire_number/3 :: (ne_binary(), ne_binary(), json_object()) -> {ok, ne_binary(), json_object()} |
                                                                       {error, ne_binary()}.
acquire_number(_, <<"avaliable">>, JObj) ->
    {ok, <<"in_service">>, JObj};
acquire_number(_, <<"discovery">>, JObj) ->
%%    order_number(JObj);
    {ok, <<"in_service">>, JObj};
acquire_number(_, <<"claim">>, JObj) ->
    {ok, <<"in_service">>, JObj};
acquire_number(_, _, _) ->
    {error, unavaliable}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec release_number/2 :: (ne_binary(), json_object()) -> {ok, json_object()}.
release_number(_, JObj) ->
    {ok, JObj}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Order a number given the discovery json object
%% @end
%%--------------------------------------------------------------------
-spec order_number/1 :: (json_object()) -> {ok, ne_binary(), json_object()} |
                                           {error, ne_binary()}.
order_number(JObj) ->
    Id = wh_json:get_string_value(<<"number_id">>, JObj),
    Endpoint = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"bandwidth.endpoint">>, <<"">>),
    OrderNamePrefix = whapps_config:get_binary(?WNM_CONFIG_CAT, <<"bandwidth.order_name_prefix">>, <<"Whistle">>),
    OrderName = list_to_binary([OrderNamePrefix, "-", wh_util:to_binary(wh_util:current_tstamp())]),
    AcquireFor = wh_json:get_ne_value(<<"acquire_for">>, JObj, <<"no_subscriber">>), 
    Props = [{'orderName', [wh_util:to_list(OrderName)]}
             ,{'extRefID', [wh_util:to_list(AcquireFor)]}
             ,{'numberIDs', [{'id', [Id]}]}
             ,{'subscriber', [wh_util:to_list(AcquireFor)]}
             ,{'endPoints', [{'host', [wh_util:to_list(Endpoint)]}]}
            ],
    case make_numbers_request('basicNumberOrder', Props) of
        {error, _}=E -> 
            E;
        {ok, Xml} ->
            Response = xmerl_xpath:string("/numberOrderResponse/numberOrder", Xml),
            {ok, <<"in_service">>, number_order_response_to_json(Response)}
    end.
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to Bandwidth.com Numbers API to preform the
%% given verb (purchase, search, provision, ect).
%% @end
%%--------------------------------------------------------------------
-spec make_numbers_request/2 :: (atom(), proplist()) -> {ok, term()} | {error, term()}.
make_numbers_request(Verb, Props) ->
    ?LOG("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    DevKey = whapps_config:get_string(?WNM_CONFIG_CAT, <<"bandwidth.developer_key">>, <<>>),
    Request = [{'developerKey', [DevKey]}
               | Props],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,xmerl_xml
                               ,[{prolog, ?BW_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", "Whistle Number Manager 1.0.0"}
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
            ?LOG("bandwidth.com request error: 401 (unauthenticated)"),
            {error, authentication};
        {ok, "403", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n403~n~s~n", [_Response])
                                              ,[append]),
            ?LOG("bandwidth.com request error: 403 (unauthorized)"),
            {error, authorization};
        {ok, "404", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n404~n~s~n", [_Response])
                                              ,[append]),
            ?LOG("bandwidth.com request error: 404 (not found)"),
            {error, not_found};
        {ok, "500", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n500~n~s~n", [_Response])
                                              ,[append]),
            ?LOG("bandwidth.com request error: 500 (server error)"),
            {error, server_error};
        {ok, "503", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n503~n~s~n", [_Response])
                                              ,[append]),
            ?LOG("bandwidth.com request error: 503"),
            {error, server_error};
        {ok, Code, _, [$<,$?,$x,$m,$l|_]=Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, Response])
                                              ,[append]),
            ?LOG("received response from bandwidth.com"),
            try
                {Xml, _} = xmerl_scan:string(Response),
                verify_response(Xml)
            catch
                _:R ->
                    ?LOG("failed to decode xml: ~p", [R]),
                    {error, empty_response}
            end;
        {ok, Code, _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n~p~n~s~n", [Code, _Response])
                                              ,[append]),
            ?LOG("bandwidth.com empty response: ~p", [Code]),
            {error, empty_response};
        {error, _}=E ->
            ?LOG("bandwidth.com request error: ~p", [E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number order response to json
%% @end
%%--------------------------------------------------------------------
-spec number_order_response_to_json/1 :: (term()) -> json_object().
number_order_response_to_json([]) ->
    wh_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    Props = [{<<"order_id">>, get_xml_value("orderID/text()", Xml)}
             ,{<<"order_number">>, get_xml_value("orderNumber/text()", Xml)}
             ,{<<"order_name">>, get_xml_value("orderName/text()", Xml)}
             ,{<<"ext_ref_id">>, get_xml_value("extRefID/text()", Xml)}
             ,{<<"accountID">>, get_xml_value("accountID/text()", Xml)}
             ,{<<"accountName">>, get_xml_value("accountName/text()", Xml)}
             ,{<<"quantity">>, get_xml_value("quantity/text()", Xml)}
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
-spec number_search_response_to_json/1 :: (term()) -> json_object().
number_search_response_to_json([]) ->
    wh_json:new();
number_search_response_to_json([Xml]) ->
    number_search_response_to_json(Xml);
number_search_response_to_json(Xml) ->
    Props = [{<<"number_id">>, get_xml_value("numberID/text()", Xml)}
             ,{<<"ten_digit">>, get_xml_value("tenDigit/text()", Xml)}
             ,{<<"formatted_number">>, get_xml_value("formattedNumber/text()", Xml)}
             ,{<<"e164">>, get_xml_value("e164/text()", Xml)}
             ,{<<"npa_nxx">>, get_xml_value("npaNxx/text()", Xml)}
             ,{<<"status">>, get_xml_value("status/text()", Xml)}
             ,{<<"rate_center">>, rate_center_to_json(xmerl_xpath:string("rateCenter", Xml))}
            ],
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a rate center XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec rate_center_to_json/1 :: (list()) -> json_object().
rate_center_to_json([]) ->
    wh_json:new();
rate_center_to_json([Xml]) ->
    rate_center_to_json(Xml);
rate_center_to_json(Xml) ->
    Props = [{<<"name">>, get_xml_value("name/text()", Xml)}
             ,{<<"lata">>, get_xml_value("lata/text()", Xml)}
             ,{<<"state">>, get_xml_value("state/text()", Xml)}
            ],
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generic helper to get the text value of a XML path
%% @end
%%--------------------------------------------------------------------
-spec get_xml_value/2 :: (string(), term()) -> undefined | binary().    
get_xml_value(Path, Xml) ->
    try xmerl_xpath:string(Path, Xml) of
        [#xmlText{value=Value}] ->
            wh_util:to_binary(Value);
        [#xmlText{}|_]=Values ->
            [wh_util:to_binary(Value) 
             || #xmlText{value=Value} <- Values];
        _ -> undefined
    catch
        E:R ->
            ?LOG("~s getting value of '~s': ~p", [E, Path, R]),
            undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the request was successful, and if not extract any
%% error text
%% @end
%%--------------------------------------------------------------------
-spec verify_response/1 :: (term()) -> {ok, term()} | {error, undefined | binary() | [binary(),...]}.
verify_response(Xml) ->
    case get_xml_value("/*/status/text()", Xml) of
        <<"success">> -> 
            ?LOG("request was successful"),
            {ok, Xml};
        _ ->
            ?LOG("request failed"),
            {error, get_xml_value("/*/errors/error/message/text()", Xml)}
    end.
