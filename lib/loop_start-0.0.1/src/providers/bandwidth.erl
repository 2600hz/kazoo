%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(bandwidth).

-export([search_numbers/1, search_numbers/2]).

-include("../../include/loopstart.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(SERVER, ?MODULE).
-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE, [{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
                           ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
                           ,{'xmlns', "http://www.bandwidth.com/api/"}]).
-define(BW_NUMBER_URL, whapp_config:get_string(<<"loopstart">>
                                                   ,<<"bandwidth.numbers_api_url">>
                                                   ,<<"https://api.bandwidth.com/public/v2/numbers.api">>)).
-define(BW_CDR_URL, whapp_config:get_string(<<"loopstart">>
                                                ,<<"bandwidth.cdrs_api_url">>
                                                ,<<"https://api.bandwidth.com/api/public/v2/cdrs.api">>)).
-define(BW_DEBUG, whapp_config:get_is_true(<<"loopstart">>, <<"bandwidth.debug">>, false)).

search_numbers(Partial) ->
    search_numbers(Partial, 1).

search_numbers(<<"+", Rest/binary>>, Quanity) ->
    search_numbers(Rest, Quanity);
search_numbers(<<"1", Rest/binary>>, Quanity) ->
    search_numbers(Rest, Quanity);
search_numbers(<<NPA:3/binary>>, Quanity) ->
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
search_numbers(Search, Quanity) ->
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

make_numbers_request(Verb, Props) ->
    ?LOG("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    DevKey = whapp_config:get_string(<<"loopstart">>, <<"bandwidth.developer_key">>, <<>>),
    Request = [{'developerKey', [DevKey]}
               | Props],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,xmerl_xml
                               ,[{prolog, ?BW_XML_PROLOG}]),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", "Loop Start Erlang Library 0.0.1"}
               ,{"X-BWC-IN-Control-Processing-Type", "process"}
               ,{"Content-Type", "text/xml"}],
    HTTPOptions = [{ssl,[{verify,0}]}],
    ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                      ,io_lib:format("Request:~n~s ~s~n~s~n", [post, ?BW_NUMBER_URL, Body])),
    case ibrowse:send_req(?BW_NUMBER_URL, Headers, post, unicode:characters_to_binary(Body), HTTPOptions) of
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
        {ok, "426", _, _Response} ->
            ?BW_DEBUG andalso file:write_file("/tmp/bandwidth.com.xml"
                                              ,io_lib:format("Response:~n426~n~s~n", [_Response])
                                              ,[append]),
            ?LOG("bandwidth.com request error: 426 (upgrade required)"),
            {error, upgrade_required};
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
            {Xml, _} = xmerl_scan:string(Response),
            verify_response(Xml);
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

rate_center_to_json([]) ->
    wh_json:new();
rate_center_to_json([Xml]) ->
    Props = [{<<"name">>, get_xml_value("name/text()", Xml)}
             ,{<<"lata">>, get_xml_value("lata/text()", Xml)}
             ,{<<"state">>, get_xml_value("state/text()", Xml)}
            ],
    wh_json:from_list([{K, V} || {K, V} <- Props, V =/= undefined]).
    
get_xml_value(Path, Xml) ->
    case xmerl_xpath:string(Path, Xml) of
        [#xmlText{value=Value}] ->
            wh_util:to_binary(Value);
        [#xmlText{}|_]=Values ->
            [wh_util:to_binary(Value) 
             || #xmlText{value=Value} <- Values];
        _ -> undefined
    end.

verify_response(Xml) ->
    case get_xml_value("/*/status/text()", Xml) of
        <<"success">> -> 
            {ok, Xml};
        _ ->
            {error, get_xml_value("/*/errors/error/message/text()", Xml)}
    end.
