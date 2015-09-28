%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(knm_bandwidth).

-behaviour(knm_gen_carrier).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([get_number_data/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../knm.hrl").

-define(KNM_BW_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".bandwidth">>).

-define(SERVER, ?MODULE).
-define(BW_XML_PROLOG, "<?xml version=\"1.0\"?>").
-define(BW_XML_NAMESPACE
        ,[{'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"}
          ,{'xmlns:xsd', "http://www.w3.org/2001/XMLSchema"}
          ,{'xmlns', "http://www.bandwidth.com/api/"}
         ]).
-define(BW_NUMBER_URL
        ,whapps_config:get_string(?KNM_BW_CONFIG_CAT
                                  ,<<"numbers_api_url">>
                                  ,<<"https://api.bandwidth.com/public/v2/numbers.api">>
                                 )
       ).

-define(BW_CDR_URL
        ,whapps_config:get_string(?KNM_BW_CONFIG_CAT
                                  ,<<"cdrs_api_url">>
                                  ,<<"https://api.bandwidth.com/api/public/v2/cdrs.api">>
                                 )
       ).

-ifdef(TEST).
-define(BW_DEBUG, 'false').
-else.
-define(BW_DEBUG
        ,whapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"debug">>, 'false')
       ).
-endif.

-define(BW_DEBUG(Format, Args)
        ,?BW_DEBUG
        andalso file:write_file("/tmp/bandwidth.com.xml"
                                ,io_lib:format(Format, Args)
                                ,['append']
                               )
       ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', _}.
find_numbers(<<"+", Rest/binary>>, Quanity, Options) ->
    find_numbers(Rest, Quanity, Options);
find_numbers(<<"1", Rest/binary>>, Quanity, Options) ->
    find_numbers(Rest, Quanity, Options);
find_numbers(<<NPA:3/binary>>, Quanity, Options) ->
    Props = [{'areaCode', [wh_util:to_list(NPA)]}
             ,{'maxQuantity', [wh_util:to_list(Quanity)]}
            ],
    case make_numbers_request('areaCodeNumberSearch', Props) of
        {'error', _}=E -> E;
        {'ok', Xml} -> process_numbers_search_resp(Xml, Options)
    end;
find_numbers(Search, Quanity, Options) ->
    NpaNxx = binary:part(Search, 0, (case size(Search) of L when L < 6 -> L; _ -> 6 end)),
    Props = [{'npaNxx', [wh_util:to_list(NpaNxx)]}
             ,{'maxQuantity', [wh_util:to_list(Quanity)]}
            ],
    case make_numbers_request('npaNxxNumberSearch', Props) of
        {'error', _}=E -> E;
        {'ok', Xml} -> process_numbers_search_resp(Xml, Options)
    end.

-spec process_numbers_search_resp(string(), wh_proplist()) ->
                                         {'ok', knm_number:knm_numbers()}.
process_numbers_search_resp(Xml, Options) ->
    TelephoneNumbers = "/numberSearchResponse/telephoneNumbers/telephoneNumber",
    AccountId = props:get_value(<<"account_id">>, Options),

    {'ok', [found_number_to_object(Number, AccountId)
            || Number <- xmerl_xpath:string(TelephoneNumbers, Xml)
           ]
    }.

-spec found_number_to_object(term(), api_binary()) ->
                                    knm_number:knm_number().
found_number_to_object(Found, AccountId) ->
    JObj = number_search_response_to_json(Found),
    Num = wh_json:get_value(<<"e164">>, JObj),
    NormalizedNum = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(NormalizedNum),

    Updates = [{fun knm_phone_number:set_number/2, NormalizedNum}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_module_name/2, wh_util:to_binary(?MODULE)}
               ,{fun knm_phone_number:set_carrier_data/2, JObj}
               ,{fun knm_phone_number:set_number_db/2, NumberDb}
               ,{fun knm_phone_number:set_state/2, ?NUMBER_STATE_DISCOVERY}
               ,{fun knm_phone_number:set_assign_to/2, AccountId}
              ],
    PhoneNumber = knm_phone_number:setters(knm_phone_number:new(), Updates),
    knm_number:set_phone_number(
      knm_number:new()
      ,PhoneNumber
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) ->
    Debug = whapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"sandbox_provisioning">>, 'true'),
    case whapps_config:get_is_true(?KNM_BW_CONFIG_CAT, <<"enable_provisioning">>, 'true') of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            acquire_and_provision_number(Number)
    end.

-spec acquire_and_provision_number(knm_number:knm_number()) ->
                                          knm_number:knm_number().
acquire_and_provision_number(Number) ->
    PhoneNumber = knm_number:phone_number(Number),
    AuthBy = knm_phone_number:auth_by(PhoneNumber),
    AssignedTo = knm_phone_number:assigned_to(PhoneNumber),
    Data = knm_phone_number:carrier_data(PhoneNumber),
    Id = wh_json:get_string_value(<<"number_id">>, Data),
    Hosts = case whapps_config:get(?KNM_BW_CONFIG_CAT, <<"endpoints">>) of
                'undefined' -> [];
                Endpoint when is_binary(Endpoint) ->
                    [{'endPoints', [{'host', [wh_util:to_list(Endpoint)]}]}];
                Endpoints ->
                    [{'endPoints', [{'host', [wh_util:to_list(E)]} || E <- Endpoints]}]
            end,
    OrderNamePrefix = whapps_config:get_binary(?KNM_BW_CONFIG_CAT, <<"order_name_prefix">>, <<"Kazoo">>),
    OrderName = list_to_binary([OrderNamePrefix, "-", wh_util:to_binary(wh_util:current_tstamp())]),
    ExtRef = case wh_util:is_empty(AuthBy) of
                 'true' -> "no_authorizing_account";
                 'false' -> wh_util:to_list(AuthBy)
             end,
    AcquireFor = case wh_util:is_empty(AuthBy) of
                     'true' -> "no_assigned_account";
                     'false' -> wh_util:to_list(AssignedTo)
                 end,
    Props = [{'orderName', [wh_util:to_list(OrderName)]}
             ,{'extRefID', [wh_util:to_list(ExtRef)]}
             ,{'numberIDs', [{'id', [Id]}]}
             ,{'subscriber', [wh_util:to_list(AcquireFor)]}
             | Hosts
            ],
    case make_numbers_request('basicNumberOrder', Props) of
        {'error', Error} ->
            knm_errors:unspecified(Error, Number);
        {'ok', Xml} ->
            Response = xmerl_xpath:string("/numberOrderResponse/numberOrder", Xml),
            Data = number_order_response_to_json(Response),
            knm_number:set_phone_number(
              Number
              ,knm_phone_number:set_carrier_data(PhoneNumber, Data)
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the Bandwidth.com system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec get_number_data(ne_binary()) -> wh_json:object().
get_number_data(<<"+", Rest/binary>>) ->
    get_number_data(Rest);
get_number_data(<<"1", Rest/binary>>) ->
    get_number_data(Rest);
get_number_data(Number) ->
    Props = [{'getType', ["10digit"]}
             ,{'getValue', [wh_util:to_list(Number)]}
            ],
    case make_numbers_request('getTelephoneNumber', Props) of
        {'error', _} -> wh_json:new();
        {'ok', Xml} ->
            Response = xmerl_xpath:string("/getResponse/telephoneNumber", Xml),
            number_search_response_to_json(Response)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_number:knm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to Bandwidth.com Numbers API to preform the
%% given verb (purchase, search, provision, ect).
%% @end
%%--------------------------------------------------------------------
-spec make_numbers_request(atom(), wh_proplist()) ->
                                  {'ok', term()} |
                                  {'error', term()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
make_numbers_request('npaNxxNumberSearch', _Props) ->
    {Xml, _} = xmerl_scan:string(?BANDWIDTH_NPAN_RESPONSE),
    verify_response(Xml);
make_numbers_request('areaCodeNumberSearch', _Props) ->
    {Xml, _} = xmerl_scan:string(?BANDWIDTH_AREACODE_RESPONSE),
    verify_response(Xml).
-else.
make_numbers_request(Verb, Props) ->
    lager:debug("making ~s request to bandwidth.com ~s", [Verb, ?BW_NUMBER_URL]),
    DevKey = whapps_config:get_string(?KNM_BW_CONFIG_CAT, <<"developer_key">>, <<>>),
    Request = [{'developerKey', [DevKey]}
               | Props
              ],
    Body = xmerl:export_simple([{Verb, ?BW_XML_NAMESPACE, Request}]
                               ,'xmerl_xml'
                               ,[{'prolog', ?BW_XML_PROLOG}]
                              ),
    Headers = [{"Accept", "*/*"}
               ,{"User-Agent", ?KNM_USER_AGENT}
               ,{"X-BWC-IN-Control-Processing-Type", "process"}
               ,{"Content-Type", "text/xml"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 0}]}
                   ,{'inactivity_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ],
    ?BW_DEBUG("Request:~n~s ~s~n~s~n"
              ,['post', ?BW_NUMBER_URL, Body]
             ),
    case ibrowse:send_req(?BW_NUMBER_URL
                          ,Headers
                          ,'post'
                          ,unicode:characters_to_binary(Body)
                          ,HTTPOptions
                          ,180 * ?MILLISECONDS_IN_SECOND
                         )
    of
        {'ok', "401", _, _Response} ->
            ?BW_DEBUG("Response:~n401~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 401 (unauthenticated)"),
            {'error', 'authentication'};
        {'ok', "403", _, _Response} ->
            ?BW_DEBUG("Response:~n403~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 403 (unauthorized)"),
            {'error', 'authorization'};
        {'ok', "404", _, _Response} ->
            ?BW_DEBUG("Response:~n404~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 404 (not found)"),
            {'error', 'not_found'};
        {'ok', "500", _, _Response} ->
            ?BW_DEBUG("Response:~n500~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 500 (server error)"),
            {'error', 'server_error'};
        {'ok', "503", _, _Response} ->
            ?BW_DEBUG("Response:~n503~n~s~n", [_Response]),
            lager:debug("bandwidth.com request error: 503"),
            {'error', 'server_error'};
        {'ok', Code, _, [$<,$?,$x,$m,$l|_]=Response} ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number order response to json
%% @end
%%--------------------------------------------------------------------
-spec number_order_response_to_json(term()) -> wh_json:object().
number_order_response_to_json([]) ->
    wh_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    Props = [{<<"order_id">>, get_cleaned("orderID/text()", Xml)}
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
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number search response XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec number_search_response_to_json(term()) -> wh_json:object().
number_search_response_to_json([]) ->
    wh_json:new();
number_search_response_to_json([Xml]) ->
    number_search_response_to_json(Xml);
number_search_response_to_json(Xml) ->
    Props = [{<<"number_id">>, get_cleaned("numberID/text()", Xml)}
             ,{<<"ten_digit">>, get_cleaned("tenDigit/text()", Xml)}
             ,{<<"formatted_number">>, get_cleaned("formattedNumber/text()", Xml)}
             ,{<<"e164">>, get_cleaned("e164/text()", Xml)}
             ,{<<"npa_nxx">>, get_cleaned("npaNxx/text()", Xml)}
             ,{<<"status">>, get_cleaned("status/text()", Xml)}
             ,{<<"rate_center">>, rate_center_to_json(xmerl_xpath:string("rateCenter", Xml))}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

-spec get_cleaned(wh_deeplist(), xml_el()) -> api_binary().
get_cleaned(Path, Xml) ->
    case wh_util:get_xml_value(Path, Xml) of
        'undefined' -> 'undefined';
        V -> wh_util:strip_binary(V, [$\s, $\n])
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
    Props = [{<<"name">>, get_cleaned("name/text()", Xml)}
             ,{<<"lata">>, get_cleaned("lata/text()", Xml)}
             ,{<<"state">>, get_cleaned("state/text()", Xml)}
            ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine if the request was successful, and if not extract any
%% error text
%% @end
%%--------------------------------------------------------------------
-spec verify_response(term()) ->
                             {'ok', term()} |
                             {'error', api_binary() | ne_binaries()}.
verify_response(Xml) ->
    case get_cleaned("/*/status/text()", Xml) of
        <<"success">> ->
            lager:debug("request was successful"),
            {'ok', Xml};
        _ ->
            lager:debug("request failed"),
            {'error', get_cleaned("/*/errors/error/message/text()", Xml)}
    end.
