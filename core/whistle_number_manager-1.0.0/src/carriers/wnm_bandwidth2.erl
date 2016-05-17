%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% Handle client requests for phone_number documents using new bandwidth api
%%%
%%% @end
%%% @contributors
%%%     Karl Anderson
%%%     Mark Magnusson
%%%-------------------------------------------------------------------
-module(wnm_bandwidth2).

-export([sites/0, peers/1]).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../wnm.hrl").

-define(WNM_BW_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".bandwidth2">>).

-define(BW_DEBUG, whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"debug">>, 'false')).
-define(BW_DEBUG_FILE, "/tmp/bandwidth2.com.xml").

-define(DEBUG_WRITE(Format, Args),
        _ = ?BW_DEBUG andalso
            file:write_file(?BW_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?BW_DEBUG andalso
            file:write_file(?BW_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).

-define(BW_BASE_URL, "https://api.inetwork.com/v1.0").

-define(IS_SANDBOX_PROVISIONING_TRUE,
        whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"sandbox_provisioning">>, 'true')).
-define(IS_PROVISIONING_ENABLED,
        whapps_config:get_is_true(?WNM_BW_CONFIG_CAT, <<"enable_provisioning">>, 'true')).
-define(BW_ORDER_NAME_PREFIX,
        whapps_config:get_binary(?WNM_BW_CONFIG_CAT, <<"order_name_prefix">>, <<"Kazoo">>)).

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

find_numbers(<<"8", Second:1/binary, _/binary>>, Quantity, _) ->
    UseQuantity = list_to_binary(integer_to_list(Quantity)),
    {'ok', Result} = search(<<"tollFreeWildCardPattern=8", Second/binary, "*&enableTNDetail=true&quantity=", UseQuantity/binary>>),
    process_tollfree_response(Result);

find_numbers(<<NPA:3/binary>>, Quantity, _) ->
    UseQuantity = list_to_binary(integer_to_list(Quantity)),
    {'ok', Result} = search(<<"areaCode=", NPA/binary, "&enableTNDetail=true&quantity=", UseQuantity/binary>>),
    process_search_response(Result);

find_numbers(Search, Quantity, _) ->
    NpaNxx = wh_util:truncate_right_binary(Search, 6),
    UseQuantity = list_to_binary(integer_to_list(Quantity)),
    {'ok', Result} = search(<<"npaNxx=", NpaNxx/binary, "&enableTNDetail=true&quantity=", UseQuantity/binary>>),
    process_search_response(Result).

-spec process_tollfree_response(xml_el()) -> {'ok', wh_json:object()}.
process_tollfree_response(Result) ->
    Results   = xmerl_xpath:string("TelephoneNumberList/TelephoneNumber", Result),
    Formatted = [tollfree_search_response_to_json(X) || X <- Results],
    {'ok', wh_json:from_list(Formatted)}.

-spec process_search_response(xml_el()) -> {'ok', wh_json:object()}.
process_search_response(Result) ->
    Numbers   = xmerl_xpath:string("TelephoneNumberDetailList/TelephoneNumberDetail", Result),
    Formatted = [search_response_to_json(X) || X <- Numbers],
    {'ok', wh_json:from_list(Formatted)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().

acquire_number(#number{dry_run = 'true'}=Number) -> Number;
acquire_number(#number{auth_by = AuthBy
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
            Num  = wh_json:get_string_value(<<"number">>, Data),
            Peer = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"sip_peer">>),
            Site = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"site_id">>),
            ON   = list_to_binary([?BW_ORDER_NAME_PREFIX, "-", wh_util:to_binary(wh_util:current_tstamp())]),

            ExtRef = case wh_util:is_empty(AuthBy) of
                'true' -> "no-authorizing-account";
                'false' -> wh_util:to_list(AuthBy)
            end,

            Props = [
                {'Name', [wh_util:to_list(ON)]}
               ,{'CustomerOrderId', [wh_util:to_list(ExtRef)]}
               ,{'SiteId', [wh_util:to_list(Site)]}
               ,{'PeerId', [wh_util:to_list(Peer)]}
               ,{'ExistingTelephoneNumberOrderType',[
                    {'TelephoneNumberList', [{'TelephoneNumber', [Num]}]}
                ]}
            ],

            AccountId = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"account_id">>),
            Url       = lists:flatten(io_lib:format("~s/accounts/~s/orders", [?BW_BASE_URL, AccountId])),
            Body      = xmerl:export_simple([{'Order', Props}], 'xmerl_xml'),

            case api_post(Url, Body, []) of
                {'error', Reason} ->
                    Error = <<"Unable to acquire number: ", (wh_util:to_binary(Reason))/binary>>,
                    wnm_number:error_carrier_fault(Error, N);

                {'ok', Xml} ->
                    Response = xmerl_xpath:string("Order", Xml),
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

-spec sites() -> 'ok'.
sites() ->
    AccountId   = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"account_id">>),
    SiteUrl     = lists:flatten(io_lib:format("~s/accounts/~s/sites", [?BW_BASE_URL, AccountId])),
    {'ok', Xml} = api_get(SiteUrl),

    io:format("listing all sites for account ~p~n", [AccountId]),
    Sites = xmerl_xpath:string("Sites/Site", Xml),
    [process_site(X) || X <- Sites],
    io:format("done.~n").

-spec process_site(xml_el()) -> 'ok'.
process_site(Site) ->
    Id   = wh_util:get_xml_value("Id/text()", Site),
    Name = wh_util:get_xml_value("Name/text()", Site),
    io:format("Id: ~p Name: ~p~n",[Id, Name]).

-spec peers(binary()) -> 'ok'.
peers(SiteId) ->
    AccountId = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"account_id">>),
    PeerUrl   = lists:flatten(io_lib:format("~s/accounts/~s/sites/~s/sippeers", [?BW_BASE_URL, AccountId, SiteId])),
    {'ok', Xml} = api_get(PeerUrl),

    io:format("listing all peers for account ~p, site ~p~n", [AccountId, SiteId]),
    Peers = xmerl_xpath:string("SipPeers/SipPeer", Xml),
    [process_peer(X) || X <- Peers],
    io:format("done.~n").

-spec process_peer(xml_el()) -> 'ok'.
process_peer(Peer) ->
    Id   = wh_util:get_xml_value("PeerId/text()", Peer),
    Name = wh_util:get_xml_value("PeerName/text()", Peer),
    io:format("Id: ~p Name: ~p~n", [Id, Name]).

-type api_res() :: {'ok', xml_el()} | {'error', atom()}.
-spec search(string()) -> api_res().
search(Params) ->
    AccountId = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"account_id">>),
    SearchUrl = lists:flatten(io_lib:format("~s/accounts/~s/availableNumbers?~s", [?BW_BASE_URL, AccountId, Params])),

    api_get(SearchUrl).

-spec api_get(string()) -> api_res().
api_get(Url) ->
    Username = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"api_username">>),
    Password = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"api_password">>),
    Response = ibrowse:send_req(Url, [], 'get', [], [{'basic_auth', {Username, Password}}]),

    handle_ibrowse_response(Response).

-spec api_post(string(), binary(), list()) -> api_res().
api_post(Url, Body, AdditionalHeaders) ->
    Username = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"api_username">>),
    Password = whapps_config:get_string(?WNM_BW_CONFIG_CAT, <<"api_password">>),

    Headers = AdditionalHeaders ++ [
        {"Accept", "*/*"},
        {"User-Agent", ?WNM_USER_AGENT},
        {"Content-Type", "application/xml"}
    ],

    HTTPOptions = [
        {'basic_auth', {Username, Password}}
    ],

    Timeout = 180 * ?MILLISECONDS_IN_SECOND,

    ?DEBUG_WRITE("Request:~n~s ~s~n~s~n", ['post', Url, Body]),

    UnicodeBody = unicode:characters_to_binary(Body),
    Response    = ibrowse:send_req(Url, Headers, 'post', UnicodeBody, HTTPOptions, Timeout),

    handle_ibrowse_response(Response).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Make a REST request to Bandwidth.com Numbers API to perform the
%% given verb (purchase, search, provision, etc).
%% @end
%%--------------------------------------------------------------------

-spec handle_ibrowse_response({'ok', string(), any(), string()} | {'error', any()}) -> api_res().
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
-spec number_order_response_to_json(iolist()) -> wh_json:object().
number_order_response_to_json([]) ->
    wh_json:new();
number_order_response_to_json([Xml]) ->
    number_order_response_to_json(Xml);
number_order_response_to_json(Xml) ->
    Props = [
        {<<"order_id">>, wh_util:get_xml_value("id/text()", Xml)}
       ,{<<"order_name">>, wh_util:get_xml_value("Name/text()", Xml)}
       ,{<<"number">>, wh_util:get_xml_value("TelephoneNumberList/TelephoneNumber/text()", Xml)}
    ],
    wh_json:from_list(props:filter_undefined(Props)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a number search response XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec search_response_to_json(xml_el() | xml_els()) -> wh_json:object().
search_response_to_json([]) ->
    wh_json:new();
search_response_to_json([Xml]) ->
    search_response_to_json(Xml);
search_response_to_json(Xml) ->
    Number = wh_util:get_xml_value("//FullNumber/text()", Xml),
    Props = [
        {<<"number">>, wnm_util:to_e164(Number)}
       ,{<<"rate_center">>, rate_center_to_json(Xml)}
    ],

    {Number, wh_json:from_list(props:filter_undefined(Props))}.

-spec tollfree_search_response_to_json(xml_el()) -> wh_json:object().
tollfree_search_response_to_json(Xml) ->
    Number = wh_util:get_xml_value("//TelephoneNumber/text()", Xml),
    Props = [
        {<<"number">>, wnm_util:to_e164(Number)}
    ],

    {Number, wh_json:from_list(Props)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a rate center XML entity to json
%% @end
%%--------------------------------------------------------------------
-spec rate_center_to_json(xml_el() | xml_els()) -> wh_json:object().
rate_center_to_json([]) ->
    wh_json:new();
rate_center_to_json([Xml]) ->
    rate_center_to_json(Xml);
rate_center_to_json(Xml) ->
    Props = [
        {<<"name">>, wh_util:get_xml_value("//RateCenter/text()", Xml)}
       ,{<<"lata">>, wh_util:get_xml_value("//LATA/text()", Xml)}
       ,{<<"state">>, wh_util:get_xml_value("//State/text()", Xml)}
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
