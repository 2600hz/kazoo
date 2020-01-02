%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc A Number Manager module for carrier: VoIPInnovations.com
%%% @author Pierre Fenoll, Joe Black
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_voip_innovations).
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

-define(KNM_VI_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".voip_innovations">>).
-define(VI_DEFAULT_NAMESPACE, "http://tempuri.org/").

%% (XML POST)
-define(VI_URL_V2, "https://backoffice.voipinnovations.com/api2.pl").
%% (Web Service)
-define(VI_URL_V3, "https://backoffice.voipinnovations.com/Services/APIService.asmx").
-define(VI_URL_V3_SANDBOX, "http://dev.voipinnovations.com/VOIP/Services/APIService.asmx").

-ifdef(TEST).
-define(DEBUG_WRITE(_Format, _Args), 'ok').
-define(DEBUG_APPEND(_Format, _Args), 'ok').
-else.
-define(VI_DEBUG, kapps_config:get_is_true(?KNM_VI_CONFIG_CAT, <<"debug">>, 'false')).
-define(VI_DEBUG_FILE, "/tmp/voipinnovations.xml").
-define(DEBUG_WRITE(Format, Args),
        _ = ?VI_DEBUG
        andalso file:write_file(?VI_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?VI_DEBUG
        andalso file:write_file(?VI_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).
-endif.

-define(IS_SANDBOX_PROVISIONING_TRUE
       ,kapps_config:get_is_true(?KNM_VI_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')
       ).
-define(IS_PROVISIONING_ENABLED
       ,kapps_config:get_is_true(?KNM_VI_CONFIG_CAT, <<"enable_provisioning">>, 'true')
       ).

-define(VI_LOGIN, kapps_config:get_string(?KNM_VI_CONFIG_CAT, <<"login">>, <<>>)).
-define(VI_PASSWORD, kapps_config:get_string(?KNM_VI_CONFIG_CAT, <<"password">>, <<>>)).
-define(VI_ENDPOINT_GROUP, kapps_config:get_string(?KNM_VI_CONFIG_CAT, <<"endpoint_group">>, <<>>)).

-define(URL_IN_USE
       ,case ?IS_SANDBOX_PROVISIONING_TRUE of
            'true' -> ?VI_URL_V3_SANDBOX;
            'false' -> ?VI_URL_V3
        end
       ).

-define(API_SUCCESS, <<"100">>).

-type soap_response() :: {'ok', kz_types:xml_el()} | {'error', any()}.
-type handle_action_resp_ret() :: {'ok', kz_json:object() | kz_json:objects()} | {'error', any()}.

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
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', any()}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.


%%------------------------------------------------------------------------------
%% @doc Query the system for a quantity of available numbers in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
          knm_search:mod_response().
find_numbers(<<"+", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<"1", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<NPA:3/binary>>, Quantity, Options) ->
    Resp = soap_request("getDIDs", [{"npa", NPA}]),
    MaybeJson = handle_action_resp('find_numbers', Quantity, Resp),
    do_find_numbers(MaybeJson, knm_search:query_id(Options));
find_numbers(<<NXX:6/binary,_/binary>>, Quantity, Options) ->
    Resp = soap_request("getDIDs", [{"nxx", NXX}]),
    MaybeJson = handle_action_resp('find_numbers', Quantity, Resp),
    do_find_numbers(MaybeJson, knm_search:query_id(Options)).

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_phone_number:record()) -> knm_phone_number:record().
acquire_number(PN) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PN;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PN);
        'true' ->
            N = sanitize_number(
                  knm_phone_number:number(PN)
                 ),
            Resp = soap_request("assignDID", [N]),
            Ret = handle_action_resp('acquire_number', [N], Resp),
            maybe_return(Ret, PN)
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_phone_number:record()) ->
          knm_phone_number:record().
disconnect_number(PN) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            PN;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', PN);
        'true' ->
            N = sanitize_number(
                  knm_phone_number:number(PN)
                 ),
            Resp = soap_request("releaseDID", [N]),
            Ret = handle_action_resp('disconnect_number', [N], Resp),
            maybe_return(Ret, PN)
    end.

-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-spec sanitize_number(kz_term:ne_binary()) -> kz_term:ne_binary().
sanitize_number(<<"+", Rest/binary>>) ->
    sanitize_number(Rest);
sanitize_number(<<"1", Rest/binary>>) ->
    sanitize_number(Rest);
sanitize_number(Else) ->
    Else.

-spec do_find_numbers(handle_action_resp_ret(), kz_term:ne_binary()) -> knm_search:mod_response().
do_find_numbers({'error', _}, _) ->
    {'error', 'datastore_fault'};
do_find_numbers({'ok',JObjs}, QID) ->
    Numbers =
        [{QID, {kz_json:get_value(<<"e164">>, JObj), ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}
         || JObj <- JObjs
        ],
    {'ok', Numbers}.

-spec maybe_return(handle_action_resp_ret(), knm_phone_number:record()) ->
          knm_phone_number:record().
maybe_return({'error', Reason}, PN) ->
    knm_errors:by_carrier(?MODULE, Reason, PN);
maybe_return({'ok', JObj}, PN) ->
    case ?API_SUCCESS == kz_json:get_value(<<"code">>, JObj) of
        'true' -> PN;
        'false' ->
            Reason = kz_json:get_value(<<"msg">>, JObj),
            knm_errors:by_carrier(?MODULE, Reason, PN)
    end.

-spec handle_action_resp(atom(), any(), soap_response()) -> handle_action_resp_ret().

handle_action_resp('find_numbers', Quantity, {'ok', Xml}) ->
    XPath = xpath("getDIDs", ["DIDLocators", "DIDLocator"]),
    DIDs = xmerl_xpath:string(XPath, Xml),
    lager:debug("found ~p numbers", [length(DIDs)]),
    {'ok',
     [ kz_json:from_list(
         [{<<"e164">>, knm_converters:normalize(kz_xml:get_value("//tn/text()", DID))}
         ,{<<"rate_center">>, kz_xml:get_value("//rateCenter/text()", DID)}
         ,{<<"state">>, kz_xml:get_value("//state/text()", DID)}
         ,{<<"cnam">>, kz_term:is_true(kz_xml:get_value("//outboundCNAM/text()", DID))}
         ,{<<"t38">>, kz_term:is_true(kz_xml:get_value("//t38/text()", DID))}
         ])
       || DID=#xmlElement{} <- lists:sublist(DIDs, Quantity)
     ]
    };

handle_action_resp('acquire_number', _Numbers, {'ok', Xml}) ->
    XPath = xpath("assignDID", ["DIDs", "DID"]),
    [JObj] = [ begin
                   Code = kz_xml:get_value("//statusCode/text()", DID),
                   lager:debug("acquire ~s: ~s:~s",
                               [kz_xml:get_value("//tn/text()", DID)
                               ,Code
                               ,kz_xml:get_value("//status/text()", DID)
                               ]),
                   kz_json:from_list([{<<"code">>, Code}])
               end
               || DID=#xmlElement{name = 'DID'}
                      <- xmerl_xpath:string(XPath, Xml)
             ],
    {'ok', JObj};

handle_action_resp('disconnect_number', _Numbers, {'ok', Xml}) ->
    XPath = xpath("releaseDID", ["DIDs", "DID"]),
    [JObj] = [ begin
                   N = kz_xml:get_value("//tn/text()", DID),
                   Msg = kz_xml:get_value("//status/text()", DID),
                   Code = kz_xml:get_value("//statusCode/text()", DID),
                   lager:debug("disconnect ~s: ~s:~s", [N, Code, Msg]),
                   kz_json:from_list(
                     [{<<"code">>, Code}
                     ,{<<"msg">>, Msg}
                     ,{<<"e164">>, knm_converters:normalize(N)}
                     ])
               end
               || DID=#xmlElement{name = 'DID'}
                      <- xmerl_xpath:string(XPath, Xml)
             ],
    {'ok', JObj};

handle_action_resp(_Target, _, {'error',_R}=Error) ->
    Error.

-spec soap_request(nonempty_string(), any()) -> soap_response().
-ifndef(TEST).
soap_request(Action, Props) ->
    Body = soap_envelope(Action, Props),
    Url = ?URL_IN_USE,
    Headers = [{"SOAPAction", ?VI_DEFAULT_NAMESPACE++Action}
              ,{"Accept", "*/*"}
              ,{"User-Agent", ?KNM_USER_AGENT}
              ,{"Content-Type", "text/xml;charset=UTF-8"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 'verify_none'}, {versions, ['tlsv1.2']}]}
                  ,{'timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ,{'body_format', 'string'}
                  ],
    ?DEBUG_WRITE("Request:~n~s ~s~n~p~n~s~n", ['post', Url, Headers, Body]),
    UnicodeBody = unicode:characters_to_binary(Body),
    Resp = kz_http:post(Url, Headers, UnicodeBody, HTTPOptions),
    handle_response(Resp).
-else.
soap_request(Action, Props) ->
    _Body = soap_envelope(Action, Props),
    handle_response({'ok', 200, [], get_response_fixture(Action, Props)}).

-spec get_response_fixture(nonempty_string(), any()) -> soap_response().
get_response_fixture("queryDID", _) ->
    knm_util:fixture("voip_innovations_query_response.xml");
get_response_fixture("getDIDs", Props) ->
    case props:get_value("npa", Props) of
        <<"877">> -> knm_util:fixture("voip_innovations_get_tollfree_response.xml");
        _ -> knm_util:fixture("voip_innovations_get_response.xml")
    end;
get_response_fixture("assignDID", _) ->
    knm_util:fixture("voip_innovations_assign_response.xml");
get_response_fixture("releaseDID", _) ->
    knm_util:fixture("voip_innovations_release_response.xml").
-endif.

-spec soap_envelope(nonempty_string(), any()) -> iolist().
soap_envelope(Action, Props) ->
    ["<?xml version='1.0' encoding='UTF-8'?>"
     "<env:Envelope xmlns:xsd='http://www.w3.org/2001/XMLSchema'"
     " xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'"
     " xmlns:tns='", ?VI_DEFAULT_NAMESPACE, "'"
     " xmlns:env='http://schemas.xmlsoap.org/soap/envelope/'>"
     "<env:Body>"
     "<tns:", Action, ">",
     body(Action, Props),
     "<tns:login>", ?VI_LOGIN, "</tns:login>"
     "<tns:secret>", ?VI_PASSWORD, "</tns:secret>"
     "</tns:", Action, ">"
     "</env:Body>"
     "</env:Envelope>"
    ].

-spec body(nonempty_string(), any()) -> iolist().

body("queryDID", DID) ->
    ["<tns:did>", DID, "</tns:did>"];

body("getDIDs", Props) ->
    PossibleKeys = ["cnam"
                   ,"lata"
                   ,"npa"
                   ,"nxx"
                   ,"rateCenter"
                   ,"state"
                   ,"t38"
                   ,"tier"
                   ],
    [case props:get_value(Key, Props) of
         'undefined' ->
             ["<tns:", Key, " xsi:nil='true'/>"];
         Value ->
             ["<tns:", Key, ">", Value, "</tns:", Key, ">"]
     end
     || Key <- PossibleKeys
    ];

body("assignDID", Numbers=[_|_]) ->
    ["<tns:didParams>",
     [ ["<tns:DIDParam>"
        "<tns:tn>", Number, "</tns:tn>"
        "<tns:epg>", ?VI_ENDPOINT_GROUP, "</tns:epg>"
        "</tns:DIDParam>"]
       || Number <- Numbers
     ],
     "</tns:didParams>"
    ];

body("releaseDID", Numbers=[_|_]) ->
    ["<tns:didParams>",
     [ ["<tns:DIDParam>"
        "<tns:tn>", Number, "</tns:tn>"
        "</tns:DIDParam>"]
       || Number <- Numbers
     ],
     "</tns:didParams>"
    ].

-spec handle_response(kz_http:ret()) -> soap_response().
handle_response({'ok', _Code, _Headers, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [_Code, _Headers, Response]),
    lager:debug("received response"),
    try
        {Xml, _} = xmerl_scan:string(Response),
        verify_response(Xml)
    catch
        _:R ->
            lager:debug("failed to decode xml: ~p", [R]),
            {'error', 'empty_response'}
    end;
handle_response({'ok', Code, _Headers, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, _Response]),
    Reason = http_code(Code),
    lager:debug("request error: ~p (~s)", [Code, Reason]),
    {'error', Reason};
handle_response({'error', _}=E) ->
    lager:debug("request error: ~p", [E]),
    E.

-spec verify_response(kz_types:xml_el()) -> soap_response().
verify_response(Xml) ->
    RespCode = kz_xml:get_value("//responseCode/text()", Xml),
    RespMsg = kz_xml:get_value("//responseMessage/text()", Xml),
    lager:debug("carrier response: ~s ~s", [RespCode, RespMsg]),
    case RespCode of
        ?API_SUCCESS ->
            lager:debug("api request was successful"),
            {'ok', Xml};
        _ErrorResp ->
            lager:debug("api request failed: ~p", [_ErrorResp]),
            {'error', RespMsg}
    end.

-spec http_code(pos_integer()) -> atom().
http_code(401) -> 'unauthenticated';
http_code(403) -> 'unauthorized';
http_code(404) -> 'not_found';
http_code(Code) when Code >= 500 -> 'server_error';
http_code(_Code) -> 'empty_response'.

-spec xpath(nonempty_string(), [nonempty_string()]) -> nonempty_string().
xpath(Action, CategoryHierarchy) ->
    ToJoin = ["//" ++ Action ++ "Result" | CategoryHierarchy],
    string:join(ToJoin, "/").

%% API status codes
%% https://github.com/skywiretech/voip_api/blob/3ce46e6a43fa340237017bae4aae41bcc4d80506/lib/voip_api/response/did_response.rb#L109

%% Sandbox API
%% http://wiki.voipinnovations.com/display/VI/Supported+in+Sandbox
%% http://dev.voipinnovations.com/VOIP/Services/APIService.asmx?wsdl

%%% End of Module
