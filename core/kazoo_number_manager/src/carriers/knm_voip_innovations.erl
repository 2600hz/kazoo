%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc A Number Manager module for carrier: VoIPInnovations.com
%%% @author Pierre Fenoll, Joe Black
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

-ifdef(TEST).
-export([soap_request/2]).  %% Only to pass compilation
-endif.

-include("knm.hrl").

-define(KNM_VI_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".voip_innovations">>).
-define(VI_DEFAULT_NAMESPACE, "http://tempuri.org/").

%% (XML POST)
-define(VI_URL_V2, "https://backoffice.voipinnovations.com/api2.pl").
%% (Web Service)
-define(VI_URL_V3, "https://backoffice.voipinnovations.com/Services/APIService.asmx").
-define(VI_URL_V3_SANDBOX, "http://dev.voipinnovations.com/VOIP/Services/APIService.asmx").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(DEBUG_WRITE(Format, Args), ?debugFmt(Format, Args)).
-define(DEBUG_APPEND(Format, Args), ?debugFmt(Format, Args)).
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
-type to_json_ret() :: {'ok', kz_json:object() | kz_json:objects()} |
                       {'error', any()}.

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

-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {ok, kz_json:object()} |
                                              {error, any()}.
check_numbers(_Numbers) -> {error, not_implemented}.


%%------------------------------------------------------------------------------
%% @doc Query the system for a quantity of available numbers in a rate center
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_search:options()) ->
                          {'ok', list()} |
                          {'error', any()}.
find_numbers(<<"+", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<"1", Rest/binary>>, Quantity, Options) ->
    find_numbers(Rest, Quantity, Options);
find_numbers(<<NPA:3/binary>>, Quantity, Options) ->
    Resp = soap("getDIDs", [{"npa", NPA}]),
    MaybeJson = to_json('find_numbers', Quantity, Resp),
    to_numbers(MaybeJson, knm_search:query_id(Options));
find_numbers(<<NXX:6/binary,_/binary>>, Quantity, Options) ->
    Resp = soap("getDIDs", [{"nxx", NXX}]),
    MaybeJson = to_json('find_numbers', Quantity, Resp),
    to_numbers(MaybeJson, knm_search:query_id(Options)).

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from the carrier
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) -> knm_number:knm_number().
acquire_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            N = 'remove +1'(
                  knm_phone_number:number(knm_number:phone_number(Number))
                 ),
            Resp = soap("assignDID", [N]),
            Ret = to_json('acquire_number', [N], Resp),
            maybe_return(Ret, Number)
    end.

%%------------------------------------------------------------------------------
%% @doc Release a number from the routing table
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    Debug = ?IS_SANDBOX_PROVISIONING_TRUE,
    case ?IS_PROVISIONING_ENABLED of
        'false' when Debug ->
            lager:debug("allowing sandbox provisioning"),
            Number;
        'false' ->
            knm_errors:unspecified('provisioning_disabled', Number);
        'true' ->
            N = 'remove +1'(
                  knm_phone_number:number(knm_number:phone_number(Number))
                 ),
            Resp = soap("releaseDID", [N]),
            Ret = to_json('disconnect_number', [N], Resp),
            maybe_return(Ret, Number)
    end.

-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-spec 'remove +1'(kz_term:ne_binary()) -> kz_term:ne_binary().
'remove +1'(<<"+", Rest/binary>>) ->
    'remove +1'(Rest);
'remove +1'(<<"1", Rest/binary>>) ->
    'remove +1'(Rest);
'remove +1'(Else) ->
    Else.

-spec to_numbers(to_json_ret(), kz_term:ne_binary()) ->
                        {'ok', [tuple()]} |
                        {'error', any()}.
to_numbers({'error',_R}=Error, _) ->
    Error;
to_numbers({'ok',JObjs}, QID) ->
    Numbers =
        [{QID, {kz_json:get_value(<<"e164">>, JObj), ?MODULE, ?NUMBER_STATE_DISCOVERY, JObj}}
         || JObj <- JObjs
        ],
    {'ok', Numbers}.

-spec maybe_return(to_json_ret(), knm_number:knm_number()) ->
                          knm_number:knm_number().
maybe_return({'error', Reason}, N) ->
    knm_errors:by_carrier(?MODULE, Reason, N);
maybe_return({'ok', JObj}, N) ->
    case ?API_SUCCESS == kz_json:get_value(<<"code">>, JObj) of
        'true' -> N;
        'false' ->
            Reason = kz_json:get_value(<<"msg">>, JObj),
            maybe_return({'error', Reason}, N)
    end.

-spec to_json(atom(), any(), soap_response()) -> to_json_ret().

to_json('find_numbers', Quantity, {'ok', Xml}) ->
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

to_json('acquire_number', _Numbers, {'ok', Xml}) ->
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

to_json('disconnect_number', _Numbers, {'ok', Xml}) ->
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

to_json(_Target, _, {'error',_R}=Error) ->
    Error.

-spec soap(nonempty_string(), any()) -> soap_response().
-ifndef(TEST).
soap(Action, Props) ->
    Body = soap_envelope(Action, Props),
    soap_request(Action, Body).
-else.
soap(Action, Props) ->
    _Body = soap_envelope(Action, Props),
    Resp =
        case Action of
            "queryDID" ->
                knm_util:fixture("voip_innovations_query_response.xml");
            "getDIDs" ->
                case props:get_value("npa", Props) of
                    <<"877">> -> knm_util:fixture("voip_innovations_get_tollfree_response.xml");
                    _ -> knm_util:fixture("voip_innovations_get_response.xml")
                end;
            "assignDID" ->
                knm_util:fixture("voip_innovations_assign_response.xml");
            "releaseDID" ->
                knm_util:fixture("voip_innovations_release_response.xml")
        end,
    handle_response({'ok', 200, [], Resp}).
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
     "</env:Envelope>"].

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
     "</tns:didParams>"];

body("releaseDID", Numbers=[_|_]) ->
    ["<tns:didParams>",
     [ ["<tns:DIDParam>"
        "<tns:tn>", Number, "</tns:tn>"
        "</tns:DIDParam>"]
       || Number <- Numbers
     ],
     "</tns:didParams>"].

-spec soap_request(nonempty_string(), iolist()) -> soap_response().
soap_request(Action, Body) ->
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

-spec handle_response(kz_http:ret()) -> soap_response().
handle_response({'ok', Code, _Headers, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, Response]),
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
