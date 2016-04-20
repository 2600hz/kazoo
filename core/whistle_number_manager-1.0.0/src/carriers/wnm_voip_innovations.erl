%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% A Number Manager module for carrier: VoIPInnovations.com
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(wnm_voip_innovations).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../wnm.hrl").

-define(WNM_VI_CONFIG_CAT, <<(?WNM_CONFIG_CAT)/binary, ".voip_innovations">>).

-define(VI_DEBUG, whapps_config:get_is_true(?WNM_VI_CONFIG_CAT, <<"debug">>, 'false')).
-define(VI_DEBUG_FILE, "/tmp/voipinnovations.xml").
-define(DEBUG_WRITE(Format, Args),
        _ = ?VI_DEBUG andalso
            file:write_file(?VI_DEBUG_FILE, io_lib:format(Format, Args))
       ).
-define(DEBUG_APPEND(Format, Args),
        _ = ?VI_DEBUG andalso
            file:write_file(?VI_DEBUG_FILE, io_lib:format(Format, Args), ['append'])
       ).

-define(VI_DEFAULT_NAMESPACE, "http://tempuri.org/").

-define(IS_SANDBOX_PROVISIONING_TRUE,
       whapps_config:get_is_true(?WNM_VI_CONFIG_CAT, <<"sandbox_provisioning">>, 'false')).
-define(IS_PROVISIONING_ENABLED,
        whapps_config:get_is_true(?WNM_VI_CONFIG_CAT, <<"enable_provisioning">>, 'true')).

-define(VI_URL_V2, %% (XML POST)
        "https://backoffice.voipinnovations.com/api2.pl").
-define(VI_URL_V3, %% (Web Service)
        "https://backoffice.voipinnovations.com/Services/APIService.asmx").
-define(VI_URL_V3_SANDBOX,
        "http://dev.voipinnovations.com/VOIP/Services/APIService.asmx").
-define(URL_IN_USE,
       case ?IS_SANDBOX_PROVISIONING_TRUE of 'true' -> ?VI_URL_V3_SANDBOX; 'false' -> ?VI_URL_V3 end).

-define(VI_LOGIN, whapps_config:get_string(?WNM_VI_CONFIG_CAT, <<"login">>, <<>>)).
-define(VI_PASSWORD, whapps_config:get_string(?WNM_VI_CONFIG_CAT, <<"password">>, <<>>)).
-define(VI_ENDPOINT_GROUP, whapps_config:get_string(?WNM_VI_CONFIG_CAT, <<"endpoint_group">>, <<>>)).


-type soap_response() :: {'ok', xml_el()} | {'error', any()}.
-type to_json_ret() :: {'ok', wh_json:object()} | {'error', any()}.

%%% API

%% @public
-spec is_number_billable(wnm_number()) -> boolean().
is_number_billable(_Number) -> 'true'.


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the system for a quantity of available numbers in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) -> to_json_ret().
find_numbers(<<"+", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<"1", Rest/binary>>, Quantity, Opts) ->
    find_numbers(Rest, Quantity, Opts);
find_numbers(<<NPA:3/binary>>, Quantity, _) ->
    Resp = soap("getDIDs", [{"npa", NPA}]),
    to_json('find_numbers', Quantity, Resp);
find_numbers(<<NXX:6/binary,_/binary>>, Quantity, _) ->
    Resp = soap("getDIDs", [{"nxx", NXX}]),
    to_json('find_numbers', Quantity, Resp).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(wnm_number()) -> wnm_number().
acquire_number(#number{dry_run = 'true'}=Number) -> Number;
acquire_number(#number{number = <<$+,Number/binary>>}=NR) ->
    acquire_number(NR#number{number = Number});
acquire_number(#number{number = <<$1,Number/binary>>}=NR) ->
    acquire_number(NR#number{number = Number});
acquire_number(#number{number = Number}=NR) ->
    N = NR#number{number = wnm_util:normalize_number(Number)},
    case ?IS_PROVISIONING_ENABLED or ?IS_SANDBOX_PROVISIONING_TRUE of
        'true' ->
            Resp = soap("assignDID", [Number]),
            maybe_return(N, to_json('acquire_number', [Number], Resp));
        'false' ->
            Error = <<"Unable to acquire numbers on this system, carrier provisioning is disabled">>,
            wnm_number:error_carrier_fault(Error, N)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(#number{dry_run = 'true'}=Number) -> Number;
disconnect_number(#number{number = <<$+,Number/binary>>}=NR) ->
    disconnect_number(NR#number{number = Number});
disconnect_number(#number{number = <<$1,Number/binary>>}=NR) ->
    disconnect_number(NR#number{number = Number});
disconnect_number(#number{number = Number}=NR) ->
    N = NR#number{number = wnm_util:normalize_number(Number)},
    case ?IS_PROVISIONING_ENABLED or ?IS_SANDBOX_PROVISIONING_TRUE of
        'true' ->
            Resp = soap("releaseDID", [Number]),
            maybe_return(N, to_json('disconnect_number', [Number], Resp));
        'false' ->
            Error = <<"Unable to acquire numbers on this system, carrier provisioning is disabled">>,
            wnm_number:error_carrier_fault(Error, N)
    end.

%% @public
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.


%%% Internals

-spec maybe_return(wnm_number(), to_json_ret()) -> wnm_number().
maybe_return(N, {'error', Reason}) ->
    wnm_number:error_carrier_fault(Reason, N);
maybe_return(N, {'ok', JObj}) ->
    case <<"100">> == wh_json:get_value(<<"code">>, JObj) of
        'true' -> N;
        'false' ->
            Reason = wh_json:get_value(<<"msg">>, JObj),
            maybe_return(N, {'error', Reason})
    end.

-spec to_json(atom(), any(), soap_response()) -> to_json_ret().

to_json('get_number_data', _Number, {'ok', Xml}) ->
    XPath = xpath("queryDID", ["DIDs", "DID"]),
    [DID] = xmerl_xpath:string(XPath, Xml),
    Code = wh_util:get_xml_value("//statusCode/text()", DID),
    Msg = wh_util:get_xml_value("//status/text()", DID),
    lager:debug("lookup ~s: ~s:~s", [_Number, Code, Msg]),
    R = [{<<"e164">>, wh_util:get_xml_value("//tn/text()", DID)}
        ,{<<"status">>, wh_util:get_xml_value("//availability/text()", DID)}
        ,{<<"msg">>, Msg}
        ,{<<"code">>, Code}
        ,{<<"expireDate">>, wh_util:get_xml_value("//expireDate/text()", DID)}
        ,{<<"has411">>, wh_util:is_true(wh_util:get_xml_value("//has411/text()", DID))}
        ,{<<"has911">>, wh_util:is_true(wh_util:get_xml_value("//has911/text()", DID))}
        ,{<<"t38">>, wh_util:is_true(wh_util:get_xml_value("//t38/text()", DID))}
        ,{<<"cnam">>, wh_util:is_true(wh_util:get_xml_value("//cnam/text()", DID))}
        ,{<<"cnamStorageActive">>, wh_util:is_true(wh_util:get_xml_value("//cnamStorageActive/text()", DID))}
        ,{<<"cnamStorageAvailability">>, wh_util:is_true(wh_util:get_xml_value("//cnamStorageAvailability/text()", DID))}
        ],
    {'ok', wh_json:from_list(R)};

to_json('find_numbers', Quantity, {'ok', Xml}) ->
    XPath = xpath("getDIDs", ["DIDLocators", "DIDLocator"]),
    DIDs = xmerl_xpath:string(XPath, Xml),
    lager:debug("found ~p numbers", [length(DIDs)]),
    R =
        [ begin
              Number = wh_util:get_xml_value("//tn/text()", DID),
              JObj =
                  wh_json:from_list(
                    [{<<"e164">>, Number}
                    ,{<<"rate_center">>, wh_util:get_xml_value("//rateCenter/text()", DID)}
                    ,{<<"state">>, wh_util:get_xml_value("//state/text()", DID)}
                    ,{<<"cnam">>, wh_util:is_true(wh_util:get_xml_value("//outboundCNAM/text()", DID))}
                    ,{<<"t38">>, wh_util:is_true(wh_util:get_xml_value("//t38/text()", DID))}
                    ]),
              {Number, JObj}
          end
          || DID=#xmlElement{} <- lists:sublist(DIDs, Quantity)
        ],
    {'ok', wh_json:from_list(R)};

to_json('acquire_number', _Numbers, {'ok', Xml}) ->
    XPath = xpath("assignDID", ["DIDs", "DID"]),
    [JObj] = [ begin
                   Code = wh_util:get_xml_value("//statusCode/text()", DID),
                   lager:debug("acquire ~s: ~s:~s",
                               [wh_util:get_xml_value("//tn/text()", DID)
                               ,Code
                               ,wh_util:get_xml_value("//status/text()", DID)
                               ]),
                   wh_json:from_list([{<<"code">>, Code}])
               end
               || DID=#xmlElement{name = 'DID'}
                      <- xmerl_xpath:string(XPath, Xml)
             ],
    {'ok', JObj};

to_json('disconnect_number', _Numbers, {'ok', Xml}) ->
    XPath = xpath("releaseDID", ["DIDs", "DID"]),
    [JObj] = [ begin
                   Code = wh_util:get_xml_value("//statusCode/text()", DID),
                   lager:debug("disconnect ~s: ~s:~s",
                               [wh_util:get_xml_value("//tn/text()", DID)
                               ,Code
                               ,wh_util:get_xml_value("//status/text()", DID)
                               ]),
                   wh_json:from_list([{<<"code">>, Code}])
               end
               || DID=#xmlElement{name = 'DID'}
                      <- xmerl_xpath:string(XPath, Xml)
             ],
    {'ok', JObj};

to_json(_Target, _, {'error',_R}=Error) ->
    Error.

-spec soap(nonempty_string(), any()) -> soap_response().
soap(Action, Props) ->
    Body = soap_envelope(Action, Props),
    soap_request(Action, Body).

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
     "</tns:didParams>"];

body("auditDIDs", []) ->
    "".

-spec soap_request(nonempty_string(), iolist()) -> soap_response().
soap_request(Action, Body) ->
    Url = ?URL_IN_USE,
    Headers = [{"SOAPAction", ?VI_DEFAULT_NAMESPACE++Action}
               ,{"Accept", "*/*"}
               ,{"User-Agent", ?WNM_USER_AGENT}
               ,{"Content-Type", "text/xml;charset=UTF-8"}
              ],
    HTTPOptions = [{'ssl', [{'verify', 0}]}
                   ,{'inactivity_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                   ,{'connect_timeout', 180 * ?MILLISECONDS_IN_SECOND}
                  ],
    Timeout = 180 * ?MILLISECONDS_IN_SECOND,
    ?DEBUG_WRITE("Request:~n~s ~s~n~p~n~s~n", ['post', Url, Headers, Body]),
    UnicodeBody = unicode:characters_to_binary(Body),
    Resp = ibrowse:send_req(Url, Headers, 'post', UnicodeBody, HTTPOptions, Timeout),
    handle_response(Resp).

-spec handle_response(ibrowse_ret()) -> soap_response().
handle_response({'ok', Code, _Headers, "<?xml"++_=Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, Response]),
    lager:debug("received response"),
    try
        {Xml, _} = xmerl_scan:string(Response),
        verify_response(Xml)
    catch
        _:R ->
            lager:debug("failed to decode xml: ~p", [R]),
            lager:debug("st: ~p", [erlang:get_stacktrace()]),
            {'error', 'empty_response'}
    end;
handle_response({'ok', Code, _Headers, _Response}) ->
    ?DEBUG_APPEND("Response:~n~p~n~p~n~s~n", [Code, _Headers, _Response]),
    Reason = http_code(Code),
    lager:debug("request error: ~s (~s)", [Code, Reason]),
    {'error', Reason};
handle_response({'error', _}=E) ->
    lager:debug("request error: ~p", [E]),
    E.

-spec verify_response(xml_el()) -> soap_response().
verify_response(Xml) ->
    RespCode = wh_util:get_xml_value("//responseCode/text()", Xml),
    RespMsg = wh_util:get_xml_value("//responseMessage/text()", Xml),
    lager:debug("carrier response: ~s ~s", [RespCode, RespMsg]),
    case RespCode == <<"100">> of
        'true' ->
            lager:debug("request was successful"),
            {'ok', Xml};
        'false' ->
            lager:debug("request failed"),
            {'error', RespMsg}
    end.

-spec http_code(nonempty_string()) -> atom().
http_code("401") -> 'unauthenticated';
http_code("403") -> 'unauthorized';
http_code("404") -> 'not_found';
http_code("5"++[_,_]) -> 'server_error';
http_code(_Code) -> 'empty_response'.

-spec xpath(nonempty_string(), [nonempty_string()]) -> nonempty_string().
xpath(Action, []) ->
    "//" ++ Action ++ "Result";
xpath(Action, CategoryHierarchy) ->
    ToJoin = ["//" ++ Action ++ "Result" | CategoryHierarchy],
    string:join(ToJoin, "/").


%% API status codes
%% https://github.com/skywiretech/voip_api/blob/3ce46e6a43fa340237017bae4aae41bcc4d80506/lib/voip_api/response/did_response.rb#L109

%% Sandbox API
%% http://wiki.voipinnovations.com/display/VI/Supported+in+Sandbox
%% http://dev.voipinnovations.com/VOIP/Services/APIService.asmx?wsdl

%%% End of Module
