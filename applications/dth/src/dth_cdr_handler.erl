%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @author James Aimonetti <james@2600hz.org>
%%% @doc
%%% Send a CDR payload to DTH
%%% @end
%%% Created : 29 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth_cdr_handler).

-export([init/0, handle_req/2]).
%% -export([handle_req2/2]).

-include("dth.hrl").

-define(DTH_SUBMITCALLRECORD, <<"<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">
<s:Body xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><SubmitCallRecord xmlns=\"http://tempuri.org/\"><oCallRecord><AccountCode>~s</AccountCode><OriginatingNumber>~s</OriginatingNumber><DestinationNumber>~s</DestinationNumber><StartTime>~s</StartTime><Duration>~s</Duration><UniqueID>~s</UniqueID><BilledDuration>0</BilledDuration><CallCost>0</CallCost><CallTotal>0</CallTotal><Direction>0</Direction><WholesaleRate>0</WholesaleRate><WholesaleCost>0</WholesaleCost><RetailRate>0</RetailRate><CallTax>0</CallTax><IsIncluded>0</IsIncluded><BilledTier>0</BilledTier><PrintIndicator>0</PrintIndicator><EndTime>0001-01-01T00:00:00</EndTime><CallType>~s</CallType></oCallRecord></SubmitCallRecord></s:Body></s:Envelope>">>).
%% AccountCode: ab/12/345asdfhlj (17 chars) (add -IN if originated off-net)
%% OriginatingNumber: 2223334444
%% DestinationNumber: 2223334444
%% StartTime: 2010-05-24T01:55:00Z
%% Duration: 100 (in seconds)
%% UniqueID: callid
%% CallType: Interstate

-spec init() -> 'ok'.
init() ->
    ok.

%% handle_req2(JObj, Props) ->
%%     true = kapi_call:cdr_v(JObj),

%%     CallID = kz_json:get_value(<<"Call-ID">>, JObj),
%%     CallDirection = kz_json:get_value(<<"Call-Direction">>, JObj),
%%     ?LOG_SYS(CallID, "Valid call cdr", []),
%%     ?LOG_SYS(CallID, "Call Direction: ~s", [CallDirection]),

%%     <<"outbound">> = CallDirection, %% b-leg only, though not the greatest way of determining this

%%     Timestamp = kz_util:to_integer(kz_json:get_value(<<"Timestamp">>, JObj, kz_util:current_tstamp())),
%%     BillingSec = kz_util:to_integer(kz_json:get_value(<<"Billing-Seconds">>, JObj, 0)),

%%     DateTime = now_to_datetime(Timestamp - BillingSec),

%%     ToE164 = kz_util:to_e164(get_to_user(JObj)),
%%     FromE164 = kz_util:to_e164(get_from_user(JObj)),

%%     AccountCode = get_account_code(JObj),

%%     ?LOG(CallID, "CDR from ~s to ~s with account code ~s", [FromE164, ToE164, AccountCode]),

%%     CallRecord = #'p:CallRecord'{'CustomerID'=AccountCode
%%                                  ,'OriginatingNumber'=FromE164
%%                                  ,'DestinationNumber'=ToE164
%%                                  ,'StartTime'=DateTime
%%                                  ,'Duration'=kz_util:to_binary(BillingSec)
%%                                  ,'UniqueID'=CallID
%%                                  ,'CallType'=?DTH_CALL_TYPE_OTHER},
%%     WsdlModel = props:get_value(wsdl, Props),
%%     detergent:call(WsdlModel, "SubmitCallRecord", [CallRecord]).

-spec handle_req(kz_json:object(), kz_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = kapi_call:event_v(JObj),
    CallID = kz_json:get_value(<<"Call-ID">>, JObj),
    kz_util:put_callid(CallID),

    CallDirection = kz_json:get_value(<<"Call-Direction">>, JObj),
    lager:debug("valid call cdr"),
    lager:debug("call direction: ~s", [CallDirection]),

    <<"outbound">> = CallDirection, %% b-leg only, though not the greatest way of determining this

    Timestamp = kz_util:to_integer(kz_json:get_value(<<"Timestamp">>, JObj, kz_util:current_tstamp())),
    BillingSec = kz_util:to_integer(kz_json:get_value(<<"Billing-Seconds">>, JObj, 0)),

    DateTime = now_to_datetime(Timestamp - BillingSec),

    ToE164 = knm_converters:normalize(get_to_user(JObj)),
    FromE164 = knm_converters:normalize(get_from_user(JObj)),

    AccountCode = get_account_code(JObj),

    lager:debug("cdr from ~s to ~s with account code ~s", [FromE164, ToE164, AccountCode]),

    XML = iolist_to_binary(io_lib:format(?DTH_SUBMITCALLRECORD
                                        ,[AccountCode
                                         ,FromE164
                                         ,ToE164
                                         ,DateTime
                                         ,kz_util:to_binary(BillingSec)
                                         ,CallID
                                         ,?DTH_CALL_TYPE_OTHER
                                         ])),

    lager:debug("XML to send: ~s", [XML]),

    send_xml(XML, Props).

send_xml(XML, Props) ->
    Headers = [{"Content-Type", "text/xml; charset=utf-8"}
              ,{"Content-Length", binary:referenced_byte_size(XML)}
              ,{"SOAPAction", "http://tempuri.org/SubmitCallRecord"}
              ],

    case kz_http:post(props:get_value('cdr_url', Props), Headers, XML) of
        {'ok', 200, _, RespXML} ->
            lager:debug("XML sent to DTH successfully: ~s", [RespXML]);
        _Resp ->
            lager:debug("error with request: ~p", [_Resp])
    end.

now_to_datetime(Secs) ->
    {{YY,MM,DD},{Hour,Min,Sec}} = calendar:gregorian_seconds_to_datetime(Secs),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                                   [YY, MM, DD, Hour, Min, Sec])).

-spec get_to_user(kz_json:object()) -> binary().
get_to_user(JObj) ->
    case kz_json:get_value(<<"Callee-ID-Number">>, JObj) of
        undefined ->
            case kz_json:get_value(<<"To-Uri">>, JObj) of
                undefined -> <<"+00000000000">>;
                ToUri ->
                    [To, _ToRealm] = binary:split(ToUri, <<"@">>),
                    To
            end;
        To ->
            To
    end.

-spec get_from_user(kz_json:object()) -> binary().
get_from_user(JObj) ->
    case kz_json:get_value(<<"Caller-ID-Number">>, JObj) of
        undefined ->
            case kz_json:get_value(<<"From-Uri">>, JObj) of
                undefined -> <<"+00000000000">>;
                FromUri ->
                    [From, _FromRealm] = binary:split(FromUri, <<"@">>),
                    From
            end;
        From ->
            From
    end.

-spec get_account_code(kz_json:object()) -> ne_binary().
get_account_code(JObj) ->
    AccountID = kz_util:truncate_left_binary(
                  kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj)
                                            ,17
                 ),
    case kz_json:get_value([<<"Custom-Channel-Vars">>, <<"Inception">>], JObj) of
        'undefined' -> AccountID;
        _Else -> << AccountID/binary, "-IN">>
    end.

