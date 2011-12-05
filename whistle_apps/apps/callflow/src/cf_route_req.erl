%%%-------------------------------------------------------------------
%%% @author Karl anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% handler for route requests, responds if callflows match
%%% @end
%%% Created : 30 Nov 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_route_req).

-include("callflow.hrl").

-export([handle_req/2]).

-spec handle_req/2 :: (json_object(), proplist()) -> no_return().
handle_req(JObj, Options) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        undefined ->
            ok;
        AccountId ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            put(callid, CallId),

            ?LOG_START("received route request"),

            CVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
            Request = wh_json:get_value(<<"Request">>, JObj, <<"nouser@norealm">>),
            From = wh_json:get_value(<<"From">>, JObj, <<"nouser@norealm">>),
            To = wh_json:get_value(<<"To">>, JObj, <<"nouser@norealm">>),

            [ToUser, ToRealm] = binary:split(To, <<"@">>),
            [FromUser, FromRealm] = binary:split(From, <<"@">>),
            [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),

            Call = #cf_call{call_id = CallId
                            ,bdst_q = props:get_value(queue, Options)
                            ,cid_name = wh_json:get_value(<<"Caller-ID-Name">>, JObj, <<"Unknown">>)
                            ,cid_number = wh_json:get_value(<<"Caller-ID-Number">>, JObj, <<"0000000000">>)
                            ,request = Request
                            ,request_user = wh_util:to_e164(RequestUser)
                            ,request_realm = RequestRealm
                            ,from = From
                            ,from_user = FromUser
                            ,from_realm = FromRealm
                            ,to = To
                            ,to_user = ToUser
                            ,to_realm = ToRealm
                            ,inception = wh_json:get_value(<<"Inception">>, CVs)
                            ,account_id = AccountId
                            ,account_db = whapps_util:get_db_name(AccountId, encoded)
                            ,authorizing_id = wh_json:get_ne_value(<<"Authorizing-ID">>, CVs)
                            ,channel_vars = CVs
                            ,inception_during_transfer = wh_json:is_true(<<"During-Transfer">>, JObj)
                           },
            fulfill_call_request(JObj, Call)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% attempt to fulfill authorized call requests
%% @end
%%-----------------------------------------------------------------------------
-spec fulfill_call_request/2 :: (#cf_call{}, json_object()) -> no_return().
fulfill_call_request(JObj, #cf_call{call_id=CallId, account_id=AccountId}=Call) ->
    case cf_util:lookup_callflow(Call) of
        {ok, Flow, NoMatch} ->
            FlowId = wh_json:get_value(<<"_id">>, Flow),
            ?LOG("callflow ~s in ~s satisfies request", [FlowId, AccountId]),
            wh_cache:store({cf_call, CallId}, Call#cf_call{flow_id = FlowId, no_match = NoMatch}, 5),
            send_route_response(JObj, Call);
        {error, R} ->
            ?LOG_END("unable to find callflow ~p", [R])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response/2 :: (json_object(), #cf_call{}) -> 'ok'.
send_route_response(JObj, #cf_call{channel_vars=CVs, bdst_q=Q}) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            ,{<<"channel_vars">>, CVs}
            | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)],
    wapi_route:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp),
    ?LOG_END("sent route response to park the call").
