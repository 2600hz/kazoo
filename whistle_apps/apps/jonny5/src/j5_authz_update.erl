%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_update).

-export([handle_req/2]).

-include("jonny5.hrl").

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_authz:update_v(JObj),
    wh_util:put_callid(JObj),
    timer:sleep(crypto:rand_uniform(0, 1000)),
    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    AccountId = wh_json:get_value(<<"Account-ID">>, CCV),
    ResellerId = wh_json:get_value(<<"Reseller-ID">>, CCV),
    case wh_json:get_value(<<"Account-Billing">>, CCV) of
        <<"per_minute">> -> j5_credit:session_heartbeat(AccountId, JObj);
        <<"allotment">> -> j5_allotments:session_heartbeat(AccountId, JObj);
        _ -> ok
    end,
    case wh_json:get_value(<<"Reseller-Billing">>, CCV) of
        <<"per_minute">> -> j5_credit:session_heartbeat(ResellerId, JObj);
        <<"allotment">> -> j5_allotments:session_heartbeat(ResellerId, JObj);
        _ -> ok
    end.
 
