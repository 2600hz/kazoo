%% @author root
%% @doc @todo Add description to doodle_util.


-module(doodle_util).

-include("doodle.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([send_sms/2]).

-spec send_sms(whapps_call:call(), wh_proplist()) -> any().
send_sms(Call, Endpoints) ->
    lager:debug("Endpoint ~p",[Endpoints]),
    AccountId = whapps_call:account_id(Call),
    AccountRealm =  whapps_call:to_realm(Call),
    CCVUpdates = props:filter_undefined(
                   [{<<"Ignore-Display-Updates">>, <<"true">>}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Account-Realm">>, AccountRealm}
                    ,{<<"From-User">>, whapps_call:from_user(Call)}
                    ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
                    ,{<<"From-URI">>, whapps_call:from(Call)}
                    ,{<<"Reseller-ID">>, wh_services:find_reseller_id(AccountId)}
                   ]),
    
    Payload = [             
               {<<"Message-ID">>, whapps_call:kvs_fetch(<<"Message-ID">>, Call)}
               ,{<<"Msg-ID">>, wh_util:rand_hex_binary(16)}
               ,{<<"Call-ID">>, wh_util:rand_hex_binary(16)}
               ,{<<"Body">>, whapps_call:kvs_fetch(<<"Body">>, Call)}
               ,{<<"From">>, whapps_call:from(Call)}
               ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
               ,{<<"To">>, whapps_call:to(Call)}
               ,{<<"Request">>, whapps_call:request(Call) }
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Application-Name">>, <<"send">>}
               ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, wh_json:new())}
                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
              ],    
    
    lager:info("Payload ~p",[Payload]),
    whapps_util:amqp_pool_request(Payload
                                  ,fun wapi_sms:publish_message/1
                                  ,fun wapi_sms:delivery_v/1).



%% ====================================================================
%% Internal functions
%% ====================================================================
    



