%% @author root
%% @doc @todo Add description to doodle_util.


-module(doodle_util).

-include("doodle.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_sms/2, save_sms/2]).

-spec create_sms(whapps_call:call(), wh_proplist()) -> any().
create_sms(Call, Endpoints) ->
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
    
    [             
     {<<"Message-ID">>, whapps_call:kvs_fetch(<<"Message-ID">>, Call)}
     ,{<<"Call-ID">>, whapps_call:call_id(Call)}
     ,{<<"Body">>, whapps_call:kvs_fetch(<<"Body">>, Call)}
     ,{<<"From">>, whapps_call:from(Call)}
     ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
     ,{<<"To">>, whapps_call:to(Call)}
     ,{<<"Request">>, whapps_call:request(Call) }
     ,{<<"Endpoints">>, Endpoints}
     ,{<<"Application-Name">>, <<"send">>}
     ,{<<"Custom-Channel-Vars">>, wh_json:set_values(CCVUpdates, wh_json:new())}
         | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].    

-spec save_sms(wh_json:object(), whapps_call:call()) -> 'ok'.
save_sms(JObj, Call) ->
    AccountId = whapps_call:account_id(Call),
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    OwnerId = whapps_call:owner_id(Call),
    AuthType = whapps_call:authorizing_type(Call),
    AuthId = whapps_call:authorizing_id(Call),
    Body = wh_json:get_value(<<"Body">>, JObj),
    To = whapps_call:to(Call),
    From = whapps_call:from(Call),
    Request = whapps_call:request(Call),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),    
    
    Doc = props:filter_undefined(
            [
             {<<"_id">>, whapps_call:call_id(Call)}
             ,{<<"pvt_type">>, <<"sms">> }
             ,{<<"account_id">>, AccountId }
             ,{<<"owner_id">>, OwnerId }
             ,{<<"authorization_type">>, AuthType }
             ,{<<"authorization_id">>, AuthId }
             ,{<<"to">>, To }
             ,{<<"to_user">>, ToUser }
             ,{<<"to_realm">>, ToRealm }
             ,{<<"from">>, From }
             ,{<<"from_user">>, FromUser }
             ,{<<"from_realm">>, FromRealm }
             ,{<<"request">>, Request }
             ,{<<"request_user">>, RequestUser }
             ,{<<"request_realm">>, RequestRealm }
             ,{<<"body">>, Body }
             ,{<<"message_id">>, MessageId}
             ,{<<"pvt_created">>, wh_util:current_tstamp()}
             ,{<<"pvt_status">>, <<"pending">>}
             ,{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"pvt_call">>, whapps_call:to_json(Call)}                                 
             ,{<<"pvt_json">>, JObj}                                 
            ]),
    {'ok', JObjSaved} = kazoo_modb:save_doc(AccountId, wh_json:from_list(Doc), []).

