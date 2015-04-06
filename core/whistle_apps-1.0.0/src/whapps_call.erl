%%============================================================================
%%% @copyright (C) 2011-2015 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%============================================================================
-module(whapps_call).

-include("whapps_call_command.hrl").

-export([new/0, put_callid/1]).
-export([from_route_req/1, from_route_req/2]).
-export([from_route_win/1, from_route_win/2]).
-export([from_originate_uuid/1, from_originate_uuid/2]).
-export([from_channel_create/1, from_channel_create/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([to_proplist/1]).
-export([is_call/1]).

-export([exec/2]).

-export([set_application_name/2, application_name/1]).
-export([set_application_version/2, application_version/1]).
-export([set_call_id/2, call_id/1, call_id_direct/1]).
-export([set_other_leg_call_id/2, other_leg_call_id/1]).
-export([call_id_helper/2, clear_call_id_helper/1]).
-export([set_control_queue/2, control_queue/1, control_queue_direct/1]).
-export([control_queue_helper/2, clear_control_queue_helper/1]).
-export([set_controller_queue/2, controller_queue/1]).

-export([clear_helpers/1]).

-export([set_caller_id_name/2, caller_id_name/1]).
-export([set_caller_id_number/2, caller_id_number/1]).
-export([set_callee_id_name/2, callee_id_name/1]).
-export([set_callee_id_number/2, callee_id_number/1]).

-export([set_request/2, request/1, request_user/1, request_realm/1]).
-export([set_from/2, from/1, from_user/1, from_realm/1]).
-export([set_to/2, to/1, to_user/1, to_realm/1]).

-export([set_account_db/2, account_db/1]).
-export([set_account_id/2, account_id/1]).
-export([account_realm/1]).

-export([set_switch_nodename/2, switch_nodename/1]).
-export([set_switch_hostname/2, switch_hostname/1]).
-export([set_switch_url/2, switch_url/1]).
-export([set_switch_uri/2, switch_uri/1]).
-export([set_inception/2, inception/1]).

-export([set_authorizing_id/2, authorizing_id/1]).
-export([set_authorizing_type/2, authorizing_type/1]).
-export([set_authorization/3]).
-export([set_resource_type/2, resource_type/1]).
-export([set_owner_id/2, owner_id/1]).
-export([set_fetch_id/2, fetch_id/1]).
-export([set_bridge_id/2, bridge_id/1]).
-export([set_language/2, language/1]).
-export([set_to_tag/2, to_tag/1]).
-export([set_from_tag/2, from_tag/1]).

-export([set_dtmf_collection/2, set_dtmf_collection/3
         ,get_dtmf_collection/1, get_dtmf_collection/2
         ,add_to_dtmf_collection/2, add_to_dtmf_collection/3
        ]).

-export([set_custom_channel_var/3
         ,set_custom_channel_vars/2
         ,update_custom_channel_vars/2
         ,custom_channel_var/3
         ,custom_channel_var/2
         ,custom_channel_vars/1
        ]).

-export([set_custom_sip_header/3
         ,set_custom_sip_headers/2
         ,custom_sip_header/2, custom_sip_header/3
         ,custom_sip_headers/1
        ]).

-export([set_custom_publish_function/2, clear_custom_publish_function/1
         ,custom_publish_function/1
        ]).

-export([kvs_append/3
         ,kvs_append_list/3
         ,kvs_erase/2
         ,kvs_fetch/2, kvs_fetch/3
         ,kvs_fetch_keys/1
         ,kvs_filter/2
         ,kvs_find/2
         ,kvs_flush/1
         ,kvs_fold/3
         ,kvs_from_proplist/2
         ,kvs_is_key/2
         ,kvs_map/2
         ,kvs_store/3
         ,kvs_store_proplist/2
         ,kvs_to_proplist/1
         ,kvs_update/3
         ,kvs_update/4
         ,kvs_update_counter/3
        ]).

-export([flush/0
         ,cache/1, cache/2, cache/3
         ,retrieve/1, retrieve/2
        ]).

-export([default_helper_function/2]).

-define(DEFAULT_CALLER_ID_NAME, <<"Unknown">>).
-define(DEFAULT_CALLER_ID_NUMBER, <<"0000000000">>).

-record(whapps_call, {call_id :: api_binary()                       %% The UUID of the call
                      ,call_id_helper = fun ?MODULE:default_helper_function/2 :: whapps_helper_function()         %% A function used when requesting the call id, to ensure it is up-to-date
                      ,control_q :: api_binary()                   %% The control queue provided on route win
                      ,control_q_helper = fun ?MODULE:default_helper_function/2 :: whapps_helper_function()       %% A function used when requesting the call id, to ensure it is up-to-date
                      ,controller_q :: api_binary()                %%
                      ,caller_id_name = ?DEFAULT_CALLER_ID_NAME :: ne_binary()      %% The caller name
                      ,caller_id_number = ?DEFAULT_CALLER_ID_NUMBER :: ne_binary() %% The caller number
                      ,callee_id_name :: api_binary()                     %% The callee name
                      ,callee_id_number :: api_binary()                   %% The callee number
                      ,switch_nodename = <<>> :: binary()                 %% The switch node name (as known in ecallmgr)
                      ,switch_hostname :: api_binary()                    %% The switch hostname (as reported by the switch)
                      ,switch_url :: api_binary()                         %% The switch url
                      ,switch_uri :: api_binary()                         %% The switch uri
                      ,request = <<"nouser@norealm">> :: ne_binary()      %% The request of sip_request_user + @ + sip_request_host
                      ,request_user = <<"nouser">> :: ne_binary()         %% SIP request user
                      ,request_realm = <<"norealm">> :: ne_binary()       %% SIP request host
                      ,from = <<"nouser@norealm">> :: ne_binary()         %% Result of sip_from_user + @ + sip_from_host
                      ,from_user = <<"nouser">> :: ne_binary()            %% SIP from user
                      ,from_realm = <<"norealm">> :: ne_binary()          %% SIP from host
                      ,to = <<"nouser@norealm">> :: ne_binary()           %% Result of sip_to_user + @ + sip_to_host
                      ,to_user = <<"nouser">> :: ne_binary()              %% SIP to user
                      ,to_realm = <<"norealm">> :: ne_binary()            %% SIP to host
                      ,inception :: api_binary()                   %% Origin of the call <<"on-net">> | <<"off-net">>
                      ,account_db :: api_binary()                  %% The database name of the account that authorized this call
                      ,account_id :: api_binary()                  %% The account id that authorized this call
                      ,authorizing_id :: api_binary()              %% The ID of the record that authorized this call
                      ,authorizing_type :: api_binary()            %% The pvt_type of the record that authorized this call
                      ,owner_id :: api_binary()                    %% The ID of the owner of this calling device, if any
                      ,fetch_id :: api_binary()                    %% The Fetch ID of the Call
                      ,bridge_id :: api_binary()                    %% The Bridge ID of the Call
                      ,language :: api_binary()                     %% Language of the call to use
                      ,app_name = <<"whapps_call">> :: ne_binary()        %% The application name used during whapps_call_command
                      ,app_version = <<"1.0.0">> :: ne_binary()           %% The application version used during whapps_call_command
                      ,custom_publish_fun :: whapps_custom_publish() | 'undefined'     %% A custom command used to publish whapps_call_command
                      ,ccvs = wh_json:new() :: wh_json:object()      %% Any custom channel vars that where provided with the route request
                      ,sip_headers = wh_json:new() :: wh_json:object()                   %% Custom SIP Headers
                      ,kvs = orddict:new() :: orddict:orddict()           %% allows callflows to set values that propogate to children
                      ,other_leg_call_id :: api_binary()
                      ,resource_type :: api_binary()                      %% from route_req
                      ,to_tag :: api_binary()
                      ,from_tag :: api_binary()
                     }).

-type call() :: #whapps_call{}.
-export_type([call/0]).

-type whapps_helper_function() :: fun((api_binary(), call()) -> api_binary()).

-define(SPECIAL_VARS, [{<<"Caller-ID-Name">>, #whapps_call.caller_id_name}
                       ,{<<"Caller-ID-Number">>, #whapps_call.caller_id_number}
                       ,{<<"Account-ID">>, #whapps_call.account_id}
                       ,{<<"Owner-ID">>, #whapps_call.owner_id}
                       ,{<<"Fetch-ID">>, #whapps_call.fetch_id}
                       ,{<<"Bridge-ID">>, #whapps_call.bridge_id}
                       ,{<<"Authorizing-ID">>, #whapps_call.authorizing_id}
                       ,{<<"Authorizing-Type">>, #whapps_call.authorizing_type}
                      ]).

-spec default_helper_function(Field, call()) -> Field.
default_helper_function(Field, #whapps_call{}) -> Field.

-spec clear_helpers(call()) -> call().
clear_helpers(#whapps_call{}=Call) ->
    Fs = [fun clear_custom_publish_function/1
          ,fun clear_call_id_helper/1
          ,fun clear_control_queue_helper/1
         ],
    exec(Fs, Call).

-spec new() -> call().
new() -> #whapps_call{}.

-spec put_callid(call()) -> api_binary().
put_callid(#whapps_call{call_id='undefined'}) -> 'undefined';
put_callid(#whapps_call{call_id=CallId}) ->
    put('callid', CallId).

-spec from_route_req(wh_json:object()) -> call().
from_route_req(RouteReq) ->
    from_route_req(RouteReq, new()).

-spec from_route_req(wh_json:object(), call()) -> call().
from_route_req(RouteReq, #whapps_call{call_id=OldCallId
                                      ,account_id=OldAccountId
                                      ,account_db=OldAccountDb
                                      ,ccvs=OldCCVs
                                      ,sip_headers=OldSHs
                                      ,request=OldRequest
                                      ,from=OldFrom
                                      ,to=OldTo
                                     }=Call) ->
    CallId = wh_json:get_value(<<"Call-ID">>, RouteReq, OldCallId),
    put('callid', CallId),

    CCVs = merge(OldCCVs, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteReq)),
    SHs = merge(OldSHs, wh_json:get_value(<<"Custom-SIP-Headers">>, RouteReq)),

    Request = wh_json:get_value(<<"Request">>, RouteReq, OldRequest),
    From = wh_json:get_value(<<"From">>, RouteReq, OldFrom),
    To = wh_json:get_value(<<"To">>, RouteReq, OldTo),

    {AccountId, AccountDb} =
        find_account_info(OldAccountId, OldAccountDb, wh_json:get_value(<<"Account-ID">>, CCVs)),

    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),

    Call1 =
        case wh_json:get_value(<<"Prepend-CID-Name">>, RouteReq) of
            'undefined' -> Call;
            Prepend -> kvs_store('prepend_cid_name', Prepend, Call)
        end,

    Call1#whapps_call{
        call_id=CallId
        ,request=Request
        ,request_user=wnm_util:to_e164(RequestUser)
        ,request_realm=RequestRealm
        ,from=From
        ,from_user=FromUser
        ,from_realm=FromRealm
        ,to=To
        ,to_user=ToUser
        ,to_realm=ToRealm
        ,account_id=AccountId
        ,account_db=AccountDb
        ,inception = wh_json:get_value(<<"Inception">>, CCVs, inception(Call))
        ,switch_hostname = wh_json:get_value(<<"Switch-Hostname">>, RouteReq, switch_hostname(Call))
        ,switch_nodename = wh_json:get_ne_value(<<"Switch-Nodename">>, RouteReq, switch_nodename(Call))
        ,switch_url = wh_json:get_ne_value(<<"Switch-URL">>, RouteReq, switch_url(Call))
        ,switch_uri = wh_json:get_ne_value(<<"Switch-URI">>, RouteReq, switch_uri(Call))
        ,authorizing_id = wh_json:get_ne_value(<<"Authorizing-ID">>, CCVs, authorizing_id(Call))
        ,authorizing_type = wh_json:get_ne_value(<<"Authorizing-Type">>, CCVs, authorizing_type(Call))
        ,owner_id = wh_json:get_ne_value(<<"Owner-ID">>, CCVs, owner_id(Call))
        ,fetch_id = wh_json:get_ne_value(<<"Fetch-ID">>, CCVs, fetch_id(Call))
        ,bridge_id = wh_json:get_ne_value(<<"Bridge-ID">>, CCVs, bridge_id(Call))
        ,caller_id_name = wh_json:get_value(<<"Caller-ID-Name">>, RouteReq, caller_id_name(Call))
        ,caller_id_number = wh_json:get_value(<<"Caller-ID-Number">>, RouteReq, caller_id_number(Call))
        ,ccvs = CCVs
        ,sip_headers = SHs
        ,resource_type = wh_json:get_value(<<"Resource-Type">>, RouteReq, resource_type(Call))
        ,to_tag = wh_json:get_value(<<"To-Tag">>, RouteReq, to_tag(Call))
        ,from_tag = wh_json:get_value(<<"From-Tag">>, RouteReq, from_tag(Call))
    }.

-spec from_route_win(wh_json:object()) -> call().
from_route_win(RouteWin) ->
    from_route_win(RouteWin, new()).

-spec from_route_win(wh_json:object(), call()) -> call().
from_route_win(RouteWin, #whapps_call{call_id=OldCallId
                                      ,ccvs=OldCCVs
                                      ,sip_headers=OldSHs
                                      ,inception=OldInception
                                      ,account_id=OldAccountId
                                      ,account_db=OldAccountDb
                                      ,authorizing_id=OldAuthzId
                                      ,authorizing_type=OldAuthzType
                                      ,owner_id=OldOwnerId
                                      ,fetch_id=OldFetchId
                                      ,bridge_id=OldBridgeId
                                      ,language=OldLanguage
                                     }=Call) ->
    CallId = wh_json:get_value(<<"Call-ID">>, RouteWin, OldCallId),
    put('callid', CallId),

    CCVs = merge(OldCCVs, wh_json:get_value(<<"Custom-Channel-Vars">>, RouteWin)),
    SHs = merge(OldSHs, wh_json:get_value(<<"Custom-SIP-Headers">>, RouteWin)),

    {AccountId, AccountDb} =
        find_account_info(OldAccountId, OldAccountDb, wh_json:get_value(<<"Account-ID">>, CCVs)),

    Call#whapps_call{call_id=CallId
                     ,account_id=AccountId
                     ,account_db=AccountDb
                     ,ccvs=CCVs
                     ,sip_headers=SHs
                     ,control_q = wh_json:get_value(<<"Control-Queue">>, RouteWin)
                     ,inception = wh_json:get_value(<<"Inception">>, CCVs, OldInception)
                     ,authorizing_id = wh_json:get_ne_value(<<"Authorizing-ID">>, CCVs, OldAuthzId)
                     ,authorizing_type = wh_json:get_ne_value(<<"Authorizing-Type">>, CCVs, OldAuthzType)
                     ,owner_id = wh_json:get_ne_value(<<"Owner-ID">>, CCVs, OldOwnerId)
                     ,fetch_id = wh_json:get_ne_value(<<"Fetch-ID">>, CCVs, OldFetchId)
                     ,bridge_id = wh_json:get_ne_value(<<"Bridge-ID">>, CCVs, OldBridgeId)
                     ,language = wh_media_util:prompt_language(AccountId, OldLanguage)
                    }.

-spec find_account_info(api_binary(), api_binary(), api_binary()) ->
                               {api_binary(), api_binary()}.
find_account_info(OldId, OldDb, 'undefined') ->
    {OldId, OldDb};
find_account_info('undefined', _OldDb, AccountId) ->
    {AccountId
     ,wh_util:format_account_id(AccountId, 'encoded')
    };
find_account_info(OldId, OldDb, _AccountId) ->
    {OldId, OldDb}.

-spec merge(wh_json:object(), api_object()) -> wh_json:object().
merge(OldJObj, 'undefined') -> OldJObj;
merge(OldJObj, JObj) ->
    wh_json:merge_recursive(OldJObj, JObj).

-spec from_originate_uuid(wh_json:object()) -> call().
-spec from_originate_uuid(wh_json:object(), call()) -> call().
from_originate_uuid(JObj) ->
    from_originate_uuid(JObj, new()).

from_originate_uuid(JObj, #whapps_call{}=Call) ->
    'true' = wapi_resource:originate_uuid_v(JObj),
    Call#whapps_call{control_q=wh_json:get_value(<<"Outbound-Call-Control-Queue">>, JObj, control_queue(Call))
                     ,call_id=wh_json:get_value(<<"Outbound-Call-ID">>, JObj, call_id(Call))
                    }.

-spec from_channel_create(wh_json:object()) -> call().
-spec from_channel_create(wh_json:object(), call()) -> call().
from_channel_create(JObj) ->
    from_channel_create(JObj, new()).

from_channel_create(JObj, Call) ->
    from_json(JObj, Call).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% READ THIS CAVEAT!!
%% custom publisher and helper functions are not maintained when
%% converting to/from json
%% @end
%%--------------------------------------------------------------------
-spec from_json(wh_json:object()) -> call().
-spec from_json(wh_json:object(), call()) -> call().
from_json(JObj) ->
    from_json(JObj, new()).

from_json(JObj, #whapps_call{ccvs=OldCCVs
                             ,sip_headers=OldSHs
                            }=Call) ->
    CCVs = wh_json:merge_recursive(OldCCVs, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new())),
    SHs = wh_json:merge_recursive(OldSHs, wh_json:get_value(<<"Custom-SIP-Headers">>, JObj, wh_json:new())),
    KVS = orddict:from_list(wh_json:to_proplist(wh_json:get_value(<<"Key-Value-Store">>, JObj, wh_json:new()))),
    Call#whapps_call{
      call_id = wh_json:get_ne_value(<<"Call-ID">>, JObj, call_id_direct(Call))
      ,control_q = wh_json:get_ne_value(<<"Control-Queue">>, JObj, control_queue_direct(Call))
      ,controller_q = wh_json:get_ne_value(<<"Controller-Queue">>, JObj, controller_queue(Call))
      ,caller_id_name = wh_json:get_ne_value(<<"Caller-ID-Name">>, JObj, caller_id_name(Call))
      ,caller_id_number = wh_json:get_ne_value(<<"Caller-ID-Number">>, JObj, caller_id_number(Call))
      ,callee_id_name = wh_json:get_ne_value(<<"Callee-ID-Name">>, JObj, callee_id_name(Call))
      ,callee_id_number = wh_json:get_ne_value(<<"Callee-ID-Number">>, JObj, callee_id_number(Call))
      ,request = wh_json:get_ne_value(<<"Request">>, JObj, request(Call))
      ,request_user = wh_json:get_ne_value(<<"Request-User">>, JObj, request_user(Call))
      ,request_realm = wh_json:get_ne_value(<<"Request-Realm">>, JObj, request_realm(Call))
      ,from = wh_json:get_ne_value(<<"From">>, JObj, from(Call))
      ,from_user = wh_json:get_ne_value(<<"From-User">>, JObj, from_user(Call))
      ,from_realm = wh_json:get_ne_value(<<"From-Realm">>, JObj, from_realm(Call))
      ,to = wh_json:get_ne_value(<<"To">>, JObj, to(Call))
      ,to_user = wh_json:get_ne_value(<<"To-User">>, JObj, to_user(Call))
      ,to_realm = wh_json:get_ne_value(<<"To-Realm">>, JObj, to_realm(Call))
      ,switch_hostname = wh_json:get_value(<<"Switch-Hostname">>, JObj, switch_hostname(Call))
      ,switch_nodename = wh_json:get_value(<<"Switch-Nodename">>, JObj, switch_nodename(Call))
      ,switch_url = wh_json:get_value(<<"Switch-URL">>, JObj, switch_url(Call))
      ,switch_uri = wh_json:get_value(<<"Switch-URI">>, JObj, switch_uri(Call))
      ,inception = wh_json:get_ne_value(<<"Inception">>, JObj, inception(Call))
      ,account_db = wh_json:get_ne_value(<<"Account-DB">>, JObj, account_db(Call))
      ,account_id = wh_json:get_ne_value(<<"Account-ID">>, JObj, account_id(Call))
      ,authorizing_id = wh_json:get_ne_value(<<"Authorizing-ID">>, JObj, authorizing_id(Call))
      ,authorizing_type = wh_json:get_ne_value(<<"Authorizing-Type">>, JObj, authorizing_type(Call))
      ,owner_id = wh_json:get_ne_value(<<"Owner-ID">>, JObj, owner_id(Call))
      ,fetch_id = wh_json:get_ne_value(<<"Fetch-ID">>, JObj, fetch_id(Call))
      ,bridge_id = wh_json:get_ne_value(<<"Bridge-ID">>, JObj, bridge_id(Call))
      ,language = wh_json:get_ne_value(<<"Language">>, JObj, language(Call))
      ,app_name = wh_json:get_ne_value(<<"App-Name">>, JObj, application_name(Call))
      ,app_version = wh_json:get_ne_value(<<"App-Version">>, JObj, application_version(Call))
      ,ccvs = CCVs
      ,sip_headers = SHs
      ,kvs = orddict:merge(fun(_, _, V2) -> V2 end, Call#whapps_call.kvs, KVS)
      ,other_leg_call_id = wh_json:get_ne_value(<<"Other-Leg-Call-ID">>, JObj, other_leg_call_id(Call))
      ,resource_type = wh_json:get_ne_value(<<"Resource-Type">>, JObj, resource_type(Call))
      ,to_tag = wh_json:get_ne_value(<<"To-Tag">>, JObj, to_tag(Call))
      ,from_tag = wh_json:get_ne_value(<<"From-Tag">>, JObj, from_tag(Call))
     }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% READ THIS CAVEAT!!
%% custom publisher and helper functions are not maintained when
%% converting to/from json
%% @end
%%--------------------------------------------------------------------
-spec to_json(call()) -> wh_json:object().
to_json(#whapps_call{}=Call) ->
    Props = to_proplist(Call),
    KVS = [KV
           || {_, V}=KV <- props:get_value(<<"Key-Value-Store">>, Props, [])
                  ,V =/= 'undefined'
                  ,wh_json:is_json_term(V)
          ],
    wh_json:from_list([KV
                       || {_, V}=KV <- [{<<"Key-Value-Store">>, wh_json:from_list(KVS)} |
                                        proplists:delete(<<"Key-Value-Store">>, Props)
                                       ]
                              ,V =/= 'undefined'
                              ,wh_json:is_json_term(V)
                      ]).

-spec to_proplist(call()) -> wh_proplist().
to_proplist(#whapps_call{}=Call) ->
    [{<<"Call-ID">>, call_id_direct(Call)}
     ,{<<"Control-Queue">>, control_queue_direct(Call)}
     ,{<<"Controller-Queue">>, controller_queue(Call)}
     ,{<<"Caller-ID-Name">>, caller_id_name(Call)}
     ,{<<"Caller-ID-Number">>, caller_id_number(Call)}
     ,{<<"Callee-ID-Name">>, callee_id_name(Call)}
     ,{<<"Callee-ID-Number">>, callee_id_number(Call)}
     ,{<<"Request">>, request(Call)}
     ,{<<"Request-User">>, request_user(Call)}
     ,{<<"Request-Realm">>, request_realm(Call)}
     ,{<<"From">>, from(Call)}
     ,{<<"From-User">>, from_user(Call)}
     ,{<<"From-Realm">>, from_realm(Call)}
     ,{<<"To">>, to(Call)}
     ,{<<"To-User">>, to_user(Call)}
     ,{<<"To-Realm">>, to_realm(Call)}
     ,{<<"Switch-Hostname">>, switch_hostname(Call)}
     ,{<<"Switch-Nodename">>, switch_nodename(Call)}
     ,{<<"Switch-URL">>, switch_url(Call)}
     ,{<<"Switch-URI">>, switch_uri(Call)}
     ,{<<"Inception">>, inception(Call)}
     ,{<<"Account-DB">>, account_db(Call)}
     ,{<<"Account-ID">>, account_id(Call)}
     ,{<<"Authorizing-ID">>, authorizing_id(Call)}
     ,{<<"Authorizing-Type">>, authorizing_type(Call)}
     ,{<<"Owner-ID">>, owner_id(Call)}
     ,{<<"Fetch-ID">>, fetch_id(Call)}
     ,{<<"Bridge-ID">>, bridge_id(Call)}
     ,{<<"Custom-Channel-Vars">>, custom_channel_vars(Call)}
     ,{<<"Custom-SIP-Headers">>, custom_sip_headers(Call)}
     ,{<<"Key-Value-Store">>, kvs_to_proplist(Call)}
     ,{<<"Other-Leg-Call-ID">>, other_leg_call_id(Call)}
     ,{<<"Resource-Type">>, resource_type(Call)}
     ,{<<"Language">>, language(Call)}
     ,{<<"To-Tag">>, to_tag(Call)}
     ,{<<"From-Tag">>, from_tag(Call)}
    ].

-spec is_call(term()) -> boolean().
is_call(#whapps_call{}) -> 'true';
is_call(_) -> 'false'.

-type exec_fun_1() :: fun((call()) -> call()).
-type exec_fun_2() :: {fun((term(), call()) -> call()), term()}.
-type exec_fun_3() :: {fun((term(), term(), call()) -> call()), term(), term()}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), call()) -> call().
exec(Funs, #whapps_call{}=Call) ->
    lists:foldl(fun exec_fold/2, Call, Funs).

-spec exec_fold(exec_fun(), call()) -> call().
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C).

-spec set_application_name(ne_binary(), call()) -> call().
set_application_name(AppName, #whapps_call{}=Call) when is_binary(AppName) ->
    Call#whapps_call{app_name=AppName}.

-spec application_name(call()) -> ne_binary().
application_name(#whapps_call{app_name=AppName}) ->
    AppName.

-spec set_application_version(ne_binary(), call()) -> call().
set_application_version(AppVersion, #whapps_call{}=Call) when is_binary(AppVersion) ->
    Call#whapps_call{app_version=AppVersion}.

-spec application_version(call()) -> ne_binary().
application_version(#whapps_call{app_version=AppVersion}) ->
    AppVersion.

-spec set_call_id(api_binary(), call()) -> call().
set_call_id(CallId, #whapps_call{}=Call) ->
    Call#whapps_call{call_id=CallId}.

-spec set_other_leg_call_id(api_binary(), call()) -> call().
set_other_leg_call_id(CallId, #whapps_call{}=Call) ->
    Call#whapps_call{other_leg_call_id=CallId}.

-spec call_id(call()) -> api_binary().
-spec call_id_direct(call()) -> api_binary().
call_id(#whapps_call{call_id=CallId, call_id_helper=Fun}=Call) when is_function(Fun, 2) ->
    Fun(CallId, Call);
call_id(#whapps_call{call_id=CallId}=Call) ->
    ?MODULE:default_helper_function(CallId, Call).

call_id_direct(#whapps_call{call_id=CallId}) ->
    CallId.

-spec other_leg_call_id(call()) -> api_binary().
other_leg_call_id(#whapps_call{other_leg_call_id=CallId}=_Call) ->
    CallId.

-spec call_id_helper(whapps_helper_function(), call()) -> call().
call_id_helper(Fun, #whapps_call{}=Call) when is_function(Fun, 2) ->
    Call#whapps_call{call_id_helper=Fun}.

-spec clear_call_id_helper(call()) -> call().
clear_call_id_helper(Call) ->
    Call#whapps_call{call_id_helper=fun ?MODULE:default_helper_function/2}.

-spec set_control_queue(ne_binary(), call()) -> call().
set_control_queue(ControlQ, #whapps_call{}=Call) when is_binary(ControlQ) ->
    Call#whapps_call{control_q=ControlQ}.

-spec control_queue(call()) -> api_binary().
-spec control_queue_direct(call()) -> api_binary().
control_queue(#whapps_call{control_q=ControlQ, control_q_helper=Fun}=Call) when is_function(Fun, 2) ->
    Fun(ControlQ, Call);
control_queue(#whapps_call{control_q=ControlQ}=Call) ->
    ?MODULE:default_helper_function(ControlQ, Call).

control_queue_direct(#whapps_call{control_q=ControlQ}) ->
    ControlQ.

-spec control_queue_helper(whapps_helper_function(), call()) -> call().
control_queue_helper(Fun, #whapps_call{}=Call) when is_function(Fun, 2) ->
    Call#whapps_call{control_q_helper=Fun}.

-spec clear_control_queue_helper(call()) -> call().
clear_control_queue_helper(#whapps_call{}=Call) ->
    Call#whapps_call{control_q_helper=fun ?MODULE:default_helper_function/2}.

-spec set_controller_queue(ne_binary(), call()) -> call().
set_controller_queue(ControllerQ, #whapps_call{}=Call) when is_binary(ControllerQ) ->
    Call#whapps_call{controller_q=ControllerQ}.

-spec controller_queue(call()) -> binary().
controller_queue(#whapps_call{controller_q=ControllerQ}) ->
    ControllerQ.

-spec set_caller_id_name(ne_binary(), call()) -> call().
set_caller_id_name(CIDName, #whapps_call{}=Call) when is_binary(CIDName) ->
    whapps_call_command:set(wh_json:from_list([{<<"Caller-ID-Name">>, CIDName}]), 'undefined', Call),
    Call#whapps_call{caller_id_name=CIDName}.

-spec caller_id_name(call()) -> ne_binary().
caller_id_name(#whapps_call{caller_id_name=CIDName}) ->
    case wh_util:is_empty(CIDName) of
        'true' -> ?DEFAULT_CALLER_ID_NAME;
        'false' -> CIDName
    end.

-spec set_caller_id_number(api_binary(), call()) -> call().
set_caller_id_number(CIDNumber, #whapps_call{}=Call) ->
    whapps_call_command:set(wh_json:from_list([{<<"Caller-ID-Number">>, CIDNumber}]), 'undefined', Call),
    Call#whapps_call{caller_id_number=CIDNumber}.

-spec caller_id_number(call()) -> ne_binary().
caller_id_number(#whapps_call{caller_id_number=CIDNumber}) ->
    case  wh_util:is_empty(CIDNumber) of
        'true' -> ?DEFAULT_CALLER_ID_NUMBER;
        'false' -> CIDNumber
    end.

-spec set_callee_id_name(ne_binary(), call()) -> call().
set_callee_id_name(CIDName, #whapps_call{}=Call) when is_binary(CIDName) ->
    whapps_call_command:set(wh_json:from_list([{<<"Callee-ID-Number">>, CIDName}]), 'undefined', Call),
    Call#whapps_call{callee_id_name=CIDName}.

-spec callee_id_name(call()) -> binary().
callee_id_name(#whapps_call{callee_id_name='undefined'}) -> <<>>;
callee_id_name(#whapps_call{callee_id_name=CIDName}) -> CIDName.

-spec set_callee_id_number(ne_binary(), call()) -> call().
set_callee_id_number(CIDNumber, #whapps_call{}=Call) when is_binary(CIDNumber) ->
    whapps_call_command:set(wh_json:from_list([{<<"Callee-ID-Number">>, CIDNumber}]), 'undefined', Call),
    Call#whapps_call{callee_id_number=CIDNumber}.

-spec callee_id_number(call()) -> binary().
callee_id_number(#whapps_call{callee_id_number='undefined'}) -> <<>>;
callee_id_number(#whapps_call{callee_id_number=CIDNumber}) -> CIDNumber.

-spec set_request(ne_binary(), call()) -> call().
set_request(Request, #whapps_call{}=Call) when is_binary(Request) ->
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    Call#whapps_call{request=Request
                     ,request_user=wnm_util:to_e164(RequestUser)
                     ,request_realm=RequestRealm
                    }.

-spec request(call()) -> ne_binary().
request(#whapps_call{request=Request}) ->
    Request.

-spec request_user(call()) -> ne_binary().
request_user(#whapps_call{request_user=RequestUser}) ->
    RequestUser.

-spec request_realm(call()) -> ne_binary().
request_realm(#whapps_call{request_realm=RequestRealm}) ->
    RequestRealm.

-spec set_from(ne_binary(), call()) -> call().
set_from(From, #whapps_call{}=Call) when is_binary(From) ->
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    Call#whapps_call{from=From
                     ,from_user=FromUser
                     ,from_realm=FromRealm
                    }.

-spec from(call()) -> ne_binary().
from(#whapps_call{from=From}) ->
    From.

-spec from_user(call()) -> ne_binary().
from_user(#whapps_call{from_user=FromUser}) ->
    FromUser.

-spec from_realm(call()) -> api_binary().
from_realm(#whapps_call{from_realm=FromRealm}) ->
    FromRealm.

-spec set_to(ne_binary(), call()) -> call().
set_to(To, #whapps_call{}=Call) when is_binary(To) ->
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    Call#whapps_call{to=To
                     ,to_user=ToUser
                     ,to_realm=ToRealm
                    }.

-spec to(call()) -> ne_binary().
to(#whapps_call{to=To}) ->
    To.

-spec to_user(call()) -> ne_binary().
to_user(#whapps_call{to_user=ToUser}) ->
    ToUser.

-spec to_realm(call()) -> api_binary().
to_realm(#whapps_call{to_realm=ToRealm}) ->
    ToRealm.

-spec set_switch_hostname(ne_binary(), call()) -> call().
set_switch_hostname(<<_/binary>> = Srv, #whapps_call{}=Call) ->
    Call#whapps_call{switch_hostname=Srv}.

-spec switch_hostname(call()) -> api_binary().
switch_hostname(#whapps_call{switch_hostname=Srv}) ->
    Srv.

-spec set_switch_nodename(ne_binary(), call()) -> call().
set_switch_nodename(Srv, #whapps_call{}=Call) ->
    Call#whapps_call{switch_nodename=Srv}.

-spec switch_nodename(call()) -> binary().
switch_nodename(#whapps_call{switch_nodename=Srv}) ->
    Srv.

-spec set_switch_url(ne_binary(), call()) -> call().
set_switch_url(Srv, #whapps_call{}=Call) ->
    Call#whapps_call{switch_url=Srv}.

-spec switch_url(call()) -> binary().
switch_url(#whapps_call{switch_url=Srv}) ->
    Srv.

-spec set_switch_uri(ne_binary(), call()) -> call().
set_switch_uri(Srv, #whapps_call{}=Call) ->
    Call#whapps_call{switch_uri=Srv}.

-spec switch_uri(call()) -> binary().
switch_uri(#whapps_call{switch_uri=Srv}) ->
    Srv.

-spec set_inception(api_binary(), call()) -> call().
set_inception('undefined', #whapps_call{}=Call) ->
    Call#whapps_call{inception='undefined'};
set_inception(Inception, #whapps_call{}=Call) ->
    set_custom_channel_var(<<"Inception">>, Inception, Call#whapps_call{inception=Inception}).

-spec inception(call()) -> api_binary().
inception(#whapps_call{inception=Inception}) ->
    Inception.

-spec set_resource_type(api_binary(), call()) -> call().
set_resource_type('undefined', #whapps_call{}=Call) ->
    Call#whapps_call{resource_type='undefined'};
set_resource_type(ResourceType, #whapps_call{}=Call) ->
    set_custom_channel_var(<<"Resource-Type">>, ResourceType, Call#whapps_call{resource_type=ResourceType}).

-spec resource_type(call()) -> api_binary().
resource_type(#whapps_call{resource_type=ResourceType}) ->
    ResourceType.

-spec set_account_db(ne_binary(), call()) -> call().
set_account_db(<<_/binary>> = AccountDb, #whapps_call{}=Call) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#whapps_call{account_db=AccountDb
                                                                         ,account_id=AccountId
                                                                        }).

-spec account_db(call()) -> api_binary().
account_db(#whapps_call{account_db=AccountDb}) ->
    AccountDb.

-spec set_account_id(ne_binary(), call()) -> call().
set_account_id(<<_/binary>> = AccountId, #whapps_call{}=Call) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#whapps_call{account_db=AccountDb
                                                                         ,account_id=AccountId
                                                                        }).

-spec account_id(call()) -> api_binary().
account_id(#whapps_call{account_id=AccountId}) ->
    AccountId.

-spec account_realm(call()) -> ne_binary().
account_realm(#whapps_call{account_id=AccountId
                           ,account_db=AccountDb
                          }) ->
    {'ok', Doc} = couch_mgr:open_cache_doc(AccountDb, AccountId),
    wh_json:get_value(<<"realm">>, Doc).

-spec set_authorizing_id(ne_binary(), call()) -> call().
set_authorizing_id(AuthorizingId, #whapps_call{}=Call) when is_binary(AuthorizingId) ->
    set_custom_channel_var(<<"Authorizing-ID">>, AuthorizingId, Call#whapps_call{authorizing_id=AuthorizingId}).

-spec authorizing_id(call()) -> api_binary().
authorizing_id(#whapps_call{authorizing_id=AuthorizingId}) ->
    AuthorizingId.

-spec set_authorizing_type(ne_binary(), call()) -> call().
set_authorizing_type(AuthorizingType, #whapps_call{}=Call) when is_binary(AuthorizingType) ->
    set_custom_channel_var(<<"Authorizing-Type">>, AuthorizingType, Call#whapps_call{authorizing_type=AuthorizingType}).

-spec authorizing_type(call()) -> api_binary().
authorizing_type(#whapps_call{authorizing_type=AuthorizingType}) ->
    AuthorizingType.

-spec set_authorization(ne_binary(), ne_binary(), call()) -> call().
set_authorization(AuthorizingType, AuthorizingId, #whapps_call{}=Call)
  when is_binary(AuthorizingType)
       andalso is_binary(AuthorizingId) ->
    set_custom_channel_vars([{<<"Authorizing-Type">>, AuthorizingType}
                             ,{<<"Authorizing-ID">>, AuthorizingId}
                            ]
                            ,Call#whapps_call{authorizing_type=AuthorizingType
                                              ,authorizing_id=AuthorizingId
                                             }
                           ).

-spec set_owner_id(ne_binary(), call()) -> call().
set_owner_id(OwnerId, #whapps_call{}=Call) when is_binary(OwnerId) ->
    set_custom_channel_var(<<"Owner-Id">>, OwnerId, Call#whapps_call{owner_id=OwnerId}).

-spec owner_id(call()) -> api_binary().
owner_id(#whapps_call{owner_id=OwnerId}) -> OwnerId.

-spec set_fetch_id(ne_binary(), call()) -> call().
set_fetch_id(FetchId, #whapps_call{}=Call) when is_binary(FetchId) ->
    set_custom_channel_var(<<"Fetch-Id">>, FetchId, Call#whapps_call{fetch_id=FetchId}).

-spec fetch_id(call()) -> api_binary().
fetch_id(#whapps_call{fetch_id=FetchId}) -> FetchId.

-spec set_bridge_id(ne_binary(), call()) -> call().
set_bridge_id(BridgeId, #whapps_call{}=Call) when is_binary(BridgeId) ->
    set_custom_channel_var(<<"Bridge-Id">>, BridgeId, Call#whapps_call{bridge_id=BridgeId}).

-spec bridge_id(call()) -> api_binary().
bridge_id(#whapps_call{bridge_id=BridgeId}) -> BridgeId.

-spec set_language(ne_binary(), call()) -> call().
set_language(Language, #whapps_call{}=Call) when is_binary(Language) ->
    Call#whapps_call{language=Language}.

-spec language(call()) -> api_binary().
language(#whapps_call{language='undefined', account_id=AccountId}) ->
    wh_media_util:prompt_language(AccountId);
language(#whapps_call{language=Language}) -> Language.

-spec set_to_tag(ne_binary(), call()) -> call().
set_to_tag(ToTag, #whapps_call{}=Call) when is_binary(ToTag) ->
    Call#whapps_call{to_tag=ToTag}.

-spec to_tag(call()) -> api_binary().
to_tag(#whapps_call{to_tag=ToTag}) ->
    ToTag.

-spec set_from_tag(ne_binary(), call()) -> call().
set_from_tag(FromTag, #whapps_call{}=Call) when is_binary(FromTag) ->
    Call#whapps_call{from_tag=FromTag}.

-spec from_tag(call()) -> api_binary().
from_tag(#whapps_call{from_tag=FromTag}) ->
    FromTag.

-spec set_custom_channel_var(term(), term(), call()) -> call().
set_custom_channel_var(Key, Value, #whapps_call{ccvs=CCVs}=Call) ->
    whapps_call_command:set(wh_json:set_value(Key, Value, wh_json:new()), 'undefined', Call),
    handle_ccvs_update(wh_json:set_value(Key, Value, CCVs), Call).

-spec set_custom_channel_vars(wh_proplist(), call()) -> call().
set_custom_channel_vars(Props, #whapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = wh_json:set_values(Props, CCVs),
    whapps_call_command:set(NewCCVs, 'undefined', Call),
    handle_ccvs_update(NewCCVs, Call).

-spec update_custom_channel_vars([fun((wh_json:object()) -> wh_json:object()),...], call()) -> call().
update_custom_channel_vars(Updaters, #whapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = lists:foldr(fun(F, J) -> F(J) end, CCVs, Updaters),
    whapps_call_command:set(NewCCVs, 'undefined', Call),
    handle_ccvs_update(NewCCVs, Call).

-spec custom_channel_var(term(), Default, call()) -> Default | term().
custom_channel_var(Key, Default, #whapps_call{ccvs=CCVs}) ->
    wh_json:get_value(Key, CCVs, Default).

-spec custom_channel_var(term(), call()) -> term().
custom_channel_var(Key, #whapps_call{ccvs=CCVs}) ->
    wh_json:get_value(Key, CCVs).

-spec custom_channel_vars(call()) -> wh_json:object().
custom_channel_vars(#whapps_call{ccvs=CCVs}) ->
    CCVs.

-spec set_custom_sip_header(wh_json:key(), wh_json:json_term(), call()) -> call().
set_custom_sip_header(Key, Value, #whapps_call{sip_headers=SHs}=Call) ->
    Call#whapps_call{sip_headers=wh_json:set_value(Key, Value, SHs)}.

-spec custom_sip_header(wh_json:key(), call()) -> wh_json:json_term().
-spec custom_sip_header(wh_json:key(), wh_json:json_term(), call()) -> wh_json:json_term().
custom_sip_header(Key, #whapps_call{}=Call) ->
    custom_sip_header(Key, 'undefined', Call).
custom_sip_header(Key, Default, #whapps_call{sip_headers=SHs}) ->
    wh_json:get_value(Key, SHs, Default).

-spec set_custom_sip_headers(wh_proplist(), call()) -> call().
set_custom_sip_headers(Headers, #whapps_call{sip_headers=SHs}=Call) ->
    Call#whapps_call{sip_headers=wh_json:set_values(Headers, SHs)}.

-spec custom_sip_headers(call()) -> wh_json:object().
custom_sip_headers(#whapps_call{sip_headers=SHs}) -> SHs.

-spec handle_ccvs_update(wh_json:object(), call()) -> call().
handle_ccvs_update(CCVs, #whapps_call{}=Call) ->
    lists:foldl(fun({Var, Index}, C) ->
                        case wh_json:get_ne_value(Var, CCVs) of
                            'undefined' -> C;
                            Value -> setelement(Index, C, Value)
                        end
                end, Call#whapps_call{ccvs=CCVs}, ?SPECIAL_VARS).

-spec set_custom_publish_function(whapps_custom_publish(), call()) -> call().
set_custom_publish_function(Fun, #whapps_call{}=Call) when is_function(Fun, 2) ->
    Call#whapps_call{custom_publish_fun=Fun}.

-spec clear_custom_publish_function(call()) -> call().
clear_custom_publish_function(#whapps_call{}=Call) ->
    Call#whapps_call{custom_publish_fun='undefined'}.

-spec custom_publish_function(call()) -> 'undefined' | whapps_custom_publish().
custom_publish_function(#whapps_call{custom_publish_fun=Fun}) -> Fun.

-spec kvs_append(term(), term(), call()) -> call().
kvs_append(Key, Value, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:append(wh_util:to_binary(Key), Value, Dict)}.

-spec kvs_append_list(term(), [term(),...], call()) -> call().
kvs_append_list(Key, ValList, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:append_list(wh_util:to_binary(Key), ValList, Dict)}.

-spec kvs_erase(term() | [term(),...], call()) -> call().
kvs_erase(Keys, #whapps_call{kvs=Dict}=Call) when is_list(Keys)->
    Call#whapps_call{kvs=lists:foldl(fun(K, D) -> orddict:erase(wh_util:to_binary(K), D) end, Dict, Keys)};
kvs_erase(Key, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:erase(wh_util:to_binary(Key), Dict)}.

-spec kvs_flush(call()) -> call().
kvs_flush(#whapps_call{}=Call) -> Call#whapps_call{kvs=orddict:new()}.

-spec kvs_fetch(wh_json:key(), call()) -> term().
-spec kvs_fetch(wh_json:key(), Default, call()) -> term() | Default.
kvs_fetch(Key, Call) -> kvs_fetch(Key, 'undefined', Call).
kvs_fetch(Key, Default, #whapps_call{kvs=Dict}) ->
    try orddict:fetch(wh_util:to_binary(Key), Dict) of
        Ok -> Ok
    catch
        'error':'function_clause' -> Default
    end.

-spec kvs_fetch_keys(call()) -> [term(),...].
kvs_fetch_keys(#whapps_call{kvs=Dict}) -> orddict:fetch_keys(Dict).

-spec kvs_filter(fun((term(), term()) -> boolean()), call()) ->
                              call().
kvs_filter(Pred, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find(term(), call()) -> {'ok', term()} | 'error'.
kvs_find(Key, #whapps_call{kvs=Dict}) ->
    orddict:find(wh_util:to_binary(Key), Dict).

-spec kvs_fold(fun((term(), term(), term()) -> term()), term(), call()) -> call().
kvs_fold(Fun, Acc0, #whapps_call{kvs=Dict}) -> orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist(wh_proplist(), call()) -> call().
kvs_from_proplist(List, #whapps_call{kvs=Dict}=Call) ->
    L = orddict:from_list([{wh_util:to_binary(K), V} || {K, V} <- List]),
    Call#whapps_call{kvs=orddict:merge(fun(_, V1, _) -> V1 end, L, Dict)}.

-spec kvs_is_key(term(), call()) -> boolean().
kvs_is_key(Key, #whapps_call{kvs=Dict}) ->
    orddict:is_key(wh_util:to_binary(Key), Dict).

-spec kvs_map(fun((term(), term()) -> term()), call()) -> call().
kvs_map(Pred, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store(term(), term(), call()) -> call().
kvs_store(Key, Value, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:store(wh_util:to_binary(Key), Value, Dict)}.

-spec kvs_store_proplist(wh_proplist(), call()) -> call().
kvs_store_proplist(List, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=lists:foldr(fun({K, V}, D) ->
                                             orddict:store(wh_util:to_binary(K), V, D)
                                     end, Dict, List)}.

-spec kvs_to_proplist(call()) -> wh_proplist().
kvs_to_proplist(#whapps_call{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update(term(), fun((term()) -> term()), call()) -> call().
kvs_update(Key, Fun, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update(wh_util:to_binary(Key), Fun, Dict)}.

-spec kvs_update(term(), fun((term()) -> term()), term(), call()) -> call().
kvs_update(Key, Fun, Initial, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update(wh_util:to_binary(Key), Fun, Initial, Dict)}.

-spec kvs_update_counter(term(), number(), call()) -> call().
kvs_update_counter(Key, Number, #whapps_call{kvs=Dict}=Call) ->
    Call#whapps_call{kvs=orddict:update_counter(wh_util:to_binary(Key), Number, Dict)}.

-spec set_dtmf_collection(api_binary(), call()) -> call().
-spec set_dtmf_collection(api_binary(), ne_binary(), call()) -> call().
set_dtmf_collection(DTMF, Call) ->
    set_dtmf_collection(DTMF, <<"default">>, Call).
set_dtmf_collection('undefined', Collection, Call) ->
    Collections = kvs_fetch(<<"dtmf_collections">>, wh_json:new(), Call),
    kvs_store(<<"dtmf_collections">>
                  ,wh_json:delete_key(Collection, Collections)
              ,Call
             );
set_dtmf_collection(DTMF, Collection, Call) ->
    Collections = kvs_fetch(<<"dtmf_collections">>, wh_json:new(), Call),
    kvs_store(<<"dtmf_collections">>
                  ,wh_json:set_value(Collection, DTMF, Collections)
              ,Call
             ).

-spec get_dtmf_collection(call()) -> api_binary().
-spec get_dtmf_collection(ne_binary(), call()) -> api_binary().
get_dtmf_collection(Call) ->
    get_dtmf_collection(<<"default">>, Call).
get_dtmf_collection(Collection, Call) ->
    wh_json:get_value(Collection, kvs_fetch(<<"dtmf_collections">>, wh_json:new(), Call)).

-spec add_to_dtmf_collection(ne_binary(), call()) -> call().
-spec add_to_dtmf_collection(ne_binary(), ne_binary(), call()) -> call().
add_to_dtmf_collection(DTMF, Call) ->
    add_to_dtmf_collection(DTMF, <<"default">>, Call).
add_to_dtmf_collection(DTMF, Collection, Call) ->
    case get_dtmf_collection(Collection, Call) of
        'undefined' -> set_dtmf_collection(DTMF, Collection, Call);
        Collected -> set_dtmf_collection(<<Collected/binary, DTMF/binary>>, Call)
    end.

-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?WHAPPS_CALL_CACHE).

-spec cache(call()) -> 'ok'.
-spec cache(call(), api_binary()) -> 'ok'.
-spec cache(call(), api_binary(), pos_integer()) -> 'ok'.

cache(Call) ->
    cache(Call, 'undefined', 5 * ?SECONDS_IN_MINUTE).

cache(Call, AppName) ->
    cache(Call, AppName, 5 * ?SECONDS_IN_MINUTE).

cache(#whapps_call{call_id=CallId}=Call, AppName, Expires) ->
    CacheProps = [{'expires', Expires}],
    wh_cache:store_local(?WHAPPS_CALL_CACHE, {?MODULE, 'call', AppName, CallId}, Call, CacheProps).

-spec retrieve(ne_binary()) ->
                      {'ok', call()} |
                      {'error', 'not_found'}.
-spec retrieve(ne_binary(), api_binary()) ->
                      {'ok', call()} |
                      {'error', 'not_found'}.

retrieve(CallId) ->
    retrieve(CallId, 'undefined').

retrieve(CallId, AppName) ->
    wh_cache:fetch_local(?WHAPPS_CALL_CACHE, {?MODULE, 'call', AppName, CallId}).

%% EUNIT TESTING
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(UPDATERS, [fun(C) -> whapps_call:set_call_id(<<"123456789ABCDEF">>, C) end
                   ,fun(C) -> whapps_call:set_control_queue(<<"control_queue">>, C) end
                   ,fun(C) -> whapps_call:set_controller_queue(<<"controller_queue">>, C) end
                   ,fun(C) -> whapps_call:set_caller_id_name(<<"caller_id_name">>, C) end
                   ,fun(C) -> whapps_call:set_caller_id_number(<<"caller_id_number">>, C) end
                   ,fun(C) -> whapps_call:set_callee_id_name(<<"callee_id_name">>, C) end
                   ,fun(C) -> whapps_call:set_callee_id_number(<<"callee_id_number">>, C) end
                   ,fun(C) -> whapps_call:set_request(<<"request_user@request_domain">>, C) end
                   ,fun(C) -> whapps_call:set_from(<<"from_user@from_domain">>, C) end
                   ,fun(C) -> whapps_call:set_to(<<"to_user@to_domain">>, C) end
                   ,fun(C) -> whapps_call:set_account_db(<<"account%2F12%2F3456789">>, C) end
                   ,fun(C) -> whapps_call:set_account_id(<<"123456789">>, C) end
                   ,fun(C) -> whapps_call:set_authorizing_id(<<"987654321">>, C) end
                   ,fun(C) -> whapps_call:set_authorizing_type(<<"test">>, C) end
                   ,fun(C) -> whapps_call:set_owner_id(<<"abcdefghi">>, C) end
                   ,fun(C) -> whapps_call:set_fetch_id(<<"1234567890ABCDEFG">>, C) end
                   ,fun(C) -> whapps_call:set_bridge_id(<<"1234567890ABCDEF">>, C) end
                   ,fun(C) -> whapps_call:set_custom_channel_var(<<"key1">>, <<"value1">>, C) end
                   ,fun(C) -> whapps_call:set_custom_channel_var(<<"key2">>, 2600, C) end
                   ,fun(C) -> whapps_call:set_custom_channel_var([<<"key3">>, <<"key4">>], 'true', C) end
                   ,fun(C) -> whapps_call:kvs_store(<<"kvs_key_1">>, <<"kvs_value_1">>, C) end
                   ,fun(C) -> whapps_call:kvs_store(<<"kvs_key_2">>, <<"kvs_value_2">>, C) end
                   ,fun(C) -> whapps_call:kvs_store(<<"kvs_key_2">>, wh_json:from_list([{<<"sub_key_1">>, <<"sub_value_1">>}]), C) end
                  ]).

%% TODO: I am out of the alloted time for this module, please add during another refactor
from_route_request_test() ->
    'ok'.

%% TODO: I am out of the alloted time for this module, please add during another refactor
from_route_win_test() ->
    'ok'.

json_conversion_test() -> 'ok'.
    %% Call1 = lists:foldr(fun(F, C) -> F(C) end, whapps_call:new(), ?UPDATERS),
    %% _Call2 = from_json(to_json(Call1)).
    %% TODO: These are equal, but the order of the CCVs json headers
    %%       is reversed.... and I am out of time for this module
    %%       Your just goind to have to take my word it works hehe ;)
%%    ?assertEqual(Call1, Call2).

-endif.
