%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapps_call).

-export([new/0, put_callid/1]).
-export([from_route_req/1, from_route_req/2]).
-export([from_route_win/1, from_route_win/2]).
-export([from_originate_uuid/1, from_originate_uuid/2]).
-export([from_originate_ready/1, from_originate_ready/2]).
-export([from_channel_create/1, from_channel_create/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([to_proplist/1]).
-export([is_call/1]).

-export([exec/2]).
-export_type([exec_funs/0]).

-export([set_application_name/2, application_name/1]).
-export([set_application_version/2, application_version/1]).
-export([set_call_id/2, call_id/1, call_id_direct/1]).
-export([set_other_leg_call_id/2, other_leg_call_id/1]).
-export([call_id_helper/2, clear_call_id_helper/1]).
-export([set_origination_call_id/2, origination_call_id/1]).

-export([context/1, context/2, set_context/2]).

-export([set_control_queue/2, control_queue/1, control_queue_direct/1]).
-export([control_queue_helper/2, clear_control_queue_helper/1]).
-export([set_controller_queue/2, controller_queue/1]).

-export([clear_helpers/1]).

-export([maybe_format_caller_id/2, maybe_format_caller_id_str/2]).
-export([set_caller_id_name/2, caller_id_name/1]).
-export([unknown_caller_id_name/0, unknown_caller_id_name/1]).
-export([set_caller_id_number/2, caller_id_number/1]).
-export([set_callee_id_name/2, callee_id_name/1]).
-export([set_callee_id_number/2, callee_id_number/1]).
-export([set_callee_id/3, callee_id/1]).
-export([set_caller_id/3, caller_id/1]).

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
-export([set_inception/2, inception/1, inception_type/1]).
-export([is_inter_account/1, inter_account_id/1]).

-export([set_authorizing_id/2, authorizing_id/1]).
-export([set_authorizing_type/2, authorizing_type/1]).
-export([set_authorization/3]).
-export([set_resource_type/2, resource_type/1]).
-export([set_owner_id/2, owner_id/1]).
-export([set_fetch_id/2, fetch_id/1]).
-export([set_bridge_id/2, bridge_id/1]).
-export([set_language/2, language/1]).
-export([get_prompt/2, get_prompt/3]).
-export([set_to_tag/2, to_tag/1]).
-export([set_from_tag/2, from_tag/1]).
-export([direction/1]).
-export([set_call_bridged/2, call_bridged/1]).
-export([set_message_left/2, message_left/1]).

-export([set_dtmf_collection/2, set_dtmf_collection/3
        ,get_dtmf_collection/1, get_dtmf_collection/2
        ,add_to_dtmf_collection/2, add_to_dtmf_collection/3
        ]).

-export([set_custom_channel_var/3
        ,insert_custom_channel_var/3
        ,set_custom_channel_vars/2
        ,remove_custom_channel_vars/2
        ,update_custom_channel_vars/2
        ,custom_channel_var/3
        ,custom_channel_var/2
        ,custom_channel_vars/1
        ]).

-export([set_custom_application_var/3
        ,insert_custom_application_var/3
        ,set_custom_application_vars/2, set_custom_application_vars/3
        ,custom_application_var/3
        ,custom_application_var/2
        ,custom_application_vars/1
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

-export([start_recording/1, start_recording/2
        ,mask_recording/1, mask_recording/2
        ,unmask_recording/1, unmask_recording/2
        ,stop_recording/1 ,stop_recording/2
        ]).

-export([is_recording/1, set_is_recording/2]).
-export([is_call_forward/1]).

-ifdef(TEST).
-export([eq/2]).
-export([updateable_ccvs/2]).
-endif.

-include("kapps_call_command.hrl").

-define(CALL_CMD_CAT, <<"call_command">>).

-define(NO_USER, <<"nouser">>).
-define(NO_REALM, <<"norealm">>).
-define(NO_USER_REALM, <<"nouser@norealm">>).

-define(DEFAULT_UNKNOWN_CALLER_ID_NAME, <<"unknown">>).
-define(UNKNOWN_CALLER_ID_NAME_KEY, <<"unknown_cid_name">>).

-record(kapps_call, {call_id :: kz_term:api_binary()                       %% The UUID of the call
                    ,call_id_helper = fun default_helper_function/2 :: kapps_helper_function()         %% A function used when requesting the call id, to ensure it is up-to-date
                    ,origination_call_id :: kz_term:api_ne_binary() %% need to track the originating call id, if found
                    ,context :: kz_term:api_ne_binary()
                    ,control_q :: kz_term:api_binary()                   %% The control queue provided on route win
                    ,control_q_helper = fun default_helper_function/2 :: kapps_helper_function()       %% A function used when requesting the call id, to ensure it is up-to-date
                    ,controller_q :: kz_term:api_binary()                %%
                    ,caller_id_name :: kz_term:api_ne_binary()      %% The caller name
                    ,caller_id_number :: kz_term:api_ne_binary() %% The caller number
                    ,callee_id_name :: kz_term:api_binary()                     %% The callee name
                    ,callee_id_number :: kz_term:api_binary()                   %% The callee number
                    ,switch_nodename = <<>> :: binary()                 %% The switch node name (as known in ecallmgr)
                    ,switch_hostname :: kz_term:api_ne_binary()                    %% The switch hostname (as reported by the switch)
                    ,switch_url :: kz_term:api_binary()                         %% The switch url
                    ,switch_uri :: kz_term:api_binary()                         %% The switch uri
                    ,request = ?NO_USER_REALM :: kz_term:ne_binary()      %% The request of sip_request_user + @ + sip_request_host
                    ,request_user = ?NO_USER :: kz_term:ne_binary()         %% SIP request user
                    ,request_realm = ?NO_REALM :: kz_term:ne_binary()       %% SIP request host
                    ,from = ?NO_USER_REALM :: kz_term:ne_binary()         %% Result of sip_from_user + @ + sip_from_host
                    ,from_user = ?NO_USER :: kz_term:ne_binary()            %% SIP from user
                    ,from_realm = ?NO_REALM :: kz_term:ne_binary()          %% SIP from host
                    ,to = ?NO_USER_REALM :: kz_term:ne_binary()           %% Result of sip_to_user + @ + sip_to_host
                    ,to_user = ?NO_USER :: kz_term:ne_binary()              %% SIP to user
                    ,to_realm = ?NO_REALM :: kz_term:ne_binary()            %% SIP to host
                    ,inception :: kz_term:api_binary()                   %% Origin of the call <<"on-net">> | <<"off-net">>
                    ,account_db :: kz_term:api_binary()                  %% The database name of the account that authorized this call
                    ,account_id :: kz_term:api_binary()                  %% The account id that authorized this call
                    ,authorizing_id :: kz_term:api_binary()              %% The ID of the record that authorized this call
                    ,authorizing_type :: kz_term:api_binary()            %% The pvt_type of the record that authorized this call
                    ,owner_id :: kz_term:api_binary()                    %% The ID of the owner of this calling device, if any
                    ,fetch_id :: kz_term:api_binary()                    %% The Fetch ID of the Call
                    ,bridge_id :: kz_term:api_binary()                    %% The Bridge ID of the Call
                    ,language :: kz_term:api_binary()                     %% Language of the call to use
                    ,app_name = <<"kapps_call">> :: kz_term:ne_binary()        %% The application name used during kapps_call_command
                    ,app_version = <<"1.0.0">> :: kz_term:ne_binary()           %% The application version used during kapps_call_command
                    ,custom_publish_fun :: kapps_custom_publish() | 'undefined'     %% A custom command used to publish kapps_call_command
                    ,ccvs = kz_json:new() :: kz_json:object()           %% Any custom channel vars that where provided with the route request
                    ,cavs = kz_json:new() :: kz_json:object()           %% Any custom application vars that where provided with the route request
                    ,sip_headers = kz_json:new() :: kz_json:object()    %% Custom SIP Headers
                    ,kvs = orddict:new() :: orddict:orddict()           %% allows callflows to set values that propagate to children
                    ,other_leg_call_id :: kz_term:api_binary()
                    ,resource_type :: kz_term:api_binary()                      %% from route_req
                    ,to_tag :: kz_term:api_binary()
                    ,from_tag :: kz_term:api_binary()
                    ,direction = <<"inbound">> :: kz_term:ne_binary()
                    ,call_bridged = 'false' :: boolean()                %% Specified during call termination whether the call had been bridged
                    ,message_left = 'false' :: boolean()                %% Specified during call termination whether the caller left a voicemail message
                    ,is_recording = 'false' :: boolean()                %% Control account level recording
                    ,is_call_forward = 'false' :: boolean()             %% is this call forward call
                    }).

-type call() :: #kapps_call{}.
-export_type([call/0]).

-export_type([kapps_api_std_return/0]).

-type kapps_helper_function() :: fun((kz_term:api_binary(), call()) -> kz_term:api_binary()).

-define(SPECIAL_VARS, [{<<"Caller-ID-Name">>, #kapps_call.caller_id_name}
                      ,{<<"Caller-ID-Number">>, #kapps_call.caller_id_number}
                      ,{<<"Account-ID">>, #kapps_call.account_id}
                      ,{<<"Owner-ID">>, #kapps_call.owner_id}
                      ,{<<"Fetch-ID">>, #kapps_call.fetch_id}
                      ,{<<"Bridge-ID">>, #kapps_call.bridge_id}
                      ,{<<"Authorizing-ID">>, #kapps_call.authorizing_id}
                      ,{<<"Authorizing-Type">>, #kapps_call.authorizing_type}
                      ]).

-spec default_helper_function(Field, call()) -> Field.
default_helper_function(Field, #kapps_call{}) -> Field.

-spec clear_helpers(call()) -> call().
clear_helpers(#kapps_call{}=Call) ->
    Fs = [fun clear_custom_publish_function/1
         ,fun clear_call_id_helper/1
         ,fun clear_control_queue_helper/1
         ],
    exec(Fs, Call).

-spec new() -> call().
new() -> #kapps_call{}.

-spec put_callid(call()) -> kz_term:api_binary().
put_callid(#kapps_call{call_id='undefined'}) -> 'undefined';
put_callid(#kapps_call{call_id=CallId}) ->
    kz_util:put_callid(CallId).

-spec from_route_req(kapi_route:req()) -> call().
from_route_req(RouteReq) ->
    from_route_req(RouteReq, new()).

-spec from_route_req(kapi_route:req(), call()) -> call().
from_route_req(RouteReq, #kapps_call{call_id=OldCallId
                                    ,account_id=OldAccountId
                                    ,account_db=OldAccountDb
                                    ,ccvs=OldCCVs
                                    ,cavs=OldCAVs
                                    ,sip_headers=OldSHs
                                    ,request=OldRequest
                                    ,from=OldFrom
                                    ,to=OldTo
                                    }=Call) ->
    CallId = kz_api:call_id(RouteReq, OldCallId),
    kz_util:put_callid(CallId),

    CCVs = merge(OldCCVs, kz_json:get_json_value(<<"Custom-Channel-Vars">>, RouteReq)),
    CAVs = merge(OldCAVs, kz_json:get_json_value(<<"Custom-Application-Vars">>, RouteReq)),
    SHs = merge(OldSHs, kz_json:get_json_value(<<"Custom-SIP-Headers">>, RouteReq)),

    Request = kz_json:get_ne_binary_value(<<"Request">>, RouteReq, OldRequest),
    From = kz_json:get_ne_binary_value(<<"From">>, RouteReq, OldFrom),
    To = kz_json:get_ne_binary_value(<<"To">>, RouteReq, OldTo),

    {AccountId, AccountDb} =
        find_account_info(OldAccountId, OldAccountDb, kz_json:get_value(<<"Account-ID">>, CCVs)),

    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),

    Call1 =
        case kz_json:get_ne_binary_value(<<"Prepend-CID-Name">>, RouteReq) of
            'undefined' -> Call;
            Prepend -> kvs_store('prepend_cid_name', Prepend, Call)
        end,

    Call1#kapps_call{call_id=CallId
                    ,origination_call_id=kz_json:get_ne_binary_value(<<"Origination-Call-ID">>, RouteReq, origination_call_id(Call1))
                    ,context=kz_json:get_ne_binary_value(<<"Context">>, RouteReq, context(Call))
                    ,request=Request
                    ,request_user=to_e164(RequestUser)
                    ,request_realm=RequestRealm
                    ,from=From
                    ,from_user=FromUser
                    ,from_realm=FromRealm
                    ,to=To
                    ,to_user=ToUser
                    ,to_realm=ToRealm
                    ,account_id=AccountId
                    ,account_db=AccountDb
                    ,inception = kz_json:get_ne_binary_value(<<"Inception">>, CCVs, inception(Call))
                    ,switch_hostname = kz_json:get_ne_binary_value(<<"Switch-Hostname">>, RouteReq, switch_hostname(Call))
                    ,switch_nodename = kz_json:get_ne_binary_value(<<"Switch-Nodename">>, RouteReq, switch_nodename(Call))
                    ,switch_url = kz_json:get_ne_binary_value(<<"Switch-URL">>, RouteReq, switch_url(Call))
                    ,switch_uri = kz_json:get_ne_binary_value(<<"Switch-URI">>, RouteReq, switch_uri(Call))
                    ,authorizing_id = kz_json:get_ne_binary_value(<<"Authorizing-ID">>, CCVs, authorizing_id(Call))
                    ,authorizing_type = kz_json:get_ne_binary_value(<<"Authorizing-Type">>, CCVs, authorizing_type(Call))
                    ,owner_id = kz_json:get_ne_binary_value(<<"Owner-ID">>, CCVs, owner_id(Call))
                    ,fetch_id = kz_json:get_ne_binary_value(<<"Fetch-ID">>, CCVs, fetch_id(Call))
                    ,bridge_id = kz_json:get_ne_binary_value(<<"Bridge-ID">>, CCVs, bridge_id(Call))
                    ,caller_id_name = kz_json:get_binary_value(<<"Caller-ID-Name">>, RouteReq, caller_id_name(Call))
                    ,callee_id_name = kz_json:get_binary_value(<<"Callee-ID-Name">>, RouteReq, callee_id_name(Call))
                    ,caller_id_number = kz_json:get_binary_value(<<"Caller-ID-Number">>, RouteReq, caller_id_number(Call))
                    ,callee_id_number = kz_json:get_binary_value(<<"Callee-ID-Number">>, RouteReq, ToUser)
                    ,ccvs = CCVs
                    ,cavs = CAVs
                    ,sip_headers = SHs
                    ,resource_type = kz_json:get_ne_binary_value(<<"Resource-Type">>, RouteReq, resource_type(Call))
                    ,to_tag = kz_json:get_ne_binary_value(<<"To-Tag">>, RouteReq, to_tag(Call))
                    ,from_tag = kz_json:get_ne_binary_value(<<"From-Tag">>, RouteReq, from_tag(Call))
                    ,direction = kz_json:get_ne_binary_value(<<"Call-Direction">>, RouteReq, direction(Call))
                    ,is_call_forward = kz_json:is_true(<<"Call-Forward">>, CCVs, is_call_forward(Call))
                    }.

-spec from_route_win(kz_json:object()) -> call().
from_route_win(RouteWin) ->
    from_route_win(RouteWin, new()).

-spec from_route_win(kz_json:object(), call()) -> call().
from_route_win(RouteWin, #kapps_call{call_id=OldCallId
                                    ,ccvs=OldCCVs
                                    ,cavs=OldCAVs
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
    CallId = kz_json:get_value(<<"Call-ID">>, RouteWin, OldCallId),
    kz_util:put_callid(CallId),

    CCVs = merge(OldCCVs, kz_json:get_json_value(<<"Custom-Channel-Vars">>, RouteWin)),
    CAVs = merge(OldCAVs, kz_json:get_json_value(<<"Custom-Application-Vars">>, RouteWin)),
    SHs = merge(OldSHs, kz_json:get_json_value(<<"Custom-SIP-Headers">>, RouteWin)),

    {AccountId, AccountDb} =
        find_account_info(OldAccountId, OldAccountDb, kz_json:get_ne_binary_value(<<"Account-ID">>, CCVs)),

    Call#kapps_call{call_id=CallId
                   ,account_id=AccountId
                   ,account_db=AccountDb
                   ,ccvs=CCVs
                   ,cavs=CAVs
                   ,sip_headers=SHs
                   ,control_q = kz_json:get_ne_binary_value(<<"Control-Queue">>, RouteWin)
                   ,inception = kz_json:get_ne_binary_value(<<"Inception">>, CCVs, OldInception)
                   ,authorizing_id = kz_json:get_ne_binary_value(<<"Authorizing-ID">>, CCVs, OldAuthzId)
                   ,authorizing_type = kz_json:get_ne_binary_value(<<"Authorizing-Type">>, CCVs, OldAuthzType)
                   ,owner_id = kz_json:get_ne_binary_value(<<"Owner-ID">>, CCVs, OldOwnerId)
                   ,fetch_id = kz_json:get_ne_binary_value(<<"Fetch-ID">>, CCVs, OldFetchId)
                   ,bridge_id = kz_json:get_ne_binary_value(<<"Bridge-ID">>, CCVs, OldBridgeId)
                   ,language = kz_media_util:prompt_language(AccountId, OldLanguage)
                   }.

-spec find_account_info(kz_term:api_binary(), kz_term:api_binary(), kz_term:api_binary()) ->
                               {kz_term:api_binary(), kz_term:api_binary()}.
find_account_info(OldId, OldDb, 'undefined') ->
    {OldId, OldDb};
find_account_info('undefined', _OldDb, AccountId) ->
    {AccountId
    ,kz_util:format_account_id(AccountId, 'encoded')
    };
find_account_info(OldId, OldDb, _AccountId) ->
    {OldId, OldDb}.

-spec merge(kz_json:object(), kz_term:api_object()) -> kz_json:object().
merge(OldJObj, 'undefined') -> OldJObj;
merge(OldJObj, JObj) ->
    kz_json:merge(OldJObj, JObj).

-spec from_originate_uuid(kz_json:object()) -> call().
from_originate_uuid(JObj) ->
    from_originate_uuid(JObj, new()).

-spec from_originate_uuid(kz_json:object(), call()) -> call().
from_originate_uuid(JObj, #kapps_call{}=Call) ->
    'true' = kapi_resource:originate_uuid_v(JObj),
    Call#kapps_call{control_q=kz_json:get_value(<<"Outbound-Call-Control-Queue">>, JObj, control_queue(Call))
                   ,call_id=kz_json:get_value(<<"Outbound-Call-ID">>, JObj, call_id(Call))
                   }.


-spec from_originate_ready(kz_json:object()) -> call().
from_originate_ready(JObj) ->
    from_originate_ready(JObj, new()).

-spec from_originate_ready(kz_json:object(), call()) -> call().
from_originate_ready(JObj, #kapps_call{}=Call) ->
    'true' = kapi_resource:originate_ready_v(JObj),
    Call#kapps_call{control_q=kz_json:get_value(<<"Control-Queue">>, JObj, control_queue(Call))
                   ,call_id=kz_json:get_value(<<"Call-ID">>, JObj, call_id(Call))
                   }.

-spec from_channel_create(kz_json:object()) -> call().
from_channel_create(JObj) ->
    from_channel_create(JObj, new()).

-spec from_channel_create(kz_json:object(), call()) -> call().
from_channel_create(JObj, Call) ->
    from_json(JObj, Call).

%%------------------------------------------------------------------------------
%% @doc Creates a call record from JSON object.
%%
%% <div class="warning">Custom publisher and helper functions are not maintained
%% when converting to/from JSON</div>
%% @end
%%------------------------------------------------------------------------------

-spec from_json(kz_json:object()) -> call().
from_json(JObj) ->
    from_json(JObj, new()).

-spec from_json(kz_json:object(), call()) -> call().
from_json(JObj, #kapps_call{ccvs=OldCCVs
                           ,cavs=OldCAVs
                           ,kvs=Kvs
                           ,sip_headers=OldSHs
                           }=Call) ->
    CCVs = kz_json:merge(OldCCVs, kz_json:get_json_value(<<"Custom-Channel-Vars">>, JObj, kz_json:new())),
    CAVs = kz_json:merge(OldCAVs, kz_json:get_json_value(<<"Custom-Application-Vars">>, JObj, kz_json:new())),
    SHs = kz_json:merge(OldSHs, kz_json:get_value(<<"Custom-SIP-Headers">>, JObj, kz_json:new())),
    KVS = orddict:from_list(kz_json:to_proplist(kz_json:get_value(<<"Key-Value-Store">>, JObj, kz_json:new()))),
    Call#kapps_call{call_id = kz_json:get_ne_binary_value(<<"Call-ID">>, JObj, call_id_direct(Call))
                   ,origination_call_id = kz_json:get_ne_binary_value(<<"Origination-Call-ID">>, JObj, origination_call_id(Call))
                   ,context=kz_json:get_ne_binary_value(<<"Context">>, JObj, context(Call))
                   ,control_q = kz_json:get_ne_binary_value(<<"Control-Queue">>, JObj, control_queue_direct(Call))
                   ,controller_q = kz_json:get_ne_binary_value(<<"Controller-Queue">>, JObj, controller_queue(Call))
                   ,caller_id_name = kz_json:get_ne_binary_value(<<"Caller-ID-Name">>, JObj, caller_id_name(Call))
                   ,caller_id_number = kz_json:get_ne_binary_value(<<"Caller-ID-Number">>, JObj, caller_id_number(Call))
                   ,callee_id_name = kz_json:get_ne_binary_value(<<"Callee-ID-Name">>, JObj, callee_id_name(Call))
                   ,callee_id_number = kz_json:get_ne_binary_value(<<"Callee-ID-Number">>, JObj, callee_id_number(Call))
                   ,request = kz_json:get_ne_binary_value(<<"Request">>, JObj, request(Call))
                   ,request_user = kz_json:get_ne_binary_value(<<"Request-User">>, JObj, request_user(Call))
                   ,request_realm = kz_json:get_ne_binary_value(<<"Request-Realm">>, JObj, request_realm(Call))
                   ,from = kz_json:get_ne_binary_value(<<"From">>, JObj, from(Call))
                   ,from_user = kz_json:get_ne_binary_value(<<"From-User">>, JObj, from_user(Call))
                   ,from_realm = kz_json:get_ne_binary_value(<<"From-Realm">>, JObj, from_realm(Call))
                   ,to = kz_json:get_ne_binary_value(<<"To">>, JObj, to(Call))
                   ,to_user = kz_json:get_ne_binary_value(<<"To-User">>, JObj, to_user(Call))
                   ,to_realm = kz_json:get_ne_binary_value(<<"To-Realm">>, JObj, to_realm(Call))
                   ,switch_hostname = kz_json:get_ne_binary_value(<<"Switch-Hostname">>, JObj, switch_hostname(Call))
                   ,switch_nodename = kz_json:get_ne_binary_value(<<"Switch-Nodename">>, JObj, switch_nodename(Call))
                   ,switch_url = kz_json:get_ne_binary_value(<<"Switch-URL">>, JObj, switch_url(Call))
                   ,switch_uri = kz_json:get_ne_binary_value(<<"Switch-URI">>, JObj, switch_uri(Call))
                   ,inception = kz_json:get_ne_binary_value(<<"Inception">>, JObj, inception(Call))
                   ,account_db = kz_json:get_ne_binary_value(<<"Account-DB">>, JObj, account_db(Call))
                   ,account_id = kz_json:get_ne_binary_value(<<"Account-ID">>, JObj, account_id(Call))
                   ,authorizing_id = kz_json:get_ne_binary_value(<<"Authorizing-ID">>, JObj, authorizing_id(Call))
                   ,authorizing_type = kz_json:get_ne_binary_value(<<"Authorizing-Type">>, JObj, authorizing_type(Call))
                   ,owner_id = kz_json:get_ne_binary_value(<<"Owner-ID">>, JObj, owner_id(Call))
                   ,fetch_id = kz_json:get_ne_binary_value(<<"Fetch-ID">>, JObj, fetch_id(Call))
                   ,bridge_id = kz_json:get_ne_binary_value(<<"Bridge-ID">>, JObj, bridge_id(Call))
                   ,language = kz_json:get_ne_binary_value(<<"Language">>, JObj, language(Call))
                   ,app_name = kz_json:get_ne_binary_value(<<"App-Name">>, JObj, application_name(Call))
                   ,app_version = kz_json:get_ne_binary_value(<<"App-Version">>, JObj, application_version(Call))
                   ,ccvs = CCVs
                   ,cavs = CAVs
                   ,sip_headers = SHs
                   ,kvs = orddict:merge(fun(_, _, V2) -> V2 end, Kvs, KVS)
                   ,other_leg_call_id = kz_json:get_ne_binary_value(<<"Other-Leg-Call-ID">>, JObj, other_leg_call_id(Call))
                   ,resource_type = kz_json:get_ne_binary_value(<<"Resource-Type">>, JObj, resource_type(Call))
                   ,to_tag = kz_json:get_ne_binary_value(<<"To-Tag">>, JObj, to_tag(Call))
                   ,from_tag = kz_json:get_ne_binary_value(<<"From-Tag">>, JObj, from_tag(Call))
                   ,direction = kz_json:get_ne_binary_value(<<"Call-Direction">>, JObj, direction(Call))
                   ,call_bridged = kz_json:is_true(<<"Call-Bridged">>, JObj, call_bridged(Call))
                   ,message_left = kz_json:is_true(<<"Message-Left">>, JObj, message_left(Call))
                   ,is_recording = kz_json:is_true(<<"Is-Recording">>, JObj, is_recording(Call))
                   ,is_call_forward = kz_json:is_true(<<"Is-Call-Forward">>, JObj, is_call_forward(Call))
                   }.

%%------------------------------------------------------------------------------
%% @doc Converts a call record to JSON proplist.
%%
%% <div class="warning">Custom publisher and helper functions are not maintained
%% when converting to/from JSON</div>
%% @end
%%------------------------------------------------------------------------------
-spec to_json(call()) -> kz_json:object().
to_json(#kapps_call{}=Call) ->
    Props = to_proplist(Call),
    KVS = [KV
           || {_, V}=KV <- props:get_value(<<"Key-Value-Store">>, Props, [])
                  ,V =/= 'undefined'
                  ,kz_json:is_json_term(V)
          ],
    kz_json:from_list([KV
                       || {_, V}=KV <- [{<<"Key-Value-Store">>, kz_json:from_list(KVS)} |
                                        props:delete(<<"Key-Value-Store">>, Props)
                                       ],
                          V =/= 'undefined',
                          kz_json:is_json_term(V)
                      ]).

-spec to_proplist(call()) -> kz_term:proplist().
to_proplist(#kapps_call{}=Call) ->
    [{<<"Account-DB">>, account_db(Call)}
    ,{<<"Account-ID">>, account_id(Call)}
    ,{<<"Authorizing-ID">>, authorizing_id(Call)}
    ,{<<"Authorizing-Type">>, authorizing_type(Call)}
    ,{<<"Bridge-ID">>, bridge_id(Call)}
    ,{<<"Call-Bridged">>, call_bridged(Call)}
    ,{<<"Call-Direction">>, direction(Call)}
    ,{<<"Call-ID">>, call_id_direct(Call)}
    ,{<<"Callee-ID-Name">>, callee_id_name(Call)}
    ,{<<"Callee-ID-Number">>, callee_id_number(Call)}
    ,{<<"Caller-ID-Name">>, caller_id_name(Call)}
    ,{<<"Caller-ID-Number">>, caller_id_number(Call)}
    ,{<<"Control-Queue">>, control_queue_direct(Call)}
    ,{<<"Controller-Queue">>, controller_queue(Call)}
    ,{<<"Custom-Application-Vars">>, custom_application_vars(Call)}
    ,{<<"Custom-Channel-Vars">>, custom_channel_vars(Call)}
    ,{<<"Custom-SIP-Headers">>, custom_sip_headers(Call)}
    ,{<<"Fetch-ID">>, fetch_id(Call)}
    ,{<<"From">>, from(Call)}
    ,{<<"From-Realm">>, from_realm(Call)}
    ,{<<"From-Tag">>, from_tag(Call)}
    ,{<<"From-User">>, from_user(Call)}
    ,{<<"Inception">>, inception(Call)}
    ,{<<"Is-Recording">>, is_recording(Call)}
    ,{<<"Is-Call-Forward">>, is_call_forward(Call)}
    ,{<<"Key-Value-Store">>, kvs_to_proplist(Call)}
    ,{<<"Language">>, language(Call)}
    ,{<<"Message-Left">>, message_left(Call)}
    ,{<<"Other-Leg-Call-ID">>, other_leg_call_id(Call)}
    ,{<<"Owner-ID">>, owner_id(Call)}
    ,{<<"Request">>, request(Call)}
    ,{<<"Request-Realm">>, request_realm(Call)}
    ,{<<"Request-User">>, request_user(Call)}
    ,{<<"Resource-Type">>, resource_type(Call)}
    ,{<<"Switch-Hostname">>, switch_hostname(Call)}
    ,{<<"Switch-Nodename">>, switch_nodename(Call)}
    ,{<<"Switch-URI">>, switch_uri(Call)}
    ,{<<"Switch-URL">>, switch_url(Call)}
    ,{<<"To">>, to(Call)}
    ,{<<"To-Realm">>, to_realm(Call)}
    ,{<<"To-Tag">>, to_tag(Call)}
    ,{<<"To-User">>, to_user(Call)}
    ].

-spec is_call(any()) -> boolean().
is_call(#kapps_call{}) -> 'true';
is_call(_) -> 'false'.

-type exec_fun_1() :: fun((call()) -> call()).
-type exec_fun_2() :: {fun((_, call()) -> call()), _}.
-type exec_fun_3() :: {fun((_, _, call()) -> call()), _, _}.
-type exec_fun() :: exec_fun_1() | exec_fun_2() | exec_fun_3().
-type exec_funs() :: [exec_fun(),...].

-spec exec(exec_funs(), call()) -> call().
exec(Funs, #kapps_call{}=Call) ->
    lists:foldl(fun exec_fold/2, Call, Funs).

-spec exec_fold(exec_fun(), call()) -> call().
exec_fold({F, K, V}, C) when is_function(F, 3) -> F(K, V, C);
exec_fold({F, V}, C) when is_function(F, 2) -> F(V, C);
exec_fold(F, C) when is_function(F, 1) -> F(C).

-spec set_application_name(kz_term:ne_binary(), call()) -> call().
set_application_name(AppName, #kapps_call{}=Call) when is_binary(AppName) ->
    Call#kapps_call{app_name=AppName}.

-spec application_name(call()) -> kz_term:ne_binary().
application_name(#kapps_call{app_name=AppName}) ->
    AppName.

-spec set_application_version(kz_term:ne_binary(), call()) -> call().
set_application_version(AppVersion, #kapps_call{}=Call) when is_binary(AppVersion) ->
    Call#kapps_call{app_version=AppVersion}.

-spec application_version(call()) -> kz_term:ne_binary().
application_version(#kapps_call{app_version=AppVersion}) ->
    AppVersion.

-spec set_call_id(kz_term:api_binary(), call()) -> call().
set_call_id(CallId, #kapps_call{}=Call) ->
    Call#kapps_call{call_id=CallId}.

-spec set_origination_call_id(kz_term:api_ne_binary(), call()) -> call().
set_origination_call_id(CallId, #kapps_call{}=Call) ->
    Call#kapps_call{origination_call_id=CallId}.

-spec set_other_leg_call_id(kz_term:api_binary(), call()) -> call().
set_other_leg_call_id(CallId, #kapps_call{}=Call) ->
    Call#kapps_call{other_leg_call_id=CallId}.

-spec call_id(call()) -> kz_term:api_binary().
call_id(#kapps_call{call_id=CallId, call_id_helper=Fun}=Call) when is_function(Fun, 2) ->
    Fun(CallId, Call);
call_id(#kapps_call{call_id=CallId}=Call) ->
    default_helper_function(CallId, Call).

-spec call_id_direct(call()) -> kz_term:api_binary().
call_id_direct(#kapps_call{call_id=CallId}) ->
    CallId.

-spec origination_call_id(call()) -> kz_term:api_ne_binary().
origination_call_id(#kapps_call{origination_call_id=CallId}) ->
    CallId.

-spec other_leg_call_id(call()) -> kz_term:api_binary().
other_leg_call_id(#kapps_call{other_leg_call_id=CallId}=_Call) ->
    CallId.

-spec call_id_helper(kapps_helper_function(), call()) -> call().
call_id_helper(Fun, #kapps_call{}=Call) when is_function(Fun, 2) ->
    Call#kapps_call{call_id_helper=Fun}.

-spec clear_call_id_helper(call()) -> call().
clear_call_id_helper(Call) ->
    Call#kapps_call{call_id_helper=fun default_helper_function/2}.

-spec context(call()) -> kz_term:api_ne_binary().
context(Call) ->
    context(Call, 'undefined').

-spec context(call(), Default) -> kz_term:ne_binary() | Default.
context(#kapps_call{context='undefined'}, Default) -> Default;
context(#kapps_call{context=Context}, _Default) -> Context.

-spec set_context(call(), kz_term:ne_binary()) -> call().
set_context(#kapps_call{}=Call, Context) ->
    Call#kapps_call{context=Context}.

-spec set_control_queue(kz_term:ne_binary(), call()) -> call().
set_control_queue(ControlQ, #kapps_call{}=Call) when is_binary(ControlQ) ->
    Call#kapps_call{control_q=ControlQ}.

-spec control_queue(call()) -> kz_term:api_binary().
control_queue(#kapps_call{control_q=ControlQ, control_q_helper=Fun}=Call) when is_function(Fun, 2) ->
    Fun(ControlQ, Call);
control_queue(#kapps_call{control_q=ControlQ}=Call) ->
    default_helper_function(ControlQ, Call).

-spec control_queue_direct(call()) -> kz_term:api_binary().
control_queue_direct(#kapps_call{control_q=ControlQ}) ->
    ControlQ.

-spec control_queue_helper(kapps_helper_function(), call()) -> call().
control_queue_helper(Fun, #kapps_call{}=Call) when is_function(Fun, 2) ->
    Call#kapps_call{control_q_helper=Fun}.

-spec clear_control_queue_helper(call()) -> call().
clear_control_queue_helper(#kapps_call{}=Call) ->
    Call#kapps_call{control_q_helper=fun default_helper_function/2}.

-spec set_controller_queue(kz_term:ne_binary(), call()) -> call().
set_controller_queue(ControllerQ, #kapps_call{}=Call) when is_binary(ControllerQ) ->
    Call#kapps_call{controller_q=ControllerQ}.

-spec controller_queue(call()) -> binary().
controller_queue(#kapps_call{controller_q=ControllerQ}) ->
    ControllerQ.

-spec maybe_format_caller_id(call(), kz_term:api_object()) -> call().
maybe_format_caller_id(Call, 'undefined') -> Call;
maybe_format_caller_id(Call, Format) ->
    set_caller_id_number(maybe_format_caller_id_str(caller_id_number(Call), Format), Call).

-spec maybe_format_caller_id_str(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:ne_binary().
maybe_format_caller_id_str(Cid, 'undefined') -> Cid;
maybe_format_caller_id_str(Cid, Format) ->
    Class = knm_converters:classify(Cid),
    lager:debug("checking for caller id reformatting rules for ~s numbers", [Class]),
    case kz_json:get_ne_value(Class, Format) of
        'undefined' -> maybe_reformat_caller_id(Cid, kz_json:get_ne_value(<<"all">>, Format));
        UseFormat   -> maybe_reformat_caller_id(Cid, UseFormat)
    end.

-spec maybe_reformat_caller_id(kz_term:ne_binary(), kz_term:api_object()) -> kz_term:ne_binary().
maybe_reformat_caller_id(CallerId, 'undefined') -> CallerId;
maybe_reformat_caller_id(CallerId, Format) ->
    Regex = kz_json:get_ne_value(<<"regex">>, Format),
    maybe_regex_caller_id(CallerId, Regex, Format).

-spec maybe_regex_caller_id(kz_term:ne_binary(), kz_term:api_binary(), kz_json:object()) -> kz_term:ne_binary().
maybe_regex_caller_id(CallerId, 'undefined', _) -> CallerId;
maybe_regex_caller_id(CallerId, Regex, Format) ->
    Normalized = knm_converters:normalize(CallerId),
    case re:run(Normalized, Regex, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [UseCid|_]} ->
            lager:info("cid rewrite match found ~s from normalized caller id ~s"
                      ,[UseCid, Normalized]
                      ),
            maybe_append_caller_id(maybe_prepend_caller_id(UseCid
                                                          ,kz_json:get_ne_value(<<"prefix">>, Format)
                                                          )
                                  ,kz_json:get_ne_value(<<"suffix">>, Format)
                                  );
        _NotMatching -> CallerId
    end.

-spec maybe_prepend_caller_id(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
maybe_prepend_caller_id(CallerId, 'undefined') -> CallerId;
maybe_prepend_caller_id(CallerId, Prefix) ->
    BinPrefix   = kz_term:to_binary(Prefix),
    lager:info("prepending cid with ~s~n", [BinPrefix]),
    <<BinPrefix/binary, CallerId/binary>>.

-spec maybe_append_caller_id(kz_term:ne_binary(), kz_term:api_binary()) -> kz_term:ne_binary().
maybe_append_caller_id(CallerId, 'undefined') -> CallerId;
maybe_append_caller_id(CallerId, Suffix) ->
    BinSuffix   = kz_term:to_binary(Suffix),
    lager:info("appending cid with ~s~n", [BinSuffix]),
    <<CallerId/binary, BinSuffix/binary>>.

-spec set_caller_id(kz_term:ne_binary(), kz_term:ne_binary(), call()) -> call().
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
set_caller_id(CIDNumber, CIDName, #kapps_call{}=Call)
  when is_binary(CIDNumber)
       andalso is_binary(CIDName) ->
    Call#kapps_call{caller_id_number=CIDNumber
                   ,callee_id_name=CIDName
                   }.
-else.
set_caller_id(CIDNumber, CIDName, #kapps_call{}=Call)
  when is_binary(CIDNumber)
       andalso is_binary(CIDName) ->
    CCVs = custom_channel_vars(Call),
    JObj = kz_json:from_list([{<<"Caller-ID-Number">>, CIDNumber}
                             ,{<<"Caller-ID-Name">>, CIDName}
                             ,{<<"Privacy-Hide-Name">>
                              ,kz_privacy:should_hide_name(CCVs)
                              }
                             ,{<<"Privacy-Hide-Number">>
                              ,kz_privacy:should_hide_number(CCVs)
                              }
                             ]),
    kapps_call_command:set(JObj, 'undefined', Call),
    Call#kapps_call{caller_id_number=CIDNumber
                   ,callee_id_name=CIDName
                   }.
-endif.

-spec caller_id(call()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
caller_id(Call) ->
    {caller_id_number(Call), caller_id_name(Call)}.

-spec set_caller_id_name(kz_term:ne_binary(), call()) -> call().
-ifdef(TEST).
set_caller_id_name(CIDName, Call) ->
    Call#kapps_call{caller_id_name=CIDName}.
-else.
set_caller_id_name(CIDName, #kapps_call{}=Call) when is_binary(CIDName) ->
    CCVs = custom_channel_vars(Call),
    Props = [{<<"Caller-ID-Name">>, CIDName}
            ,{<<"Privacy-Hide-Name">>
             ,kz_privacy:should_hide_name(CCVs)
             }
            ],
    kapps_call_command:set(kz_json:from_list(Props), 'undefined', Call),
    Call#kapps_call{caller_id_name=CIDName}.
-endif.

-spec caller_id_name(call()) -> kz_term:ne_binary().
-ifdef(TEST).
caller_id_name(#kapps_call{caller_id_name=CIDName}) -> CIDName.
-else.
caller_id_name(#kapps_call{caller_id_name=CIDName
                          ,account_id=AccountId
                          }) ->
    case kz_term:is_empty(CIDName) of
        'true' -> unknown_caller_id_name(AccountId);
        'false' -> CIDName
    end.
-endif.


-spec unknown_caller_id_name() -> kz_term:ne_binary().
unknown_caller_id_name() -> unknown_caller_id_name('undefined').

-spec unknown_caller_id_name(kz_term:api_ne_binary()) -> kz_term:ne_binary().
unknown_caller_id_name('undefined') ->
    kapps_config:get_ne_binary(?CALL_CMD_CAT, ?UNKNOWN_CALLER_ID_NAME_KEY, ?DEFAULT_UNKNOWN_CALLER_ID_NAME);
unknown_caller_id_name(AccountId) ->
    kapps_account_config:get_global(AccountId, ?CALL_CMD_CAT, ?UNKNOWN_CALLER_ID_NAME_KEY, ?DEFAULT_UNKNOWN_CALLER_ID_NAME).

-spec set_caller_id_number(kz_term:api_binary(), call()) -> call().
-ifdef(TEST).
set_caller_id_number(CIDNumber, Call) ->
    Call#kapps_call{caller_id_number=CIDNumber}.
-else.
set_caller_id_number(CIDNumber, #kapps_call{}=Call) ->
    CCVs = custom_channel_vars(Call),
    Props = [{<<"Caller-ID-Number">>, CIDNumber}
            ,{<<"Privacy-Hide-Number">>
             ,kz_privacy:should_hide_number(CCVs)
             }
            ],
    kapps_call_command:set(kz_json:from_list(Props), 'undefined', Call),
    Call#kapps_call{caller_id_number=CIDNumber}.
-endif.

-spec caller_id_number(call()) -> kz_term:ne_binary().
-ifdef(TEST).
caller_id_number(#kapps_call{caller_id_number=CIDNumber}) -> CIDNumber.
-else.
caller_id_number(#kapps_call{caller_id_number=CIDNumber
                            ,account_id=AccountId
                            }) ->
    case kz_term:is_empty(CIDNumber) of
        'true' -> kz_privacy:anonymous_caller_id_number(AccountId);
        'false' -> CIDNumber
    end.
-endif.

-spec set_callee_id(kz_term:ne_binary(), kz_term:ne_binary(), call()) -> call().
-ifdef(TEST).
set_callee_id(CIDNumber, CIDName, #kapps_call{}=Call)
  when is_binary(CIDNumber)
       andalso is_binary(CIDName) ->
    Call#kapps_call{callee_id_number=CIDNumber
                   ,callee_id_name=CIDName
                   }.
-else.
set_callee_id(CIDNumber, CIDName, #kapps_call{}=Call)
  when is_binary(CIDNumber)
       andalso is_binary(CIDName) ->
    kapps_call_command:set(kz_json:from_list([{<<"Callee-ID-Number">>, CIDNumber}
                                             ,{<<"Callee-ID-Name">>, CIDName}
                                             ]), 'undefined', Call),
    Call#kapps_call{callee_id_number=CIDNumber
                   ,callee_id_name=CIDName
                   }.
-endif.

-spec callee_id(call()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
callee_id(Call) ->
    {callee_id_number(Call), callee_id_name(Call)}.

-spec set_callee_id_name(kz_term:ne_binary(), call()) -> call().
-ifdef(TEST).
set_callee_id_name(CIDName, Call) ->
    Call#kapps_call{callee_id_name=CIDName}.
-else.
set_callee_id_name(CIDName, #kapps_call{}=Call) when is_binary(CIDName) ->
    kapps_call_command:set(kz_json:from_list([{<<"Callee-ID-Name">>, CIDName}]), 'undefined', Call),
    Call#kapps_call{callee_id_name=CIDName}.
-endif.

-spec callee_id_name(call()) -> binary().
callee_id_name(#kapps_call{callee_id_name='undefined'}) -> <<>>;
callee_id_name(#kapps_call{callee_id_name=CIDName}) -> CIDName.

-spec set_callee_id_number(kz_term:ne_binary(), call()) -> call().
-ifdef(TEST).
set_callee_id_number(CIDNumber, Call) ->
    Call#kapps_call{callee_id_number=CIDNumber}.
-else.
set_callee_id_number(CIDNumber, #kapps_call{}=Call) when is_binary(CIDNumber) ->
    kapps_call_command:set(kz_json:from_list([{<<"Callee-ID-Number">>, CIDNumber}]), 'undefined', Call),
    Call#kapps_call{callee_id_number=CIDNumber}.
-endif.

-spec callee_id_number(call()) -> binary().
callee_id_number(#kapps_call{callee_id_number='undefined'}) -> <<>>;
callee_id_number(#kapps_call{callee_id_number=CIDNumber}) -> CIDNumber.

-spec set_request(kz_term:ne_binary(), call()) -> call().
set_request(Request, #kapps_call{}=Call) when is_binary(Request) ->
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    Call#kapps_call{request=Request
                   ,request_user=to_e164(RequestUser)
                   ,request_realm=RequestRealm
                   }.

-ifdef(TEST).
to_e164(Number) -> Number.
-else.
to_e164(<<"*", _/binary>>=Number) -> Number;
to_e164(Number) ->
    knm_converters:normalize(Number).
-endif.

-spec request(call()) -> kz_term:ne_binary().
request(#kapps_call{request=Request}) ->
    Request.

-spec request_user(call()) -> kz_term:ne_binary().
request_user(#kapps_call{request=?NO_USER_REALM
                        ,request_user=RequestUser
                        }) ->
    RequestUser;
request_user(#kapps_call{request=Request
                        ,request_user=?NO_USER
                        }) ->
    [RequestUser, _] = binary:split(Request, <<"@">>),
    RequestUser;
request_user(#kapps_call{request_user=RequestUser}) ->
    RequestUser.

-spec request_realm(call()) -> kz_term:ne_binary().
request_realm(#kapps_call{request=?NO_USER_REALM
                         ,request_realm=RequestRealm
                         }) ->
    RequestRealm;
request_realm(#kapps_call{request=Request
                         ,request_realm=?NO_REALM
                         }) ->
    [_, RequestRealm] = binary:split(Request, <<"@">>),
    RequestRealm;
request_realm(#kapps_call{request_realm=RequestRealm}) ->
    RequestRealm.

-spec set_from(kz_term:ne_binary(), call()) -> call().
set_from(From, #kapps_call{}=Call) when is_binary(From) ->
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    Call#kapps_call{from=From
                   ,from_user=FromUser
                   ,from_realm=FromRealm
                   }.

-spec from(call()) -> kz_term:ne_binary().
from(#kapps_call{from=From}) ->
    From.

-spec from_user(call()) -> kz_term:ne_binary().
from_user(#kapps_call{from=?NO_USER_REALM
                     ,from_user=FromUser
                     }) ->
    FromUser;
from_user(#kapps_call{from=From
                     ,from_user=?NO_USER
                     }) ->
    [FromUser, _] = binary:split(From, <<"@">>),
    FromUser;
from_user(#kapps_call{from_user=FromUser}) ->
    FromUser.

-spec from_realm(call()) -> kz_term:ne_binary().
from_realm(#kapps_call{from=?NO_USER_REALM
                      ,from_realm=FromRealm
                      }) ->
    FromRealm;
from_realm(#kapps_call{from=From
                      ,from_realm=?NO_REALM
                      }) ->
    [_, FromRealm] = binary:split(From, <<"@">>),
    FromRealm;
from_realm(#kapps_call{from_realm=FromRealm}) ->
    FromRealm.

-spec set_to(kz_term:ne_binary(), call()) -> call().
set_to(To, #kapps_call{}=Call) when is_binary(To) ->
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    Call#kapps_call{to=To
                   ,to_user=ToUser
                   ,to_realm=ToRealm
                   }.

-spec to(call()) -> kz_term:ne_binary().
to(#kapps_call{to=To}) ->
    To.

-spec to_user(call()) -> kz_term:ne_binary().
to_user(#kapps_call{to=?NO_USER_REALM
                   ,to_user=ToUser
                   }) ->
    ToUser;
to_user(#kapps_call{to=To
                   ,to_user=?NO_USER
                   }) ->
    [ToUser, _] = binary:split(To, <<"@">>),
    ToUser;
to_user(#kapps_call{to_user=ToUser}) ->
    ToUser.

-spec to_realm(call()) -> kz_term:ne_binary().
to_realm(#kapps_call{to=?NO_USER_REALM
                    ,to_realm=ToRealm
                    }) ->
    ToRealm;
to_realm(#kapps_call{to=To
                    ,to_realm=?NO_REALM
                    }) ->
    [_, ToRealm] = binary:split(To, <<"@">>),
    ToRealm;
to_realm(#kapps_call{to_realm=ToRealm}) ->
    ToRealm.

-spec set_switch_hostname(kz_term:ne_binary(), call()) -> call().
set_switch_hostname(<<_/binary>> = Srv, #kapps_call{}=Call) ->
    Call#kapps_call{switch_hostname=Srv}.

-spec switch_hostname(call()) -> kz_term:api_binary().
switch_hostname(#kapps_call{switch_hostname=Srv}) ->
    Srv.

-spec set_switch_nodename(kz_term:ne_binary(), call()) -> call().
set_switch_nodename(Srv, #kapps_call{}=Call) ->
    Call#kapps_call{switch_nodename=Srv}.

-spec switch_nodename(call()) -> binary().
switch_nodename(#kapps_call{switch_nodename=Srv}) ->
    Srv.

-spec set_switch_url(kz_term:ne_binary(), call()) -> call().
set_switch_url(Srv, #kapps_call{}=Call) ->
    Call#kapps_call{switch_url=Srv}.

-spec switch_url(call()) -> binary().
switch_url(#kapps_call{switch_url=Srv}) ->
    Srv.

-spec set_switch_uri(kz_term:ne_binary(), call()) -> call().
set_switch_uri(Srv, #kapps_call{}=Call) ->
    Call#kapps_call{switch_uri=Srv}.

-spec switch_uri(call()) -> binary().
switch_uri(#kapps_call{switch_uri=Srv}) ->
    Srv.

-spec set_inception(kz_term:api_binary(), call()) -> call().
set_inception('undefined', #kapps_call{}=Call) ->
    Call#kapps_call{inception='undefined'};
set_inception(Inception, #kapps_call{}=Call) ->
    set_custom_channel_var(<<"Inception">>, Inception, Call#kapps_call{inception=Inception}).

-spec inception(call()) -> kz_term:api_binary().
inception(#kapps_call{inception=Inception}) ->
    Inception.

-spec set_resource_type(kz_term:api_binary(), call()) -> call().
set_resource_type('undefined', #kapps_call{}=Call) ->
    Call#kapps_call{resource_type='undefined'};
set_resource_type(ResourceType, #kapps_call{}=Call) ->
    insert_custom_channel_var(<<"Resource-Type">>, ResourceType, Call#kapps_call{resource_type=ResourceType}).

-spec resource_type(call()) -> kz_term:api_ne_binary().
resource_type(#kapps_call{resource_type=ResourceType}) ->
    ResourceType.

-spec set_account_db(kz_term:ne_binary(), call()) -> call().
set_account_db(<<_/binary>> = AccountDb, #kapps_call{}=Call) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#kapps_call{account_db=AccountDb
                                                                       ,account_id=AccountId
                                                                       }).

-spec account_db(call()) -> kz_term:api_ne_binary().
account_db(#kapps_call{account_db='undefined'
                      ,account_id='undefined'
                      }) -> 'undefined';
account_db(#kapps_call{account_db='undefined'
                      ,account_id=AccountId
                      }) ->
    kz_util:format_account_db(AccountId);
account_db(#kapps_call{account_db=AccountDb}) ->
    AccountDb.

-spec set_account_id(kz_term:ne_binary(), call()) -> call().
set_account_id(<<_/binary>> = AccountId, #kapps_call{}=Call) ->
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    set_custom_channel_var(<<"Account-ID">>, AccountId, Call#kapps_call{account_db=AccountDb
                                                                       ,account_id=AccountId
                                                                       }).

-spec account_id(call()) -> kz_term:api_binary().
account_id(#kapps_call{account_id=AccountId}) ->
    AccountId.

-spec account_realm(call()) -> kz_term:ne_binary().
account_realm(#kapps_call{account_id=AccountId}) ->
    {'ok', Doc} = kzd_accounts:fetch(AccountId),
    kzd_accounts:realm(Doc).

-spec set_authorizing_id(kz_term:ne_binary(), call()) -> call().
set_authorizing_id(AuthorizingId, #kapps_call{}=Call) when is_binary(AuthorizingId) ->
    set_custom_channel_var(<<"Authorizing-ID">>, AuthorizingId, Call#kapps_call{authorizing_id=AuthorizingId}).

-spec authorizing_id(call()) -> kz_term:api_binary().
authorizing_id(#kapps_call{authorizing_id=AuthorizingId}) ->
    AuthorizingId.

-spec set_authorizing_type(kz_term:ne_binary(), call()) -> call().
set_authorizing_type(AuthorizingType, #kapps_call{}=Call) when is_binary(AuthorizingType) ->
    set_custom_channel_var(<<"Authorizing-Type">>, AuthorizingType, Call#kapps_call{authorizing_type=AuthorizingType}).

-spec authorizing_type(call()) -> kz_term:api_binary().
authorizing_type(#kapps_call{authorizing_type=AuthorizingType}) ->
    AuthorizingType.

-spec set_authorization(kz_term:ne_binary(), kz_term:ne_binary(), call()) -> call().
set_authorization(AuthorizingType, AuthorizingId, #kapps_call{}=Call)
  when is_binary(AuthorizingType)
       andalso is_binary(AuthorizingId) ->
    set_custom_channel_vars([{<<"Authorizing-Type">>, AuthorizingType}
                            ,{<<"Authorizing-ID">>, AuthorizingId}
                            ]
                           ,Call#kapps_call{authorizing_type=AuthorizingType
                                           ,authorizing_id=AuthorizingId
                                           }
                           ).

-spec set_owner_id(kz_term:ne_binary(), call()) -> call().
set_owner_id(OwnerId, #kapps_call{}=Call) when is_binary(OwnerId) ->
    set_custom_channel_var(<<"Owner-ID">>, OwnerId, Call#kapps_call{owner_id=OwnerId}).

-spec owner_id(call()) -> kz_term:api_binary().
owner_id(#kapps_call{owner_id=OwnerId}) -> OwnerId.

-spec set_fetch_id(kz_term:ne_binary(), call()) -> call().
set_fetch_id(FetchId, #kapps_call{}=Call) when is_binary(FetchId) ->
    set_custom_channel_var(<<"Fetch-Id">>, FetchId, Call#kapps_call{fetch_id=FetchId}).

-spec fetch_id(call()) -> kz_term:api_binary().
fetch_id(#kapps_call{fetch_id=FetchId}) -> FetchId.

-spec set_bridge_id(kz_term:ne_binary(), call()) -> call().
set_bridge_id(BridgeId, #kapps_call{}=Call) when is_binary(BridgeId) ->
    set_custom_channel_var(<<"Bridge-Id">>, BridgeId, Call#kapps_call{bridge_id=BridgeId}).

-spec bridge_id(call()) -> kz_term:api_binary().
bridge_id(#kapps_call{bridge_id=BridgeId}) -> BridgeId.

-spec set_language(kz_term:ne_binary(), call()) -> call().
set_language(Language, #kapps_call{}=Call) when is_binary(Language) ->
    Call#kapps_call{language=Language}.

-spec language(call()) -> kz_term:api_binary().
-ifdef(TEST).
language(#kapps_call{language=L}) -> L.
-else.
language(#kapps_call{language='undefined', account_id=AccountId}) ->
    kz_media_util:prompt_language(AccountId);
language(#kapps_call{language=Language}) -> Language.
-endif.

-spec get_prompt(call(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
get_prompt(#kapps_call{}, 'undefined') -> 'undefined';
get_prompt(#kapps_call{}=Call, Media) ->
    get_prompt(Call, Media, language(Call)).

-spec get_prompt(call(), kz_term:api_ne_binary(), kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
get_prompt(_Call, 'undefined', _Lang) -> 'undefined';
get_prompt(Call, Media, 'undefined') ->
    kz_media_util:get_prompt(Media, language(Call), account_id(Call));
get_prompt(Call, Media, Language) ->
    kz_media_util:get_prompt(Media, Language, account_id(Call)).

-spec set_to_tag(kz_term:ne_binary(), call()) -> call().
set_to_tag(ToTag, #kapps_call{}=Call) when is_binary(ToTag) ->
    Call#kapps_call{to_tag=ToTag}.

-spec to_tag(call()) -> kz_term:api_binary().
to_tag(#kapps_call{to_tag=ToTag}) ->
    ToTag.

-spec set_from_tag(kz_term:ne_binary(), call()) -> call().
set_from_tag(FromTag, #kapps_call{}=Call) when is_binary(FromTag) ->
    Call#kapps_call{from_tag=FromTag}.

-spec from_tag(call()) -> kz_term:api_binary().
from_tag(#kapps_call{from_tag=FromTag}) ->
    FromTag.

-spec direction(call()) -> kz_term:ne_binary().
direction(#kapps_call{direction=Direction}) ->
    Direction.

-spec call_bridged(call()) -> boolean().
call_bridged(#kapps_call{call_bridged=IsBridged}) ->
    kz_term:is_true(IsBridged).

-spec set_call_bridged(boolean(), call()) -> call().
set_call_bridged(IsBridged, #kapps_call{}=Call) when is_boolean(IsBridged) ->
    Call#kapps_call{call_bridged=IsBridged}.

-spec message_left(call()) -> boolean().
message_left(#kapps_call{message_left=MessageLeft}) ->
    kz_term:is_true(MessageLeft).

-spec set_message_left(boolean(), call()) -> call().
set_message_left(MessageLeft, #kapps_call{}=Call) when is_boolean(MessageLeft) ->
    Call#kapps_call{message_left=MessageLeft}.

-spec remove_custom_channel_vars(kz_json:keys(), call()) -> call().
remove_custom_channel_vars(Keys, #kapps_call{}=Call) ->
    kapps_call_command:set(kz_json:from_list([{Key, <<>>} || Key <- Keys]), 'undefined', Call),
    handle_ccvs_remove(Keys, Call).

-spec handle_ccvs_remove(kz_json:keys(), call()) -> call().
handle_ccvs_remove(Keys, #kapps_call{ccvs=CCVs}=Call) ->
    lists:foldl(fun ccv_remove_fold/2
               ,Call#kapps_call{ccvs=kz_json:delete_keys(Keys, CCVs)}
               ,Keys
               ).

-spec ccv_remove_fold(kz_json:key(), call()) -> call().
ccv_remove_fold(Key, Call) ->
    case props:get_value(Key, ?SPECIAL_VARS) of
        'undefined' -> Call;
        Index -> setelement(Index, Call, 'undefined')
    end.

-spec set_custom_channel_var(kz_json:key(), kz_json:json_term(), call()) -> call().
-ifdef(TEST).
set_custom_channel_var(Key, Value, Call) ->
    insert_custom_channel_var(Key, Value, Call).
-else.
set_custom_channel_var(Key, Value, Call) ->
    kapps_call_command:set(kz_json:set_value(Key, Value, kz_json:new()), 'undefined', Call),
    insert_custom_channel_var(Key, Value, Call).
-endif.

-spec insert_custom_channel_var(kz_json:key(), kz_json:json_term(), call()) -> call().
insert_custom_channel_var(Key, Value, #kapps_call{ccvs=CCVs}=Call) ->
    handle_ccvs_update(kz_json:set_value(Key, Value, CCVs), Call).

-spec set_custom_channel_vars(kz_term:proplist(), call()) -> call().
-ifdef(TEST).
set_custom_channel_vars(Props, #kapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = kz_json:set_values(Props, CCVs),
    handle_ccvs_update(NewCCVs, Call).
-else.
set_custom_channel_vars(Props, #kapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = kz_json:set_values(Props, CCVs),

    maybe_update_call_ccvs(Call, Props, kz_json:to_proplist(CCVs)),

    handle_ccvs_update(NewCCVs, Call).

-spec maybe_update_call_ccvs(call(), kz_term:proplist(), kz_term:proplist()) -> 'ok'.
maybe_update_call_ccvs(Call, NewCCVs, ExistingCCVs) ->
    case updateable_ccvs(NewCCVs, ExistingCCVs) of
        [] -> 'ok';
        Updates -> kapps_call_command:set(kz_json:from_list(Updates), 'undefined', Call)
    end.

-endif.

-spec updateable_ccvs(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
updateable_ccvs(New, Existing) ->
    Exceptions = [<<"Privacy-Hide-Name">>
                 ,<<"Privacy-Hide-Number">>
                 ],
    New -- props:delete_keys(Exceptions, Existing).

-spec update_custom_channel_vars([fun((kz_json:object()) -> kz_json:object()),...], call()) -> call().
-ifdef(TEST).
update_custom_channel_vars(Updaters, #kapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = lists:foldr(fun(F, J) -> F(J) end, CCVs, Updaters),
    handle_ccvs_update(NewCCVs, Call).
-else.
update_custom_channel_vars(Updaters, #kapps_call{ccvs=CCVs}=Call) ->
    NewCCVs = lists:foldr(fun(F, J) -> F(J) end, CCVs, Updaters),
    kapps_call_command:set(NewCCVs, 'undefined', Call),
    handle_ccvs_update(NewCCVs, Call).
-endif.

-spec custom_channel_var(any(), Default, call()) -> Default | _.
custom_channel_var(Key, Default, #kapps_call{ccvs=CCVs}) ->
    kz_json:get_value(Key, CCVs, Default).

-spec custom_channel_var(any(), call()) -> any().
custom_channel_var(Key, #kapps_call{ccvs=CCVs}) ->
    kz_json:get_value(Key, CCVs).

-spec custom_channel_vars(call()) -> kz_json:object().
custom_channel_vars(#kapps_call{ccvs=CCVs}) ->
    CCVs.

-spec set_custom_application_var(kz_json:path(), kz_json:json_term(), call()) -> call().
set_custom_application_var(Key, Value, Call) ->
    set_custom_application_vars([{Key, Value}], Call).

-spec insert_custom_application_var(kz_json:path(), kz_json:json_term(), call()) -> call().
insert_custom_application_var(Key, Value, #kapps_call{cavs=CAVs}=Call) ->
    Call#kapps_call{cavs=kz_json:set_value(Key, Value, CAVs)}.

-spec set_custom_application_vars(kz_term:proplist(), call()) -> call().
set_custom_application_vars(Props, Call) ->
    set_custom_application_vars(Props, Call, 'false').

-spec set_custom_application_vars(kz_term:proplist(), call(), boolean()) -> call().
set_custom_application_vars(Props, #kapps_call{cavs=CAVs}=Call, Export) ->
    NewCAVs = kz_json:set_values(Props, CAVs),

    maybe_update_call_cavs(Call, Props, kz_json:to_proplist(CAVs), Export),

    Call#kapps_call{cavs=NewCAVs}.

-spec maybe_update_call_cavs(call(), kz_term:proplist(), kz_term:proplist(), boolean()) -> 'ok'.
maybe_update_call_cavs(Call, NewCAVs, ExistingCAVs, Export) ->
    %% Modeled after updateable_ccvs/2
    case NewCAVs -- ExistingCAVs of
        [] -> 'ok';
        Updates ->
            kapps_call_command:set('undefined', 'undefined', kz_json:from_list(Updates), Export, Call)
    end.

-spec custom_application_var(any(), Default, call()) -> Default | _.
custom_application_var(Key, Default, #kapps_call{cavs=CAVs}) ->
    kz_json:get_value(Key, CAVs, Default).

-spec custom_application_var(any(), call()) -> any().
custom_application_var(Key, #kapps_call{cavs=CAVs}) ->
    kz_json:get_value(Key, CAVs).

-spec custom_application_vars(call()) -> kz_json:object().
custom_application_vars(#kapps_call{cavs=CAVs}) ->
    CAVs.

-spec set_custom_sip_header(kz_json:path(), kz_json:json_term(), call()) -> call().
set_custom_sip_header(Key, Value, #kapps_call{sip_headers=SHs}=Call) ->
    Call#kapps_call{sip_headers=kz_json:set_value(Key, Value, SHs)}.

-spec custom_sip_header(kz_json:get_key(), call()) -> kz_json:api_json_term().
custom_sip_header(Key, #kapps_call{}=Call) ->
    custom_sip_header(Key, 'undefined', Call).

-spec custom_sip_header(kz_json:get_key(), Default, call()) -> kz_json:json_term() | Default.
custom_sip_header(Key, Default, #kapps_call{sip_headers=SHs}) ->
    kz_json:get_value(Key, SHs, Default).

-spec set_custom_sip_headers(kz_term:proplist(), call()) -> call().
set_custom_sip_headers(Headers, #kapps_call{sip_headers=SHs}=Call) ->
    Call#kapps_call{sip_headers=kz_json:set_values(Headers, SHs)}.

-spec custom_sip_headers(call()) -> kz_json:object().
custom_sip_headers(#kapps_call{sip_headers=SHs}) -> SHs.

-spec handle_ccvs_update(kz_json:object(), call()) -> call().
handle_ccvs_update(CCVs, #kapps_call{}=Call) ->
    lists:foldl(fun({Var, Index}, C) ->
                        case kz_json:get_ne_value(Var, CCVs) of
                            'undefined' -> C;
                            Value -> setelement(Index, C, Value)
                        end
                end
               ,Call#kapps_call{ccvs=CCVs}
               ,?SPECIAL_VARS
               ).

-spec set_custom_publish_function(kapps_custom_publish(), call()) -> call().
set_custom_publish_function(Fun, #kapps_call{}=Call) when is_function(Fun, 2) ->
    Call#kapps_call{custom_publish_fun=Fun}.

-spec clear_custom_publish_function(call()) -> call().
clear_custom_publish_function(#kapps_call{}=Call) ->
    Call#kapps_call{custom_publish_fun='undefined'}.

-spec custom_publish_function(call()) -> 'undefined' | kapps_custom_publish().
custom_publish_function(#kapps_call{custom_publish_fun=Fun}) -> Fun.

-spec kvs_append(any(), any(), call()) -> call().
kvs_append(Key, Value, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:append(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_append_list(any(), [any(),...], call()) -> call().
kvs_append_list(Key, ValList, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:append_list(kz_term:to_binary(Key), ValList, Dict)}.

-spec kvs_erase(any() | [any(),...], call()) -> call().
kvs_erase(Keys, #kapps_call{kvs=Dict}=Call) when is_list(Keys)->
    Call#kapps_call{kvs=erase_keys(Keys, Dict)};
kvs_erase(Key, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=erase_key(Key, Dict)}.

-spec erase_keys(list(), orddict:orddict()) -> orddict:orddict().
erase_keys(Keys, Dict) ->
    lists:foldl(fun erase_key/2, Dict, Keys).

-spec erase_key(any(), orddict:orddict()) -> orddict:orddict().
erase_key(K, D) -> orddict:erase(kz_term:to_binary(K), D).

-spec kvs_flush(call()) -> call().
kvs_flush(#kapps_call{}=Call) -> Call#kapps_call{kvs=orddict:new()}.

-spec kvs_fetch(any(), call()) -> any().
kvs_fetch(Key, Call) -> kvs_fetch(Key, 'undefined', Call).

-spec kvs_fetch(any(), Default, call()) -> any() | Default.
kvs_fetch(Key, Default, #kapps_call{kvs=Dict}) ->
    try orddict:fetch(kz_term:to_binary(Key), Dict)
    catch
        'error':'function_clause' -> Default
    end.

-spec kvs_fetch_keys(call()) -> [any(),...].
kvs_fetch_keys(#kapps_call{kvs=Dict}) -> orddict:fetch_keys(Dict).

-spec kvs_filter(fun((any(), any()) -> boolean()), call()) -> call().
kvs_filter(Pred, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:filter(Pred, Dict)}.

-spec kvs_find(any(), call()) -> {'ok', any()} | 'error'.
kvs_find(Key, #kapps_call{kvs=Dict}) ->
    orddict:find(kz_term:to_binary(Key), Dict).

-spec kvs_fold(fun((any(), any(), any()) -> any()), any(), call()) -> call().
kvs_fold(Fun, Acc0, #kapps_call{kvs=Dict}) -> orddict:fold(Fun, Acc0, Dict).

-spec kvs_from_proplist(kz_term:proplist(), call()) -> call().
kvs_from_proplist(List, #kapps_call{kvs=Dict}=Call) ->
    L = orddict:from_list([{kz_term:to_binary(K), V} || {K, V} <- List]),
    Call#kapps_call{kvs=orddict:merge(fun(_, V1, _) -> V1 end, L, Dict)}.

-spec kvs_is_key(any(), call()) -> boolean().
kvs_is_key(Key, #kapps_call{kvs=Dict}) ->
    orddict:is_key(kz_term:to_binary(Key), Dict).

-spec kvs_map(fun((any(), any()) -> any()), call()) -> call().
kvs_map(Pred, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:map(Pred, Dict)}.

-spec kvs_store(any(), any(), call()) -> call().
kvs_store(Key, Value, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:store(kz_term:to_binary(Key), Value, Dict)}.

-spec kvs_store_proplist(kz_term:proplist(), call()) -> call().
kvs_store_proplist(List, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=add_to_store(List, Dict)}.

add_to_store(List, Dict) ->
    lists:foldr(fun add_to_store_fold/2, Dict, List).

add_to_store_fold({K, V}, D) ->
    orddict:store(kz_term:to_binary(K), V, D).

-spec kvs_to_proplist(call()) -> kz_term:proplist().
kvs_to_proplist(#kapps_call{kvs=Dict}) ->
    orddict:to_list(Dict).

-spec kvs_update(any(), fun((any()) -> any()), call()) -> call().
kvs_update(Key, Fun, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:update(kz_term:to_binary(Key), Fun, Dict)}.

-spec kvs_update(any(), fun((any()) -> any()), any(), call()) -> call().
kvs_update(Key, Fun, Initial, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:update(kz_term:to_binary(Key), Fun, Initial, Dict)}.

-spec kvs_update_counter(any(), number(), call()) -> call().
kvs_update_counter(Key, Number, #kapps_call{kvs=Dict}=Call) ->
    Call#kapps_call{kvs=orddict:update_counter(kz_term:to_binary(Key), Number, Dict)}.

-spec set_dtmf_collection(kz_term:api_binary(), call()) -> call().
set_dtmf_collection(DTMF, Call) ->
    set_dtmf_collection(DTMF, <<"default">>, Call).

-spec set_dtmf_collection(kz_term:api_binary(), kz_term:ne_binary(), call()) -> call().
set_dtmf_collection('undefined', Collection, Call) ->
    Collections = kvs_fetch(<<"dtmf_collections">>, kz_json:new(), Call),
    kvs_store(<<"dtmf_collections">>
             ,kz_json:delete_key(Collection, Collections)
             ,Call
             );
set_dtmf_collection(DTMF, Collection, Call) ->
    Collections = kvs_fetch(<<"dtmf_collections">>, kz_json:new(), Call),
    kvs_store(<<"dtmf_collections">>
             ,kz_json:set_value(Collection, DTMF, Collections)
             ,Call
             ).

-spec get_dtmf_collection(call()) -> kz_term:api_binary().
get_dtmf_collection(Call) ->
    get_dtmf_collection(<<"default">>, Call).

-spec get_dtmf_collection(kz_term:ne_binary(), call()) -> kz_term:api_binary().
get_dtmf_collection(Collection, Call) ->
    kz_json:get_value(Collection, kvs_fetch(<<"dtmf_collections">>, kz_json:new(), Call)).

-spec add_to_dtmf_collection(kz_term:ne_binary(), call()) -> call().
add_to_dtmf_collection(DTMF, Call) ->
    add_to_dtmf_collection(DTMF, <<"default">>, Call).

-spec add_to_dtmf_collection(kz_term:ne_binary(), kz_term:ne_binary(), call()) -> call().
add_to_dtmf_collection(DTMF, Collection, Call) ->
    case get_dtmf_collection(Collection, Call) of
        'undefined' -> set_dtmf_collection(DTMF, Collection, Call);
        Collected -> set_dtmf_collection(<<Collected/binary, DTMF/binary>>, Call)
    end.

-spec flush() -> 'ok'.
flush() ->
    kz_cache:flush_local(?KAPPS_CALL_CACHE).

-spec cache(call()) -> 'ok'.
cache(Call) ->
    cache(Call, 'undefined', 5 * ?SECONDS_IN_MINUTE).

-spec cache(call(), kz_term:api_binary()) -> 'ok'.
cache(Call, AppName) ->
    cache(Call, AppName, 5 * ?SECONDS_IN_MINUTE).

-spec cache(call(), kz_term:api_binary(), pos_integer()) -> 'ok'.
cache(#kapps_call{call_id=CallId}=Call, AppName, Expires) ->
    CacheProps = [{'expires', Expires}],
    kz_cache:store_local(?KAPPS_CALL_CACHE, {?MODULE, 'call', AppName, CallId}, Call, CacheProps).

-spec retrieve(kz_term:ne_binary()) ->
                      {'ok', call()} |
                      {'error', 'not_found'}.
retrieve(CallId) ->
    retrieve(CallId, 'undefined').

-spec retrieve(kz_term:ne_binary(), kz_term:api_binary()) ->
                      {'ok', call()} |
                      {'error', 'not_found'}.
retrieve(CallId, AppName) ->
    kz_cache:fetch_local(?KAPPS_CALL_CACHE, {?MODULE, 'call', AppName, CallId}).

-define(RECORDING_ID_KEY, <<"media_name">>).
-define(RECORDINGS_KEY, <<"recordings">>).

-spec start_recording(call()) -> call().
start_recording(Call) ->
    start_recording(kz_json:new(), Call).

-spec start_recording(kz_term:api_object(), call()) -> call().
start_recording('undefined', Call) -> Call;
start_recording(Data0, Call) ->
    Data = update_recording_id(Data0),
    Command = kapps_call_recording:record_call_command(Data, Call),
    RecordOnAnswer = kz_json:is_true(<<"record_on_answer">>, Data, 'false'),
    RecordOnBridge = kz_json:is_true(<<"record_on_bridge">>, Data, 'false'),
    Cmd = case {RecordOnAnswer, RecordOnBridge} of
              {'false', 'false'} ->
                  Command;
              {'true', _} ->
                  Actions = kz_json:set_value([<<"Execute-On-Answer">>, <<"Record-Call">>], Command, kz_json:new()),
                  kapps_call_command:event_actions_command(Actions, Call);
              {_, 'true'} ->
                  Actions = kz_json:set_value([<<"Execute-On-Bridge">>, <<"Record-Call">>], Command, kz_json:new()),
                  kapps_call_command:event_actions_command(Actions, Call)
          end,
    kapps_call_command:send_command(Cmd, Call),
    Routines = [{fun store_recording/2
                ,kz_json:get_ne_binary_value(?RECORDING_ID_KEY, Data)
                }
               ],
    exec(Routines, Call).

-spec update_recording_id(kz_json:object()) -> kz_json:object().
update_recording_id(Data) ->
    RecID = kz_binary:rand_hex(16),
    Format = kapps_call_recording:get_format(kz_json:get_ne_binary_value(<<"format">>, Data)),
    DefaultMediaName = kapps_call_recording:get_media_name(RecID, Format),
    MediaName = kz_json:get_ne_binary_value(?RECORDING_ID_KEY, Data, DefaultMediaName),
    kz_json:set_value(?RECORDING_ID_KEY, MediaName, Data).

-spec stop_recording(call()) -> call().
stop_recording(OriginalCall) ->
    stop_recording(call_id(OriginalCall), OriginalCall).

-spec stop_recording(kz_term:ne_binary(), call()) -> call().
stop_recording(LegId, OriginalCall) ->
    case LegId =:= call_id(OriginalCall)
        andalso retrieve_recording(OriginalCall) of
        'false' -> %% requested stop recording on b-leg
            API = [{<<"Call-ID">>, LegId}],
            kapps_call_command:stop_record_call(API, OriginalCall),
            OriginalCall;
        {'ok', MediaName, Call} ->
            kapps_call_command:stop_record_call([{<<"Media-Name">>, MediaName}], Call),
            Call;
        {'empty', Call} ->
            MediaName = custom_channel_var(<<"Media-Name">>, Call),
            API = props:filter_undefined([{<<"Media-Name">>, MediaName}]),
            kapps_call_command:stop_record_call(API, Call),
            Call
    end.

-spec mask_recording(call()) -> call().
mask_recording(OriginalCall) ->
    mask_recording(call_id(OriginalCall), OriginalCall).

-spec mask_recording(kz_term:ne_binary(), call()) -> call().
mask_recording(LegId, OriginalCall) ->
    mask_recording(LegId, OriginalCall, call_id(OriginalCall)).

mask_recording(LegId, OriginalCall, LegId) ->
    case retrieve_recording(OriginalCall) of
        {'ok', MediaName, Call} ->
            kapps_call_command:mask_record_call([{<<"Media-Name">>, MediaName}], Call),
            Call;
        {'empty', Call} ->
            MediaName = custom_channel_var(<<"Media-Name">>, Call),
            API = props:filter_undefined([{<<"Media-Name">>, MediaName}]),
            kapps_call_command:mask_record_call(API, Call),
            Call
    end;
mask_recording(LegId, OriginalCall, _OriginalCallId) ->
    API = [{<<"Call-ID">>, LegId}],
    kapps_call_command:mask_record_call(API, OriginalCall),
    OriginalCall.

-spec unmask_recording(call()) -> call().
unmask_recording(OriginalCall) ->
    unmask_recording(call_id(OriginalCall), OriginalCall).

-spec unmask_recording(kz_term:ne_binary(), call()) -> call().
unmask_recording(LegId, OriginalCall) ->
    unmask_recording(LegId, OriginalCall, call_id(OriginalCall)).

unmask_recording(LegId, OriginalCall, LegId) ->
    case retrieve_recording(OriginalCall) of
        {'ok', MediaName, Call} ->
            kapps_call_command:unmask_record_call([{<<"Media-Name">>, MediaName}], Call),
            Call;
        {'empty', Call} ->
            MediaName = custom_channel_var(<<"Media-Name">>, Call),
            API = props:filter_undefined([{<<"Media-Name">>, MediaName}]),
            kapps_call_command:unmask_record_call(API, Call),
            Call
    end;
unmask_recording(LegId, OriginalCall, _OriginalCallId) ->
    API = [{<<"Call-ID">>, LegId}],
    kapps_call_command:unmask_record_call(API, OriginalCall),
    OriginalCall.

-spec store_recording(kz_term:ne_binary(), call()) -> call().
store_recording(MediaName, Call) ->
    Q = queue:in(MediaName, get_recordings(Call)),
    kvs_store(?RECORDINGS_KEY, Q, Call).

-type recording_ref() :: kz_term:ne_binary().
-type store_return() :: {'ok', recording_ref(), call()} | {'empty', call()}.

-spec retrieve_recording(call()) -> store_return().
retrieve_recording(Call) ->
    case queue:out_r(get_recordings(Call)) of
        {{'value', MediaRef}, Q} ->
            Routines = [{fun kvs_store/3, ?RECORDINGS_KEY, Q}],
            {'ok', MediaRef, exec(Routines, Call)};
        {'empty', _} ->
            {'empty', Call}
    end.

-spec get_recordings(call()) -> queue:queue().
get_recordings(Call) ->
    case kvs_fetch(?RECORDINGS_KEY, Call) of
        'undefined' -> queue:new();
        Q -> Q
    end.

-spec inception_type(call()) -> kz_term:api_binary().
inception_type(#kapps_call{inception='undefined'}) -> <<"onnet">>;
inception_type(#kapps_call{}) -> <<"offnet">>.

-spec is_inter_account(call()) -> boolean().
is_inter_account(#kapps_call{}=Call) ->
    inter_account_id(Call) /= 'undefined'.

-spec inter_account_id(call()) -> kz_term:api_binary().
inter_account_id(#kapps_call{}=Call) ->
    custom_channel_var(<<"Inception-Account-ID">>, Call).

-spec set_is_recording(boolean(), call()) -> call().
set_is_recording(IsRecording, #kapps_call{}=Call) ->
    Call#kapps_call{is_recording=IsRecording}.

-spec is_recording(call()) -> boolean().
is_recording(#kapps_call{is_recording=IsRecording}) ->
    IsRecording.

-spec is_call_forward(call()) -> boolean().
is_call_forward(#kapps_call{is_call_forward=IsCallForward}) ->
    IsCallForward.

-ifdef(TEST).
eq(Call, Call1) ->
    eq(tuple_to_list(Call), tuple_to_list(Call1), 1).
eq([], [], _) -> 'true';
eq([V|C1], [V|C2], Pos) ->
    eq(C1, C2, Pos+1);
eq([V1|C1], [V2|C2], #kapps_call.ccvs=Pos) ->
    case kz_json:are_equal(V1, V2) of
        'true' -> eq(C1, C2, Pos+1);
        'false' ->
            ?debugFmt("CCVs vary:~n~p~n~p~n", [V1, V2]),
            'false'
    end;
eq([V1|_], [V2|_], Pos) ->
    ?debugFmt("at ~p, vary:~n~p~n~p~n", [Pos, V1, V2]),
    'false'.
-endif.
