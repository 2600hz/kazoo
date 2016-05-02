-ifndef(KAPI_OFFNET_RESOURCE_HRL).

-include_lib("kazoo/include/kz_api.hrl").

-define(KEY_ACCOUNT_ID, <<"Account-ID">>).
-define(KEY_ACCOUNT_REALM, <<"Account-Realm">>).
-define(KEY_APPLICATION_DATA, <<"Application-Data">>).
-define(KEY_APPLICATION_NAME, <<"Application-Name">>).
-define(KEY_BODY, <<"Body">>).
-define(KEY_BYPASS_E164, <<"Bypass-E164">>).
-define(KEY_B_LEG_EVENTS, <<"B-Leg-Events">>).
-define(KEY_CALL_ID, <<"Call-ID">>).
-define(KEY_CCVS, <<"Custom-Channel-Vars">>).
-define(KEY_CONTROL_QUEUE, <<"Control-Queue">>).
-define(KEY_CSHS, <<"Custom-SIP-Headers">>).
-define(KEY_ENABLE_T38 ,<<"Enable-T38-Fax">>).
-define(KEY_ENABLE_T38_GATEWAY, <<"Enable-T38-Gateway">>).
-define(KEY_ENABLE_T38_PASSTHROUGH, <<"Enable-T38-Passthrough">>).
-define(KEY_ENABLE_T38_REQUEST, <<"Enable-T38-Fax-Request">>).
-define(KEY_E_CALLER_ID_NAME, <<"Emergency-Caller-ID-Name">>).
-define(KEY_E_CALLER_ID_NUMBER, <<"Emergency-Caller-ID-Number">>).
-define(KEY_FAX_IDENTITY_NAME, <<"Fax-Identity-Name">>).
-define(KEY_FAX_IDENTITY_NUMBER, <<"Fax-Identity-Number">>).
-define(KEY_FAX_TIMEZONE, <<"Fax-Timezone">>).
-define(KEY_FLAGS, <<"Flags">>).
-define(KEY_FORCE_FAX, <<"Force-Fax">>).
-define(KEY_FORCE_OUTBOUND, <<"Force-Outbound">>).
-define(KEY_FORMAT_FROM_URI, <<"Format-From-URI">>).
-define(KEY_FROM_URI_REALM, <<"From-URI-Realm">>).
-define(KEY_GROUP_ID, <<"Group-ID">>).
-define(KEY_HOLD_MEDIA, <<"Hold-Media">>).
-define(KEY_HUNT_ACCOUNT_ID, <<"Hunt-Account-ID">>).
-define(KEY_IGNORE_EARLY_MEDIA, <<"Ignore-Early-Media">>).
-define(KEY_INCEPTION, <<"Inception">>).
-define(KEY_MEDIA, <<"Media">>).
-define(KEY_MESSAGE_ID, <<"Message-ID">>).
-define(KEY_MODE, <<"Mode">>).
-define(KEY_OUTBOUND_CALLEE_ID_NAME, <<"Outbound-Callee-ID-Name">>).
-define(KEY_OUTBOUND_CALLEE_ID_NUMBER, <<"Outbound-Callee-ID-Number">>).
-define(KEY_OUTBOUND_CALLER_ID_NAME, <<"Outbound-Caller-ID-Name">>).
-define(KEY_OUTBOUND_CALLER_ID_NUMBER, <<"Outbound-Caller-ID-Number">>).
-define(KEY_OUTBOUND_CALL_ID, <<"Outbound-Call-ID">>).
-define(KEY_PRESENCE_ID, <<"Presence-ID">>).
-define(KEY_RESOURCE_TYPE, <<"Resource-Type">>).
-define(KEY_RINGBACK, <<"Ringback">>).
-define(KEY_T38_ENABLED, <<"Fax-T38-Enabled">>).
-define(KEY_TIMEOUT, <<"Timeout">>).
-define(KEY_TO_DID, <<"To-DID">>).

-define(RESOURCE_TYPE_AUDIO, <<"audio">>).
-define(RESOURCE_TYPE_ORIGINATE, <<"originate">>).
-define(RESOURCE_TYPE_SMS, <<"sms">>).
-define(RESOURCE_TYPE_VIDEO, <<"video">>).

-define(APPLICATION_BRIDGE, <<"bridge">>).
-define(APPLICATION_EAVESDROP, <<"eavesdrop">>).
-define(APPLICATION_FAX, <<"fax">>).
-define(APPLICATION_PARK, <<"park">>).
-define(APPLICATION_SMS, <<"sms">>).
-define(APPLICATION_TRANSFER, <<"transfer">>).

-define(MEDIA_AUTO, <<"auto">>).
-define(MEDIA_BYPASS, <<"bypass">>).
-define(MEDIA_PROCESS, <<"process">>).

-define(MODE_FULL, <<"full">>).
-define(MODE_LISTEN, <<"listen">>).
-define(MODE_WHISPER, <<"whisper">>).

-define(CATEGORY_REQ, <<"resource">>).
-define(EVENT_REQ, <<"offnet_req">>).

-define(KAPI_OFFNET_REQUEST_HRL, 'true').
-endif.
