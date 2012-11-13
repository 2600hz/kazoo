-ifndef(KZT_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

-type ok_return() :: {'ok', whapps_call:call()}.
-type stop_return() :: {'stop', whapps_call:call()}.

%% {request, Call, URI, Method, BaseParams}
-type request_return() :: {'request', whapps_call:call(), ne_binary(), 'get' | 'post', api_terms()}.

-type exec_element_return() ::
        ok_return() |
        stop_return() |
        request_return().

-type exec_return() ::
        {'stop', whapps_call:call()} |
        {'request', whapps_call:call(), ne_binary(), 'get' | 'post', api_terms()}.

-define(KZT_USER_VARS, <<"user_vars">>).

-define(STATUS_QUEUED, <<"queued">>).
-define(STATUS_RINGING, <<"ringing">>).
-define(STATUS_ANSWERED, <<"in-progress">>).
-define(STATUS_COMPLETED, <<"completed">>).
-define(STATUS_BUSY, <<"busy">>).
-define(STATUS_FAILED, <<"failed">>).
-define(STATUS_NOANSWER, <<"no-answer">>).
-define(STATUS_CANCELED, <<"canceled">>).

-define(APP_NAME, <<"translator">>).
-define(APP_VERSION, <<"0.4.0">>).

-define(KZ_HRL, true).
-endif.
