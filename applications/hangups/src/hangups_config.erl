-module(hangups_config).

-export([monitored_hangup_causes/0
        ,ignored_hangup_causes/0
        ]).

-include("hangups.hrl").

-spec monitored_hangup_causes() -> ne_binaries().
monitored_hangup_causes() ->
    kapps_config:get(?APP_NAME
                    ,<<"hangups_to_monitor">>
                    ,[<<"CALL_REJECTED">>
                     ,<<"MANDATORY_IE_MISSING">>
                     ,<<"NO_ROUTE_DESTINATION">>
                     ,<<"PROGRESS_TIMEOUT">>
                     ,<<"RECOVERY_ON_TIMER_EXPIRE">>
                     ,<<"WRONG_CALL_STATE">>
                     ]).

-spec ignored_hangup_causes() -> ne_binaries().
ignored_hangup_causes() ->
    kapps_config:get(?APP_NAME
                    ,<<"ignore_hangup_causes">>
                    ,[<<"ALLOTTED_TIMEOUT">>
                     ,<<"ATTENDED_TRANSFER">>
                     ,<<"LOSE_RACE">>
                     ,<<"NORMAL_CLEARING">>
                     ,<<"NO_ANSWER">>
                     ,<<"NO_USER_RESPONSE">>
                     ,<<"ORIGINATOR_CANCEL">>
                     ,<<"USER_BUSY">>
                     ,<<"PICKED_OFF">>
                     ]
                    ).
