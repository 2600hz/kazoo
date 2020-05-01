-ifndef(KNM_PORT_REQUEST_HRL).

-define(TYPE_PORT_REQUEST, <<"port_request">>).

-define(PORT_ATTACHMENT, <<"attachments">>).
-define(PORT_CANCELED, <<"canceled">>).
-define(PORT_COMPLETED, <<"completed">>).
-define(PORT_DESCENDANTS, <<"descendants">>).
-define(PORT_PENDING, <<"pending">>).
-define(PORT_REJECTED, <<"rejected">>).
-define(PORT_SCHEDULED, <<"scheduled">>).
-define(PORT_SUBMITTED, <<"submitted">>).
-define(PORT_UNCONFIRMED, <<"unconfirmed">>).
-define(PORT_WAITING, ?PORT_UNCONFIRMED).

-define(PORT_TRANSITION, <<"transition">>).

-define(TRANSITION_REASON, <<"reason">>).
-define(TRANSITION_TIMESTAMP, <<"timestamp">>).
-define(TRANSITION_TYPE, <<"type">>).

-define(PORT_STATES, [?PORT_UNCONFIRMED
                     ,?PORT_SUBMITTED
                     ,?PORT_PENDING
                     ,?PORT_SCHEDULED
                     ,?PORT_COMPLETED
                     ,?PORT_REJECTED
                     ,?PORT_CANCELED
                     ]).

-define(PORT_ACTIVE_STATES, [?PORT_UNCONFIRMED
                            ,?PORT_SUBMITTED
                            ,?PORT_PENDING
                            ,?PORT_SCHEDULED
                            ,?PORT_REJECTED
                            ]).

-define(PORT_PROGRESSING_STATES, [?PORT_SUBMITTED
                                 ,?PORT_PENDING
                                 ,?PORT_SCHEDULED
                                 ]).

-define(PORT_SUSPENDED_STATES, [?PORT_UNCONFIRMED
                               ,?PORT_REJECTED
                               ]).

-define(PORT_COMPLETED_STATES, [?PORT_COMPLETED
                               ,?PORT_CANCELED
                               ]).

-define(KNM_PORT_REQUEST_HRL, 'true').
-endif.
