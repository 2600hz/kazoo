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

-define(PORT_PVT_ACCOUNT_DB, <<"pvt_account_db">>).
-define(PORT_PVT_ACCOUNT_ID, <<"pvt_account_id">>).
-define(PORT_PVT_CREATED, <<"pvt_created">>).
-define(PORT_PVT_ID, <<"_id">>).
-define(PORT_PVT_MODIFIED, <<"pvt_modified">>).
-define(PORT_PVT_REV, <<"_rev">>).
-define(PORT_PVT_SENT, <<"pvt_sent">>).
-define(PORT_PVT_STATE, <<"pvt_port_state">>).
-define(PORT_PVT_TRANSITIONS, <<"pvt_transitions">>).
-define(PORT_PVT_TREE, <<"pvt_tree">>).
-define(PORT_PVT_TYPE, <<"pvt_type">>).
-define(PORT_PVT_VSN, <<"pvt_vsn">>).

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
