-ifndef(KNM_PORT_REQUEST_HRL).

-define(PORT_UNCONFIRMED, <<"unconfirmed">>).
-define(PORT_WAITING, ?PORT_UNCONFIRMED).
-define(PORT_SUBMITTED, <<"submitted">>).
-define(PORT_PENDING, <<"pending">>).
-define(PORT_SCHEDULED, <<"scheduled">>).
-define(PORT_COMPLETE, <<"completed">>).
-define(PORT_REJECT, <<"rejected">>).
-define(PORT_CANCELED, <<"canceled">>).
-define(PORT_ATTACHMENT, <<"attachments">>).
-define(PORT_DESCENDANTS, <<"descendants">>).
-define(PORT_PVT_STATE, <<"pvt_port_state">>).
-define(PVT_SENT, <<"pvt_sent">>).

-define(PORT_STATES,
        [?PORT_UNCONFIRMED
         ,?PORT_SUBMITTED
         ,?PORT_PENDING
         ,?PORT_SCHEDULED
         ,?PORT_COMPLETE
         ,?PORT_REJECT
         ,?PORT_CANCELED
        ]
       ).

-define(KNM_PORT_REQUEST_HRL, 'true').
-endif.
