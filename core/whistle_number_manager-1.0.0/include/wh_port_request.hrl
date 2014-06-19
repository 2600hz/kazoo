-ifndef(WH_PORT_REQUEST_HRL).

-define(PORT_WAITING, <<"unconfirmed">>).
-define(PORT_READY, <<"submitted">>).
-define(PORT_PROGRESS, <<"scheduled">>).
-define(PORT_COMPLETE, <<"completed">>).
-define(PORT_REJECT, <<"rejected">>).
-define(PORT_ATTACHMENT, <<"attachments">>).
-define(PORT_DESCENDANTS, <<"descendants">>).
-define(PORT_PVT_STATE, <<"pvt_port_state">>).
-define(PORT_STATE, <<"port_state">>).

-define(WH_PORT_REQUEST_HRL, 'true').
-endif.
