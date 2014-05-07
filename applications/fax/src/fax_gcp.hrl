-ifndef(FAX_GCP_HRL).

-include("fax.hrl").
-include_lib("kazoo_oauth/include/kazoo_oauth_types.hrl").

-define(GPC_URL, "https://www.google.com/cloudprint/").
-define(GPC_URL_REGISTER, <<?GPC_URL,"register">>).
-define(GPC_PROXY_HEADER,{"X-CloudPrint-Proxy","kazoo-cloud-fax-printer-proxy"}).
-define(GPC_SCOPE,"https://www.googleapis.com/auth/cloudprint").

-define(POOL_URL,"https://www.google.com/cloudprint/fetch?printerid=").
-define(TICKET_URL,"https://www.google.com/cloudprint/ticket?use_cjt=true&jobid=").
-define(JOBCTL_URL,"https://www.google.com/cloudprint/control").



-define(FAX_GCP_HRL, 'true').
-endif.
