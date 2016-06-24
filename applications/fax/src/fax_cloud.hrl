-ifndef(FAX_CLOUD_HRL).

-include_lib("fax/src/fax.hrl").

-include_lib("kazoo_oauth/include/kazoo_oauth_types.hrl").

-define(GPC_URL, "https://www.google.com/cloudprint/").
-define(GPC_URL_REGISTER, <<?GPC_URL,"register">>).
-define(GPC_PROXY_HEADER,{"X-CloudPrint-Proxy","kazoo-cloud-fax-printer-proxy"}).

-define(GCP_SCOPE,<<"https://www.googleapis.com/auth/cloudprint">>).

-define(POOL_URL,"https://www.google.com/cloudprint/fetch?printerid=").
-define(TICKET_URL,"https://www.google.com/cloudprint/ticket?use_cjt=true&jobid=").
-define(JOBCTL_URL,"https://www.google.com/cloudprint/control").

-define(MULTIPART_BOUNDARY,<<"------a450glvjfEoqerAc1p431paQlfDac152cadADfd">>).


-define(FAX_CLOUD_HRL, 'true').
-endif.
