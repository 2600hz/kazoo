-ifndef(KZ_WEB_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(APP_NAME, <<"kazoo_web">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(HTTP_OPTIONS, ['autoredirect'
                      ,'connect_timeout'
                      ,'essl'
                      ,'proxy_auth'
                      ,'relaxed'
                      ,'ssl'
                      ,'timeout'
                      ,'url_encode'
                      ,'version'
                      ]).

-define(OPTIONS, ['body_format'
                 ,'full_result'
                 ,'headers_as_is'
                 ,'ipv6_host_with_brackets'
                 ,'receiver'
                 ,'socket_opts'
                 ,'stream'
                 ,'sync'
                 ]).

-define(KZ_WEB_HRL, 'true').
-endif.
