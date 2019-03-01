-ifndef(KZ_ATT_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(CONFIG_CAT, <<"attachments">>).

%% {Db, DocId, AttachmentName}
-type attachment_info() :: {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}.

-type url_field() :: {'arg', kz_term:ne_binary()} |
                     {'field', kz_term:ne_binary()} |
                     {'const', kz_term:ne_binary()} |
                     {'group', url_fields()}.
-type url_fields() :: [url_field()].

-define(KZ_ATT_HRL, 'true').
-endif.
