-ifndef(KAZOO_RECORDS_INCLUDED).

-record(whapp_info, {startup :: kz_time:api_seconds()
                    ,roles = [] :: kz_term:ne_binaries()
                    }).

-record(kz_node, {node = node() :: atom() | '$1' | '$2' | '_'
                 ,md5 :: kz_term:api_binary() | '_'
                 ,expires = 0 :: non_neg_integer() | 'undefined' | '$2' | '_'
                 ,kapps = [] :: kz_types:kapps_info() | '$1' | '_'
                 ,media_servers = [] :: kz_types:media_servers() | '_'
                 ,last_heartbeat = kz_time:now_ms() :: pos_integer() | 'undefined' | '$3' | '_'
                 ,zone :: atom() | 'undefined' | '$2' | '_'
                 ,broker :: kz_term:api_binary() | '_'
                 ,used_memory = 0 :: non_neg_integer() | '_'
                 ,processes = 0 :: non_neg_integer() | '_'
                 ,ports = 0 :: non_neg_integer() | '_'
                 ,version :: kz_term:api_binary() | '_'
                 ,channels = 0 :: non_neg_integer() | '_'
                 ,conferences = 0 :: non_neg_integer() | '_'
                 ,registrations = 0 :: non_neg_integer() | '_'
                 ,globals = [] :: kz_term:proplist() | '$1' | '_'
                 ,node_info :: kz_term:api_object() | '_'
                 ,runtime :: kz_term:api_object() | '_'
                 ,modules :: kz_term:api_object() | '_'
                 ,roles = [] :: kz_term:proplist() | '$1' | '_'
                 }).

-define(KAZOO_RECORDS_INCLUDED, 'true').
-endif.
