{application, whistle
 ,[
   {description, "Whistle Helpers"}
   ,{vsn, "1.0.0"}
   ,{modules, [gen_listener, kz_token_bucket, lager_kazoo_formatter, listener_utils, perf, props, wh_api, wh_cache, wh_call_response, whistle_maintenance, wh_json, wh_json_validator, wh_json_validator_tests, wh_mime_types, wh_network_utils, wh_nodes, wh_notify, wh_util,wapi_asr, wapi_authn, wapi_authz, wapi_call, wapi_conference, wapi_conf, wapi_dialplan, wapi_fs, wapi_media, wapi_money, wapi_nodes, wapi_notifications, wapi_offnet_resource, wapi_rate, wapi_registration, wapi_resource, wapi_route, wapi_self, wapi_switch, wapi_sysconf]}
   ,{registered, []}
   ,{applications
     ,[
       kernel
       ,stdlib
       ,crypto
       ,sasl
      ]}
   ,{mod, { whistle_app
            ,[

             ]
          }
    }
   ,{env, []}
 ]}.
