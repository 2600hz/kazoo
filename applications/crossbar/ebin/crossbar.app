{application, crossbar,
 [
  {description, "Crossbar - REST Interface to the stars"}
  ,{vsn, "0.3.5"}
  ,{modules, [cb_context, cb_test, crossbar_app, crossbar_bindings, crossbar_cleanup, crossbar_default_handler, crossbar_doc, crossbar, crossbar_maintenance, crossbar_module_sup, crossbar_sup, crossbar_util, plists, v1_resource, v1_util,cb_about, cb_accounts, cb_acls, cb_agents, cb_api_auth, cb_braintree, cb_buckets_ets, cb_buckets_mgr, cb_buckets_sup, cb_bulk, cb_callflows, cb_cdrs, cb_clicktocall, cb_conferences, cb_configs, cb_connectivity, cb_contact_list, cb_devices, cb_directories, cb_events, cb_events_srv, cb_events_sup, cb_faxes, cb_global_provisioner_templates, cb_global_resources, cb_groups, cb_hotdesks, cb_ip_auth, cb_killio, cb_kz_buckets_sup, cb_limits, cb_local_provisioner_templates, cb_local_resources, cb_media, cb_menus, cb_modules_util, cb_noauthn, cb_noauthz, cb_onboard, cb_phone_numbers, cb_queues, cb_rates, cb_registrations, cb_schemas, cb_servers, cb_service_plans, cb_services, cb_shared_auth, cb_signup, cb_simple_authz, cb_skels, cb_templates, cb_temporal_rules, cb_token_auth, cb_transactions, cb_user_auth, cb_users, cb_vmboxes, cb_webhooks, cb_whitelabel, provisioner_contact_list, provisioner_util]}
  ,{registered, []}
  ,{applications, [
                   kernel
                   ,stdlib
                   ,crypto
                  ]}
  ,{mod, { crossbar_app, []} }
  ,{env, []}
 ]}.
