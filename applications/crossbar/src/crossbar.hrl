-ifndef(CROSSBAR_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").
-include_lib("kazoo_documents/include/kazoo_documents.hrl").

-include("crossbar_types.hrl").

-define(APP, crossbar).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).
-define(AUTH_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".auth">>).

-define(CACHE_NAME, 'crossbar_cache').

-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).

-define(LIST_BY_USERNAME, <<"users/list_by_username">>).

-define(CB_APPS_STORE_LIST, <<"apps_store/crossbar_listing">>).

-define(INBOUND_HOOK, <<"hooks">>).

-define(NO_ENVELOPE_VERSIONS, [?INBOUND_HOOK]).
-define(INBOUND_HOOKS, [?INBOUND_HOOK]).

-define(CACHE_TTL, kapps_config:get_integer(?CONFIG_CAT, <<"cache_ttl">>, 300)).

-define(CROSSBAR_DEFAULT_CONTENT_TYPE, {<<"application">>, <<"json">>, '*'}).

-define(CB_ACCOUNT_TOKEN_RESTRICTIONS, <<"token_restrictions">>).

-define(CONTENT_PROVIDED, [{'to_json', ?JSON_CONTENT_TYPES}]).
-define(CONTENT_ACCEPTED, [{'from_json', ?JSON_CONTENT_TYPES}
                          ,{'from_form', ?MULTIPART_CONTENT_TYPES}
                          ,{'from_binary', ?CSV_CONTENT_TYPES}
                          ]).
-define(ALLOWED_METHODS, [?HTTP_GET
                         ,?HTTP_POST
                         ,?HTTP_PUT
                         ,?HTTP_DELETE
                         ,?HTTP_HEAD
                         ,?HTTP_PATCH
                         ,?HTTP_OPTIONS
                         ]).

-define(QUICKCALL_PATH_TOKEN, <<"quickcall">>).
-define(DEVICES_QCALL_NOUNS(DeviceId, Number)
       ,[{<<"quickcall">>, [Number]}
        ,{<<"devices">>, [DeviceId]}
        ,{?KZ_ACCOUNTS_DB, [_AccountId]}
        ]).
-define(USERS_QCALL_NOUNS(UserId, Number)
       ,[{<<"quickcall">>, [Number]}
        ,{<<"users">>, [UserId]}
        ,{?KZ_ACCOUNTS_DB, [_AccountId]}
        ]).

-define(DEVICES_ALLOW_AGGREGATES
       ,kapps_config:get_is_true(<<(?CONFIG_CAT)/binary, ".devices">>, <<"allow_aggregates">>, 'true')
       ).

-define(DEFAULT_MODULES, ['cb_about'
                         ,'cb_accounts'
                         ,'cb_alerts'
                         ,'cb_api_auth'
                         ,'cb_apps_store'
                         ,'cb_auth'
                         ,'cb_basic_auth'
                         ,'cb_blacklists'
                         ,'cb_callflows'
                         ,'cb_cdrs'
                         ,'cb_channels'
                         ,'cb_clicktocall'
                         ,'cb_comments'
                         ,'cb_conferences'
                         ,'cb_configs'
                         ,'cb_connectivity'
                         ,'cb_contact_list'
                         ,'cb_devices'
                         ,'cb_directories'
                         ,'cb_faxboxes'
                         ,'cb_faxes'
                         ,'cb_groups'
                         ,'cb_hotdesks'
                         ,'cb_ips'
                         ,'cb_ledgers'
                         ,'cb_limits'
                         ,'cb_media'
                         ,'cb_menus'
                         ,'cb_metaflows'
                         ,'cb_multi_factor'
                         ,'cb_notifications'
                         ,'cb_pivot'
                         ,'cb_phone_numbers'
                         ,'cb_port_requests'
                         ,'cb_presence'
                         ,'cb_quickcall'
                         ,'cb_rates'
                         ,'cb_registrations'
                         ,'cb_resource_templates'
                         ,'cb_resources'
                         ,'cb_schemas'
                         ,'cb_search'
                         ,'cb_security'
                         ,'cb_service_plans'
                         ,'cb_services'
                         ,'cb_simple_authz'
                         ,'cb_sms'
                         ,'cb_system_configs'
                         ,'cb_tasks'
                         ,'cb_templates'
                         ,'cb_temporal_rules'
                         ,'cb_temporal_rules_sets'
                         ,'cb_token_auth'
                         ,'cb_token_restrictions'
                         ,'cb_transactions'
                         ,'cb_user_auth'
                         ,'cb_users'
                         ,'cb_vmboxes'
                         ,'cb_webhooks'
                         ,'cb_websockets'
                         ,'cb_whitelabel'
                         ,'cb_parked_calls'
                         ]).

-define(DEPRECATED_MODULES, ['cb_local_resources'
                            ,'cb_global_resources'
                            ,'cb_signup'
                            ]).

-record(cb_context, {content_types_provided = [] :: crossbar_content_handlers()
                    ,content_types_accepted = [] :: crossbar_content_handlers()
                    ,allowed_methods = ?ALLOWED_METHODS :: http_methods()
                    ,allow_methods = ?ALLOWED_METHODS :: http_methods()
                    ,languages_provided = [<<"en">>, <<"en-us">>, <<"en-gb">>] :: kz_term:ne_binaries() %% english by default
                    ,charsets_provided = [<<"iso-8859-1">>] :: kz_term:ne_binaries() %% all charsets provided
                    ,encodings_provided = [<<"gzip;q=1.0">>,<<"identity;q=0.5">>] :: kz_term:ne_binaries() %% gzip and identity
                    ,auth_token = 'undefined' :: kz_term:api_ne_binary()
                    ,auth_token_type = 'x-auth-token' :: 'x-auth-token' | 'basic' | 'oauth' | 'unknown'
                    ,auth_account_id :: kz_term:api_ne_binary()
                    ,auth_doc :: kz_term:api_object()
                    ,req_verb = ?HTTP_GET :: http_method() % see ?ALLOWED_METHODS
                    ,req_nouns = [{<<"404">>, []}] :: req_nouns() % {module, [id]} most typical
                    ,req_json = kz_json:new() :: req_json()
                    ,req_files = [] :: req_files()
                    ,req_data = kz_json:new() :: kz_json:json_term()  % the "data" from the request JSON envelope
                    ,req_headers = #{} :: cowboy:http_headers()
                    ,query_json = kz_json:new() :: kz_json:object()
                    ,account_id :: kz_term:api_ne_binary()
                    ,account_name :: kz_term:api_ne_binary()
                    ,user_id :: kz_term:api_ne_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/users/{user-id}/*
                    ,device_id :: kz_term:api_ne_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/devices/{device-id}/*
                    ,reseller_id :: kz_term:api_ne_binary()
                    ,db_name :: kz_term:api_binary() | kz_term:ne_binaries()
                    ,doc :: kz_term:api_object() | kz_json:objects()
                    ,pretty_print = 'false' :: boolean()
                    ,resp_expires = {{1999,1,1},{0,0,0}} :: kz_time:datetime()
                    ,resp_etag :: 'automatic' | string() | kz_term:api_binary()
                    ,resp_status = 'error' :: crossbar_status()
                    ,resp_error_msg :: kz_term:api_ne_binary()
                    ,resp_error_code :: kz_term:api_integer()
                    ,resp_file = <<>> :: binary()
                    ,resp_data :: resp_data()
                    ,resp_headers = #{} :: cowboy:http_headers() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
                    ,resp_envelope = kz_json:new() :: kz_json:object()
                    ,start = os:timestamp() :: kz_time:now()
                    ,req_id = ?DEFAULT_LOG_SYSTEM_ID :: binary()
                    ,storage = [] :: kz_term:proplist()
                    ,raw_host = <<>> :: binary()
                    ,port = 8000 :: integer()
                    ,raw_path = <<>> :: binary()
                    ,raw_qs = <<>> :: binary()
                    ,method = ?HTTP_GET :: http_method()
                    ,validation_errors = kz_json:new() :: kz_json:object()
                    ,client_ip = <<"127.0.0.1">> :: kz_term:api_ne_binary()
                    ,load_merge_bypass :: kz_term:api_object()
                    ,profile_id :: kz_term:api_ne_binary()
                    ,api_version = ?VERSION_1 :: kz_term:ne_binary()
                    ,magic_pathed = 'false' :: boolean()
                    ,should_paginate :: kz_term:api_boolean()
                    ,host_url = <<>> :: binary()
                    }).

-define(MAX_RANGE, kapps_config:get_pos_integer(?CONFIG_CAT
                                               ,<<"maximum_range">>
                                               ,(?SECONDS_IN_DAY * 31 + ?SECONDS_IN_HOUR)
                                               )
       ).

-define(OPTION_EXPECTED_TYPE, 'expected_type').
-define(TYPE_CHECK_OPTION(ExpectedType), [{?OPTION_EXPECTED_TYPE, ExpectedType}]).
-define(TYPE_CHECK_OPTION_ANY, ?TYPE_CHECK_OPTION(<<"any">>)).

-define(SPECIAL_EXPECTED_TYPE, [{<<"allotments">>, <<"limits">>}
                               ,{<<"connectivity">>, <<"sys_info">>}
                               ,{<<"directories">>, <<"directory">>}
                               ,{<<"faxes">>, <<"fax">>}
                               ,{<<"global_provisioner_templates">>, <<"provisioner_template">>}
                               ,{<<"global_resources">>, <<"resource">>}
                               ,{<<"local_provisioner_templates">>, <<"provisioner_template">>}
                               ,{<<"local_resources">>, <<"resource">>}
                               ,{<<"rate_limit">>, <<"resource">>}
                               ,{<<"sms">>, <<"sms">>}
                               ,{<<"phone_numbers">>, <<"phone_numbers">>} %% weird...
                               ,{<<"vmboxes">>, <<"vmbox">>}
                               ,{<<"resource_selectors">>, <<"resource_selectors_rules">>}
                               ]).

-define(CSV_HEADER_MAP, [{<<"account_id">>, <<"Account ID">>}
                        ,{<<"account_name">>, <<"Account Name">>}
                        ,{<<"amount">>, <<"Amount">>}
                        ,{<<"description">>, <<"Description">>}
                        ,{<<"id">>, <<"ID">>}
                        ,{<<"metadata_callee_id_number">>, <<"Callee ID Number">>}
                        ,{<<"metadata_callee_id_name">>, <<"Callee ID Name">>}
                        ,{<<"metadata_caller_id_number">>, <<"Caller ID Number">>}
                        ,{<<"metadata_caller_id_name">>, <<"Caller ID Name">>}
                        ,{<<"metadata_direction">>, <<"Direction">>}
                        ,{<<"metadata_from">>, <<"From">>}
                        ,{<<"metadata_to">>, <<"To">>}
                        ,{<<"metadata_rate_description">>, <<"Rate Description">>}
                        ,{<<"metadata_rate_increment">>, <<"Rate Increment">>}
                        ,{<<"metadata_rate_minimum">>, <<"Rate Minimum">>}
                        ,{<<"metadata_rate_name">>, <<"Rate Name">>}
                        ,{<<"metadata_rate_nocharge_time">>, <<"Grace Period">>}
                        ,{<<"metadata_rate_value">>, <<"Rate">>}
                        ,{<<"period_start">>, <<"Start Time">>}
                        ,{<<"period_end">>, <<"End Time">>}
                        ,{<<"source_id">>, <<"Source ID">>}
                        ,{<<"source_service">>, <<"Source Service">>}
                        ,{<<"usage_quantity">>, <<"Usage Quantity">>}
                        ,{<<"usage_type">>, <<"Usage Type">>}
                        ,{<<"usage_unit">>, <<"Usage Unit">>}
                        ]).

-define(CROSSBAR_HRL, 'true').
-endif.
