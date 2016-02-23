-ifndef(CROSSBAR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/kz_system_config.hrl").

-include("crossbar_types.hrl").

-define(CONFIG_CAT, <<"crossbar">>).

-define(CROSSBAR_CACHE, 'crossbar_cache').

-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).

-define(CB_APPS_STORE_LIST, <<"apps_store/crossbar_listing">>).

-define(APP_NAME, <<"crossbar">>).
-define(APP_VERSION, <<"4.0.0">>).

-define(VERSION_1, <<"v1">>).
-define(VERSION_2, <<"v2">>).
-define(VERSION_SUPPORTED, [?VERSION_1, ?VERSION_2]).
-define(CURRENT_VERSION, ?VERSION_2).

-define(CACHE_TTL, whapps_config:get_integer(<<"crossbar">>, <<"cache_ttl">>, 300)).

-define(CROSSBAR_DEFAULT_CONTENT_TYPE, {<<"application">>, <<"json">>, []}).

-define(CB_ACCOUNT_TOKEN_RESTRICTIONS, <<"token_restrictions">>).

-define(CONTENT_PROVIDED, [{'to_json', ?JSON_CONTENT_TYPES}]).
-define(CONTENT_ACCEPTED, [{'from_json', ?JSON_CONTENT_TYPES}
                           ,{'from_form', ?MULTIPART_CONTENT_TYPES}
                           ,{'from_binary', ?CSV_CONTENT_TYPES}
                          ]).
-define(ALLOWED_METHODS, [?HTTP_GET
                          ,?HTTP_POST
                          ,?HTTP_PUT
                          ,?HTTP_GET
                          ,?HTTP_DELETE
                          ,?HTTP_HEAD
                          ,?HTTP_PATCH
                          ,?HTTP_OPTIONS
                         ]).

-define(QUICKCALL_PATH_TOKEN, <<"quickcall">>).
-define(DEVICES_QCALL_NOUNS(DeviceId, Number)
        ,[{<<"devices">>, [DeviceId, ?QUICKCALL_PATH_TOKEN, Number]}
          ,{?WH_ACCOUNTS_DB, [_]}
         ]).
-define(USERS_QCALL_NOUNS(UserId, Number)
        ,[{<<"users">>, [UserId, ?QUICKCALL_PATH_TOKEN , Number]}
          ,{?WH_ACCOUNTS_DB, [_]}
         ]).

-define(DEFAULT_MODULES, ['cb_about'
                          ,'cb_accounts'
                          ,'cb_alerts'
                          ,'cb_api_auth'
                          ,'cb_apps_store'
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
                          ,'cb_notifications'
                          ,'cb_pivot'
                          ,'cb_phone_numbers'
                          ,'cb_port_requests'
                          ,'cb_presence'
                          ,'cb_rates'
                          ,'cb_registrations'
                          ,'cb_resource_templates'
                          ,'cb_resources'
                          ,'cb_schemas'
                          ,'cb_search'
                          ,'cb_service_plans'
                          ,'cb_services'
                          ,'cb_simple_authz'
                          ,'cb_sms'
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
                         ]).

-define(DEPRECATED_MODULES, ['cb_local_resources'
                             ,'cb_global_resources'
                             ,'cb_signup'
                            ]).

-record(cb_context, {
           content_types_provided = [] :: crossbar_content_handlers()
          ,content_types_accepted = [] :: crossbar_content_handlers()
          ,allowed_methods = ?ALLOWED_METHODS :: http_methods()
          ,allow_methods = ?ALLOWED_METHODS :: http_methods()
          ,languages_provided = [<<"en">>, <<"en-us">>, <<"en-gb">>] :: ne_binaries() %% english by default
          ,charsets_provided = [<<"iso-8859-1">>] :: ne_binaries() %% all charsets provided
          ,encodings_provided = [<<"gzip;q=1.0">>,<<"identity;q=0.5">>] :: ne_binaries() %% gzip and identity
          ,auth_token = <<>> :: binary() | 'undefined'
          ,auth_token_type = 'x-auth-token' :: 'x-auth-token' | 'basic' | 'oauth' | 'unknown'
          ,auth_account_id :: api_binary()
          ,auth_doc :: api_object()
          ,req_verb = ?HTTP_GET :: http_method() % see ?ALLOWED_METHODS
          ,req_nouns = [{<<"404">>, []}] :: req_nouns() % {module, [id]} most typical
          ,req_json = wh_json:new() :: req_json()
          ,req_files = [] :: req_files()
          ,req_data :: wh_json:json_term()  % the "data" from the request JSON envelope
          ,req_headers = [] :: cowboy:http_headers()
          ,query_json = wh_json:new() :: api_object()
          ,account_id :: api_binary()
          ,user_id :: api_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/users/{user-id}/*
          ,device_id :: api_binary()   % Will be loaded in validate stage for endpoints such as /accounts/{acct-id}/devices/{device-id}/*
          ,reseller_id :: api_binary()
          ,db_name :: api_binary() | ne_binaries()
          ,doc :: api_object() | wh_json:objects()
          ,resp_expires = {{1999,1,1},{0,0,0}} :: wh_datetime()
          ,resp_etag :: 'automatic' | string() | api_binary()
          ,resp_status = 'error' :: crossbar_status()
          ,resp_error_msg :: wh_json:key()
          ,resp_error_code :: api_integer()
          ,resp_data :: resp_data()
          ,resp_headers = [] :: wh_proplist() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
          ,resp_envelope = wh_json:new() :: wh_json:object()
          ,start = os:timestamp() :: wh_now()
          ,req_id = ?LOG_SYSTEM_ID :: ne_binary()
          ,storage = [] :: wh_proplist()
          ,raw_host = <<>> :: binary()
          ,port = 8000 :: integer()
          ,raw_path = <<>> :: binary()
          ,raw_qs = <<>> :: binary()
          ,method = ?HTTP_GET :: http_method()
          ,validation_errors = wh_json:new() :: wh_json:object()
          ,client_ip = <<"127.0.0.1">> :: api_binary()
          ,load_merge_bypass :: api_object()
          ,profile_id :: api_binary()
          ,api_version = ?VERSION_1 :: ne_binary()
          ,magic_pathed = 'false' :: boolean()
          ,should_paginate :: api_boolean()
         }).

-define(MAX_RANGE, whapps_config:get_integer(?CONFIG_CAT
                                            ,<<"maximum_range">>
                                            ,(?SECONDS_IN_DAY * 31 + ?SECONDS_IN_HOUR)
                                            )
       ).

-define(OPTION_EXPECTED_TYPE, 'expected_type').
-define(TYPE_CHECK_OPTION(ExpectedType), [{?OPTION_EXPECTED_TYPE, ExpectedType}]).

-define(CROSSBAR_HRL, 'true').
-endif.
