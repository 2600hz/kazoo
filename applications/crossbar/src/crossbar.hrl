-ifndef(CROSSBAR_HRL).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-include("crossbar_types.hrl").

-define(CONFIG_CAT, <<"crossbar">>).

-define(TOKEN_DB, <<"token_auth">>).

-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).

-define(APP_NAME, <<"crossbar">>).
-define(APP_VERSION, <<"0.8.0">>).

-define(CACHE_TTL, whapps_config:get_integer(<<"crossbar">>, <<"cache_ttl">>, 300)).

-define(CROSSBAR_DEFAULT_CONTENT_TYPE, {<<"application">>, <<"json">>, []}).

-define(CONTENT_PROVIDED, [{'to_json', ?JSON_CONTENT_TYPES}]).
-define(CONTENT_ACCEPTED, [{'from_json', ?JSON_CONTENT_TYPES}
                           ,{'from_form', [{<<"application">>, <<"x-www-form-urlencoded">>}]}
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

-define(CROSSBAR_CACHE, 'crossbar_cache').

-define(USERS_QCALL_NOUNS, [{<<"users">>, [_UserId, <<"quickcall">>, _Number]}
                            ,{?WH_ACCOUNTS_DB, [_]}
                           ]).
-define(DEVICES_QCALL_NOUNS, [{<<"devices">>, [_DeviceId, <<"quickcall">>, _Number]}
                              ,{?WH_ACCOUNTS_DB, [_]}
                             ]).

-record(cb_context, {
           content_types_provided = [] :: crossbar_content_handlers()
          ,content_types_accepted = [] :: crossbar_content_handlers()
          ,allowed_methods = ?ALLOWED_METHODS :: http_methods()
          ,allow_methods = ?ALLOWED_METHODS :: http_methods()
          ,languages_provided = [<<"en">>, <<"en-us">>, <<"en-gb">>] :: ne_binaries() %% english by default
          ,charsets_provided = [<<"iso-8859-1">>] :: ne_binaries() %% all charsets provided
          ,encodings_provided = [<<"gzip;q=1.0">>,<<"identity;q=0.5">>] :: ne_binaries() %% gzip and identity
          ,auth_token = <<>> :: binary()
          ,auth_account_id :: api_binary()
          ,auth_doc :: api_object()
          ,req_verb = ?HTTP_GET :: http_method() % see ?ALLOWED_METHODS
          ,req_nouns = [{<<"404">>, []}] :: [{ne_binary(), wh_json:json_strings()},...] | [] % {module, [id]} most typical
          ,req_json = wh_json:new() :: wh_json:object() | {'malformed', binary()} %% the request JSON envelope
          ,req_files = [] :: [{ne_binary(), wh_json:object()},...] | [] %% {file_name, {"contents":<<bin>>, "headers":{"content-type":"", "content-length":1}}}
          ,req_data :: wh_json:json_term()  % the "data" from the request JSON envelope
          ,query_json = wh_json:new() :: wh_json:object()
          ,account_id :: api_binary()
          ,db_name :: api_binary()
          ,doc :: api_object() | wh_json:objects()
          ,resp_expires = {{1999,1,1},{0,0,0}} :: wh_datetime()
          ,resp_etag :: 'automatic' | string() | api_binary()
          ,resp_status = 'error' :: crossbar_status()
          ,resp_error_msg :: wh_json:json_string()
          ,resp_error_code :: pos_integer()
          ,resp_data :: wh_json:object() | wh_json:objects() | api_binary() | wh_json:json_term()
          ,resp_headers = [] :: wh_proplist() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
          ,start = erlang:now() :: wh_now()
          ,req_id = <<"000000000000">> :: ne_binary()
          ,storage = [] :: wh_proplist()
          ,raw_host = <<>> :: binary()
          ,port = 8000 :: integer()
          ,raw_path = <<>> :: binary()
          ,raw_qs = <<>> :: binary()
          ,method = ?HTTP_GET :: http_method()
          ,validation_errors = wh_json:new() :: wh_json:object()
          ,client_ip = <<"127.0.0.1">> :: ne_binary()
          ,load_merge_bypass :: api_object()
         }).

-define(CROSSBAR_HRL, 'true').
-endif.
