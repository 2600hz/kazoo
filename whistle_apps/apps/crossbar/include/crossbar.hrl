-ifndef(CROSSBAR_HRL).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("cowboy/include/http.hrl").

-include_lib("webmachine/include/webmachine.hrl").

-include("crossbar_types.hrl").

-define(CONFIG_CAT, <<"crossbar">>).

-define(TOKEN_DB, <<"token_auth">>).

-define(MAINTENANCE_VIEW_FILE, <<"views/maintenance.json">>).
-define(ACCOUNTS_AGG_VIEW_FILE, <<"views/accounts.json">>).

-define(APP_NAME, <<"crossbar">>).
-define(APP_VERSION, <<"0.8.0">>).

-define(CACHE_TTL, whapps_config:get_integer(<<"crossbar">>, <<"cache_ttl">>, 300)).

-define(CONTENT_PROVIDED, [{to_json, [{<<"application">>, <<"json">>},{<<"application">>, <<"x-json">>}]}]).
-define(CONTENT_ACCEPTED, [{from_json, [{<<"application">>, <<"json">>},{<<"application">>, <<"x-json">>}]}
                           ,{from_form, [{<<"application">>, <<"x-www-form-urlencoded">>}]}
                           ,{from_binary, [{<<"text">>, <<"csv">>}]}
                          ]).
-define(ALLOWED_METHODS, ['GET'
                          ,'POST'
                          ,'PUT'
                          ,'DELETE'
                          ,'OPTIONS'
                          ,'HEAD'
                         ]).

-record(cb_context, {
           content_types_provided = ?CONTENT_PROVIDED :: [crossbar_content_handler(),...] | []
          ,content_types_accepted = ?CONTENT_ACCEPTED :: [crossbar_content_handler(),...] | []
          ,allowed_methods = ?ALLOWED_METHODS :: [atom(),...] | []
          ,allow_methods = ?ALLOWED_METHODS :: [atom(),...] | []
          ,languages_provided = [<<"en">>] :: [ne_binary(),...] %% english by default
          ,charsets_provided = [<<"*">>] :: [ne_binary(),...] %% all charsets provided
          ,encodings_provided = [<<"gzip;q=1.0">>,<<"identity;q=0.5">>] :: [ne_binary(),...] %% gzip and identity
          ,auth_token = <<>> :: binary()
          ,auth_account_id = 'undefined' :: 'undefined' | ne_binary()
          ,auth_doc = 'undefined' :: wh_json:json_object() | 'undefined'
          ,req_verb = <<"get">> :: ne_binary() % <<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"head">>
          ,req_nouns = [{<<"404">>, []}] :: [{ne_binary(), list()},...] | []
          ,req_json = wh_json:new() :: wh_json:json_object() | {'malformed', binary()} %% the request JSON envelope
          ,req_files = [] :: [{ne_binary(), wh_json:json_object()},...] | [] %% {file_name, {"contents":<<bin>>, "headers":{"content-type":"", "content-length":1}}}
          ,req_data = [] :: wh_json:json_objects()  % the "data" from the request JSON envelope
          ,query_json = wh_json:new() :: wh_json:json_object()
          ,account_id = 'undefined' :: 'undefined' | ne_binary()
          ,db_name = 'undefined' :: 'undefined' | ne_binary()
          ,doc = wh_json:new() :: wh_json:json_object() | wh_json:json_objects()
          ,resp_expires = {{1999,1,1},{0,0,0}} :: wh_datetime()
          ,resp_etag = 'undefined' :: 'undefined' | 'automatic' | ne_binary()
          ,resp_status = 'error' :: crossbar_status()
          ,resp_error_msg = 'undefined' :: wh_json:json_string() | 'undefined'
          ,resp_error_code = 'undefined' :: wh_json:json_number() | 'undefined'
          ,resp_data = wh_json:new() :: wh_json:json_object()
          ,resp_headers = [] :: proplist() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
          ,start = erlang:now() :: wh_now()
          ,req_id = <<"000000000000">> :: ne_binary()
          ,storage = []
         }).

-ifdef(PROFILE).
-define(TIMER_START(Str), wh_timer:start(Str)).
-define(TIMER_TICK(Str), wh_timer:tick(Str)).
-define(TIMER_STOP(Str), wh_timer:stop(Str)).
-else.
-define(TIMER_START(Str), ok).
-define(TIMER_TICK(Str), ok).
-define(TIMER_STOP(Str), ok).
-endif.

-define(CROSSBAR_HRL, true).
-endif.
