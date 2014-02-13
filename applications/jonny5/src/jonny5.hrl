-ifndef(JONNY5_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(JONNY5_CACHE, jonny5_cache).

-define(DEFAULT_RATE, 0.5).

-define(APP_VERSION, <<"2.0.0">>).
-define(APP_NAME, <<"jonny5">>).

-type(authz_success() :: 'limits_disabled' | 'allotment' | 'flat_rate' | 'per_minute' | 'soft_limit').
-type(authz_failure() :: 'account_unknown' | 'hard_limit' | 'limits_enforced' | 'per_minute' | 'disabled').
-type(authz_reasons() :: authz_success() | authz_failure()).
-type(authz_result() :: {'true', authz_success()} |
                        {'false', authz_failure()}).

-record(request, {account_id :: api_binary()
                  ,account_billing :: authz_reasons()
                  ,account_authorized = 'false' :: boolean()
                  ,reseller_id :: api_binary()
                  ,reseller_billing :: authz_reasons()
                  ,reseller_authorized = 'false' :: boolean()
                  ,call_id :: api_binary()
                  ,call_direction :: api_binary()
                  ,sip_to :: api_binary()
                  ,sip_from :: api_binary()
                  ,sip_request :: api_binary()
                  ,message_id :: api_binary()
                  ,server_id :: api_binary()
                  ,billing_seconds = 0 :: non_neg_integer()
                  ,answered_time = 0 :: non_neg_integer()
                  ,timestamp = wh_util:current_tstamp()
                  ,authorized
                 }).
-type(j5_request() :: #request{}).

-define(JONNY5_HRL, true).
-endif.
