-ifndef(CF_TEMPORAL_ROUTE_HRL).

-define(FIND_RULES, <<>>).

-type improper_month() :: non_neg_integer().
-type improper_day() :: non_neg_integer().
-type improper_date() :: {kz_time:year(), improper_month(), improper_day()}.
-type strict_ordinal() :: binary(). %%<<"first">> | <<"second">> | <<"third">> | <<"fourth">> | <<"fifth">>.
-type broad_ordinal() :: binary(). %%<<"every">> | <<"last">>.
-type ordinal() :: strict_ordinal() | broad_ordinal().

%% <<"monday">> | <<"tuesday">> | <<"wensday">> | <<"wednesday">> | <<"thursday">>
%% | <<"friday">> | <<"saturday">> | <<"sunday">>.
-type wday() :: kz_term:ne_binary().

-type cycle_type() :: binary(). %%<<"date">> | <<"daily">> | <<"weekly">> | <<"monthly">> | <<"yearly">>.

-record(keys, {enable = <<"1">> :: kz_term:ne_binary()
              ,disable = <<"2">> :: kz_term:ne_binary()
              ,reset = <<"3">> :: kz_term:ne_binary()
              }).
-type keys() :: #keys{}.

-record(prompts, {marked_disabled = <<"temporal-marked_disabled">> :: kz_term:ne_binary()
                 ,marked_enabled = <<"temporal-marked_enabled">> :: kz_term:ne_binary()
                 ,marker_reset = <<"temporal-marker_reset">> :: kz_term:ne_binary()
                 ,main_menu = <<"temporal-menu">> :: kz_term:ne_binary()
                 }).
-type prompts() :: #prompts{}.

-define(RULE_DEFAULT_NAME, <<"no_name">>).
-define(RULE_DEFAULT_CYCLE, <<>>).
-define(RULE_DEFAULT_INTERVAL, 1).
-define(RULE_DEFAULT_DAYS, []).
-define(RULE_DEFAULT_WDAYS, []).
-define(RULE_DEFAULT_ORDINAL, <<"first">>).
-define(RULE_DEFAULT_MONTH, 1).
-define(RULE_DEFAULT_START_DATE, {2011,1,1}).
-define(RULE_DEFAULT_WTIME_START, 0).
-define(RULE_DEFAULT_WTIME_STOP, ?SECONDS_IN_DAY).

-type interval() :: non_neg_integer().

-record(rule, {id = <<>> :: binary()
              ,enabled :: kz_term:api_boolean()
              ,name = ?RULE_DEFAULT_NAME :: binary()
              ,cycle = ?RULE_DEFAULT_CYCLE :: cycle_type()
              ,interval = ?RULE_DEFAULT_INTERVAL :: interval()
              ,days = ?RULE_DEFAULT_DAYS :: [kz_time:day()]
              ,wdays = ?RULE_DEFAULT_WDAYS :: [wday()]
              ,ordinal = ?RULE_DEFAULT_ORDINAL :: ordinal()
              ,month = ?RULE_DEFAULT_MONTH :: kz_time:month()
              ,start_date = ?RULE_DEFAULT_START_DATE :: kz_time:date()
              ,wtime_start = ?RULE_DEFAULT_WTIME_START :: non_neg_integer()
              ,wtime_stop = ?RULE_DEFAULT_WTIME_STOP :: non_neg_integer()
              }).

-type rule() :: #rule{}.
-type rules() :: [rule()].

-type route() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-type routes() :: [route()].

-record(temporal, {local_sec = 0 :: non_neg_integer()
                  ,local_date = {2011, 1, 1} :: kz_time:date()
                  ,local_time = {0, 0, 0} :: kz_time:time()
                  ,routes = [] :: routes()
                  ,timezone :: kz_term:api_binary()
                  ,prompts = #prompts{} :: prompts()
                  ,keys = #keys{} :: keys()
                  ,interdigit_timeout = kapps_call_command:default_interdigit_timeout() :: pos_integer()
                  }).
-type temporal() :: #temporal{}.

-define(CF_TEMPORAL_ROUTE_HRL, 'true').
-endif.
