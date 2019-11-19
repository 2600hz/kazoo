-ifndef(KAZOO_TYPES_INCLUDED).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kazoo_stdlib/include/kz_records.hrl").

-define(MILLISECONDS_IN_SECOND, 1000).
-define(MILLISECONDS_IN_MINUTE, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_MINUTE)).
-define(MILLISECONDS_IN_HOUR, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_HOUR)).
-define(MILLISECONDS_IN_DAY, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_DAY)).

-define(MICROSECONDS_IN_SECOND, (1000 * ?MILLISECONDS_IN_SECOND)).

-define(SECONDS_IN_MINUTE,     60).
-define(SECONDS_IN_HOUR,     3600).
-define(SECONDS_IN_DAY,     86400).
-define(SECONDS_IN_WEEK,   604800).
-define(SECONDS_IN_YEAR, 31540000).

-define(MINUTES_IN_HOUR, 60).
-define(HOURS_IN_DAY, 24).

-define(BYTES_K,          1024).
-define(BYTES_M,       1048576).
-define(BYTES_G,    1073741824).
-define(BYTES_T, 1099511627776).

-define(ANY_DIGIT, [<<"1">>, <<"2">>, <<"3">>
                   ,<<"4">>, <<"5">>, <<"6">>
                   ,<<"7">>, <<"8">>, <<"9">>
                   ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

%% non-empty binary
-define(NE_BINARY, <<_:8,_/binary>>).

%% result of calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
%% Subtract this value from a Gregorian seconds version of a date
%% to get the Unix timestamp
%% datetime_to_gregorian_seconds({kz_time:date(),kz_time:time()}) - ?UNIX_EPOCH_IN_GREGORIAN.
-define(UNIX_EPOCH_IN_GREGORIAN, 62167219200).

%% Helper macro for declaring children of supervisor
-define(WORKER(I), {I, {I, 'start_link', []}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).
-define(WORKER_APP_INIT(I, T), {I, {I, 'start_link', []}, 'temporary', T * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).

-define(SUPER(I), {I, {I, 'start_link', []}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_TYPE(I, Type), {I, {I, 'start_link', []}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS(I, Args), {I, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_ARGS_TYPE(I, Args, Type), {I, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS(I, N, Args), {N, {I, 'start_link', Args}, 'permanent', 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_ARGS_TYPE(N, I, Args, Type), {N, {I, 'start_link', Args}, Type, 'infinity', 'supervisor', [I]}).
-define(SUPER_NAME_TYPE(N, I, Type), {N, {I, 'start_link', []}, Type, 'infinity', 'supervisor', [I]}).

-define(CACHE(N), {N, {'kz_cache_sup', 'start_link', [N]}, 'permanent', 'infinity', 'supervisor', ['kz_cache_sup']}).
-define(CACHE_ARGS(N, Arg), {N, {'kz_cache_sup', 'start_link', [N, Arg]}, 'permanent', 'infinity', 'supervisor', ['kz_cache_sup']}).
-define(CACHE_ARGS(N, Expires, Props), {N, {'kz_cache_sup', 'start_link', [N, Expires, Props]}, 'permanent', 'infinity', 'supervisor', ['kz_cache_sup']}).

%% Used by ecallmgr and kapi_dialplan at least
-define(CALL_EVENTS,
        [<<"CALL_SECURE">>,<<"CALL_UPDATE">>
        ,<<"CHANNEL_ANSWER">>
        ,<<"CHANNEL_CREATE">>, <<"CHANNEL_DESTROY">>
        ,<<"CHANNEL_EXECUTE">>, <<"CHANNEL_EXECUTE_COMPLETE">>,<<"CHANNEL_EXECUTE_ERROR">>
        ,<<"CHANNEL_FAX_STATUS">>,<<"CHANNEL_INTERCEPTED">>
        ,<<"CHANNEL_PROGRESS_MEDIA">>,<<"CHANNEL_REPLACED">>
        ,<<"CHANNEL_TRANSFEREE">>,<<"CHANNEL_TRANSFEROR">>
        ,<<"CHANNEL_BRIDGE">>, <<"CHANNEL_UNBRIDGE">>
        ,<<"CHANNEL_HOLD">>, <<"CHANNEL_UNHOLD">>
        ,<<"DETECTED_TONE">>,<<"DTMF">>
        ,<<"LEG_CREATED">>, <<"LEG_DESTROYED">>
        ,<<"RECORD_START">>,<<"RECORD_STOP">>
        ,<<"dialplan">> %% errors are sent with this
        ]).

-define(KZ_RECORDER, <<"kz_media_recording">>).

-define(CHANNEL_LOOPBACK_HEADER_PREFIX, "Export-Loopback-").
-define(CALL_INTERACTION_ID, "Call-Interaction-ID").
-define(CALL_INTERACTION_DEFAULT
       ,list_to_binary([kz_term:to_binary(kz_time:now_s())
                       ,"-", kz_binary:rand_hex(4)
                       ])
       ).

-define(BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S, 20).

-define(MATCH_ACCOUNT_RAW(Account)
       ,<<(Account):32/binary>>
       ).
-define(MATCH_ACCOUNT_UNENCODED(Account)
       ,<<"account/", (Account):34/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(Account)
       ,<<"account%2F", (Account):38/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(Account)
       ,<<"account%2f", (Account):38/binary>>
       ).

-define(MATCH_ACCOUNT_RAW(A, B, Rest)
       ,<<(A):2/binary, (B):2/binary, (Rest):28/binary>>  %% FIXME: add missing size (Rest:28)
       ).
-define(MATCH_ACCOUNT_UNENCODED(A, B, Rest)
       ,<<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(A, B, Rest)
       ,<<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(A, B, Rest)
       ,<<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary>>
       ).

-define(MATCH_PROVISIONER_RAW(Account)
       ,<<"account/", (Account):34/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_ENCODED(Account)
       ,<<"account%2F", (Account):38/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_encoded(Account)
       ,<<"account%2f", (Account):38/binary, "-provisioner">>
       ).

-define(MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month)
       ,<<(A):2/binary, (B):2/binary, (Rest):28/binary
         ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month)
       ,<<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
         ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month)
       ,<<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
         ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_encoded(A, B, Rest, Year, Month)
       ,<<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
         ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).

%% FIXME: replace these with the above ones, actually matching: "account..."
%% FIXME: add MATCH_MODB_SUFFIX_encoded/3
-define(MATCH_MODB_SUFFIX_RAW(Account, Year, Month)
       ,<<(Account):32/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(Account, Year, Month)
       ,<<(Account):42/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(Account, Year, Month)
       ,<<(Account):48/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).

-define(MATCH_MODB_PREFIX(Year, Month, Account)
       ,<<(Year):4/binary, (Month):2/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).
-define(MATCH_MODB_PREFIX_M1(Year, Month, Account)
       ,<<(Year):4/binary, (Month):1/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(Account)
       ,<<(Account):32/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(Account)
       ,<<"account/", (Account):34/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(Account)
       ,<<"account%2F", (Account):38/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(Account)
       ,<<"account%2f", (Account):38/binary, "-selectors">>
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest)
       ,<<(A):2/binary, (B):2/binary, (Rest):28/binary
         ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest)
       ,<<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
         ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest)
       ,<<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
         ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(A, B, Rest)
       ,<<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
         ,"-selectors"
        >>
       ).

-define(FAKE_CALLID(C), kz_term:to_hex_binary(crypto:hash('md5', C))).

-define(KAZOO_TYPES_INCLUDED, 'true').
-endif.
