-ifndef(KAZOO_TYPES_INCLUDED).
-include_lib("xmerl/include/xmerl.hrl").

-define(MILLISECONDS_IN_SECOND, 1000).
-define(MILLISECONDS_IN_MINUTE, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_MINUTE)).
-define(MILLISECONDS_IN_HOUR, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_HOUR)).
-define(MILLISECONDS_IN_DAY, (?MILLISECONDS_IN_SECOND * ?SECONDS_IN_DAY)).

-define(MICROSECONDS_IN_SECOND, (1000 * ?MILLISECONDS_IN_SECOND)).

-define(SECONDS_IN_MINUTE, 60).
-define(SECONDS_IN_HOUR, 3600).
-define(SECONDS_IN_DAY, 86400).
-define(SECONDS_IN_WEEK, 604800).
-define(SECONDS_IN_YEAR, 31540000).

-define(BYTES_K, 1024).
-define(BYTES_M, 1048576).
-define(BYTES_G, 1073741824).
-define(BYTES_T, 1099511627776).

-define(ANY_DIGIT, [<<"1">>, <<"2">>, <<"3">>
                   ,<<"4">>, <<"5">>, <<"6">>
                   ,<<"7">>, <<"8">>, <<"9">>
                   ,<<"*">>, <<"0">>, <<"#">>
                   ]).

-define(DEFAULT_CONTENT_TYPE, <<"application/json">>).

-type text() :: string() | atom() | binary() | iolist().

-type atoms() :: [atom()].
-type pids() :: [pid()].
-type references() :: [reference()].

-type kz_proplist_key() :: any().
-type kz_proplist_value() :: any().
-type kz_proplist_property() :: kz_proplist_key() | {kz_proplist_key(), kz_proplist_value()}.
-type kz_proplist() :: proplists:proplist().
-type kz_proplists() :: [kz_proplist()].
-type kz_proplist_kv(K, V) :: [{K, V}].

-type pid_ref() :: {pid(), reference()}.
-type pid_refs() :: [pid_ref()].
-type api_pid_ref() :: pid_ref() | 'undefined'.
-type api_pid_refs() :: pid_refs() | 'undefined'.

-type api_terms() :: kz_json:object() | kz_proplist().
-type api_binary() :: binary() | 'undefined'.
-type api_ne_binary() :: ne_binary() | 'undefined'.
-type api_ne_binaries() :: [api_ne_binary()] | 'undefined'.
-type api_binaries() :: [api_binary()] | 'undefined'.
-type api_object() :: kz_json:object() | 'undefined'.
-type api_objects() :: kz_json:objects() | 'undefined'.
-type api_boolean() :: boolean() | 'undefined'.
-type api_atom() :: atom() | 'undefined'.
-type api_atoms() :: atoms() | 'undefined'.
-type api_string() :: string() | 'undefined'.
-type api_reference() :: reference() | 'undefined'.
-type api_pid() :: pid() | 'undefined'.
-type api_list() :: list() | 'undefined'.

-type api_number() :: number() | 'undefined'.
-type api_integer() :: integer() | 'undefined'.
-type api_pos_integer() :: pos_integer() | 'undefined'.
-type api_non_neg_integer() :: non_neg_integer() | 'undefined'.
-type api_float() :: float() | 'undefined'.

-type kz_deeplist() :: iolist(). %[any() | kz_deeplist()].

-type kz_std_return() :: {'ok', any()} | {'error', any()}.

-type kz_jobj_return() :: {'ok', kz_json:object()} | {'error', any()}.
-type kz_jobjs_return() :: {'ok', kz_json:objects()} | {'error', any()}.

%% non-empty binary
-define(NE_BINARY, <<_:8,_/binary>>).
-type ne_binary() :: <<_:8,_:_*8>>.
-type ne_binaries() :: [ne_binary()].
-type binaries() :: [binary()].

-type strings() :: [string()].
-type integers() :: [integer()].

-type functions() :: [function()].

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: {ne_binary(), ne_binary(), kz_proplist(), kz_proplist(), ne_binary() | iolist()}.

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

%% result of calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
%% Subtract this value from a gregorian seconds version of a date
%% to get the Unix timestamp
%% datetime_to_gregorian_seconds({date(),time()}) - ?UNIX_EPOCH_IN_GREGORIAN.
-define(UNIX_EPOCH_IN_GREGORIAN, 62167219200).

-type kz_now() :: erlang:timestamp().
-type kz_year() :: non_neg_integer().
-type kz_month() :: 1..12.
-type kz_day() :: 1..31.
-type kz_hour() :: 0..23.
-type kz_minute() :: 0..59.
-type kz_second() :: 0..59.
-type kz_daynum() :: 1..7.
-type kz_weeknum() :: 1..53.
-type kz_date() :: calendar:date(). %%{kz_year(), kz_month(), kz_day()}.
-type kz_time() :: calendar:time(). %%{kz_hour(), kz_minute(), kz_second()}.
-type kz_datetime() :: calendar:datetime(). %%{kz_date(), kz_time()}.
-type kz_iso_week() :: calendar:yearweeknum(). %%{kz_year(), kz_weeknum()}.
-type gregorian_seconds() :: pos_integer().
-type unix_seconds() :: pos_integer().
-type api_seconds() :: 'undefined' | gregorian_seconds().

-type kz_timeout() :: non_neg_integer() | 'infinity'.

-type kz_ip_list() :: ne_binaries().

%% Recreate the non-exported types defined in the erlang supervisor source
-type sup_child_spec() :: supervisor:child_spec().
-type sup_child_specs() :: [sup_child_spec()].
-type sup_start_flags() :: supervisor:sup_flags().
-type sup_init_ret() :: {'ok', {sup_start_flags(), sup_child_specs()}} |
                        'ignore'.

-type sup_child_id() :: api_pid().
-type sup_startchild_err() :: 'already_present' |
                              {'already_started', sup_child_id()} |
                              any().
-type sup_startchild_ret() :: {'ok', sup_child_id()} |
                              {'ok', sup_child_id(), any()} |
                              {'error', sup_startchild_err()}.

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

-define(CACHE(N), {N, {'kz_cache', 'start_link', [N]}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', ['kz_cache']}).
-define(CACHE_ARGS(N, Arg), {N, {'kz_cache', 'start_link', [N, Arg]}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', ['kz_cache']}).

%% Recreate the non-exported types defined in the erlang gen_server source
-type startlink_err() :: {'already_started', pid()} |
                         'shutdown' |
                         any().
-type startlink_ret() :: {'ok', pid()} |
                         'ignore' |
                         {'error', startlink_err()}.
-type startapp_ret() :: {'ok', pid()} |
                        {'ok', pid(), any()} |
                        {'error', startlink_err()}.

-type call_from() :: pid_ref().
-type gen_server_timeout() :: 'hibernate' | non_neg_integer().
-type handle_call_ret() :: {'reply', any(), any()} |
                           {'reply', any(), any(), gen_server_timeout()} |
                           {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()} |
                           {'stop', any(), any(), any()}.

-type handle_call_ret_state(State) :: {'reply', any(), State} |
                                      {'reply', any(), State, gen_server_timeout()} |
                                      {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State} |
                                      {'stop', any(), State, any()}.

-type handle_cast_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_cast_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_info_ret() :: {'noreply', any()} |
                           {'noreply', any(), gen_server_timeout()} |
                           {'stop', any(), any()}.
-type handle_info_ret_state(State) :: {'noreply', State} |
                                      {'noreply', State, gen_server_timeout()} |
                                      {'stop', any(), State}.

-type handle_fsm_ret(State) :: {'next_state', atom(), State} |
                               {'next_state', atom(), State, timeout() | 'hibernate'} |
                               {'stop', any(), State}.

-type handle_sync_event_ret(State) :: handle_fsm_ret(State) |
                                      {'reply', any(), atom(), State} |
                                      {'reply', any(), atom(), State, timeout() | 'hibernate'} |
                                      {'stop', any(), any(), State}.

-type server_ref() :: atom() |
                      {atom(), atom()} |
                      {'global', any()} |
                      {'via', atom(), any()} |
                      pid().

-type gen_server_name() :: {'local', atom()} |
                           {'global', any()} |
                           {'via', atom(), any()}.
-type gen_server_option() :: {'debug', list()} |
                             {'timeout', non_neg_integer()} |
                             {'spawn_opt', list()}.
-type gen_server_options() :: [gen_server_option()].

%% XML types
-type xml_attrib_name() :: atom().
-type xml_attrib_value() :: ne_binary() | nonempty_string() | iolist() | atom() | number().
-type xml_attrib() :: #xmlAttribute{}.
-type xml_attribs() :: [xml_attrib()].

-type xml_el() :: #xmlElement{}.
-type xml_els() :: [xml_el()].

-type xml_text() :: #xmlText{value :: iolist()}.
-type xml_texts() :: [xml_text()].

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

-define(CHANNEL_LOOPBACK_HEADER_PREFIX, "Export-Loopback-").
-define(CALL_INTERACTION_ID, "Call-Interaction-ID").
-define(CALL_INTERACTION_DEFAULT
       ,<<(kz_term:to_binary(kz_time:current_tstamp()))/binary
          ,"-", (kz_binary:rand_hex(4))/binary
        >>).

-define(BRIDGE_DEFAULT_SYSTEM_TIMEOUT_S, 20).

-type xml_thing() :: xml_el() | xml_text().
-type xml_things() :: xml_els() | xml_texts().


-define(MATCH_ACCOUNT_RAW(Account),
        <<(Account):32/binary>>
       ).
-define(MATCH_ACCOUNT_UNENCODED(Account),
        <<"account/", (Account):34/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(Account),
        <<"account%2F", (Account):38/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(Account),
        <<"account%2f", (Account):38/binary>>
       ).

-define(MATCH_ACCOUNT_RAW(A, B, Rest),
        <<(A):2/binary, (B):2/binary, (Rest)/binary>>  %% FIXME: add missing size (Rest:28)
       ).
-define(MATCH_ACCOUNT_UNENCODED(A, B, Rest),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_ENCODED(A, B, Rest),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary>>
       ).
-define(MATCH_ACCOUNT_encoded(A, B, Rest),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary>>
       ).

-define(MATCH_PROVISIONER_RAW(Account),
        <<"account/", (Account):34/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_ENCODED(Account),
        <<"account%2F", (Account):38/binary, "-provisioner">>
       ).
-define(MATCH_PROVISIONER_encoded(Account),
        <<"account%2f", (Account):38/binary, "-provisioner">>
       ).

-define(MATCH_MODB_SUFFIX_RAW(A, B, Rest, Year, Month),
        <<(A):2/binary, (B):2/binary, (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(A, B, Rest, Year, Month),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(A, B, Rest, Year, Month),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).
-define(MATCH_MODB_SUFFIX_encoded(A, B, Rest, Year, Month),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
          ,"-", (Year):4/binary, (Month):2/binary
        >>
       ).

%% FIXME: replace these with the above ones, actually matching: "account..."
%% FIXME: add MATCH_MODB_SUFFIX_encoded/3
-define(MATCH_MODB_SUFFIX_RAW(Account, Year, Month),
        <<(Account):32/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_UNENCODED(Account, Year, Month),
        <<(Account):42/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).
-define(MATCH_MODB_SUFFIX_ENCODED(Account, Year, Month),
        <<(Account):48/binary, "-", (Year):4/binary, (Month):2/binary>>
       ).

-define(MATCH_MODB_PREFIX(Year, Month, Account),
        <<(Year):4/binary, (Month):2/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).
-define(MATCH_MODB_PREFIX_M1(Year, Month, Account),
        <<(Year):4/binary, (Month):1/binary, "-", (Account)/binary>>  %% FIXME: add missing size
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(Account),
        <<(Account):32/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(Account),
        <<"account/", (Account):34/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(Account),
        <<"account%2F", (Account):38/binary, "-selectors">>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(Account),
        <<"account%2f", (Account):38/binary, "-selectors">>
       ).

-define(MATCH_RESOURCE_SELECTORS_RAW(A, B, Rest),
        <<(A):2/binary, (B):2/binary, (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_UNENCODED(A, B, Rest),
        <<"account/", (A):2/binary, "/", (B):2/binary, "/", (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_ENCODED(A, B, Rest),
        <<"account%2F", (A):2/binary, "%2F", (B):2/binary, "%2F", (Rest):28/binary
          ,"-selectors"
        >>
       ).
-define(MATCH_RESOURCE_SELECTORS_encoded(A, B, Rest),
        <<"account%2f", (A):2/binary, "%2f", (B):2/binary, "%2f", (Rest):28/binary
          ,"-selectors"
        >>
       ).

%% KZ_NODES types
-record(whapp_info, {startup :: gregorian_seconds()
                    ,roles = [] :: ne_binaries()
                    }).

-type whapp_info() :: #whapp_info{}.
-type kapps_info() :: [{binary(), whapp_info()}].

-type media_server() :: {ne_binary(), kz_json:object()}.
-type media_servers() :: [media_server()].

-record(kz_node, {node = node() :: atom() | '$1' | '$2' | '_'
                 ,md5 :: api_binary() | '_'
                 ,expires = 0 :: non_neg_integer() | 'undefined' | '$2' | '_'
                 ,kapps = [] :: kapps_info() | '$1' | '_'
                 ,media_servers = [] :: media_servers() | '_'
                 ,last_heartbeat = kz_time:now_ms() :: pos_integer() | 'undefined' | '$3' | '_'
                 ,zone :: atom() | 'undefined' | '$2' | '_'
                 ,broker :: api_binary() | '_'
                 ,used_memory = 0 :: non_neg_integer() | '_'
                 ,processes = 0 :: non_neg_integer() | '_'
                 ,ports = 0 :: non_neg_integer() | '_'
                 ,version :: api_binary() | '_'
                 ,channels = 0 :: non_neg_integer() | '_'
                 ,registrations = 0 :: non_neg_integer() | '_'
                 ,globals = [] :: kz_proplist() | '$1' | '_'
                 ,node_info :: api_object() | '_'
                 }).

-type kz_node() :: #kz_node{}.
-type kz_nodes() :: [kz_node()].

-define(FAKE_CALLID(C), kz_term:to_hex_binary(crypto:hash(md5, C))).

-define(KAZOO_TYPES_INCLUDED, 'true').
-endif.
