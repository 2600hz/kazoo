-ifndef(WHISTLE_TYPES_INCLUDED).

-define(MILLISECONDS_IN_DAY, 86400000).
-define(SECONDS_IN_DAY, 86400).

-define(IS_JSON_OBJECT,
        fun({struct, L}) when is_list(L) ->
                lists:all(fun({K, V}) when (is_binary(K) orelse is_atom(K)) andalso
                                           (is_binary(V) orelse is_number(V)) -> true;
                             (_) -> false
                          end, L);
           (_) -> false
        end).

-type api_terms() :: json_object() | proplist().

%% non-empty binary
-type ne_binary() :: <<_:8,_:_*8>>.

-type proplist_key() :: nonempty_string() | ne_binary() | atom().
-type proplist() :: [{proplist_key(), term()} | atom(),...] | [].

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: {ne_binary(), ne_binary(), proplist(), proplist(), ne_binary() | iolist()}.

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

-define(EMPTY_JSON_OBJECT, {'struct', []}).

-type wh_proplist() :: [{ne_binary() | atom(), binary() | atom() | integer() | float() | string()} | atom(),...] | [].

-type json_string() :: atom() | ne_binary().
-type json_strings() :: [json_string(),...].
-type json_number() :: integer() | float().
-type json_array() :: [json_term(),...] | [].
-type json_proplist() :: [{json_string(), json_term()},...] | [].
-type json_object() :: {'struct', json_proplist()} | ?EMPTY_JSON_OBJECT.
-type json_iolist() :: {'json', iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().
-type json_objects() :: [json_object(),...] | [].
-type mochijson() :: json_object() | json_objects() | json_term() | [].

%% result of calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
%% Subtract this value from a gregorian seconds version of a date
%% to get the Unix timestamp
%% datetime_to_gregorian_seconds({date(),time()}) - ?UNIX_EPOCH_IN_GREGORIAN.
-define(UNIX_EPOCH_IN_GREGORIAN, 62167219200).

-type wh_now() :: calendar:t_now().
-type wh_year() :: non_neg_integer().
-type wh_month() :: 1..12.
-type wh_day() :: 1..31.
-type wh_hour() :: 0..23.
-type wh_minute() :: 0..59.
-type wh_second() :: 0..59.
-type wh_daynum() :: 1..7.
-type wh_weeknum() :: 1..53.
-type wh_date() :: calendar:t_date(). %%{wh_year(), wh_month(), wh_day()}.
-type wh_time() :: calendar:t_time(). %%{wh_hour(), wh_minute(), wh_second()}.
-type wh_datetime() :: calendar:t_datetime(). %%{wh_date(), wh_time()}.
-type wh_iso_week() :: calendar:t_yearweeknum(). %%{wh_year(), wh_weeknum()}.

%% Recreate the non-exported types defined in the erlang supervisor source
-type sup_child_spec() :: supervisor:child_spec().
-type sup_child_specs() :: [sup_child_spec()] | [].
-type sup_start_flags() :: {supervisor:strategy(), integer(), integer()}.
-type sup_init_ret() :: {'ok', {sup_start_flags(), sup_child_specs()}}.
-type sup_child_id() :: pid() | 'undefined'.
-type sup_startchild_err() :: 'already_present' | {'already_started', sup_child_id()} | term().
-type sup_startchild_ret() :: {'ok', sup_child_id()} | {'ok', sup_child_id(), term()}
                            | {'error', sup_startchild_err()}.


%% Recreate the non-exported types defined in the erlang gen_server source
-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-type gen_server_timeout() :: 'hibernate' | non_neg_integer().
-type handle_call_ret() :: {'reply', term(), term()} | {'reply', term(), term(), gen_server_timeout()} |
			   {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
			   {'stop', term(), term()} | {'stop', term(), term(), term()}.

-type handle_cast_ret() :: {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
			   {'stop', term(), term()}.

-type handle_info_ret() :: {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
			   {'stop', term(), term()}.

-define(WHISTLE_TYPES_INCLUDED, true).
-endif.
