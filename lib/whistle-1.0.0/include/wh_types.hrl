-ifndef(WHISTLE_TYPES_INCLUDED).

-define(SECONDS_IN_DAY, 86400).

-define(IS_JSON_OBJECT,
        fun({struct, L}) when is_list(L) ->
                lists:all(fun({K, V}) when (is_binary(K) orelse is_atom(K)) andalso
                                           (is_binary(V) orelse is_number(V)) -> true;
                             (_) -> false
                          end, L);
           (_) -> false
        end).

-type proplist() :: [{binary() | atom(), term()} | binary() | atom(),...] | [].

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

-define(EMPTY_JSON_OBJECT, {'struct', []}).

-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term(),...] | [].
-type json_object() :: {'struct', [{json_string(), json_term()},...]} | ?EMPTY_JSON_OBJECT.
-type json_iolist() :: {'json', iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().
-type json_objects() :: [json_object(),...] | [].
-type mochijson() :: json_object() | json_objects() | json_term() | [].

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
