-ifndef(WHISTLE_TYPES_INCLUDED).
-include_lib("xmerl/include/xmerl.hrl").

-define(MILLISECONDS_IN_DAY, 86400000).
-define(SECONDS_IN_DAY, 86400).

-define(ANY_DIGIT, [<<"1">>, <<"2">>, <<"3">>
                    ,<<"4">>, <<"5">>, <<"6">>
                    ,<<"7">>, <<"8">>, <<"9">>
                    ,<<"*">>, <<"0">>, <<"#">>
                   ]).

%% Hangup Causes that are fine
-define(SUCCESSFUL_HANGUPS, [<<"NORMAL_CLEARING">>, <<"ORIGINATOR_CANCEL">>, <<"SUCCESS">>]).

-define(IS_JSON_GUARD(Obj), is_tuple(Obj)
       andalso is_list(element(1, Obj))
      ).

-define(IS_JSON_OBJECT,
        fun({struct, L}) when is_list(L) ->
                lists:all(fun({K, V}) when (is_binary(K) orelse is_atom(K)) andalso
                                           (is_binary(V) orelse is_number(V)) -> true;
                             (_) -> false
                          end, L);
           (_) -> false
        end).

-type text() :: string() | atom() | binary().

-type api_terms() :: wh_json:json_object() | wh_json:json_proplist().
-type api_binary() :: ne_binary() | 'undefined'.
-type api_binaries() :: [api_binary(),...] | [] | 'undefined'.

-type wh_deeplist() :: iolist(). %[term() | wh_deeplist()].

-type wh_std_return() :: {'ok', _} | {'error', _}.

-type wh_jobj_return() :: {'ok', wh_json:json_object()} | {'error', _}.
-type wh_jobjs_return() :: {'ok', wh_json:json_objects()} | {'error', _}.

%% non-empty binary
-define(NE_BINARY, <<_:8,_/binary>>).
-type ne_binary() :: <<_:8,_:_*8>>.

%% when using gen_smtp to send emails, it takes a 5-tuple for a message-body part
-type mail_message_body() :: {ne_binary(), ne_binary(), proplist(), proplist(), ne_binary() | iolist()}.

%% for setting types on dicts
-type dict(K,V) :: [{K, V}].

-type wh_proplist_value() :: any().
-type wh_proplist_key() :: binary() | atom() | number() | string().
-type wh_proplist_kv(K, V) :: [{K, V} | atom(),...] | [].
-type wh_proplist_k(K) :: wh_proplist_kv(K, wh_proplist_value()).
-type wh_proplist() :: wh_proplist_kv(wh_proplist_key(), wh_proplist_value()).

-type proplist_key() :: wh_proplist_key().
-type proplist() :: wh_proplist().

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

-type wh_timeout() :: non_neg_integer() | 'infinity'.

-type wh_ip_list() :: [ne_binary(),...] | [].

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

-type call_from() :: {pid(), reference()}.
-type gen_server_timeout() :: 'hibernate' | non_neg_integer().
-type handle_call_ret() :: {'reply', term(), term()} | {'reply', term(), term(), gen_server_timeout()} |
                           {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
                           {'stop', term(), term()} | {'stop', term(), term(), term()}.

-type handle_cast_ret() :: {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
                           {'stop', term(), term()}.

-type handle_info_ret() :: {'noreply', term()} | {'noreply', term(), gen_server_timeout()} |
                           {'stop', term(), term()}.

-type server_ref() :: atom() | {atom(), atom()} | {global, term()} | {via, atom(), term()} | pid().

%% Ibrowse-related types
-type ibrowse_ret() :: {'ok', string(), wh_proplist(), string() | binary()} |
                       {'error', 'req_timedout' | 'sel_conn_closed' | {'EXIT', term()}}.
%% When using the stream_to option, ibrowse:send_req returns this tuple ReqID
-type ibrowse_req_id() :: {pos_integer(), pos_integer(), pos_integer()}.

%% XML types
-type xml_attrib_name() :: atom().
-type xml_attrib_value() :: ne_binary() | nonempty_string() | iolist() | atom().
-type xml_attrib() :: #xmlAttribute{}.

-type xml_el() :: #xmlElement{}.
-type xml_els() :: [xml_el(),...] | [].

-define(WHISTLE_TYPES_INCLUDED, true).
-endif.
