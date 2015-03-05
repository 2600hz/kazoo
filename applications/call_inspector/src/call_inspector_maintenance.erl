%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(call_inspector_maintenance).

-export([import_freeswitch_log/2
         ,import_kamailio_log/2]).
-export([flush/0
         ,flush/1
        ]).
-export([callid_details/1]).

-include("call_inspector.hrl").

-spec import_freeswitch_log(text(), text()) -> 'ok'.
import_freeswitch_log(Filename, IPAddr) ->
    'ok' = ci_parser_fs:open_logfile(Filename, IPAddr),
    ci_parser_fs:start_parsing().

-spec import_kamailio_log(text(), text()) -> 'ok'.
import_kamailio_log(Filename, KamailioIP) ->
    'ok' = ci_parser_kamailio:open_logfile(Filename, KamailioIP),
    ci_parser_kamailio:start_parsing().

-spec flush() -> 'ok'.
flush() ->
    ci_datastore:flush().

-spec flush(text()) -> 'ok'.
flush(CallId) ->
    ci_datastore:flush(CallId).

-spec callid_details(text()) -> 'no_return'.
callid_details(CallId) ->
    Props = ci_datastore:lookup_callid(CallId),
    Chunks = props:get_value('chunks', Props),
    'ok' = print_chunks(ci_chunk:sort_by_timestamp(Chunks)),
    'no_return'.

-spec print_chunks(ci_chunk:chunks()) -> 'ok'.
print_chunks(Chunks) ->
    JSONArray = lists:map(fun ci_chunk:to_json/1, Chunks),
    io:fwrite(io_lib:format("~ts\n", [wh_json:encode(JSONArray)])).
