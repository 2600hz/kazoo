%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(call_inspector_maintenance).

-export([list_active_parsers/0]).
-export([import_freeswitch_log/2
         ,import_kamailio_log/2]).
-export([flush/0
         ,flush/1
        ]).
-export([callid_details/1]).

-include("call_inspector.hrl").

-spec list_active_parsers() -> 'ok'.
list_active_parsers() ->
    io:format("~p\n", [supervisor:which_children('ci_parsers_sup')]).

-spec import_freeswitch_log(text(), text()) -> 'ok'.
import_freeswitch_log(Filename, LogIP) ->
    Args = [{'parser_args', Filename, LogIP}],
    ci_parsers_sup:start_child('ci_parser_fs', Args).

-spec import_kamailio_log(text(), text()) -> 'ok'.
import_kamailio_log(Filename, LogIP) ->
    Args = [{'parser_args', Filename, LogIP}],
    ci_parsers_sup:start_child('ci_parser_kamailio', Args).

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
