%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(call_inspector_maintenance).

-export([import_freeswitch_log/1]).
-export([flush/0
         ,flush/1
        ]).
-export([callid_details/1]).

-include("call_inspector.hrl").

-spec import_freeswitch_log(text()) -> 'ok'.
import_freeswitch_log(Filename) ->
    _ = ci_parser_fs:open_logfile(wh_util:to_list(Filename)),
    ci_parser_fs:start_parsing().

-spec flush() -> 'ok'.
flush() ->
    ci_datastore:flush().

-spec flush(text()) -> 'ok'.
flush(CallId) ->
    ci_datastore:flush(CallId).

-spec callid_details(text()) -> 'no_return'.
callid_details(CallId) ->
    Props = ci_datastore:lookup_callid(CallId),
    _ = case props:get_value('chunks', Props) of
            [] ->
                io:format("not found", []);
            Chunks ->
                print_chunks(ci_chunk:sort_by_timestamp(Chunks))
        end,
    'no_return'.

-spec print_chunks(ci_chunk:chunks()) -> 'ok'.
print_chunks([]) -> 'ok';
print_chunks([Chunk|Chunks]) ->
    io:fwrite(ci_chunk:data(Chunk)),
    print_chunks(Chunks).
