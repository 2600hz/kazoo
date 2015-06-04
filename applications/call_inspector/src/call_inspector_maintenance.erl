%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(call_inspector_maintenance).

-export([list_active_parsers/0
         ,stop_active_parser/1
         ,start_freeswitch_parser/2
         ,start_kamailio_parser/2
         ,start_hep_parser/2
        ]).
-export([flush/0
         ,flush/1
        ]).
-export([callid_details/1]).

-include("call_inspector.hrl").

%% API

-spec list_active_parsers() -> 'no_return'.
list_active_parsers() ->
    Ids = ci_parsers_sup:children(),
    lists:foreach(fun (Id) -> io:format("~p\n", [Id]) end, Ids),
    'no_return'.

-spec stop_active_parser(text()) -> 'ok'.
stop_active_parser(Id)
  when not is_atom(Id) ->
    stop_active_parser(
      ci_parsers_util:make_name(
        wh_util:to_binary(Id)
       )
     );
stop_active_parser(Id)
  when is_atom(Id) ->
    ci_parsers_sup:stop_child(Id).

-spec start_freeswitch_parser(text(), text()) -> 'ok'.
start_freeswitch_parser(Filename, LogIP) ->
    Args = [{'parser_args', Filename, wh_util:to_binary(LogIP)}],
    ci_parsers_sup:start_child('ci_parser_freeswitch', Args).

-spec start_kamailio_parser(text(), text()) -> 'ok'.
start_kamailio_parser(Filename, LogIP) ->
    Args = [{'parser_args', Filename, wh_util:to_binary(LogIP)}],
    ci_parsers_sup:start_child('ci_parser_kamailio', Args).

-spec start_hep_parser(text(), text()) -> 'ok'.
start_hep_parser(IP, Port) ->
    Args = [{'parser_args', wh_util:to_binary(IP), wh_util:to_integer(Port)}],
    ci_parsers_sup:start_child('ci_parser_hep', Args).

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

%% Internals

%% End of Module.
