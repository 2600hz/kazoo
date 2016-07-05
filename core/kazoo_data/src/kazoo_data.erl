-module(kazoo_data).

-include("kz_data.hrl").

-export_type([db_create_options/0
             ,data_error/0
             ,data_errors/0
             ,get_results_return/0
             ,db_classifications/0
             ,view_options/0
             ,docid/0, docids/0
             ]).

-export([trace_file/0, trace_file/1, trace_file/2, trace_file/3]).
-export([stop_trace/1]).

-define(DEFAULT_TRACE_OUTPUT_FORMAT, [time," [",severity,"] |", from_app,  "|", {callid, <<"0000000000">>}, "|", mod, ":" , func, ":", line, " (",pid, ") ", message, "\n"]).
-define(DEFAULT_TRACE_PROPS,
        [{formatter, lager_default_formatter}
        ,{formatter_config, ?DEFAULT_TRACE_OUTPUT_FORMAT}
        ]
       ).

trace_file() ->
    trace_file([{'function', '*'}]).

trace_file(Query) ->
    trace_file(Query, <<"/tmp/", (kz_util:rand_hex_binary(16))/binary, ".log">>).

trace_file(Query, Filename) ->
    trace_file(Query, Filename, ?DEFAULT_TRACE_PROPS).

trace_file(Query, Filename, Format) ->
    lager:trace_file(kz_util:to_list(Filename), [{sink,data_lager_event} | Query], debug, Format).

stop_trace(Trace) ->
    lager:stop_trace(Trace).
