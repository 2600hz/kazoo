-module(kz_data_tracing).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([trace_file/0, trace_file/1, trace_file/2, trace_file/3]).
-export([stop_trace/1]).

-include_lib("kazoo/include/kz_types.hrl").

-define(DEFAULT_TRACE_OUTPUT_FORMAT, ['time'
                                     ," [", 'severity', "] |"
                                     ,'from_app', "|"
                                     ,{'callid', <<"0000000000">>}, "|"
                                     ,'mod', ":" , 'func', ":", 'line'
                                     ," (", 'pid', ") "
                                     ,'message', "\n"
                                     ]).
-define(DEFAULT_TRACE_PROPS,
        [{'formatter', 'lager_default_formatter'}
        ,{'formatter_config', ?DEFAULT_TRACE_OUTPUT_FORMAT}
        ]
       ).

-type trace_error() :: 'invalid_trace' |
                       'invalid_level' |
                       'file_in_use'.

-type filter() :: glc_ops:op().
-type filters() :: [filter()].

-export_type([filter/0, filters/0]).

-type trace_result() :: {{'lager_file_backend', file:filename_all()}, filters(), lager:log_level()}.
-type trace_results() :: [{ne_binary(), file:filename_all(), trace_result()}].

-record(state, {traces = [] :: trace_results()
               }).
-type state() :: #state{}.

-spec trace_file() ->
                        {'ok', ne_binary()} |
                        {'error', trace_error()}.
-spec trace_file(filters()) ->
                        {'ok', ne_binary()} |
                        {'error', trace_error()}.
-spec trace_file(filters(), file:filename_all()) ->
                        {'ok', ne_binary()} |
                        {'error', trace_error()}.
-spec trace_file(filters(), file:filename_all(), list()) ->
                        {'ok', ne_binary()} |
                        {'error', trace_error()}.
trace_file() ->
    trace_file([{'function', '*'}]).

trace_file(Filters) ->
    trace_file(Filters, <<"/tmp/", (kz_util:rand_hex_binary(16))/binary, ".log">>).

trace_file(Filters, Filename) ->
    trace_file(Filters, Filename, ?DEFAULT_TRACE_PROPS).

trace_file(Filters, Filename, Format) ->
    gen_server:call(?MODULE, {'trace_file', Filters, Filename, Format}).

-spec stop_trace(ne_binary()) ->
                        {'ok', file:filename_all()} |
                        {'error', trace_error()}.
stop_trace(TraceRef) ->
    gen_server:call(?MODULE, {'stop_trace', TraceRef}).

-spec start_link() -> sup_startchild_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

-spec handle_call(any(), pid_ref(), state()) -> handle_call_ret_state(state()).
handle_call({'trace_file', Filters, Filename, Format}
           ,_From
           ,#state{traces=Traces}=State
           ) ->
    case start_trace(Filters, Filename, Format) of
        {'ok', TraceResult} ->
            Ref = kz_util:rand_hex_binary(6),
            {'reply', {'ok', Ref}, State#state{traces=[{Ref, Filename, TraceResult}|Traces]}};
        Result ->
            {'reply', Result, State}
    end;
handle_call({'stop_trace', TraceRef}
           ,_From
           ,#state{traces=Traces}=State
           ) ->
    case lists:keytake(TraceRef, 1, Traces) of
        'false' -> {'reply', {'error', 'invalid_trace'}, State};
        {'value', {TraceRef, Filename, TraceResult}, Traces1} ->
            case stop_trace_file(TraceResult) of
                'ok' ->
                    {'reply', {'ok', Filename} , State#state{traces=Traces1}};
                Error ->
                    {'reply', Error, State#state{traces=Traces1}}
            end
    end.

-spec handle_cast(any(), state()) -> handle_cast_ret_state(state()).
handle_cast(_Req, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> handle_info_ret_state(state()).
handle_info(_Msg, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("terminating ~p", [_Reason]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_Vsn, State, _Extra) ->
    {'ok', State}.


-spec stop_trace_file(trace_result()) -> 'ok' | {'error', trace_error()}.
stop_trace_file(Trace) ->
    lager:stop_trace(Trace).

-spec start_trace(filters(), file:filename_all(), list()) ->
                         {'ok', trace_result()} |
                         {'error', trace_error()}.
start_trace(Filters, Filename, Format) ->
    lager:trace_file(kz_util:to_list(Filename)
                    ,[{'sink', 'data_lager_event'} | Filters]
                    ,'debug'
                    ,Format
                    ).
