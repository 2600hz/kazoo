%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_data_tracing).
-behaviour(gen_server).

-export([start_link/0
        ,status/0, status/1
        ,clear_all_traces/0
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-export([trace_file/0, trace_file/1, trace_file/2, trace_file/3, trace_file/4]).
-export([stop_trace/1]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(DEFAULT_TRACE_OUTPUT_FORMAT, ['time'
                                     ," [", 'severity', "] |"
                                     ,'from_app', "|"
                                     ,{'callid', <<"0000000000">>}, "|"
                                     ,'mod', ":" , 'func', ":", 'line'
                                     ," (", 'pid', ") "
                                     ,'message', "\n"
                                     ]).
-define(DEFAULT_TRACE_PROPS(Format),
        [{'formatter', 'lager_default_formatter'}
        ,{'formatter_config', Format}
        ]
       ).

-type trace_error() :: 'invalid_trace' |
                       'invalid_level' |
                       'file_in_use'.

-type trace_ref() :: binary().

-type filter() :: glc_ops:op().
-type filters() :: [filter()].

-export_type([filter/0, filters/0
             ,trace_result/0, trace_results/0

             ]).

-type trace_result() :: {{'lager_file_backend', file:filename_all()}, filters(), lager:log_level()}.
-type trace_results() :: [{kz_term:ne_binary(), file:filename_all(), trace_result()}].

-type trace_options() :: #{'filters' => filters()
                          ,'filename' => file:filename_all()
                          ,'trace_properties' => list()
                          ,'log_level' => atom()
                          }.

-record(state, {traces = [] :: trace_results()
               }).
-type state() :: #state{}.

-spec trace_file() ->
          {'ok', trace_ref()} |
          {'error', trace_error()}.
trace_file() ->
    trace_file([{'function', '*'}]).

-spec trace_file(filters()) ->
          {'ok', trace_ref()} |
          {'error', trace_error()}.
trace_file(Filters) ->
    trace_file(Filters, <<"/tmp/", (kz_binary:rand_hex(16))/binary, ".log">>).

-spec trace_file(filters(), file:filename_all()) ->
          {'ok', trace_ref()} |
          {'error', trace_error()}.
trace_file(Filters, Filename) ->
    trace_file(Filters, Filename, ?DEFAULT_TRACE_PROPS(?DEFAULT_TRACE_OUTPUT_FORMAT)).

-spec trace_file(filters(), file:filename_all(), list()) ->
          {'ok', trace_ref()} |
          {'error', trace_error()}.
trace_file(Filters, Filename, Format) ->
    trace_file(Filters, Filename, Format, 'debug').

-spec trace_file(filters(), file:filename_all(), list(), atom()) ->
          {'ok', trace_ref()} |
          {'error', trace_error()}.
trace_file(Filters, Filename, Format, LogLevel) ->
    gen_server:call(?MODULE
                   ,{'trace_file'
                    ,#{'filters' => Filters
                      ,'filename' => Filename
                      ,'trace_properties' => ?DEFAULT_TRACE_PROPS(Format)
                      ,'log_level' => LogLevel
                      }
                    }
                   ).

-spec status() -> trace_results().
status() ->
    gen_server:call(?MODULE, 'status').

-spec status(trace_ref()) -> trace_result() | 'false'.
status(Ref) ->
    gen_server:call(?MODULE, {'status', Ref}).

-spec clear_all_traces() -> 'ok'.
clear_all_traces() ->
    gen_server:call(?MODULE, 'clear_all_traces', 1 * ?MILLISECONDS_IN_HOUR).

-spec stop_trace(trace_ref()) ->
          {'ok', file:filename_all()} |
          {'error', trace_error()}.
stop_trace(TraceRef) ->
    gen_server:call(?MODULE, {'stop_trace', TraceRef}).

-spec start_link() -> kz_types:sup_startchild_ret().
start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'trace_file', TraceOptions}, _From, #state{traces=Traces}=State) ->
    case start_trace(TraceOptions) of
        {'ok', TraceResult} ->
            Ref = kz_binary:rand_hex(6),
            {'reply'
            ,{'ok', Ref}
            ,State#state{traces=[trace_ref(Ref, TraceOptions, TraceResult) | Traces]}
            };
        Result ->
            {'reply', Result, State}
    end;
handle_call('status', _From, #state{traces=Traces}=State) ->
    {'reply', Traces, State};
handle_call({'status', Ref}, _From, #state{traces=Traces}=State) ->
    {'reply', lists:keyfind(Ref, 1, Traces), State};
handle_call('clear_all_traces', _From, #state{traces=Traces}=State) ->
    {'reply'
    ,[{Ref, stop_trace_file(TraceResult)}
      || {Ref, _Filename, TraceResult} <- Traces
     ]
    ,State#state{traces=[]}
    };
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

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Req, State) ->
    {'noreply', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
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

-spec start_trace(trace_options()) ->
          {'ok', trace_result()} |
          {'error', trace_error()}.

start_trace(#{'filters' := Filters
             ,'filename' := Filename
             ,'trace_properties' := Format
             ,'log_level' := LogLevel
             }) ->
    lager:trace_file(kz_term:to_list(Filename)
                    ,[{'sink', 'data_lager_event'} | Filters]
                    ,LogLevel
                    ,Format
                    ).

-spec trace_ref(Ref, trace_options(), trace_result()) ->
          {Ref, file:filename_all(), trace_result()}.
trace_ref(Ref, TraceOptions, TraceResult) ->
    {Ref, maps:get('filename', TraceOptions), TraceResult}.
