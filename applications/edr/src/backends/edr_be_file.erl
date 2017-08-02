%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% File backend module
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%    Max Lay
%%%-------------------------------------------------------------------
-module(edr_be_file).

-behaviour(gen_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {pid :: pid()
               ,formatter :: module()
               ,formatter_options :: module()
               }).
-type state() :: #state{}.

-spec start_link(backend()) -> startlink_ret().
start_link(Args) ->
    gen_backend:start_link(?MODULE, Args, []).

-spec init(backend())-> init_ret(state()).
init(#backend{options=Options})->
    %% Default to JSON formatter
    Formatter = gen_backend:formatter(Options, 'edr_fmt_json'),
    FormatterOptions = gen_backend:formatter_options(Options),
    lager:debug("formatter ~s", [Formatter]),
    lager:debug("formatter options ~s", [kz_json:encode(FormatterOptions)]),
    case kz_json:get_value(<<"path">>, Options) of
        'undefined' ->
            lager:error("no path, stopping backend"),
            {'stop', 'no_path'};
        Path ->
            lager:info("using path ~s", [Path]),
            {'ok', Pid} = file:open(Path, ['append', 'delayed_write', 'raw']),
            {'ok', #state{pid=Pid, formatter=Formatter, formatter_options=FormatterOptions}}
    end;
init(_Other)->
    'ignore'.

-spec push(state(), event()) -> 'ok' | {'error', any()}.
push(#state{pid=Pid, formatter=Formatter, formatter_options=FormatterOptions}, Event)->
    case file:write(Pid, Formatter:format_event(FormatterOptions, Event)) of
        'ok' -> 'ok';
        {'error', Reason} -> error(Reason)
    end.

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.

-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    lager:debug("unexpected message ~p", [_Response]),
    {'error', {'unexpected_message', _Response}}.
