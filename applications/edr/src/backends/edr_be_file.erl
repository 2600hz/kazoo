%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc File backend module
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_be_file).

-behaviour(gen_edr_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {pid :: pid()
               ,formatter :: module()
               ,formatter_options :: kz_json:object()
               }).
-type state() :: #state{}.

-spec start_link(backend()) -> kz_types:startlink_ret().
start_link(Backend) ->
    gen_edr_backend:start_link(?MODULE, Backend).

-spec init(backend())-> init_ret(state()).
init(#backend{options=Options})->
    %% Default to JSON formatter
    Formatter = edr_util:formatter(Options, 'edr_fmt_json'),
    FormatterOptions = edr_util:formatter_options(Options),
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

-spec push(state(), edr_event()) -> 'ok' | {'error', any()}.
push(#state{pid=Pid, formatter=Formatter, formatter_options=FormatterOptions}, Event)->
    case file:write(Pid, io_lib:format("~s~n", [Formatter:format_event(FormatterOptions, Event)])) of
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
