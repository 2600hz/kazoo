%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% File backend module
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_file).

-behaviour(gen_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/3
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {pid :: pid()}).
-type state() :: #state{}.

-spec start_link(backend()) -> startlink_ret().
start_link(Args) ->
    gen_backend:start_link(?MODULE, Args, []).

-spec init(backend())-> init_ret(state()).
init(#backend{options = ConnectionInfo})->
    case kz_json:get_value(<<"Path">>, ConnectionInfo) of
        'undefined' -> {'stop', 'no_path'};
        Path -> {'ok', Pid} = file:open(Path, ['append', 'delayed_write', 'raw']),
                {'ok', #state{pid = Pid}}
    end;
init(_Other)->
    'ignore'.

-spec push(state(), non_neg_integer(), kz_json:object()) -> 'ok' | {'error', any()}.
push(#state{pid = Pid}, Timestamp, Data)->
    case file:write(Pid, io_lib:format("~B: ~p~n", [Timestamp, Data])) of
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
