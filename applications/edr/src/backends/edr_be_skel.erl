%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Skeleton of backend module
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_be_skel).

-behaviour(gen_edr_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {}).
-type state() :: #state{}.

-spec start_link(backend()) -> kz_types:startlink_ret().
start_link(Backend) ->
    gen_edr_backend:start_link(?MODULE, Backend).

-spec init(backend())-> init_ret(state()).
init(#backend{})->
    {'ok', #state{}};
init(_Other)->
    'ignore'.

-spec push(state(), edr_event()) -> 'ok'.
push(_State, _Event)->
    'ok'.

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.
-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    'ok'.
