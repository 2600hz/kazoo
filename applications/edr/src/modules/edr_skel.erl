%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Skeleton of backend module
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_skel).

-behaviour(gen_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/3
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {}).
-type state() :: #state{}.

-spec start_link(backend()) -> startlink_ret().
start_link(Args) ->
    gen_backend:start_link(?MODULE, Args, []).

-spec init(backend())-> init_ret(state()).
init(#backend{})->
    {'ok', #state{}};
init(_Other)->
    'ignore'.

-spec push(state(), non_neg_integer(), kz_json:object()) -> 'ok'.
push(_State, _Timestamp, _Data)->
    'ok'.

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.
-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    'ok'.
