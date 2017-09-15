%%%-------------------------------------------------------------------
%%% @copyright (C) 2017 Conversant Ltd
%%% @doc
%%% Relays EDR messages to bh_edr via kapi_edr_blackhole
%%% @end
%%% @contributors
%%%    Max Lay
%%%-------------------------------------------------------------------
-module(edr_be_blackhole).

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

-spec start_link(backend()) -> startlink_ret().
start_link(Args) ->
    gen_edr_backend:start_link(?MODULE, Args).

-spec init(backend())-> init_ret(state()).
init(#backend{})->
    lager:info("starting bh_edr"),
    {'ok', #state{}};
init(_Other)->
    'ignore'.

-spec push(state(), edr_event()) -> 'ok'.
push(_State, #edr_event{account_id='undefined'})->
    'ok';
push(_State, Event=#edr_event{app_name=AppName, app_version=AppVersion})->
    FormatterOptions = kz_json:from_list([{<<"include_metadata">>, 'true'}
                                         ,{<<"normalize">>, 'false'}
                                         ,{<<"encode">>, 'false'}
                                         ]),
    Formatted = edr_fmt_json:format_event(FormatterOptions, Event),
    kapi_edr_blackhole:publish_event(kz_json:set_values(kz_api:default_headers(<<"edr">>, <<"event">>, AppName, AppVersion), Formatted)).

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.
-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    'ok'.
