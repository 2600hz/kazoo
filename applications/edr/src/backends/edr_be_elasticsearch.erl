%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Elasticsearch backend module
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_be_elasticsearch).

-behaviour(gen_edr_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {options :: [{atom(), any()}]
               ,url :: string()
               }).
-type state() :: #state{}.

-spec start_link(backend()) -> startlink_ret().
start_link(Backend) ->
    gen_edr_backend:start_link(?MODULE, Backend).

-spec init(backend())-> init_ret(state()).
init(#backend{options=ConnectionInfo})->
    Options = props:filter_undefined([{'connect_timeout', kz_json:get_integer_value(<<"Connect-Timeout">>, ConnectionInfo)}
                                     ,{'response_timeout', kz_json:get_integer_value(<<"Response-Timeout">>, ConnectionInfo)}
                                     ,{'is_ssl', kz_json:is_true(<<"Is-SSL">>, ConnectionInfo)}
                                     ,{'stream_to', self()}
                                     ,{'max_sessions', kz_json:get_integer_value(<<"Max-Sessions">>, ConnectionInfo)}
                                     ]),
    case kz_json:get_value(<<"Url">>, ConnectionInfo) of
        'undefined' -> {'stop', 'no_url'};
        Url -> {'ok', #state{url = Url, options = Options}}
    end;
init(_Other)->
    'ignore'.

-spec push(state(), edr_event()) -> work_result().
push(#state{options=Opts, url=Url}, #edr_event{timestamp=Timestamp, body=Data})->
    RawBody = kz_json:from_list([{<<"Timestamp">>, Timestamp}
                                ,{<<"Edr">>, Data}
                                ]),
    Body  = kz_json:encode(RawBody),
    Headers = [],                               %TODO?
    case ibrowse:send_req(Url, Headers, 'put', Body, Opts) of
        {'ibrowse_req_id', _} -> 'ok';
        {'error', Reason} -> {'error', Reason};
        _Other -> {'error', _Other}
    end.


-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.


-spec async_response_handler(any()) -> work_result().
async_response_handler({'ibrowse_async_headers', _, [$2| _], _Data})-> 'ok';
async_response_handler({'ibrowse_async_headers', _, Code, _Data})->
    {'error', {'bad_result_code', Code}};
async_response_handler({'ibrowse_async_response', _, _})-> 'ok';
async_response_handler({'ibrowse_async_response_end', _})-> 'ok';
async_response_handler(_Response)->
    lager:debug("unexpected message ~p", [_Response]),
    {'error', {'unexpected_message', _Response}}.
