%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Generic HTTP backend module
%%% @author SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%% @author Conversant Ltd (Max Lay)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_be_http).

-behaviour(gen_edr_backend).

-include("../edr.hrl").

-export([start_link/1]).

-export([push/2
        ,init/1
        ,stop/2
        ,async_response_handler/1
        ]).

-record(state, {url :: string()
               ,method :: atom()
               ,headers :: kz_term:proplist()
               ,async :: boolean()
               ,httpc_options :: [{atom(), any()}]
               ,formatter :: module()
               ,formatter_options :: kz_json:object()
               }).
-type state() :: #state{}.

-spec start_link(backend()) -> kz_types:startlink_ret().
start_link(Backend) ->
    gen_edr_backend:start_link(?MODULE, Backend).

-spec init(backend())-> init_ret(state()).
init(#backend{options=Options})->
    HttpcOptions = props:filter_undefined([{'connect_timeout', kz_json:get_integer_value(<<"connect_timeout">>, Options)}
                                          ,{'timeout', kz_json:get_integer_value(<<"timeout">>, Options)}
                                          ]),
    case kz_json:get_value(<<"url">>, Options) of
        'undefined' -> {'stop', 'no_url'};
        Url -> {'ok', #state{url=kz_term:to_list(Url)
                            ,method=kz_json:get_atom_value(<<"method">>, Options, 'post')
                            ,headers=kz_json:to_proplist(kz_json:get_json_value(<<"headers">>, Options, kz_json:new()))
                            ,async=kz_json:is_true(<<"async">>, Options)
                            ,httpc_options=HttpcOptions
                            ,formatter=edr_util:formatter(Options, 'edr_fmt_json')
                            ,formatter_options=edr_util:formatter_options(Options)
                            }}
    end;
init(_Other)->
    'ignore'.

-spec push(state(), edr_event()) -> work_result().
push(#state{async='false', httpc_options=Opts, url=Url, method=Method, headers=Headers}=State, #edr_event{}=Event)->
    case kz_http:req(Method, Url, Headers, payload(State, Event), Opts) of
        {'ok', Code, _, _} when Code >= 200
                                andalso Code < 300 ->
            'ok';
        {'ok', Code, _, _} ->
            lager:info("non 2xx response code: ~p", [Code]),
            {'error', Code};
        {'error', Reason} ->
            lager:info("error sending edr event: ", [Reason]),
            {'error', Reason}
    end;
push(#state{async='true', httpc_options=_Opts, url=_Url, method=_Method, headers=_Headers}=_State, #edr_event{}=_Event)->
    %% TODO: Implement - I haven't yet figured out how httpc async requests work
    {'error', 'not_implemented'}.

-spec payload(state(), edr_event()) -> kz_term:ne_binary().
payload(#state{formatter=Formatter, formatter_options=FormatterOptions}, #edr_event{}=Event) ->
    Formatter:format_event(FormatterOptions, Event).

-spec stop(state(), any()) -> 'ok'.
stop(_State, _Reason)->
    'ok'.

-spec async_response_handler(any()) -> work_result().
async_response_handler(_Response)->
    lager:debug("unexpected message ~p", [_Response]),
    {'error', {'unexpected_message', _Response}}.
