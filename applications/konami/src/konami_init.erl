%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz
%%% @doc
%%% Handles setup/initialization of Konami
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(konami_init).

-export([start_link/0
         ,init/0
        ]).

-include("konami.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    _ = wh_util:spawn(fun init/0),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    %% Preload configs
    _Ns = konami_config:numbers(),
    _Ps = konami_config:patterns(),
    _Bk = konami_config:binding_digit(),
    _T = konami_config:timeout(),

    lager:debug("default numbers: ~s", [wh_json:encode(_Ns)]),
    lager:debug("default patterns: ~s", [wh_json:encode(_Ps)]),
    lager:debug("default binding digit: '~s'", [_Bk]),
    lager:debug("default timout: ~b", [_T]).
