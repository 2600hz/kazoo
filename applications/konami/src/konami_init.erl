%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handles setup/initialization of Konami
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(konami_init).

-export([start_link/0
        ,init/0
        ]).

-include("konami.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    _ = kz_util:spawn(fun init/0),
    'ignore'.

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    %% Preload configs
    _Ns = konami_config:numbers(),
    _Ps = konami_config:patterns(),
    _Bk = konami_config:binding_digit(),
    _T = konami_config:timeout(),

    lager:debug("default numbers: ~s", [kz_json:encode(_Ns)]),
    lager:debug("default patterns: ~s", [kz_json:encode(_Ps)]),
    lager:debug("default binding digit: '~s'", [_Bk]),
    lager:debug("default timout: ~b", [_T]).
