%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% NoAuthN module
%%%
%%% Authenticates everyone! PARTY TIME!
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_noauthn).

-export([init/0
        ,authenticate/1
        ]).

-include("crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    ok.

-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    lager:debug("noauthn authenticating request"),
    cb_context:is_context(Context).
