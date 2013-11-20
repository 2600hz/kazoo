%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, authenticate).

authenticate(#cb_context{}) ->
    lager:debug("noauthn authenticating request"),
    true.
