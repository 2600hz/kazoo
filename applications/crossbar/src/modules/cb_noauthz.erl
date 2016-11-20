%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% NoAuthZ module
%%%
%%% Authorizes everyone! PARTY TIME!
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_noauthz).

-export([init/0
        ,authorize/1
        ]).

-include("crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    ok.

-spec authorize(cb_context:context()) -> 'true'.
authorize(_) ->
    lager:debug("noauthz authorizing request"),
    'true'.
