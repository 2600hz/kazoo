%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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

-include("../crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, authorize).

-spec authorize(#cb_context{}) -> 'true'.
authorize(_) ->
    lager:debug("noauthz authorizing request"),
    true.
