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

-include("src/crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize).

-spec authorize(#cb_context{}) -> 'true'.
authorize(_) ->
    lager:debug("noauthz authorizing request"),
    true.
