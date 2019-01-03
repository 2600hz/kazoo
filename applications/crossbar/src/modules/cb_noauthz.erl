%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc NoAuthZ module
%%% Authorizes everyone! PARTY TIME!
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_noauthz).

-export([init/0
        ,authorize/1
        ]).

-include("crossbar.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    'ok'.

-spec authorize(cb_context:context()) -> 'true'.
authorize(_) ->
    lager:debug("noauthz authorizing request"),
    'true'.
