%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
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

-include_lib("crossbar/include/crossbar.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize).

authorize(#cb_context{req_id=ReqId}) ->
    lager:debug("noauthz authorizing request"),
    true.
