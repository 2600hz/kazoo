%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Application for passing CDRs to DTH billing service
%%% @end
%%% Created : 15 Jun 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(dth).

-author('James Aimonetti <james@2600hz.org>').
-export([start/0, start_link/0, stop/0, add_binding_to_q/2, rm_binding_from_q/1]).

-include("dth.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    start_deps(),
    dth_sup:start_link().

%% @spec start() -> ok
%% @doc Start the app
start() ->
    start_deps(),
    application:start(dth).

start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    inets:start(),
    wh_util:ensure_started(sasl), % logging
    wh_util:ensure_started(crypto), % random
    wh_util:ensure_started(ibrowse),
    wh_util:ensure_started(whistle_amqp), % amqp wrapper
    wh_util:ensure_started(whistle_couch). % couch wrapper

%% @spec stop() -> ok
%% @doc Stop the basicapp server.
stop() ->
    application:stop(dth).

add_binding_to_q(Q, _Props) ->
    amqp_util:bind_q_to_callmgr(Q, ?KEY_DTH_BLACKLIST_REQ),
    ok.

rm_binding_from_q(Q) ->
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_DTH_BLACKLIST_REQ).
