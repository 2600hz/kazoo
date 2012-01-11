%%%-------------------------------------------------------------------
%%% @author James Aimonetti <>
%%% @copyright (C) 2012, James Aimonetti
%%% @doc
%%% Handle agents coming online and going offline
%%% @end
%%% Created : 11 Jan 2012 by James Aimonetti <>
%%%-------------------------------------------------------------------
-module(dg_agent).

-export([init/0, handle_req/2]).

-include("datinggame.hrl").

init() ->
    'ok'.

handle_req(JObj, Props) ->
    ok.
