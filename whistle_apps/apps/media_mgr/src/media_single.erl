%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handles single requests for media binaries
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(media_single).

-export([init/3, terminate/2]).

init({Transport, Proto}, Req, Opts) ->
    lager:debug("req came in on ~s:~s", [Transport, Proto]),
    lager:debug("REQ: ~p", [Req]),
    lager:debug("OPTS: ~p", [Opts]),
    ok.

terminate(A, B) ->
    lager:debug("terminate: ~p", [A]),
    lager:debug("terminate: ~p", [B]).
