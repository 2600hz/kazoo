%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_fax).

-include("webhooks.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    io:format("webhooks_fax.erl:MARKER:15 ~p~n", [JObj]),
    io:format("webhooks_fax.erl:MARKER:16 ~p~n", [_Props]).
