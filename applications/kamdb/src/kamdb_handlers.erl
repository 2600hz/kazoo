%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(kamdb_handlers).

-export([handle_rate_req/2
        ]).

-include("kamdb.hrl").

-spec handle_rate_req(wh_json:object(), wh_proplist()) -> any().
handle_rate_req(_JObj, _Props) ->
    lager:info("JObj: ~p", [_JObj]).
