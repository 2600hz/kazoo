%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(webhooks_init).

-export([start_link/0]).

-include("webhooks.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    init_modules(),
    'ignore'.

-spec init_modules() -> 'ok'.
init_modules() ->
    'ok'.
