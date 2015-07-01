%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is used to initialize data on start for the application
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(cm_init).

-include("circlemaker.hrl").

%% API
-export([start_link/0
         ,load_tables/0
        ]).

-spec start_link() -> 'ignore'.
start_link() ->
    _ = spawn(fun load_tables/0),
    'ignore'.

load_tables() ->
    eradius:load_tables().
