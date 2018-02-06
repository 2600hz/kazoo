%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2016
%%% @contributors
%%%   SIPLABS, LLC (Vorontsov Nikita)
%%%   Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(edr_app).

-behaviour(application).

-include("edr.hrl").

-export([start/2
        ,stop/1
        ]).

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    edr_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) -> 'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    'ok'.
