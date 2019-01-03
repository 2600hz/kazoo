%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author SIPLABS, LLC (Vorontsov Nikita)
%%% @author Conversant Ltd (Max Lay)
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_app).

-behaviour(application).

-include("edr.hrl").

-export([start/2
        ,stop/1
        ]).

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    _ = declare_exchanges(),
    edr_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) -> 'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    'ok'.
