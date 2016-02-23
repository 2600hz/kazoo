%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_app).

-behaviour(application).

-include_lib("whistle/include/wh_types.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) ->
                   {'ok', pid()} |
                   {'error', startlink_err()}.
start(_Type, _Args) ->
    _ = declare_exchanges(),
    ananke_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) -> 'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    wapi_self:declare_exchanges(),
    wapi_notifications:declare_exchanges().
