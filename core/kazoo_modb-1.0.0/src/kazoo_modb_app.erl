%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_modb_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("kazoo_modb.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application start behaviour
%% @end
%%--------------------------------------------------------------------
-spec start(term(), term()) -> {'ok', pid()} | {'error', startlink_err()}.
start(_Type, _Args) ->
    case kazoo_modb_sup:start_link() of
        {'ok', P} -> {'ok', P};
        {error, {'already_started', P} } -> {'ok', P};
        {'error', _}=E -> E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Implement the application stop behaviour
%% @end
%%--------------------------------------------------------------------
-spec stop(term()) -> 'ok'.
stop(_State) ->
    'ok'.
