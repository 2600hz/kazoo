%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kazoo_attachments_app).

-behaviour(application).

-include("kz_att.hrl").

-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_Type, _Args) ->
    kazoo_attachments_sup:start_link().

%%--------------------------------------------------------------------
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
