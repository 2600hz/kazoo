%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(pusher).

-include_lib("pusher.hrl").

-export([start_link/0, stop/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    _ = declare_exchanges(),
    pusher_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
   exit(whereis('pusher_sup'), 'shutdown'),
   'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    %whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['public_key'
                                                ,'sasl'
                                                ,'crypto'
                                                ,'asn1'
                                                ,'ssl'
                                                ,'lager'
                                                ,'whistle_amqp'
                                                ,'whistle_couch'
                                               ]
        ],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = wapi_notifications:declare_exchanges(),
    _ = wapi_call:declare_exchanges(),
    _ = wapi_pusher:declare_exchanges(),
    _ = wapi_registration:declare_exchanges(),
    wapi_self:declare_exchanges().
