-module(kazoo_data).

-behaviour(application).

-include("kz_data.hrl").

-export_type([db_create_options/0
              ,data_error/0
              ,data_errors/0
              ,get_results_return/0
              ,db_classifications/0
              ,view_options/0
             ]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application start behaviour
%%--------------------------------------------------------------------
-spec start(application:start_type(), any()) -> startapp_ret().
start(_StartType, _StartArgs) ->
    kazoo_data_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc Implement the application stop behaviour
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.
