%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(cdr_app).

-behaviour(application).

-include("cdr.hrl").

%% Application callbacks
-export([start/2, stop/1]).
-export([register_views/0]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Implement the application start behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    _ = declare_exchanges(),
    register_views(),
    kapps_maintenance:bind_and_register_views('cdr', 'cdr_app', 'register_views'),
    cdr_sup:start_link().

%%------------------------------------------------------------------------------
%% @doc Implement the application stop behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec stop(any()) -> any().
stop(_State) ->
    'ok'.


-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    _ = kapi_call:declare_exchanges(),
    kapi_self:declare_exchanges().

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder(),
    'ok'.
