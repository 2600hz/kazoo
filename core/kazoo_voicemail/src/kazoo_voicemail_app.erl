%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_voicemail_app).
-behaviour(application).

-include("kz_voicemail.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%==============================================================================
%% Application callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(application:start_type(), any()) -> kz_types:startapp_ret().
start(_StartType, _StartArgs) ->
    kapps_maintenance:bind({'migrate', <<"4.0">>}, 'kazoo_voicemail_maintenance', 'migrate'),
    'ignore'.

-spec stop(any()) -> any().
stop(_State) ->
    kapps_maintenance:unbind({'migrate', <<"4.0">>}, 'kazoo_voicemail_maintenance', 'migrate'),
    'ok'.
