%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  Main module
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(circlemaker).

-include("circlemaker.hrl").

-export([start_link/0
         ,start/0
         ,stop/0
]).

% default AAA document (it's not empty but AAA is not enabled, so this kind of the document description is helpful for editing)
-define(DEFAULT_AAA_DOCUMENT,
    [
        {<<"aaa_mode">>, <<"off">>},
        {<<"servers">>, wh_json:from_list([
            {<<"enabled">>, 'false'},
            {<<"name">>, <<"unique_server_name">>},
            {<<"address">>, <<"127.0.0.1">>},
            {<<"port">>, 1812},
            {<<"secret">>, <<"secret_phrase">>},
            {<<"aaa_engine">>, <<"radius">>},
            {<<"dicts">>, wh_json:from_list([
                <<"dictionary_3gpp">>, <<"dictionary">>
            ])},
            {<<"avp">>, <<"strict">>},
            {<<"retries">>, 3},
            {<<"timeout">>, 5000}
        ])},
        {<<"authentication">>, wh_json:from_list([<<"unique_server_name">>])},
        {<<"authorization">>, wh_json:from_list([<<"unique_server_name">>])},
        {<<"accounting">>, wh_json:from_list([<<"unique_server_name">>])},
        {<<"workers">>, 5}
    ]
).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns default 'circlemaker' document in the system_config DB
%% @end
%%--------------------------------------------------------------------
-spec sysconfig_default_circlemaker_doc() -> [tuple(),...].
sysconfig_default_circlemaker_doc() ->
    [whapps_config:get(?APP_NAME, Key, DefaultValue) || {Key, DefaultValue} <- ?DEFAULT_AAA_DOCUMENT].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the app for inclusion in a supervisor tree
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = start_deps(),
    _ = sysconfig_default_circlemaker_doc(),
    _ = declare_exchanges(),
    cm_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the application
%% @end
%%--------------------------------------------------------------------
-spec start() -> 'ok' | {'error', _}.
start() ->
    lager:debug("> Circlemaker app started"),
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Stop the app
%% @end
%%--------------------------------------------------------------------
-spec stop() -> 'ok'.
stop() ->
    lager:debug("> Circlemaker app stopped"),
    exit(whereis('cm_sup'), 'shutdown'),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all dependencies for this app are already running
%% @end
%%--------------------------------------------------------------------
-spec start_deps() -> 'ok'.
start_deps() ->
    whistle_apps_deps:ensure(?MODULE), % if started by the whistle_controller, this will exist
    _ = [wh_util:ensure_started(App) || App <- ['crypto'
                                                ,'lager'
                                                ,'whistle_amqp'
                                                ,'whistle_couch'
                                                ,'eradius'
                                               ]],
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures that all exchanges used are declared
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    wapi_aaa:declare_exchanges(),
    wapi_self:declare_exchanges().
