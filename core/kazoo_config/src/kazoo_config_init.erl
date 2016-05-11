%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_config_init).

-include("kazoo_config.hrl").

-export([start_link/0
         ,reload/0
        ]).

%% To read config.ini
-export([load_file/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    set_env(),
    'ignore'.

ini_file() ->
    case os:getenv(?CONFIG_FILE_ENV) of
        'false' -> ?CONFIG_FILE;
        File -> File
    end.

-spec load_file() -> kz_proplist().
-spec load_file(file:name()) -> kz_proplist().
load_file() ->
    load_file(ini_file()).
load_file(File) ->
    case zucchini:parse_file(File) of
        {'ok', Prop} ->
            lager:info("loaded configs from file ~s", [File]),
            Prop;
        {'error', 'enoent'} ->
            lager:warning("file ~s does not exist or is not accessible", [File]),
            lager:warning("please create ~s or set the environment variable ~s to the path of the config file", [File, ?CONFIG_FILE_ENV]),
            lager:warning("trying defaults instead"),
            ?SECTION_DEFAULTS;
        {'error', _}=Error ->
            lager:warning("error loading file ~s: ~s", [File, Error]),
            lager:warning("trying defaults instead"),
            ?SECTION_DEFAULTS
    end.

-spec set_env() -> 'ok'.
set_env() ->
    AppEnv = load_file(),
    lager:notice("loaded settings : ~p", [AppEnv]),
    set_zone(AppEnv),
    application:set_env(?APP_NAME_ATOM, 'kz_config', AppEnv).

set_zone(AppEnv) ->
    erlang:put(?SETTINGS_KEY, AppEnv),
    [Local] = kz_config:get(kz_config:get_node_section_name(), 'zone', ['local']),
    Zone = kz_util:to_atom(Local, 'true'),
    lager:notice("setting zone to ~p", [Zone]),
    application:set_env(?APP_NAME_ATOM, 'zone', Zone).


-spec reload() -> 'ok'.
reload() ->
    set_env().
