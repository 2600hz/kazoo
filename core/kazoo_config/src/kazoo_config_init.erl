%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kazoo_config_init).

-include_lib("kazoo_config/src/kazoo_config.hrl").

-export([start_link/0
         ,reload/0
        ]).

-export([read_cookie/1]).

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
        {'ok', Props} ->
            lager:info("loaded configs from file ~s", [File]),
            cleanup_configs(Props);
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

cleanup_configs(Props) ->
    [cleanup_config(Prop) || Prop <- Props].
cleanup_config({'zone', Zone}) ->
    {'zone', cleanup_zone(Zone)};
cleanup_config({'bigcouch', _}=Config) ->
    Config;
cleanup_config({'log', _}=Config) ->
    Config;
cleanup_config({Section, Props}) ->
    {Section, cleanup_section(Props)}.

cleanup_section(Props) ->
    [cleanup_section_prop(Prop) || Prop <- Props].

cleanup_section_prop({'zone', Zone}=Prop) when is_atom(Zone) ->
    Prop;
cleanup_section_prop({'zone', Zone}) ->
    {'zone', kz_util:to_atom(Zone, 'true')};
cleanup_section_prop(Prop) -> Prop.

cleanup_zone(Zone) ->
    [cleanup_zone_prop(Prop) || Prop <- Zone].
cleanup_zone_prop({'name', Name}=Prop) when is_atom(Name) -> Prop;
cleanup_zone_prop({'name', Name}) ->
    {'name', kz_util:to_atom(Name, 'true')};
cleanup_zone_prop({'amqp_uri', URI}=Prop) when is_list(URI) -> Prop;
cleanup_zone_prop({'amqp_uri', URI}) ->
    {'amqp_uri', kz_util:to_list(URI)};
cleanup_zone_prop(Prop) -> Prop.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Reads config.ini without starting the kazoo_config application.
%% @end
%%--------------------------------------------------------------------
-spec read_cookie(atom()) -> [atom()].
read_cookie(NodeName) ->
    AppEnv = load_file(),
    erlang:put(?SETTINGS_KEY, AppEnv),
    kz_config:get_atom(NodeName, 'cookie', []).
