%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_config_init).

-include("kazoo_config.hrl").

-export([start_link/0
        ,reload/0
        ]).

-export([read_cookie/1]).

%%------------------------------------------------------------------------------
%% @doc Starts the application for inclusion in a supervisor tree.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    set_env(),
    'ignore'.

-spec ini_files() -> list().
ini_files() ->
    case os:getenv(?CONFIG_FILE_ENV) of
        'false' -> [?CONFIG_FILE, ?V4_CONFIG_FILE];
        File    -> [File]
    end.

-spec load_file() -> kz_term:proplist().
load_file() ->
    maybe_load_file(ini_files()).

-spec maybe_load_file(list(file:name())) -> kz_term:proplist().
maybe_load_file([]) ->
    lager:warning("out of config files to attempt, loading defaults..."),
    ?SECTION_DEFAULTS;

maybe_load_file([File|T]) ->
    case zucchini:parse_file(File) of
        {'ok', Props} ->
            lager:info("loaded configs from file ~s", [File]),
            cleanup_configs(Props);

        {'error', 'enoent'} ->
            lager:warning("file ~s does not exist or is not accessible", [File]),
            maybe_load_file(T);

        {'error', _}=Error ->
            lager:warning("error loading file ~s: ~p", [File, Error]),
            maybe_load_file(T)
    end.

-spec set_env() -> 'ok'.
set_env() ->
    AppEnv = load_file(),
    lager:notice("loaded settings : ~p", [AppEnv]),
    set_zone(AppEnv),
    application:set_env(?APP, 'kz_config', AppEnv).

set_zone(AppEnv) ->
    erlang:put(?SETTINGS_KEY, AppEnv),
    Zone = maybe_zone_from_env(),
    lager:notice("setting zone to ~p", [Zone]),
    application:set_env(?APP, 'zone', Zone).

-spec maybe_zone_from_env() -> atom().
maybe_zone_from_env() ->
    case os:getenv("KAZOO_ZONE", "noenv") of
        "noenv" -> zone_from_ini();
        Zone -> kz_term:to_atom(Zone, 'true')
    end.

-spec zone_from_ini() -> atom().
zone_from_ini() ->
    [Local] = kz_config:get(kz_config:get_node_section_name(), 'zone', ['local']),
    kz_term:to_atom(Local, 'true').

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
    {'zone', kz_term:to_atom(Zone, 'true')};
cleanup_section_prop(Prop) -> Prop.

cleanup_zone(Zone) ->
    [cleanup_zone_prop(Prop) || Prop <- Zone].
cleanup_zone_prop({'name', Name}=Prop) when is_atom(Name) -> Prop;
cleanup_zone_prop({'name', Name}) ->
    {'name', kz_term:to_atom(Name, 'true')};
cleanup_zone_prop({'amqp_uri', URI}=Prop) when is_list(URI) -> Prop;
cleanup_zone_prop({'amqp_uri', URI}) ->
    {'amqp_uri', kz_term:to_list(URI)};
cleanup_zone_prop(Prop) -> Prop.

%%------------------------------------------------------------------------------
%% @doc Reads `config.ini' without starting the `kazoo_config' application.
%% @end
%%------------------------------------------------------------------------------
-spec read_cookie(atom()) -> [atom()].
read_cookie(NodeName) ->
    AppEnv = load_file(),
    erlang:put(?SETTINGS_KEY, AppEnv),
    kz_config:get_atom(NodeName, 'cookie', []).
