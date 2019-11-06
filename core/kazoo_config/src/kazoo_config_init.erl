%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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
    Zone = kz_config:zone(),
    lager:notice("setting zone to ~p", [Zone]),
    application:set_env(?APP, 'zone', Zone).

-spec reload() -> 'ok'.
reload() ->
    set_env().

cleanup_configs(Props) ->
    [cleanup_config(Prop) || Prop <- Props].

cleanup_config({<<"zone">>, Zone}) ->
    {<<"zone">>, cleanup_section(cleanup_zone(Zone))};
cleanup_config({<<"log">>, _}=Config) ->
    Config;
cleanup_config({Section, Props}) ->
    {Section, cleanup_section(Props)}.

cleanup_section(Props) ->
    [cleanup_section_prop(Prop) || Prop <- Props].

cleanup_section_prop({<<"cookie">>, C}) ->
    {<<"cookie">>, kz_term:to_atom(C, 'true')};
cleanup_section_prop({Key, <<"true">>}) ->
    {Key, 'true'};
cleanup_section_prop({Key, <<"false">>}) ->
    {Key, 'false'};
cleanup_section_prop({<<"uri">>, URI}) ->
    {<<"uri">>, strip_quotes(URI)};
cleanup_section_prop({<<"ip">>, IP}) ->
    {<<"ip">>, strip_quotes(IP)};
cleanup_section_prop({<<"zone">>, Zone}=Prop) when is_atom(Zone) ->
    Prop;
cleanup_section_prop({<<"zone">>, Zone}) ->
    {<<"zone">>, kz_term:to_atom(strip_quotes(Zone), 'true')};
cleanup_section_prop({<<"host">>, Host}) ->
    {<<"host">>, strip_quotes(Host)};
cleanup_section_prop({<<"pool">>, Name}) ->
    {<<"pool">>, kz_term:to_atom(Name, 'true')};
cleanup_section_prop({<<"pool_name">>, Name}) ->
    {<<"pool_name">>, kz_term:to_atom(Name, 'true')};
cleanup_section_prop(Prop) -> Prop.

cleanup_zone(Zone) ->
    [cleanup_zone_prop(Prop) || Prop <- Zone].
cleanup_zone_prop({<<"name">>, Name}=Prop) when is_atom(Name) -> Prop;
cleanup_zone_prop({<<"name">>, Name}) ->
    {<<"name">>, kz_term:to_atom(strip_quotes(Name), 'true')};
cleanup_zone_prop({<<"amqp_uri">>, URI}) ->
    {<<"amqp_uri">>, strip_quotes(URI)};
cleanup_zone_prop(Prop) -> Prop.

%%------------------------------------------------------------------------------
%% @doc Reads `config.ini' without starting the `kazoo_config' application.
%% @end
%%------------------------------------------------------------------------------
-spec read_cookie(atom() | section()) -> [atom()].
read_cookie(NodeName) ->
    AppEnv = load_file(),
    erlang:put(?SETTINGS_KEY, AppEnv),
    kz_config:get_atom(kz_term:to_binary(NodeName), <<"cookie">>, []).

-spec strip_quotes(binary()) -> binary().
strip_quotes(Bin) ->
    kz_binary:strip(Bin, <<"\"">>).
