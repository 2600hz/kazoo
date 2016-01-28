%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_config_init).

-include("whistle_config.hrl").

-export([start_link/0]).

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

-spec load_file(string()) -> {'ok', wh_proplist()}.
load_file(File) ->
    case zucchini:parse_file(File) of
        {'ok', Prop} ->
            lager:info("loaded configs from file ~s", [File]),
            {'ok', Prop};
        {'error', 'enoent'} ->
            lager:warning("file ~s does not exist or is not accessible", [File]),
            lager:warning("please create ~s or set the environment variable ~s to the path of the config file", [File, ?CONFIG_FILE_ENV]),
            lager:warning("trying defaults instead"),
            {'ok', ?SECTION_DEFAULTS};
        {'error', _}=Error ->
            lager:warning("error loading file ~s: ~s", [File, Error]),
            lager:warning("trying defaults instead"),
            {'ok', ?SECTION_DEFAULTS}
    end.

-spec set_env() -> 'ok'.
set_env() ->
   {'ok', X} = load_file(ini_file()),
    lager:debug("setting config env ~p", [X]),
    application:set_env('whistle_config', 'wh_config', X).
