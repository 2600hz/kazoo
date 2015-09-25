%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(whistle_apps_deps).

-export([ensure/0, ensure/1]).
-export([get_base_dir/0, get_base_dir/1]).
-export([local_path/1, local_path/2]).
-export([deps_on_path/0, new_siblings/1]).

-include_lib("whistle/include/wh_types.hrl").

-type filenames() :: [file:filename()].
%% @spec deps_on_path() -> [ProjNameAndVers]
%% @doc List of project dependencies on the path.
-spec deps_on_path() -> filenames().
deps_on_path() ->
    ordsets:from_list(
      lists:foldl(fun deps_on_path_fold/2, [], code:get_path())
     ).

-spec deps_on_path_fold(file:filename(), filenames()) -> filenames().
deps_on_path_fold(Filename, Acc) ->
    ProjDir = filename:dirname(Filename),
    case {filename:basename(Filename)
          ,filename:basename(filename:dirname(ProjDir))
         } of
        {"ebin", "deps"} ->
            [filename:basename(ProjDir) | Acc];
        {"ebin", "lib"} ->
            [filename:basename(ProjDir) | Acc];
        _ ->
            Acc
    end.

%% @spec new_siblings(Module) -> [Dir]
%% @doc Find new siblings paths relative to Module that aren't already on the
%%      code path.
-spec new_siblings(atom()) -> filenames().
-spec new_siblings(atom(), [string()], filenames()) -> filenames().
new_siblings(Module) ->
    new_siblings(Module, ["deps", "lib"], []).

new_siblings(_Module, [], Sibs) -> Sibs;
new_siblings(Module, [Path|Paths], Sibs) ->
    Existing = deps_on_path(),
    SiblingEbin = filelib:wildcard(local_path([Path, "*", "ebin"], Module)),
    Siblings = [filename:dirname(X)
                || X <- SiblingEbin,
                   ordsets:is_element(
                     filename:basename(filename:dirname(X))
                     ,Existing
                    ) =:= 'false'
               ],
    new_siblings(Module, Paths, add_siblings(Siblings, Sibs)).

-spec add_siblings(filenames(), filenames()) -> filenames().
add_siblings(Siblings, Sibs) ->
    lists:foldl(fun(X, Acc) ->
                        maybe_add_path(X, ["ebin", "include"], Acc)
                end, Sibs, Siblings
               ).

-spec maybe_add_path(file:filename(), filenames(), filenames()) -> filenames().
maybe_add_path(Sibling, Dirs, Acc) ->
    lists:foldl(fun(Dir, Acc1) ->
                        F = filename:join([Sibling, Dir]),
                        case filelib:is_dir(F) of
                            'false' -> Acc1;
                            'true' -> [F | Acc1]
                        end
                end, Acc, Dirs).

-spec ensure() -> 'ok'.
-spec ensure(atom()) -> 'ok'.
ensure() ->
    ensure(?MODULE).

ensure(Module) ->
    code:add_paths(new_siblings(Module)),
    'ok'.

-spec get_base_dir() -> file:filename().
-spec get_base_dir(atom()) -> file:filename().
get_base_dir() ->
    get_base_dir(?MODULE).

get_base_dir(Module) ->
    {'file', Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

-spec local_path(strings()) -> file:filename().
-spec local_path(strings(), atom()) -> file:filename().
local_path(Components) ->
    local_path(Components, ?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).
