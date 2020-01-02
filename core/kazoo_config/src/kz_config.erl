%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_config).

-export([get/1, get/2, get/3
        ,get_atom/2, get_atom/3
        ,get_boolean/2, get_boolean/3
        ,get_integer/2, get_integer/3
        ,get_string/2, get_string/3
        ,get_binary/2, get_binary/3
        ,get_raw_string/2, get_raw_string/3
        ,get_node_section_name/0
        ,set/3, unset/2
        ,get_section/1
        ,zone/0, zone/1
        ]).

-include("kazoo_config.hrl").

-type section_key() :: kz_term:ne_binary().

%%------------------------------------------------------------------------------
%% @doc Return a section of the config file
%% @end
%%------------------------------------------------------------------------------
-spec get(section()) -> kz_term:proplist().
get(Section) ->
    find_values(Section, ?DEFAULT_DEFAULTS).

-spec get(section(), section_key()) -> kz_term:proplist().
get(Section, Key) ->
    get(Section, Key, ?DEFAULT_DEFAULTS).

-spec get(section(), section_key(), Default) -> kz_term:proplist() | Default.
get(Section, Key, Default) ->
    case find_values(Section, Key) of
        [] -> Default;
        Else -> Else
    end.

%%------------------------------------------------------------------------------
%% @doc Return values of the config file
%% @end
%%------------------------------------------------------------------------------
-spec get_atom(section(), section_key()) -> [atom(),...] | ?DEFAULT_DEFAULTS.
get_atom(Section, Key) ->
    get_atom(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_atom(section(), section_key(), Default) -> [atom(),...] | Default.
get_atom(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values ->
            [kz_term:to_atom(Value, 'true') || Value <- Values];
        Value ->
            [kz_term:to_atom(Value, 'true')]
    end.

-spec get_boolean(section(), section_key()) -> boolean().
get_boolean(Section, Key) ->
    get_boolean(Section, Key, 'false').

-spec get_boolean(section(), section_key(), boolean()) -> boolean().
get_boolean(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [Value] -> kz_term:is_true(Value)
    end.

%%------------------------------------------------------------------------------
%% @doc Return values of the config file
%% @end
%%------------------------------------------------------------------------------
-spec get_integer(section(), section_key()) -> [integer(),...] | ?DEFAULT_DEFAULTS.
get_integer(Section, Key) ->
    get_integer(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_integer(section(), section_key(), Default) -> [integer(),...] | Default.
get_integer(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [kz_term:to_integer(Value) || Value <- Values];
        Value -> [kz_term:to_integer(Value)]
    end.

%%------------------------------------------------------------------------------
%% @doc Return values of the config file
%% @end
%%------------------------------------------------------------------------------
-spec get_string(section(), section_key()) -> [string(),...] | ?DEFAULT_DEFAULTS.
get_string(Section, Key) ->
    get_string(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_string(section(), section_key(), Default) -> [string(),...] | Default.
get_string(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [kz_term:to_lower_string(Value) || Value <- Values];
        Value -> [kz_term:to_lower_string(Value)]
    end.

-spec get_raw_string(section(), section_key()) -> [string(),...] | ?DEFAULT_DEFAULTS.
get_raw_string(Section, Key) ->
    get_raw_string(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_raw_string(section(), section_key(), Default) -> [string(),...] | Default.
get_raw_string(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> Values;
        Value -> Value
    end.

%%------------------------------------------------------------------------------
%% @doc Return values of the config file
%% @end
%%------------------------------------------------------------------------------
-spec get_binary(section(), section_key()) -> [binary(),...] | ?DEFAULT_DEFAULTS.
get_binary(Section, Key) ->
    get_binary(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_binary(section(), section_key(), Default) -> [binary(),...] | Default.
get_binary(Section, Key, Default) ->
    case get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [kz_term:to_lower_binary(Value) || Value <- Values];
        Value -> [kz_term:to_lower_binary(Value)]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_node_section_name() -> section().
get_node_section_name() ->
    Node = kz_term:to_binary(node()),
    case binary:split(Node, <<"@">>) of
        [Name, _] -> Name;
        _Else -> kz_term:to_binary(node())
    end.

%%------------------------------------------------------------------------------
%% @doc Set or unset environment variables
%% @end
%%------------------------------------------------------------------------------

-spec set(section(), section_key(), term()) -> 'ok'.
set(Section, Key, Value) ->
    Props = load(),
    case props:get_value(Section, Props) of
        'undefined' ->
            NewSection = {Section, [{Key, Value}]},
            set(NewSection, Props);
        PreVal ->
            NewSection = props:insert_value({Key, Value}, props:delete(Key, PreVal)),
            set({Section, NewSection}, props:delete(Section, Props))
    end.

-spec set({section(), kz_term:proplist()}, kz_term:proplist()) -> 'ok'.
set(NewSection, Props) ->
    NewProps = props:insert_value(NewSection, Props),
    application:set_env(?APP, ?MODULE, NewProps).

-spec unset(section(), section_key()) -> 'ok'.
unset(Section, Key) ->
    Props = load(),
    case props:get_value(Section, Props) of
        'undefined' -> 'ok';
        Val ->
            NewSection = props:delete(Key, Val),
            set({Section, NewSection}, props:delete(Section, Props))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_values(section(), kz_term:proplist() | kz_term:text()) -> list().
find_values(Section, ?DEFAULT_DEFAULTS) ->
    get_sections(Section, load());
find_values(Section, Keys) when is_list(Keys) ->
    lists:reverse([find_values(Section, Key) || Key <- Keys]);
find_values(Section, Key) ->
    Sections = get_sections(Section, load()),
    get_values(Key, Sections).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_sections(section(), kz_term:proplist()) -> kz_term:proplist().
get_sections(Atom, Prop) when is_atom(Atom) ->
    get_sections(kz_term:to_binary(Atom), Prop);
get_sections(<<"zone">> = Section, Prop) ->
    Sections = props:get_all_values(Section, Prop),
    format_sections(Sections, <<"name">>, []);
get_sections(Section, Prop) ->
    Sections = props:get_all_values(Section, Prop),
    format_sections(Sections).

-spec get_section(section()) -> kz_term:proplist().
get_section(Section) ->
    Prop = load(),
    Sections = props:get_all_values(kz_term:to_binary(Section), Prop),
    format_sections(Sections, <<"__no_zone_filter">>, []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_sections(kz_term:proplist()) -> kz_term:proplist().
format_sections(Sections) -> format_sections(Sections, <<"zone">>, []).

-spec format_sections(kz_term:proplist(), section_key(), kz_term:proplist()) -> kz_term:proplist().
format_sections([], _, Acc) -> local_sections(lists:reverse(Acc));
format_sections([Section | T], ZoneFilter, Acc) ->
    case props:get_value(<<"host">>, Section, <<"zone">>) of
        <<"zone">> ->
            format_zone_section(Section, T, ZoneFilter, Acc);
        Host ->
            format_sections(T, ZoneFilter, [{kz_term:to_binary(Host), Section} | Acc])
    end.

-spec format_zone_section(kz_term:proplist(), kz_term:proplist(), section_key(), kz_term:proplist()) ->
          kz_term:proplist().
format_zone_section(Section, Sections, ZoneFilter, Acc) ->
    case props:get_value(ZoneFilter, Section, <<"generic">>) of
        <<"generic">> ->
            format_sections(Sections, ZoneFilter, [{<<"generic">>, Section} | Acc]);
        Zone ->
            format_sections(Sections, ZoneFilter, [{{<<"zone">>, kz_term:to_atom(Zone, 'true')}, Section} | Acc])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec local_sections(kz_term:proplist()) -> kz_term:proplist().
local_sections(Sections) -> local_sections(Sections, []).

-spec local_sections(kz_term:proplist(), kz_term:proplist()) -> kz_term:proplist().
local_sections([], Acc) ->
    props:get_first_defined([<<"zones">>, <<"generics">>], Acc, []);
local_sections([Section | T], Acc) ->
    case is_local_section(Section) of
        {'true', _} -> [Section];
        {'false', <<"generic">>} -> local_sections(T, add_section(<<"generics">>, Section, Acc));
        {'false', <<"zone">>} -> local_sections(T, add_section(<<"zones">>, Section, Acc));
        {'false', _} -> local_sections(T, Acc)
    end.

-type section_type() :: {kz_term:ne_binary() | {kz_term:ne_binary(), atom()}, kz_term:proplist()}.

-spec add_section(kz_term:ne_binary(), section_type(), kz_term:proplist()) ->
          kz_term:proplist().
add_section(Group, Value, Props) ->
    props:set_value(Group, [Value | props:get_value(Group, Props, [])], Props).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_local_section({kz_term:ne_binary() | atom(), any()}) -> {boolean(), kz_term:ne_binary()}.
is_local_section({<<"generic">>, _}) ->
    {'false', <<"generic">>};
is_local_section({{<<"zone">>, Zone}, _}) ->
    case zone() of
        Zone -> {'false', <<"zone">>};
        _LocalZone -> {'false', Zone}
    end;
is_local_section({SectionHost, _}) ->
    LocalHost = kz_term:to_binary(kz_network_utils:get_hostname()),
    case SectionHost =:= LocalHost of
        'true' -> {'true', LocalHost};
        'false' -> {'false', SectionHost}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_values(kz_term:text(), kz_term:proplist()) -> list().
get_values(Key, Sections) -> get_values(Sections, Key, []).

-spec get_values(kz_term:proplist(), kz_term:text(), list()) -> list().
get_values([], _, []) -> [];
get_values([], _, Acc) -> Acc;
get_values([{_, Values} | T], Key, Acc) ->
    V = props:get_all_values(kz_term:to_binary(Key), Values),
    get_values(T, Key, lists:append(V, Acc)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load() -> kz_term:proplist().
load() ->
    case erlang:get(?SETTINGS_KEY) of
        'undefined' ->
            case application:get_env(?APP, ?MODULE) of
                'undefined' -> ?SECTION_DEFAULTS;
                {'ok', Settings} -> Settings
            end;
        Settings ->
            erlang:put(?SETTINGS_KEY, 'undefined'),
            Settings
    end.

-spec zone() -> atom().
zone() ->
    case application:get_env(?APP, 'zone') of
        'undefined' -> find_zone();
        {'ok', Zone} -> Zone
    end.

-spec zone(atom()) -> kz_term:ne_binary() | atom().
zone('binary') -> kz_term:to_binary(zone());
zone(_) -> zone().

find_zone() ->
    find_zone([fun maybe_zone_from_env/0
              ,fun zone_from_ini/0
              ]).

find_zone([]) ->
    lager:info("setting zone from default: local"),
    'local';
find_zone([F|Fs]) ->
    case F() of
        'undefined' -> find_zone(Fs);
        Zone -> Zone
    end.

-spec maybe_zone_from_env() -> atom().
maybe_zone_from_env() ->
    case os:getenv("KAZOO_ZONE", "noenv") of
        "noenv" -> 'undefined';
        Zone ->
            lager:info("setting zone from env KAZOO_ZONE: ~s", [Zone]),
            kz_term:to_atom(Zone, 'true')
    end.

-spec zone_from_ini() -> atom().
zone_from_ini() ->
    Section = get_node_section_name(),
    case get(Section, <<"zone">>) of
        [] -> 'undefined';
        [Zone] ->
            lager:info("setting zone from INI section ~s: ~s", [Section, Zone]),
            kz_term:to_atom(Zone, 'true')
    end.
