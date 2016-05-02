%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------

-module(kz_config).

-export([get/1, get/2, get/3
         ,get_atom/2, get_atom/3
         ,get_integer/2, get_integer/3
         ,get_string/2, get_string/3
         ,get_raw_string/2, get_raw_string/3
         ,get_node_section_name/0
         ,set/3, unset/2
         ,get_section/1
         ,zone/0, zone/1
        ]).

-include("kazoo_config.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a section of the config file
%% @end
%%--------------------------------------------------------------------
-spec get(section()) -> kz_proplist().
get(Section) ->
    find_values(Section, ?DEFAULT_DEFAULTS).

-spec get(section(), atom()) -> kz_proplist().
get(Section, Key) ->
    get(Section, Key, ?DEFAULT_DEFAULTS).

-spec get(section(), atom(), Default) -> kz_proplist() | Default.
get(Section, Key, Default) ->
    case find_values(Section, Key) of
        [] -> Default;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
-spec get_atom(section(), atom()) -> [atom(),...] | ?DEFAULT_DEFAULTS.
get_atom(Section, Key) ->
    get_atom(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_atom(section(), atom(), Default) -> [atom(),...] | Default.
get_atom(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values ->
            [kz_util:to_atom(Value, 'true') || Value <- Values];
        Value ->
            [kz_util:to_atom(Value, 'true')]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
-spec get_integer(section(), atom()) -> [integer(),...] | ?DEFAULT_DEFAULTS.
get_integer(Section, Key) ->
    get_integer(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_integer(section(), atom(), Default) -> [integer(),...] | Default.
get_integer(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [kz_util:to_integer(Value) || Value <- Values];
        Value -> [kz_util:to_integer(Value)]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
-spec get_string(section(), atom()) -> [string(),...] | ?DEFAULT_DEFAULTS.
get_string(Section, Key) ->
    get_string(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_string(section(), atom(), Default) -> [string(),...] | Default.
get_string(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [kz_util:to_lower_string(Value) || Value <- Values];
        Value -> [kz_util:to_lower_string(Value)]
    end.

-spec get_raw_string(section(), atom()) -> [string(),...] | ?DEFAULT_DEFAULTS.
get_raw_string(Section, Key) ->
    get_raw_string(Section, Key, ?DEFAULT_DEFAULTS).

-spec get_raw_string(section(), atom(), Default) -> [string(),...] | Default.
get_raw_string(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> Values;
        Value -> Value
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_node_section_name() -> atom().
get_node_section_name() ->
    Node = kz_util:to_binary(node()),
    case binary:split(Node, <<"@">>) of
        [Name, _] -> kz_util:to_atom(Name, 'true');
        _Else -> node()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set or unset enviroment variables
%% @end
%%--------------------------------------------------------------------
-spec set(section(), atom(), term()) -> 'ok'.
-spec set({section(), kz_proplist()}, kz_proplist()) -> 'ok'.
set(Section, Key, Value) ->
    {'ok', Props} = load(),
    case props:get_value(Section, Props) of
        'undefined' ->
            NewSection = {Section, [{Key, Value}]},
            set(NewSection, Props);
        PreVal ->
            NewSection = props:insert_value({Key, Value}, props:delete(Key, PreVal)),
            set({Section, NewSection}, props:delete(Section, Props))
    end.

set(NewSection, Props) ->
    NewProps = props:insert_value(NewSection, Props),
    application:set_env('kazoo_config', 'kz_config', NewProps).

-spec unset(section(), atom()) -> 'ok'.
unset(Section, Key) ->
    {'ok', Props} = load(),
    case props:get_value(Section, Props) of
      'undefined' -> 'ok';
      Val ->
          NewSection = props:delete(Key, Val),
          set({Section, NewSection}, props:delete(Section, Props))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%-spec find_values() -> .
find_values(Section, ?DEFAULT_DEFAULTS) ->
    {'ok', Prop} = load(),
    get_sections(Section, Prop);
find_values(Section, Keys) when is_list(Keys) ->
    lists:reverse(
      lists:foldl(fun(Key, Acc) ->
                          [find_values(Section, Key)|Acc]
                  end, [], Keys)
     );
find_values(Section, Key) ->
    {'ok', Prop} = load(),
    Sections = get_sections(Section, Prop),
    get_values(Key, Sections).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_sections(section(), kz_proplist()) -> kz_proplist().
get_sections('zone' = Section, Prop) ->
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections, 'name', []);
get_sections(Section, Prop) ->
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections).

-spec get_section(section()) -> kz_proplist().
get_section(Section) ->
    {'ok', Prop} = load(),
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections, '__no_zone_filter', []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec format_sections(kz_proplist()) -> kz_proplist().
format_sections(Sections) -> format_sections(Sections, 'zone', []).

-spec format_sections(kz_proplist(), atom(), kz_proplist()) -> kz_proplist().
format_sections([], _, Acc) -> local_sections(lists:reverse(Acc));
format_sections([Section | T], ZoneFilter, Acc) ->
    case props:get_value('host', Section, 'zone') of
        'zone' -> case props:get_value(ZoneFilter, Section, 'generic') of
                      'generic' -> format_sections(T, ZoneFilter, [{'generic', Section} | Acc]);
                      Zone -> format_sections(T, ZoneFilter, [{{'zone', kz_util:to_atom(Zone, 'true')}, Section} | Acc])
                  end;
        Host -> format_sections(T, ZoneFilter, [{kz_util:to_binary(Host), Section} | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sections(kz_proplist()) -> kz_proplist().
local_sections(Sections) -> local_sections(Sections, []).

-spec local_sections(kz_proplist(), kz_proplist()) -> kz_proplist().
local_sections([], Acc) ->
    props:get_first_defined(['zones', 'generics'], Acc, []);
local_sections([Section | T], Acc) ->
    case is_local_section(Section) of
        {'true', _} -> [Section];
        {'false', 'generic'} -> local_sections(T, add_section('generics', Section, Acc));
        {'false', 'zone'} -> local_sections(T, add_section('zones', Section, Acc));
        {'false', _} -> local_sections(T, Acc)
    end.

-spec add_section(atom(), kz_proplist(), kz_proplist()) -> kz_proplist().
add_section(Group, Value, Props) ->
    props:set_value(Group, [Value | props:get_value(Group, Props, [])], Props).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_local_section({ne_binary(), any()}) -> {'false', 'generic'} |
                                                {boolean(), ne_binary()}.
is_local_section({'generic', _}) ->
    {'false', 'generic'};
is_local_section({{'zone', Zone}, _}) ->
    LocalZone = zone(),
    case Zone =:= LocalZone of
        'true' -> {'false', 'zone'};
        'false' -> {'false', Zone}
    end;
is_local_section({SectionHost, _}) ->
    LocalHost = kz_util:to_binary(kz_network_utils:get_hostname()),
    case SectionHost =:= LocalHost of
        'true' -> {'true', LocalHost};
        'false' -> {'false', SectionHost}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%-spec get_value(atom(), kz_proplist()) -> [].
get_values(Key, Sections) -> get_values(Sections, Key, []).

get_values([], _, []) -> [];
get_values([], _, Acc) -> Acc;
get_values([{_, Values} | T], Key, Acc) ->
    V = proplists:get_all_values(Key, Values),
    get_values(T, Key, lists:append(V, Acc)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec load() -> {'ok', kz_proplist()}.
load() ->
    case erlang:get('$_App_Settings') of
        'undefined' ->
            case application:get_env('kazoo_config', 'kz_config') of
                'undefined' -> {'ok', ?SECTION_DEFAULTS};
                {'ok', _}=X -> X
            end;
        Settings ->
            erlang:put('$_App_Settings', 'undefined'),
            {'ok', Settings}
    end.

-spec zone() -> atom().
zone() ->
    case application:get_env('kazoo_config', 'zone') of
        'undefined' ->
            [Local] = get(kz_config:get_node_section_name(), 'zone', ['local']),
            Zone = kz_util:to_atom(Local, 'true'),
            application:set_env('kazoo_config', 'zone', Zone),
            Zone;
        {'ok', Zone} -> Zone
    end.

-spec zone(atom()) -> ne_binary() | atom().
zone('binary') -> kz_util:to_binary(zone());
zone(_) -> zone().
