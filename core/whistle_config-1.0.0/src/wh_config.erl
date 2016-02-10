%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------

-module(wh_config).

-export([get/1, get/2, get/3
         ,get_atom/2, get_atom/3
         ,get_integer/2, get_integer/3
         ,get_string/2, get_string/3
         ,get_raw_string/2, get_raw_string/3
         ,get_node_section_name/0
         ,set/3, unset/2
        ]).

-include("whistle_config.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a section of the config file
%% @end
%%--------------------------------------------------------------------
-spec get(section()) -> wh_proplist().
get(Section) ->
    find_values(Section, ?DEFAULT_DEFAULTS).

-spec get(section(), atom()) -> wh_proplist().
get(Section, Key) ->
    get(Section, Key, ?DEFAULT_DEFAULTS).

-spec get(section(), atom(), Default) -> wh_proplist() | Default.
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
            [wh_util:to_atom(Value, 'true') || Value <- Values];
        Value ->
            [wh_util:to_atom(Value, 'true')]
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
        [_|_]=Values -> [wh_util:to_integer(Value) || Value <- Values];
        Value -> [wh_util:to_integer(Value)]
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
        [_|_]=Values -> [wh_util:to_lower_string(Value) || Value <- Values];
        Value -> [wh_util:to_lower_string(Value)]
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
    Node = wh_util:to_binary(node()),
    case binary:split(Node, <<"@">>) of
        [Name, _] -> wh_util:to_atom(Name, 'true');
        _Else -> node()
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set or unset enviroment variables
%% @end
%%--------------------------------------------------------------------
-spec set(section(), atom(), term()) -> 'ok'.
-spec set({section(), wh_proplist()}, wh_proplist()) -> 'ok'.
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
    application:set_env('whistle_config', 'wh_config', NewProps).

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
-spec get_sections(section(), wh_proplist()) -> wh_proplist().
get_sections(Section, Prop) ->
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec format_sections(wh_proplist()) -> wh_proplist().
format_sections(Sections) -> format_sections(Sections, []).

format_sections([], Acc) -> local_sections(Acc);
format_sections([Section | T], Acc) ->
    case props:get_value('host', Section, 'generic') of
        'generic' -> format_sections(T, [{'generic', Section} | Acc]);
        Host -> format_sections(T, [{wh_util:to_binary(Host), Section} | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sections(wh_proplist()) -> wh_proplist().
local_sections(Sections) -> local_sections(Sections, []).

local_sections([], Acc) -> Acc;
local_sections([Section | T], Acc) ->
    case is_local_section(Section) of
        {'true', _} -> [Section];
        {'false', 'generic'} -> local_sections(T, [Section | Acc]);
        {'false', _} -> local_sections(T, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_local_section({ne_binary(), any()}) -> {'false', 'generic'} |
                                                {boolean(), ne_binary()}.
is_local_section({SectionHost, _}) ->
    LocalHost = wh_util:to_binary(wh_network_utils:get_hostname()),
    case SectionHost of
        'generic' -> {'false', 'generic'};
        SH ->
            case SH =:= LocalHost of
                'true' -> {'true', LocalHost};
                'false' -> {'false', SectionHost}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%-spec get_value(atom(), wh_proplist()) -> [].
get_values(Key, Sections) -> get_values(Sections, Key, []).

get_values([], _, []) -> [];
get_values([], _, Acc) ->
    lists:reverse(Acc);
get_values([{_, Values} | T], Key, Acc) ->
    V = proplists:get_all_values(Key, Values),
    get_values(T, Key, lists:append(V, Acc)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

-spec load() -> {'ok', wh_proplist()}.
load() ->
    case application:get_env('whistle_config', 'wh_config') of
        'undefined' -> {'ok', ?SECTION_DEFAULTS};
        {'ok', _}=X -> X
    end.
