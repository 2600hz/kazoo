%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------

-module(wh_config).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").


-export([get/1
         ,get/2
         ,get/3
        ]).
-export([get_atom/2
         ,get_atom/3
        ]).
-export([get_integer/2
         ,get_integer/3
        ]).
-export([get_string/2
         ,get_string/3
        ]).


-define(CONFIG_FILE_ENV, "KAZOO_CONFIG").
-define(CONFIG_FILE, "/tmp/kazoo/conf.ini").
-define(DEFAULT_DEFAULT, []).

-type section() :: 'bigcouch' | 'amqp'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a section of the config file
%% @end
%%--------------------------------------------------------------------
-spec get(section()) -> wh_proplist() | ?DEFAULT_DEFAULT.
get(Section) ->
    ?MODULE:get(Section, 'undefined').

-spec get(section(), 'undefined') -> wh_proplist() | ?DEFAULT_DEFAULT;
         (section(), atom()) -> list() | ?DEFAULT_DEFAULT.
get(Section, Key) ->
    get(Section, Key, ?DEFAULT_DEFAULT).

-spec get(section(), 'undefined', Default) -> wh_proplist() | Default;
         (section(), atom(), Default) -> list() | Default.
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
-spec get_atom(section(), atom()) -> [atom(),...] | ?DEFAULT_DEFAULT.
get_atom(Section, Key) ->
    get_atom(Section, Key, ?DEFAULT_DEFAULT).

-spec get_atom(section(), atom(), Default) -> [atom(),...] | Default.
get_atom(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [wh_util:to_atom(Value) || Value <- Values]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
-spec get_integer(section(), atom(), Default) -> [integer(),...] | ?DEFAULT_DEFAULT.
get_integer(Section, Key) ->
    get_integer(Section, Key, ?DEFAULT_DEFAULT).

-spec get_integer(section(), atom(), Default) -> [integer(),...] | Default.
get_integer(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [wh_util:to_integer(Value) || Value <- Values]
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
-spec get_string(section(), string(), Default) -> [string(),...] | ?DEFAULT_DEFAULT.
get_string(Section, Key) ->
    get_string(Section, Key, ?DEFAULT_DEFAULT).

-spec get_string(section(), string(), Default) -> [string(),...] | Default.
get_string(Section, Key, Default) ->
    case ?MODULE:get(Section, Key, Default) of
        Default -> Default;
        [_|_]=Values -> [wh_util:to_lower_string(Value) || Value <- Values]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_atom(section(), atom(), Default) -> [integer(),...] | ?DEFAULT_DEFAULT.
find_values(Section, 'undefined') ->
    {'ok', Prop} = load(),
    get_sections(Section, Prop);
find_values(Section, Keys) when is_list(Keys) ->
    V = lists:foldl(fun(Key, Acc) ->
                            V = find_values(Section, Key),
                            [V | Acc]
                    end, [], Keys),
    lists:reverse(V);
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
-spec get_sections(section(), wh_proplist()) -> wh_proplist() | [].
get_sections(Section, Prop) ->
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec format_sections(wh_proplist()) -> wh_proplist() | [].
format_sections(Sections) ->
    format_sections(Sections, []).

format_sections([], Acc) ->
    local_sections(Acc);
format_sections([Section | T], Acc) ->
    case proplists:get_value('host', Section, 'generic') of
        'generic' ->
            format_sections(T, [{'generic', Section} | Acc]);
        Host ->
            format_sections(T, [{wh_util:to_binary(Host), Section} | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec local_sections(wh_proplist()) -> wh_proplist() | [].
local_sections(Sections) ->
    local_sections(Sections, []).

local_sections([], Acc) ->
    Acc;
local_sections([Section | T], Acc) ->
    case is_local_section(Section) of
        {'true', _} ->
            [Section];
        {'false', 'generic'} ->
            local_sections(T, [Section | Acc]);
         {'false', _} ->
            local_sections(T, Acc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_local_section({ne_binary(), _}) -> {'false', 'generic'} | {boolean(), ne_binary()}.
is_local_section({SectionHost, _}) ->
    LocalHost = wh_util:to_binary(wh_network_utils:get_hostname()),
    case SectionHost of
        'generic' ->
            {'false', 'generic'};
        SH ->
            case SH =:= LocalHost of
                'true' ->
                    {'true', LocalHost};
                'false' ->
                    {'false', SectionHost}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_value(atom(), wh_proplist()) -> lists().
get_values(Key, Sections) ->
    get_values(Sections, Key, []).

get_values([], _, []) ->
    [];
get_values([], _, Acc) ->
    Acc;
get_values([{_, Values} | T], Key, Acc) ->
    V = proplists:get_all_values(Key, Values),
    get_values(T, Key, lists:append([V | Acc])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load() -> {'ok', wh_proplist()} | _.
load() ->
    case os:getenv(?CONFIG_FILE_ENV) of
        'false' ->
            load_file(?CONFIG_FILE);
        File ->
            load_file(File)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_file(string()) -> {'ok', wh_proplist()} | _.
load_file(File) ->
    case zucchini:parse_file(File) of
        {'ok', Prop} ->
            lager:info("loading config file ~p~n", [File]),
            {'ok', Prop};
        {'error', _}=Error ->
            lager:error("error loading file ~p, ~p~n", [File, Error]),
            Error
    end.
