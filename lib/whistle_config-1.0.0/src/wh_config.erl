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

-export([get/1
         ,get/2
         ,get_default/3
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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return a section of the config file
%% @end
%%--------------------------------------------------------------------
get(Section) ->
    case load() of 
        {'ok', Prop} ->
            get_sections(Section, Prop);
        {'error', E} ->
            {'error', E}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
get(Section, Key) ->
    get(Section, Key, 'undefined').


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
get_atom(Section, Key) ->
    get(Section, Key, 'atom').

get_atom(Section, Key, Default) ->
    get_default(Section, Key, Default, 'atom').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
get_integer(Section, Key) ->
    get(Section, Key, 'integer').

get_integer(Section, Key, Default) ->
    get_default(Section, Key, Default, 'integer').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
get_string(Section, Key) ->
    get(Section, Key, 'string').

get_string(Section, Key, Default) ->
    get_default(Section, Key, Default, 'string').


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Return values of the config file
%% @end
%%--------------------------------------------------------------------
get_default(Section, Key, Default) ->
    get_default(Section, Key, Default, 'undefined').

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get(Section, Keys, Type) when is_list(Keys) ->
    V = lists:foldl(fun(Key, Acc) ->
                            [V] = get(Section, Key, Type),
                            [V | Acc]
                    end, [], Keys),
    lists:reverse(V);
get(Section, Key, Type) ->
    case load() of 
        {'ok', Prop} ->
            Sections = get_sections(Section, Prop),
            get_values(Key, Sections, Type);
        {'error', _E} ->
            _E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_default(Section, Key, Default, Type) ->
    case load() of 
        {'ok', Prop} ->
            Sections = get_sections(Section, Prop),
            case get_values(Key, Sections, Type) of
                [] ->
                    Default;
                Values ->
                    Values
            end;
        {'error', _E} ->
            _E
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_sections(Section, Prop) ->
    Sections = proplists:get_all_values(Section, Prop),
    format_sections(Sections).
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
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
get_values(Key, Sections, Type) ->
    get_values(Sections, Key, [], Type).

get_values([], _, [], _) ->
    [];
get_values([], _, [Res | []], Type) ->
    format_value(Res, Type);
get_values([], _, Acc, Type) ->
    format_values(Acc, Type);
get_values([{_, Values} | T], Key, Acc, Type) ->
    V = proplists:get_all_values(Key, Values),
    get_values(T, Key, lists:append([V | Acc]), Type).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
format_values(Values, Type) when is_list(Values) ->
    lists:foldr(fun(V, Acc) -> 
                        [format_value(V, Type) | Acc]
                end, [], Values).

format_value(Value, Type) ->
    case Type of
        'atom' ->
            wh_util:to_atom(Value);
        'string' ->
            wh_util:to_lower_string(Value);
        'integer' ->
            wh_util:to_integer(Value);
        _ ->
            Value
    end.








    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
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
load_file(File) ->
    case zucchini:parse_file(File) of
        {'ok', Prop} ->
            lager:info("loading config file ~p~n", [File]),
            {'ok', Prop};
        Error ->
            lager:error("error loading file ~p, ~p~n", [File ,Error]),
            Error
    end.
