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

-export([get/2
         ,get/1
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
            {Section, get_sections(Section, Prop)};
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
    case load() of 
        {'ok', Prop} ->
            Sections = get_sections(Section, Prop),
            {Section, Key, get_values(Key, Sections)};
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
get_values(Key, Sections) ->
    get_values(Sections, Key, []).

get_values([], _, Acc) ->
     lists:reverse(lists:append(Acc));
get_values([{_, Values} | T], Key, Acc) ->
    V = proplists:get_all_values(Key, Values),
    get_values(T, Key, [V | Acc]).

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
