%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc SipApps configuration
-module(nksip_sipapp_config).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([start/1, update/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Private
%% ===================================================================


%% @private Default config values
-spec default_config() ->
    nksip:optslist().

default_config() ->
    [
        {log_level, notice},
        {allow, "INVITE,ACK,CANCEL,BYE,OPTIONS,INFO,UPDATE,"
                "SUBSCRIBE,NOTIFY,REFER,MESSAGE"},
        {supported, "path"},
        {timer_t1, 500},                    % (msecs) 0.5 secs
        {timer_t2, 4000},                   % (msecs) 4 secs
        {timer_t4, 5000},                   % (msecs) 5 secs
        {timer_c,  180},                    % (secs) 3min
        {udp_timeout, 30},                  % (secs) 30 secs
        {tcp_timeout, 180},                 % (secs) 3 min
        {sctp_timeout, 180},                % (secs) 3 min
        {ws_timeout, 180},                  % (secs) 3 min
        {trans_timeout, 900},               % (secs) 15 min
        {dialog_timeout, 1800},             % (secs) 30 min
        {event_expires, 60},                % (secs) 1 min
        {event_expires_offset, 5},          % (secs) 5 secs
        {nonce_timeout, 30},                % (secs) 30 secs
        {max_calls, 100000},                % Each Call-ID counts as a call
        {max_connections, 1024},            % Per transport and SipApp
        {debug, false}                      % Used in nksip_debug plugin
    ].


%% @private
start(Opts) ->
    AppName = nksip_lib:get_value(name, Opts, nksip),
    AppId = nksip_sipapp_srv:get_appid(AppName),
    BasePath = nksip_config_cache:local_data_path(),
    {ok, UUID} = nksip_sipapp_srv:update_uuid(AppId, AppName, BasePath),
    Environment = nksip_config_cache:app_config(),
    Defaults = nksip_lib:defaults(Environment, default_config()),
    Opts1 = lists:map(
        fun(T) -> case T of {K, V} -> {K, V}; _ -> {T, true} end end, 
        Opts),
    Opts2 = nksip_lib:defaults(Opts1, Defaults),
    case parse_start(AppId, UUID, Opts2) of
        {ok, AppId, _Plugins, Syntax} ->
            {ok, Tree} = nksip_code_util:compile(AppId, Syntax),
            ok = nksip_code_util:write(AppId, Tree),
            {ok, AppId};
        {error, ParseError} ->
            {error, ParseError}
    end.


%% @private
update(AppId, Opts) ->
    Opts1 = nksip_lib:delete(Opts, transport),
    Opts2 = lists:map(
        fun(T) -> case T of {K, V} -> {K, V}; _ -> {T, true} end end, 
        Opts1),
    OldConfig = AppId:config(),
    Opts3 = Opts2 ++ nksip_lib:delete(OldConfig, [K || {K, _} <- Opts]),
    UUID = nksip_lib:get_value(uuid, OldConfig),
    case parse_start(AppId, UUID, Opts3) of
        {ok, AppId, NewPlugins, Syntax} ->
            OldPlugins = AppId:config_plugins(),
            case OldPlugins--NewPlugins of
                [] -> 
                    ok;
                ToStop -> 
                    nksip_sipapp_srv:stop_plugins(AppId, lists:reverse(ToStop))
            end,
            {ok, Tree} = nksip_code_util:compile(AppId, Syntax),
            ok = nksip_code_util:write(AppId, Tree),
            case NewPlugins--OldPlugins of
                [] -> 
                    ok;
                ToStart -> 
                    nksip_sipapp_srv:start_plugins(AppId, ToStart)
            end,
            {ok, AppId};
        {error, Error} ->
            {error, Error}
    end.


%% @private
parse_start(AppId, UUID, Opts) ->
    try
        Opts1 = nksip_lib:delete(Opts, [id, uuid, sorted_plugins, cached_configs]),
        Opts2 = parse_opts(Opts1, []),
        Plugins = sort_plugins(nksip_lib:get_value(plugins, Opts2, []), []),
        Opts3 = [
            {id, AppId},
            {uuid, UUID}, 
            {sorted_plugins, Plugins}, 
            {cached_configs, []}
            |
            Opts2
        ],
        Opts4 = parse_plugins_opts(Plugins, Opts3),
        Cache = cache_syntax(Opts4),
        PluginCallbacks = plugin_callbacks_syntax([nksip|Plugins]),
        PluginModules = [
            list_to_atom(atom_to_list(Plugin) ++ "_sipapp")
            || Plugin <- Plugins
        ],
        Module = nksip_lib:get_value(module, Opts4, nksip_sipapp),
        AppModules = [Module|PluginModules++[nksip_sipapp]],
        AppCallbacks = get_all_app_callbacks(AppModules),
        SipApp = [
            nksip_code_util:callback_expr(Mod, Fun, Arity)
            || {{Fun, Arity}, Mod} <- AppCallbacks
        ],
        Syntax = Cache ++ SipApp ++ PluginCallbacks,
        {ok, AppId, Plugins, Syntax} 
    catch
        throw:Throw -> {error, Throw}
    end.


%% @private Parte the plugins list for the application
%% For each plugin, calls Plugin:version() to get the version, and
%% Plugin:deps() to get the dependency list ([Name::atom(), RE::binary()]).
%% it then builds a list of sorted list of plugin names, where every plugin 
%% is inserted after all of its dependencies.
sort_plugins([Name|Rest], PlugList) when is_atom(Name) ->
    case lists:keymember(Name, 1, PlugList) of
        true ->
            sort_plugins(Rest, PlugList);
        false ->
            case catch Name:version() of
                Ver when is_list(Ver); is_binary(Ver) ->
                    case catch Name:deps() of
                        Deps when is_list(Deps) ->
                            case insert_plugins(PlugList, Name, Ver, Deps, []) of
                                {ok, PlugList1} -> 
                                    sort_plugins(Rest, PlugList1);
                                {insert, BasePlugin} -> 
                                    sort_plugins([BasePlugin, Name|Rest], PlugList)
                            end;
                        _ ->
                            throw({invalid_plugin, Name})
                    end;
                _ ->
                    throw({invalid_plugin, Name})
            end
    end;

sort_plugins([], PlugList) ->
    [Name || {Name, _} <- PlugList].


%% @private
insert_plugins([{CurName, CurVer}=Curr|Rest], Name, Ver, Deps, Acc) ->
    case lists:keytake(CurName, 1, Deps) of
        false ->
            insert_plugins(Rest, Name, Ver,  Deps, Acc++[Curr]);
        {value, {_, DepVer}, RestDeps} when is_list(DepVer); is_binary(DepVer) ->
            case re:run(CurVer, DepVer) of
                {match, _} ->
                    insert_plugins(Rest, Name, Ver, RestDeps, Acc++[Curr]);
                nomatch ->
                    throw({incompatible_plugin, {CurName, CurVer, DepVer}})
            end;
        _ ->
            throw({invalid_plugin, Name})
    end;

insert_plugins([], Name, Ver, [], Acc) ->
    {ok, Acc++[{Name, Ver}]};

insert_plugins([], _Name, _Ver, [{DepName, _}|_], _Acc) ->
    {insert, DepName}.


%% @private Parse the list of app start options
parse_opts([], Opts) ->
    Opts;

parse_opts([Term|Rest], Opts) ->
    Op = case Term of

        % Internal options
        {name, _Name} -> update;
        {module, Module} when is_atom(Module) -> update;

        % System options
        {timer_t1, MSecs} when is_integer(MSecs), MSecs>=10, MSecs=<2500 -> update;
        {timer_t1, _} -> error;
        {timer_t2, MSecs} when is_integer(MSecs), MSecs>=100, MSecs=<16000 -> update;
        {timer_t2, _} -> error;
        {timer_t4, MSecs} when is_integer(MSecs), MSecs>=100, MSecs=<25000 -> update;
        {timer_t4, _} -> error;
        {timer_c, Secs}  when is_integer(Secs), Secs>=1 -> update;
        {timer_c, _} -> error;
        {udp_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {udp_timeout, _} -> error;
        {tcp_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {tcp_timeout, _} -> error;
        {sctp_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {sctp_timeout, _} -> error;
        {ws_timeout, Secs} when is_integer(Secs), Secs>=5 ->  update;
        {ws_timeout, _} -> error;
        {trans_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {trans_timeout, _} -> error;
        {dialog_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {dialog_timeout, _} -> error;
        {event_expires, Secs} when is_integer(Secs), Secs>=1 -> update;
        {event_expires, _} -> error;
        {event_expires_offset, Secs} when is_integer(Secs), Secs>=0 -> update;
        {event_expires_offset, _} -> error;
        {nonce_timeout, Secs} when is_integer(Secs), Secs>=5 -> update;
        {nonce_timeout, _} -> error;
        {max_calls, Max} when is_integer(Max), Max>=1, Max=<1000000 -> update;
        {max_calls, _} -> error;
        {max_connections, Max} when is_integer(Max), Max>=1, Max=<1000000 -> update;
        {max_connections, _} -> error;

        % Startup options
        {transports, Transports} ->
            {update, parse_transports(Transports, [])};
        {certfile, File} ->
            {update, nksip_lib:to_list(File)};
        {keyfile, File} ->
            {update, nksip_lib:to_list(File)};
        {supported, Supported} ->
            case nksip_parse:tokens(Supported) of
                error -> error;
                Tokens -> {update, [T||{T, _}<-Tokens]}
            end;
        {allow, Allow} ->
            case nksip_parse:tokens(Allow) of
                error -> error;
                Tokens -> {update, [A||{A, _}<-Tokens]}
            end;
        {accept, Accept} ->
            case nksip_parse:tokens(Accept) of
                error -> error;
                Tokens -> {update, [A||{A, _}<-Tokens]}
            end;
        {events, Event} ->
            case nksip_parse:tokens(Event) of
                error -> error;
                Tokens -> {update, [T||{T, _}<-Tokens]}
            end;
        
        % Default headers and options
        {from, From} ->
            case nksip_parse:uris(From) of
                [Uri] -> {update, Uri};
                _ -> error 
            end;
        {route, Route} ->
            case nksip_parse:uris(Route) of
                error -> error;
                Uris -> {update, Uris}
            end;
        {local_host, Host} ->
            {update, nksip_lib:to_host(Host)};
        {local_host6, Host} ->
            case nksip_lib:to_ip(Host) of
                {ok, HostIp6} -> 
                    % Ensure it is enclosed in `[]'
                    {update, nksip_lib:to_host(HostIp6, true)};
                error -> 
                    {update, nksip_lib:to_binary(Host)}
            end;
        {no_100, true} ->
            {update, true};
        {no_100, _} ->
            error;

        {debug, Term} -> {update, Term};

        {log_level, debug} -> {update, 8};
        {log_level, info} -> {update, 7};
        {log_level, notice} -> {update, 6};
        {log_level, warning} -> {update, 5};
        {log_level, error} -> {update, 4};
        {log_level, critical} -> {update, 3};
        {log_level, alert} -> {update, 2};
        {log_level, emergency} -> {update, 1};
        {log_level, none} -> {update, 0};
        {log_level, Level} when Level>=0, Level=<8 -> {update, Level};
        {log_level, _} -> error;

        _Other ->
            update
    end,
    Opts1 = case Op of
        % unknown ->
        %     [Term|Opts];
        error when is_tuple(Term) -> 
            throw({invalid_config, element(1, Term)});
        error -> 
            throw({invalid_config, Term});
        _ ->
            {Key, Val} = case Op of
                update -> {element(1, Term), element(2, Term)};
                {update, Val1} -> {element(1, Term), Val1}
                % {update, Key1, Val1} -> {Key1, Val1}
            end,
            case lists:keymember(Key, 1, Opts) of
                true -> throw({invalid_config, {duplicated_key, Key}});
                false -> [{Key, Val}|Opts]
            end
    end,
    parse_opts(Rest, Opts1).


%% @private
parse_plugins_opts([], Opts) ->
    Opts;

parse_plugins_opts([Plugin|RestPlugins], Opts) ->
    case erlang:function_exported(Plugin, parse_config, 1) of
        true -> 
            case Plugin:parse_config(Opts) of
                {ok, Opts1} ->
                    parse_plugins_opts(RestPlugins, Opts1);
                {error, Error} ->
                    throw(Error)
            end;
        false ->
            parse_plugins_opts(RestPlugins, Opts)
    end.



%% @private
parse_transports([], Acc) ->
    lists:reverse(Acc);

parse_transports([Transport|Rest], Acc) ->
    case Transport of
        {Scheme, Ip, Port, TOpts} when is_list(TOpts) -> ok;
        {Scheme, Ip, Port} -> TOpts = [];
        {Scheme, Ip} -> Port = any, TOpts = [];
        Scheme -> Ip = all, Port = any, TOpts = []
    end,
    case 
        (Scheme==udp orelse Scheme==tcp orelse 
         Scheme==tls orelse Scheme==sctp orelse
         Scheme==ws  orelse Scheme==wss)
    of
        true -> ok;
        false -> throw({invalid_transport, Transport})
    end,
    Ip1 = case Ip of
        all ->
            {0,0,0,0};
        all6 ->
            {0,0,0,0,0,0,0,0};
        _ when is_tuple(Ip) ->
            case catch inet_parse:ntoa(Ip) of
                {error, _} -> throw({invalid_transport, Transport});
                {'EXIT', _} -> throw({invalid_transport, Transport});
                _ -> Ip
            end;
        _ ->
            case catch nksip_lib:to_ip(Ip) of
                {ok, PIp} -> PIp;
                _ -> throw({invalid_transport, Transport})
            end
    end,
    Port1 = case Port of
        any -> 0;
        _ when is_integer(Port), Port >= 0 -> Port;
        _ -> throw({invalid_transport, Transport})
    end,
    parse_transports(Rest, [{Scheme, Ip1, Port1, TOpts}|Acc]).



%% @private Generates a ready-to-compile config getter functions
cache_syntax(Opts) ->
    Cache = [
        {name, nksip_lib:get_value(name, Opts)},
        {module, nksip_lib:get_value(module, Opts)},
        {uuid, nksip_lib:get_value(uuid, Opts)},
        {config, Opts},
        {config_plugins, nksip_lib:get_value(sorted_plugins, Opts, [])},
        {config_log_level, nksip_lib:get_value(log_level, Opts)},
        {config_debug, nksip_lib:get_value(debug, Opts)},
        {config_max_connections, nksip_lib:get_value(max_connections, Opts)},
        {config_max_calls, nksip_lib:get_value(max_calls, Opts)},
        {config_timers, #call_timers{
            t1 = nksip_lib:get_value(timer_t1, Opts),
            t2 = nksip_lib:get_value(timer_t2, Opts),
            t4 = nksip_lib:get_value(timer_t4, Opts),
            tc = nksip_lib:get_value(timer_c, Opts),
            trans = nksip_lib:get_value(trans_timeout, Opts),
            dialog = nksip_lib:get_value(dialog_timeout, Opts)}},
        {config_from, nksip_lib:get_value(from, Opts)},
        {config_no_100, lists:member({no_100, true}, Opts)},
        {config_supported, nksip_lib:get_value(supported, Opts)},
        {config_allow, nksip_lib:get_value(allow, Opts)},
        {config_accept, nksip_lib:get_value(accept, Opts)},
        {config_events, nksip_lib:get_value(events, Opts, [])},
        {config_route, nksip_lib:get_value(route, Opts, [])},
        {config_local_host, nksip_lib:get_value(local_host, Opts, auto)},
        {config_local_host6, nksip_lib:get_value(local_host6, Opts, auto)}
    ] 
    ++
    [
        {Fun, Value} ||
        {Fun, Value} <- nksip_lib:get_value(cached_configs, Opts, [])
    ],
    lists:foldl(
        fun({Key, Value}, Acc) -> [nksip_code_util:getter(Key, Value)|Acc] end,
        [],
        Cache).


% %% @private
% tuple(Name, Opts) ->
%     tuple(Name, Opts, []).


% %% @private
% tuple(Name, Opts, Default) ->
%     case nksip_lib:get_value(Name, Opts) of
%         undefined -> Default;
%         Value -> {Name, Value}
%     end.


%% @private Generates the ready-to-compile syntax of the generated callback module
%% taking all plugins' callback functions
plugin_callbacks_syntax(Plugins) ->
    plugin_callbacks_syntax(Plugins, dict:new()).


%% @private
plugin_callbacks_syntax([Name|Rest], Dict) ->
    Mod = list_to_atom(atom_to_list(Name)++"_callbacks"),
    case nksip_code_util:get_funs(Mod) of
        error ->
            plugin_callbacks_syntax(Rest, Dict);
        List ->
            Dict1 = plugin_callbacks_syntax(List, Mod, Dict),
            plugin_callbacks_syntax(Rest, Dict1)
    end;

plugin_callbacks_syntax([], Dict) ->
    dict:fold(
        fun({Fun, Arity}, {Value, Pos}, Syntax) ->
            [nksip_code_util:fun_expr(Fun, Arity, Pos, [Value])|Syntax]
        end,
        [],
        Dict).


%% @private
plugin_callbacks_syntax([{Fun, Arity}|Rest], Mod, Dict) ->
    case dict:find({Fun, Arity}, Dict) of
        error ->
            Pos = 1,
            Value = nksip_code_util:call_expr(Mod, Fun, Arity, Pos);
        {ok, {Syntax, Pos0}} ->
            Pos = Pos0+1,
            Value = nksip_code_util:case_expr(Mod, Fun, Arity, Pos, [Syntax])
    end,
    Dict1 = dict:store({Fun, Arity}, {Value, Pos}, Dict),
    plugin_callbacks_syntax(Rest, Mod, Dict1);

plugin_callbacks_syntax([], _, Dict) ->
    Dict.


%% @private Extracts all defined app callbacks
-spec get_all_app_callbacks([atom()]) ->
    [{{Fun::atom(), Arity::integer()}, Mod::atom()}].

get_all_app_callbacks(ModList) ->
    get_all_app_callbacks(ModList, []).


%% @private
get_all_app_callbacks([Mod|Rest], Acc) ->
    Acc1 = case nksip_code_util:get_funs(Mod) of
        error ->
            Acc;
        List ->
            lists:foldl(
                fun({Fun, Arity}, FAcc) ->
                    case lists:keymember({Fun, Arity}, 1, Acc) of
                        true -> FAcc;
                        false -> [{{Fun, Arity}, Mod}|FAcc]
                    end
                end,
                Acc,
                List)
    end,
    get_all_app_callbacks(Rest, Acc1);

get_all_app_callbacks([], Acc) ->
    Acc.










