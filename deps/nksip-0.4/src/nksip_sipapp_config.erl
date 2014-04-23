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

%% @doc <i>SipApps</i> management module.

-module(nksip_sipapp_config).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_config/1]).

-include("nksip.hrl").

-define(DEFAULT_LOG_LEVEL, 8).  % 8:debug, 7:info, 6:notice, 5:warning, 4:error
-define(DEFAULT_TRACE, false).


%% ===================================================================
%% Private
%% ===================================================================


%% @private
parse_config(Opts) ->
    try
        Opts1 = parse_opts(Opts, []),
        Cache = cache_syntax(Opts1),
        Plugins = nksip_lib:get_value(plugins, Opts1, []),
        Callbacks = callbacks_syntax(Plugins),
        AppName = nksip_lib:get_value(name, Opts1, nksip),
        AppId = nksip_sipapp_srv:get_appid(AppName),
        Module = nksip_lib:get_value(module, Opts1, nksip_sipapp),
        SipApp = sipapp_syntax(Module),
        Syntax = Cache ++ SipApp ++ Callbacks,
        ok = nksip_code_util:compile(AppId, Syntax),
        {ok, AppId} 
    catch
        throw:Throw -> {error, Throw}
    end.


%% @private Parse the list of app start options
parse_opts([], Opts) ->
    Opts;

parse_opts([Term|Rest], Opts) ->
    Opts1 = case Term of

        % Internal options
        {name, Name} ->
            [{name, Name}|Opts];
        {module, Module} when is_atom(Module) ->
            [{module, Module}|Opts];

        % Startup options
        {transports, Transports} ->
            [{transports, parse_transports(Transports, [])}|Opts];
        {certfile, File} ->
            [{certfile, nksip_lib:to_list(File)}|Opts];
        {keyfile, File} ->
            [{keyfile, nksip_lib:to_list(File)}|Opts];
        {supported, Supported} ->
            case nksip_parse:tokens(Supported) of
                error -> throw({invalid, supported});
                Tokens -> [{supported, [T||{T, _}<-Tokens]}|Opts]
            end;
        {allow, Allow} ->
            case nksip_parse:tokens(Allow) of
                error -> throw({invalid, allow});
                Tokens -> [{allow, [A||{A, _}<-Tokens]}|Opts]
            end;
        {accept, Accept} ->
            case nksip_parse:tokens(Accept) of
                error -> throw({invalid, accept});
                Tokens -> [{accept, [A||{A, _}<-Tokens]}|Opts]
            end;
        {events, Event} ->
            case nksip_parse:tokens(Event) of
                error -> throw({invalid, events});
                Tokens -> [{events, [T||{T, _}<-Tokens]}|Opts]
            end;
        
        % Default headers and options
        {from, From} ->
            case nksip_parse:uris(From) of
                [Uri] -> [{from, Uri}|Opts];
                _ -> throw({invalid, from}) 
            end;
        {route, Route} ->
            case nksip_parse:uris(Route) of
                error -> throw({invalid, route});
                Uris -> [{route, Uris}|Opts]
            end;
        {pass, Pass} ->
            [{pass, Pass}|Opts];
        {local_host, Host} ->
            [{local_host, nksip_lib:to_host(Host)}|Opts];
        {local_host6, Host} ->
            case nksip_lib:to_ip(Host) of
                {ok, HostIp6} -> 
                    % Ensure it is enclosed in `[]'
                    [{local_host6, nksip_lib:to_host(HostIp6, true)}|Opts];
                error -> 
                    [{local_host6, nksip_lib:to_binary(Host)}|Opts]
            end;
        no_100 ->
            [{no_100, true}|Opts];

        {plugins, List} when is_list(List) ->
            [{plugins, parse_plugins(List, [])}|Opts];

        {log_level, debug} -> [{log_level, 8}|Opts];
        {log_level, info} -> [{log_level, 7}|Opts];
        {log_level, notice} -> [{log_level, 6}|Opts];
        {log_level, warning} -> [{log_level, 5}|Opts];
        {log_level, error} -> [{log_level, 4}|Opts];
        {log_level, critical} -> [{log_level, 3}|Opts];
        {log_level, alert} -> [{log_level, 2}|Opts];
        {log_level, emergency} -> [{log_level, 1}|Opts];
        {log_level, none} -> [{log_level, 0}|Opts];
        {log_level, Level} when Level>=0, Level=<8 -> [{log_level, Level}|Opts];

        {register, Register} ->
            case nksip_parse:uris(Register) of
                error -> throw(invalid_register);
                Uris -> [{register, Uris}|Opts]
            end;
        {register_expires, Expires} when is_integer(Expires), Expires>0 ->
            [{register_expires, Expires}|Opts];
        registrar ->
            [{registrar, true}|Opts];
        {trace, Trace} when is_boolean(Trace) ->
            [{trace, Trace}|Opts];
        {store_trace, Trace} when is_boolean(Trace) ->
            [{store_trace, Trace}|Opts];

        % Unknown options
        {Name, Value} ->
            case nksip_config:parse_config(Name, Value) of
                {ok, Value1} -> 
                    nksip_lib:store_value(Name, Value1, Opts);
                {error, _} ->
                    PlugList = lists:reverse(nksip_lib:get_value(plugins, Opts, [])),
                    case parse_external_opt({Name, Value}, PlugList) of
                        {ok, Value1} -> nksip_lib:store_value(Name, Value1, Opts);
                        _ -> throw({invalid, Name})
                    end
            end;
        Name ->
            PlugList = lists:reverse(nksip_lib:get_value(plugins, Opts, [])),
            case parse_external_opt(Name, PlugList) of
                {ok, Value1} -> nksip_lib:store_value(Name, Value1, Opts);
                _ -> throw({invalid, Name})
            end

    end,
    parse_opts(Rest, Opts1).


%% @doc
parse_external_opt(_Opt, []) ->
    error;

parse_external_opt(Opt, [Plugin|Rest]) ->
    case catch Plugin:parse_config(Opt) of
        {ok, Value} -> {ok, Value};
        _ -> parse_external_opt(Opt, Rest)
    end.


%% @private
parse_transports([], Acc) ->
    lists:reverse(Acc);

parse_transports([Transport|Rest], Acc) ->
    case Transport of
        {Scheme, Ip, Port} -> TOpts = [];
        {Scheme, Ip, Port, TOpts} when is_list(TOpts) -> ok;
        _ -> Scheme=Ip=Port=TOpts=throw(invalid_transport)
    end,
    case 
        (Scheme==udp orelse Scheme==tcp orelse 
         Scheme==tls orelse Scheme==sctp orelse
         Scheme==ws  orelse Scheme==wss)
    of
        true -> ok;
        false -> throw(invalid_transport)
    end,
    Ip1 = case Ip of
        all ->
            {0,0,0,0};
        all6 ->
            {0,0,0,0,0,0,0,0};
        _ when is_tuple(Ip) ->
            case catch inet_parse:ntoa(Ip) of
                {error, _} -> throw(invalid_transport);
                {'EXIT', _} -> throw(invalid_transport);
                _ -> Ip
            end;
        _ ->
            case catch nksip_lib:to_ip(Ip) of
                {ok, PIp} -> PIp;
                _ -> throw(invalid_transport)
            end
    end,
    Port1 = case Port of
        any -> 0;
        _ when is_integer(Port), Port >= 0 -> Port;
        _ -> throw(invalid_transport)
    end,
    parse_transports(Rest, [{Scheme, Ip1, Port1, TOpts}|Acc]).


%% @private Parte the plugins list for the application
%% For each plugin, calls Plugin:version() to get the version, and
%% Plugin:deps() to get the dependency list ([Name::atom(), RE::binary()]).
%% it then builds a list of sorted list of plugin names, where every plugin 
%% is inserted after all of its dependencies.
parse_plugins([Name|Rest], PlugList) when is_atom(Name) ->
    case lists:keymember(Name, 1, PlugList) of
        true ->
            parse_plugins(Rest, PlugList);
        false ->
            case catch Name:version() of
                Ver when is_list(Ver); is_binary(Ver) ->
                    case catch Name:deps() of
                        Deps when is_list(Deps) ->
                            case parse_plugins_insert(PlugList, Name, Ver, Deps, []) of
                                {ok, PlugList1} -> 
                                    parse_plugins(Rest, PlugList1);
                                {insert, BasePlugin} -> 
                                    parse_plugins([BasePlugin, Name|Rest], PlugList)
                            end;
                        _ ->
                            throw({invalid_plugin, Name})
                    end;
                _ ->
                    throw({invalid_plugin, Name})
            end
    end;

parse_plugins([], PlugList) ->
    [Name || {Name, _} <- PlugList].


%% @private
parse_plugins_insert([{CurName, CurVer}=Curr|Rest], Name, Ver, Deps, Acc) ->
    case lists:keytake(CurName, 1, Deps) of
        false ->
            parse_plugins_insert(Rest, Name, Ver,  Deps, Acc++[Curr]);
        {value, {_, DepVer}, RestDeps} when is_list(DepVer); is_binary(DepVer) ->
            case re:run(CurVer, DepVer) of
                {match, _} ->
                    parse_plugins_insert(Rest, Name, Ver, RestDeps, Acc++[Curr]);
                nomatch ->
                    throw({incompatible_plugin, {CurName, CurVer, DepVer}})
            end;
        _ ->
            throw({invalid_plugin, Name})
    end;

parse_plugins_insert([], Name, Ver, [], Acc) ->
    {ok, Acc++[{Name, Ver}]};

parse_plugins_insert([], _Name, _Ver, [{DepName, _}|_], _Acc) ->
    {insert, DepName}.


%% @private Generates a ready-to-compile config getter functions
cache_syntax(Opts) ->
    Cache = [
        {name, nksip_lib:get_value(name, Opts)},
        {config, Opts},
        {config_log_level, nksip_lib:get_value(log_level, Opts, ?DEFAULT_LOG_LEVEL)},
        {config_trace, 
            {nksip_lib:get_value(trace, Opts, ?DEFAULT_TRACE), 
             nksip_lib:get_value(store_trace, Opts, false)}},
        {config_max_connections, nksip_lib:get_value(max_connections, Opts)},
        {config_max_calls, nksip_lib:get_value(max_calls, Opts)},
        {config_timers, {
            nksip_lib:get_value(timer_t1, Opts),
            nksip_lib:get_value(timer_t2, Opts),
            nksip_lib:get_value(timer_t4, Opts),
            1000*nksip_lib:get_value(timer_c, Opts),
            1000*nksip_lib:get_value(sipapp_timeout, Opts)}},
        {config_registrar_timers, {
            nksip_lib:get_value(registrar_min_time, Opts),
            nksip_lib:get_value(registrar_max_time, Opts),
            nksip_lib:get_value(registrar_default_time, Opts)}},
        {config_from, nksip_lib:get_value(from, Opts)},
        {config_registrar, lists:member({registrar, true}, Opts)},
        {config_no_100, lists:member({no_100, true}, Opts)},
        {config_supported, 
            nksip_lib:get_value(supported, Opts, ?SUPPORTED)},
        {config_allow, 
            case nksip_lib:get_value(allow, Opts) of
                undefined ->
                    case lists:member({registrar, true}, Opts) of
                        true -> <<(?ALLOW)/binary, ",REGISTER">>;
                        false -> ?ALLOW
                    end;
                Allow ->
                    Allow
            end},
        {config_accept, nksip_lib:get_value(accept, Opts)},
        {config_events, nksip_lib:get_value(events, Opts, [])},
        {config_route, nksip_lib:get_value(route, Opts, [])},
        {config_local_host, nksip_lib:get_value(local_host, Opts, auto)},
        {config_local_host6, nksip_lib:get_value(local_host6, Opts, auto)},
        {config_min_session_expires, nksip_lib:get_value(min_session_expires, Opts)},
        {config_uac, lists:flatten([
            tuple(local_host, Opts),
            tuple(local_host6, Opts),
            tuple(no_100, Opts),
            tuple(pass, Opts),
            tuple(from, Opts),
            tuple(route, Opts)
        ])},
        {config_uac_proxy, lists:flatten([
            tuple(local_host, Opts),
            tuple(local_host6, Opts),
            tuple(no_100, Opts),
            tuple(pass, Opts)
        ])},
        {config_uas, lists:flatten([
            tuple(local_host, Opts),
            tuple(local_host6, Opts)
        ])}
    ],
    lists:foldl(
        fun({Key, Value}, Acc) -> [nksip_code_util:getter(Key, Value)|Acc] end,
        [],
        Cache).


%% @private
tuple(Name, Opts) ->
    tuple(Name, Opts, []).


%% @private
tuple(Name, Opts, Default) ->
    case nksip_lib:get_value(Name, Opts) of
        undefined -> Default;
        Value -> {Name, Value}
    end.


%% @private Generates the ready-to-compile syntax of the generated callback module
%% taking all plugins' callback functions
callbacks_syntax(Plugins) ->
    callbacks_syntax(Plugins, dict:new()).


%% @private
callbacks_syntax([Name|Rest], Dict) ->
    Mod = list_to_atom(atom_to_list(Name)++"_callbacks"),
    case nksip_code_util:get_funs(Mod) of
        error ->
            callbacks_syntax(Rest, Dict);
        List ->
            Dict1 = callbacks_syntax(List, Mod, Dict),
            callbacks_syntax(Rest, Dict1)
    end;

callbacks_syntax([], Dict) ->
    dict:fold(
        fun({Fun, Arity}, {Value, Pos}, Syntax) ->
            [nksip_code_util:fun_expr(Fun, Arity, Pos, [Value])|Syntax]
        end,
        [],
        Dict).


%% @private
callbacks_syntax([{Fun, Arity}|Rest], Mod, Dict) ->
    case dict:find({Fun, Arity}, Dict) of
        error ->
            Pos = 1,
            Value = nksip_code_util:call_expr(Mod, Fun, Arity, Pos);
        {ok, {Syntax, Pos0}} ->
            Pos = Pos0+1,
            Value = nksip_code_util:case_expr(Mod, Fun, Arity, Pos, [Syntax])
    end,
    Dict1 = dict:store({Fun, Arity}, {Value, Pos}, Dict),
    callbacks_syntax(Rest, Mod, Dict1);

callbacks_syntax([], _, Dict) ->
    Dict.


%% @private Generates a ready-to-compile mirror of functions in sipapp module
sipapp_syntax(Mod) ->
    case nksip_code_util:get_funs(Mod) of
        error ->
            throw(invalid_sipapp_module);
        List ->
            lists:foldl(
                fun({Fun, Arity}, Acc) ->
                    [nksip_code_util:callback_expr(Mod, Fun, Arity)|Acc]
                end,
                [],
                List)
    end.














