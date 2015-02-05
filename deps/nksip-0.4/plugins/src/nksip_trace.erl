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

%% @doc NkSIP SIP basic message print and trace tool
%%
%% This module implements a simple but useful SIP trace utility. 
%% You can configure any SipApp to trace SIP messages sent or received
%% from specific IPs, to console or a disk file.
-module(nksip_trace).

-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile({no_auto_import, [get/1, put/2]}).

-export([version/0, deps/0, parse_config/1, terminate/2]).

-export([get_all/0, start/0, start/1, start/2, start/3, stop/0, stop/1]).
-export([print/1, print/2, sipmsg/5]).

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").

-type file() :: console | string() | binary().
-type ip_list() :: all | string() | binary() | [string()|binary()].


% ===================================================================
%% Plugin specific
%% ===================================================================

%% @doc Version
-spec version() ->
    string().

version() ->
    "0.1".


%% @doc Dependant plugins
-spec deps() ->
    [{atom(), string()}].
    
deps() ->
    [].


%% @doc Parses this plugin specific configuration
-spec parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.

parse_config(Opts) ->
    Defaults = [{nksip_trace, {console, all}}],
    Opts1 = nksip_lib:defaults(Opts, Defaults),    
    try
        {File, IpList} = case nksip_lib:get_value(nksip_trace, Opts1) of
            {File0, IpList0} -> 
                case norm_file(File0) of
                    {ok, File1} ->
                        case norm_iplist(IpList0, []) of
                            {ok, IpList1} -> {File1, IpList1};
                            error -> throw({invalid_re, IpList0})
                        end;
                    error ->
                        throw({invalid_file, File0})
                end;
            File0 -> 
                case norm_file(File0) of
                    {ok, File1} -> 
                        {File1, all};
                    error ->
                        throw({invalid_file, File0})
                end
        end,
        AppId = nksip_lib:get_value(id, Opts1),
        close_file(AppId),
        case open_file(AppId, File) of
            ok -> 
                case compile_ips(IpList, []) of
                    {ok, CompIpList} ->
                        Cached1 = nksip_lib:get_value(cached_configs, Opts1, []),
                        Cached2 = nksip_lib:store_value(config_nksip_trace,
                                                        {File, CompIpList}, Cached1),
                        Opts2 = nksip_lib:store_value(cached_configs, Cached2, Opts1),
                        {ok, Opts2};
                    error ->
                        throw({invalid_re, IpList})
                end;
            error -> 
                throw({invalid_config, {could_not_open, File}})
        end
    catch
        throw:Error -> {error, {invalid_config, Error}}
    end.


%% @doc Called when the plugin is shutdown
-spec terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_sipapp_srv:state()}.

terminate(AppId, SipAppState) ->  
    close_file(AppId),
    {ok, SipAppState}.



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Get all SipApps currently tracing messages.
-spec get_all() ->
    [{App::nksip:app_name(), File::console|binary(), IpList::all|[binary()]}].

get_all() ->
    Fun = fun({AppName, AppId}, Acc) ->
        case nksip_sipapp_srv:config(AppId, nksip_trace) of
            undefined -> Acc;
            {File, IpList} -> [{AppName, File, IpList}]
        end
    end,
    lists:foldl(Fun, [], nksip:get_all()).


%% @doc Equivalent to `start(AppId, console, all)' for all started SipApps.
-spec start() -> 
    [{nksip:app_name(), ok|{error, term()}}].

start() -> 
    lists:map(fun({AppName, AppId}) -> {AppName, start(AppId)} end, nksip:get_all()).


%% @doc Equivalent to `start(AppId, console, all)'.
-spec start(nksip:app_id()|nksip:app_name()) -> 
    ok | {error, term()}.

start(AppId) -> 
    start(AppId, console, all).


%% @doc Equivalent to `start(AppId, File, all)'.
-spec start(nksip:app_id()|nksip:app_name(), file()) -> 
    ok | {error, term()}.

start(AppId, File) -> 
    start(AppId, File, all).


%% @doc Configures a SipApp to start tracing SIP messages.
-spec start(nksip:app_id()|nksip:app_id(), file(), ip_list()) ->
    ok | {error, term()}.

start(App, File, IpList) ->
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            Plugins1 = AppId:config_plugins(),
            Plugins2 = nksip_lib:store_value(nksip_trace, Plugins1),
            case nksip:update(AppId, [{plugins, Plugins2}, {nksip_trace, {File, IpList}}]) of
                {ok, _} -> ok;
                {error, Error} -> {error, Error}
            end;
        not_found ->
            {error, sipapp_not_found}
    end.


%% @doc Stop all tracing processes, closing all open files.
-spec stop() -> 
    ok.

stop() ->
    lists:map(fun({AppName, AppId}) -> {AppName, stop(AppId)} end, nksip:get_all()).


%% @doc Stop tracing a specific trace process, closing file if it is opened.
-spec stop(nksip:app_id()|nksip:app_name()) ->
    ok | {error, term()}.

stop(App) ->
    case nksip:find_app_id(App) of
        {ok, AppId} ->
            Plugins = AppId:config_plugins() -- [nksip_trace],
            case nksip:update(App, [{plugins, Plugins}]) of
                {ok, _} -> ok;
                {error, Error} -> {error, Error}
            end;
        not_found ->
            {error, sipapp_not_found}
    end.    


%% @doc Pretty-print a `Request' or `Response'.
-spec print(Input::nksip:request()|nksip:response()) ->
 ok.

print(#sipmsg{}=SipMsg) -> 
    print(<<>>, SipMsg).


%% @doc Pretty-print a `Request' or `Response' with a tag.
-spec print(string()|binary(), Input::nksip:request()|nksip:response()) ->
    ok.

print(Header, #sipmsg{}=SipMsg) ->
    Binary = nksip_unparse:packet(SipMsg),
    Lines = [
        [<<"        ">>, Line, <<"\n">>]
        || Line <- binary:split(Binary, <<"\r\n">>, [global])
    ],
    io:format("\n        ---- ~s\n~s\n", [Header, list_to_binary(Lines)]).


%% @private
-spec sipmsg(nksip:app_id(), nksip:call_id(), binary(), 
             nksip_transport:transport(), binary()) ->
    ok.

sipmsg(AppId, _CallId, Header, Transport, Binary) ->
    case AppId:config_nksip_trace() of
        {File, all} ->
            AppName = AppId:name(),
            Msg = print_packet(AppName, Header, Transport, Binary),
            write(AppId, File, Msg);
        {File, IpList} ->
            #transport{local_ip=Ip1, remote_ip=Ip2} = Transport,
            case has_ip([Ip1, Ip2], IpList) of
                true ->
                    AppName = AppId:name(),
                    Msg = print_packet(AppName, Header, Transport, Binary),
                    write(AppId, File, Msg);
                false ->
                    ok
            end;
        _ ->
            ok
    end.


%% ===================================================================
%% Private
%% ===================================================================


%% @private
norm_file(console) -> 
    {ok, console};
norm_file(File) when is_binary(File) ->
    {ok, File};
norm_file(File) when is_list(File) -> 
    {ok, list_to_binary(File)};
norm_file(_) -> 
    error.

%% @private
norm_iplist([], Acc) -> 
    {ok, lists:reverse(Acc)};
norm_iplist(all, []) -> 
    {ok, all};
norm_iplist(List, Acc) when is_integer(hd(List)) -> 
    norm_iplist(list_to_binary(List), Acc);
norm_iplist([Ip|Rest], Acc) when is_binary(Ip) -> 
    norm_iplist(Rest, [Ip|Acc]);
norm_iplist([Ip|Rest], Acc) when is_list(Ip) -> 
    norm_iplist(Rest, [list_to_binary(Ip)|Acc]);
norm_iplist([Re|Rest], Acc) when element(1, Re)==re_pattern ->
    norm_iplist(Rest, [Re|Acc]);
norm_iplist([_|_], _bAcc) ->
    error;
norm_iplist(Term, Acc) ->
    norm_iplist([Term], Acc).


%% @private
close_file(AppId) ->
    case nksip_config:get({nksip_trace_file, AppId}) of
        undefined -> 
            ok;
        {File, OldDevice} ->
            ?notice(AppId, <<>>, "Closing file ~s (~p)", [File, OldDevice]),
            nksip_config:del({nksip_trace_file, AppId}),
            file:close(OldDevice),
            ok
    end.
 

%% @private
open_file(_AppId, console) ->
    ok;

open_file(AppId, File) ->
    case file:open(binary_to_list(File), [append]) of
        {ok, IoDevice} -> 
            ?notice(AppId, <<>>, "File ~s opened for trace (~p)", [File, IoDevice]),
            nksip_config:put({nksip_trace_file, AppId}, {File, IoDevice}),
            ok;
        {error, _Error} -> 
            error
    end.


%% @private
compile_ips(all, []) ->
    {ok, all};

compile_ips([], Acc) ->
    {ok, lists:reverse(Acc)};

compile_ips([Ip|Rest], Acc) when is_binary(Ip) ->
    case re:compile(Ip) of
        {ok, Comp} -> compile_ips(Rest, [Comp|Acc]);
        {error, _Error} -> error
    end;

compile_ips([Re|Rest], Acc) when element(1, Re)==re_pattern ->
    compile_ips(Rest, [Re|Acc]).


%% @private
write(AppId, File, Msg) -> 
    Time = nksip_lib:l_timestamp_to_float(nksip_lib:l_timestamp()), 
    case File of
        console ->
            io:format("\n        ---- ~f ~s", [Time, Msg]);
        _ ->
            case nksip_config:get({nksip_trace_file, AppId}) of
                {File, Device} ->
                    Txt = io_lib:format("\n        ---- ~f ~s", [Time, Msg]),
                    catch file:write(Device, Txt);
                _ ->
                    ok
            end
    end.


%% @private
print_packet(AppId, Info, 
                #transport{
                    proto = Proto,
                    local_ip = LIp, 
                    local_port = LPort, 
                    remote_ip = RIp, 
                    remote_port = RPort
                }, 
                Binary) ->
    case catch inet_parse:ntoa(RIp) of
        {error, _} -> RHost = <<"undefined">>;
        {'EXIT', _} -> RHost = <<"undefined">>;
        RHost -> ok
    end,
    case catch inet_parse:ntoa(LIp) of
        {error, _} -> LHost = <<"undefined">>;
        {'EXIT', _} -> LHost = <<"undefined">>;
        LHost -> ok
    end,
    Lines = [
        [<<"        ">>, Line, <<"\n">>]
        || Line <- binary:split(Binary, <<"\r\n">>, [global])
    ],
    io_lib:format("~p ~s ~s:~p (~p, ~s:~p) (~p)\n\n~s", 
                    [AppId, Info, RHost, RPort, 
                    Proto, LHost, LPort, self(), list_to_binary(Lines)]).



%% @private
has_ip([], _) ->
    false;
has_ip([Ip|Rest], IpList) ->
    case has_ip2(Ip, IpList) of
        true -> true;
        false -> has_ip(Rest, IpList)
    end.


%% @private
has_ip2(_Ip, []) ->
    false;
has_ip2(Ip, [Re|Rest]) ->
    case re:run(inet_parse:ntoa(Ip), Re) of
        {match, _} -> true;
        nomatch -> has_ip2(Ip, Rest)
    end.





