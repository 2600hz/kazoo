%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(lineman_util).

-export([get_target/0, get_target/1, get_target/2, get_target/3]).
-export([get_cookie/1]).
-export([get_host/1]).

-include("lineman.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to connect to another erlang node
%% @end
%%--------------------------------------------------------------------
-spec get_target/1 :: (string()) -> {'ok', atom()} | {'error', term()}.

get_target() ->
    get_target(lineman_config:get(<<"node">>, "ecallmgr")).

get_target(Node) ->
    case get_host(Node) of
        {error, _}=E -> E;
        {ok, Host} -> get_target(Node, Host)
    end.

get_target(Node, Host) ->
    case get_cookie(Node) of
        {error, _}=E -> E;
        {ok, Cookie} -> get_target(Node, Host, Cookie)
    end.

get_target(Node, Host, Cookie) ->            
    Target = list_to_atom(Node ++ "@" ++ Host),
    erlang:set_cookie(Target, Cookie),
    case net_adm:ping(Target) of
        pong ->
            lager:debug("connected to service '~s' with cookie '~s'~n", [Target, Cookie]),
            {ok, Target};
        pang ->
            lager:info("connection to service '~s' rejected~n", [Target]),            
            {error, pang};
        Else ->
            lager:info("connection to service '~s' failed: ~p~n", [Target, Else]),            
            {error, Else}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to find the configured cookie for an erlang node, or
%% automaticly read the vm.args for the given node
%% @end
%%--------------------------------------------------------------------
-spec get_cookie/1 :: (string()) -> {'ok', atom()} | 
                                    {'error', empty_cookie | cookie_not_found}.
get_cookie(Node) ->
    case {Node, lineman_config:get([Node, <<"cookie">>])} of
        {"whistle_apps", undefined} -> get_cookie_from_vmargs(?WHAPPS_VM_ARGS);
        {"ecallmgr", undefined} -> get_cookie_from_vmargs(?ECALL_VM_ARGS);
        {_, undefined} -> {ok, erlang:get_cookie()};
        {_, C} -> {ok, list_to_atom(C)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Looks in the default location for vm.args and tries to find the
%% setcookie value
%% @end
%%--------------------------------------------------------------------
-spec get_cookie_from_vmargs/1 :: (string()) -> {'ok', atom()} | 
                                                {'error', cookie_not_found}.
get_cookie_from_vmargs(File) ->
    case file:read_file(File) of
        {error, _} -> {error, cookie_not_found};
        {ok, Bin} ->
            case re:run(Bin, <<"-setcookie (.*)\\n">>, [{capture, [1], list}]) of
                {match, [Cookie]} -> {ok, list_to_atom(Cookie)};
                _Else -> {error, cookie_not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Attempts to find the configured host and ensure that it is 
%% routable by this system
%% @end
%%--------------------------------------------------------------------
-spec get_host/1 :: (string()) -> {'ok', string()} |
                                  {'error', host_unresolvable | unknown_host}.
get_host(Node) ->
    Host = lineman_config:get([Node, <<"host">>], net_adm:localhost()),
    case inet:gethostbyname(Host) of
        {ok, _} -> {ok, Host};
        {error, nxdomain} ->
            {error, host_unresolvable};
        {error, _Reason} ->
            lager:debug("unable to resolve host '~s': ~p", [Host, _Reason]),
            {error, unknown_host}
    end.
