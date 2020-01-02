%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_parsers_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/2
        ,stop_child/1
        ,children/0
        ]).

-include("call_inspector.hrl").

-define(SERVER, ?MODULE).

-define(PARSER_TYPE_HEP, <<"hep">>).
-define(PARSER_TYPE_FREESWITCH, <<"fs_log">>).
-define(PARSER_TYPE_KAMAILIO, <<"km_log">>).
-define(PARSER_TYPE, <<"type">>).
-define(PARSER_IP, <<"ip">>).
-define(PARSER_PORT, <<"port">>).
-define(PARSER_LOGFILE, <<"logfile">>).
%% Example config:
%%   "startup_parsers": {
%%       "name_for_my_first_parser": {
%%           "type": "hep",
%%           "ip": "107.234.143.94",
%%           "port": "9061"
%%       }
%%   }

-define(DEFAULT_PARSERS, kapps_config:get_json(?CONFIG_CAT, <<"startup_parsers">>, kz_json:new())).

-define(CHILDREN, parsers_from_config(?DEFAULT_PARSERS)).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

-type parser() :: 'ci_parser_freeswitch' | 'ci_parser_kamailio' | 'ci_parser_hep'.
-spec start_child(parser(), [ci_parsers_util:parser_args()]) ->
          {'ok', atom()} |
          {'error', term()}.
start_child(Module, Args) ->
    Id = ci_parsers_util:make_name(lists:keyfind('parser_args', 1, Args)),
    ChildSpec = ?WORKER_NAME_ARGS(Module, Id, Args),
    case supervisor:start_child(?SERVER, ChildSpec) of
        {'ok', _Pid} -> {'ok', Id};
        {'error', {'already_started', _Pid}} -> {'ok', Id};
        {'error', _}=Error -> Error
    end.

-spec stop_child(atom()) -> 'ok'.
stop_child(Id) ->
    'ok' = supervisor:terminate_child(?SERVER, Id),
    'ok' = supervisor:delete_child(?SERVER, Id).

-spec children() -> [atom()].
children() ->
    [Id
     || {Id, _Pid, _Type, _Modules} <- supervisor:which_children(?SERVER)
    ].

%% Internals

parsers_from_config(JObj) ->
    lager:debug("read parsers config: ~s", [kz_json:encode(JObj)]),
    [ChildSpec
     || {Name, SubJObj} <- kz_json:to_proplist(JObj),
        ChildSpec <- [config_to_childspec(Name, kz_json:to_map(SubJObj))],
        ok =/= ChildSpec
    ].

config_to_childspec(Name, #{?PARSER_TYPE := ?PARSER_TYPE_HEP
                           ,?PARSER_IP := IP
                           ,?PARSER_PORT := Port
                           }) ->
    Module = ci_parser_hep,
    lager:info("adding child ~s '~s' ~p ~p", [Module, Name, IP, Port]),
    Args = [{parser_args, kz_term:to_binary(IP), kz_term:to_integer(Port)}],
    Id = ci_parsers_util:make_name(hd(Args)),
    ?WORKER_NAME_ARGS(Module, Id, Args);
config_to_childspec(Name, #{?PARSER_TYPE := ?PARSER_TYPE_FREESWITCH
                           ,?PARSER_IP := LogIP
                           ,?PARSER_PORT := LogPort
                           ,?PARSER_LOGFILE := Filename
                           }) ->
    Module = ci_parser_freeswitch,
    lager:info("adding child ~s '~s' ~p ~p ~p", [Module, Name, LogIP, LogPort, Filename]),
    Args = [{parser_args, Filename, kz_term:to_binary(LogIP), kz_term:to_integer(LogPort)}],
    Id = ci_parsers_util:make_name(hd(Args)),
    ?WORKER_NAME_ARGS(Module, Id, Args);
config_to_childspec(Name, #{?PARSER_TYPE := ?PARSER_TYPE_KAMAILIO
                           ,?PARSER_IP := LogIP
                           ,?PARSER_PORT := LogPort
                           ,?PARSER_LOGFILE := Filename
                           }) ->
    Module = ci_parser_kamailio,
    lager:info("adding child ~s '~s' ~p ~p ~p", [Module, Name, LogIP, LogPort, Filename]),
    Args = [{parser_args, Filename, kz_term:to_binary(LogIP), kz_term:to_integer(LogPort)}],
    Id = ci_parsers_util:make_name(hd(Args)),
    ?WORKER_NAME_ARGS(Module, Id, Args);
config_to_childspec(_Name, _Config) ->
    lager:error("bad parser config for ~p: ~p", [_Name, _Config]).

%% End of Module.
