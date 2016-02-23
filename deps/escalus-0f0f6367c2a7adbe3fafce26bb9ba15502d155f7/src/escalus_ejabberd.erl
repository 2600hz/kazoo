%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%% Note: This module requires ejabberd_node and ejabberd_cookie
%%       to be set if common test configuration file

-module(escalus_ejabberd).

-behaviour(escalus_user_db).

%% escalus_user_db callbacks
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

-export([rpc/3,
         remote_display/1,
         remote_format/1,
         remote_format/2,
         get_global_option/1,
         with_global_option/3,
         with_local_option/3,
         get_c2s_status/1,
         get_remote_sessions/1,
         default_get_remote_sessions/0,
         legacy_get_remote_sessions/0,
         unify_str_arg/1,
         unify_str_arg/2,
         wait_for_session_count/2,
         setup_option/2,
         reset_option/2]).

-include("escalus.hrl").

%%%
%%% Business API
%%%

rpc(M, F, A) ->
    Node = escalus_ct:get_config(ejabberd_node),
    Cookie = escalus_ct:get_config(ejabberd_cookie),
    escalus_ct:rpc_call(Node, M, F, A, 3000, Cookie).

remote_display(String) ->
    Line = [$\n, [$- || _ <- String], $\n],
    remote_format("~s~s~s", [Line, String, Line]).

remote_format(Format) ->
    remote_format(Format, []).

remote_format(Format, Args) ->
    group_leader(rpc(erlang, whereis, [user]), self()),
    io:format(Format, Args),
    group_leader(whereis(user), self()).

-spec get_global_option(term()) -> term().
get_global_option(Option) ->
    rpc(ejabberd_config, get_global_option, [Option]).

-spec with_global_option(atom(), any(), fun(() -> Type)) -> Type.
with_global_option(Option, Value, Fun) ->
    OldValue = rpc(ejabberd_config, get_global_option, [Option]),
    rpc(ejabberd_config, add_global_option, [Option, Value]),
    try
        Fun()
    after
        rpc(ejabberd_config, add_global_option, [Option, OldValue])
    end.

-spec with_local_option(atom(), any(), fun(() -> Type)) -> Type.
with_local_option(Option, Value, Fun) ->
    Hosts = rpc(ejabberd_config, get_global_option, [hosts]),
    OldValues = lists:map(fun(Host) ->
        OldValue = rpc(mnesia, dirty_read, [local_config, {Option, Host}]),
        rpc(ejabberd_config, add_local_option, [{Option, Host}, Value]),
        OldValue
    end, Hosts),
    try
        Fun()
    after
        lists:foreach(
            fun ({Host, [{local_config, {Host, _Opt}, OldValue}]}) ->
                    rpc(ejabberd_config, add_local_option, [{Option, Host}, OldValue]);
                ({Host, []}) ->
                    rpc(mnesia, dirty_delete, [local_config, {Option, Host}])
        end, lists:zip(Hosts, OldValues))
    end.

get_c2s_status(#client{jid=Jid}) ->
    {match, USR} = re:run(Jid, <<"([^@]*)@([^/]*)/(.*)">>, [{capture, all_but_first, list}]),
    Pid = rpc(ejabberd_sm, get_session_pid, USR),
    rpc(sys, get_status, [Pid]).

wait_for_session_count(Config, Count) ->
    wait_for_session_count(Config, Count, 0).

get_remote_sessions(Config) ->
    escalus_overridables:do(Config, get_remote_sessions, [],
                            {?MODULE, default_get_remote_sessions}).

wait_for_session_count(Config, Count, TryNo) when TryNo < 20 ->
    case length(get_remote_sessions(Config)) of
        Count ->
            ok;
        _ ->
            timer:sleep(TryNo * TryNo),
            wait_for_session_count(Config, Count, TryNo + 1)
    end;
wait_for_session_count(Config, Count, _) ->
    escalus_ct:fail({wait_for_session_count,
                     Count, get_remote_sessions(Config)}).

%% Option setup / reset.
%%
%% These two functions are intended for situations where
%% `with_global_option/3` and `with_local_option/3` do not apply
%% because the access pattern to the option is non-uniform
%% (e.g. it's not stored in `config`/`local_config` Mnesia table).
%%
%% Example:
%%
%% - define an `option()` instance in a helper function:
%%
%%     inactivity() ->
%%         {inactivity,
%%          fun() -> escalus_ejabberd:rpc(mod_bosh, get_inactivity, []) end,
%%          fun(Value) -> escalus_ejabberd:rpc(mod_bosh,
%%                                             set_inactivity, [Value]) end,
%%          ?INACTIVITY}.
%%
%%   one can also easily imagine such a constructor to be parameterised:
%%
%%     inactivity(Value) ->
%%         {inactivity,
%%          fun() -> escalus_ejabberd:rpc(mod_bosh, get_inactivity, []) end,
%%          fun(Value) -> escalus_ejabberd:rpc(mod_bosh,
%%                                             set_inactivity, [Value]) end,
%%          Value}.
%%
%% - use `setup_option/2` in `init_per_testcase/2`:
%%
%%     init_per_testcase(disconnect_inactive = CaseName, Config) ->
%%         NewConfig = escalus_ejabberd:setup_option(inactivity(), Config),
%%         escalus:init_per_testcase(CaseName, NewConfig);
%%
%% - use `reset_option/2` in `end_per_testcase/2`:
%%
%%     end_per_testcase(disconnect_inactive = CaseName, Config) ->
%%         NewConfig = escalus_ejabberd:reset_option(inactivity(), Config),
%%         escalus:end_per_testcase(CaseName, NewConfig);
%%
%% Inside `setup_option/2` the current value of `inactivity`,
%% as returned by the RPC call, will be stored in `Config`.
%% The value from `option()` instance will be used
%% to set the new value on the server.
%%
%% `reset_option/2` will restore that saved value using the appropriate
%% setter function and delete it from `Config`.

-ifdef(no_types_with_arities).
-type option() :: {Name   :: atom(),
                   Getter :: fun(() -> any()),
                   Setter :: fun((any()) -> any()),
                   Value  :: any()}.
-else.
-type option(Type) :: {Name   :: atom(),
                       Getter :: fun(() -> Type),
                       Setter :: fun((Type) -> any()),
                       Value  :: Type}.
-type option() :: option(_).
-endif.

-spec setup_option(option(), Config) -> Config.
setup_option({Option, Get, Set, Value}, Config) ->
    Saved = Get(),
    Set(Value),
    [{{saved, Option}, Saved} | Config].

-spec reset_option(option(), Config) -> Config.
reset_option({Option, _, Set, _}, Config) ->
    case proplists:get_value({saved, Option}, Config) of
        undefined ->
            ok;
        Saved ->
            Set(Saved)
    end,
    proplists:delete({saved, Option}, Config).

%%--------------------------------------------------------------------
%% escalus_user_db callbacks
%%--------------------------------------------------------------------

start(_) -> ok.
stop(_) -> ok.

-spec create_users(escalus:config(), escalus_users:who()) -> escalus:config().
create_users(Config, Who)
  when is_atom(Who); is_tuple(Who) ->
    Users = escalus_users:get_users(Who),
    lists:foreach(fun({_Name, UserSpec}) ->
                          register_user(Config, UserSpec)
                  end, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), escalus_users:who()) -> escalus:config().
delete_users(Config, Who)
  when is_atom(Who); is_tuple(Who) ->
    Users = escalus_users:get_users(Who),
    lists:foreach(fun({_Name, UserSpec}) ->
                          unregister_user(Config, UserSpec)
                  end, Users),
    Config.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

register_user(Config, UserSpec) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    USP = [unify_str_arg(Arg, StrFormat) ||
           Arg <- escalus_users:get_usp(Config, UserSpec)],
    rpc(ejabberd_admin, register, USP).

unregister_user(Config, UserSpec) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    USP = [unify_str_arg(Arg, StrFormat) ||
           Arg <- escalus_users:get_usp(Config, UserSpec)],
    [U, S, _P] = USP,
    rpc(ejabberd_admin, unregister, [U, S]).

default_get_remote_sessions() ->
    rpc(ejabberd_sm, get_full_session_list, []).

legacy_get_remote_sessions() ->
    rpc(ejabberd_sm, dirty_get_sessions_list, []).

unify_str_arg(Arg) ->
    StrFormat = escalus_ct:get_config(ejabberd_string_format),
    unify_str_arg(Arg, StrFormat).

unify_str_arg(Arg, str) when is_binary(Arg) ->
    binary_to_list(Arg);
unify_str_arg(Arg, _) ->
    Arg.

