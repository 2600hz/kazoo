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

-module(escalus_users).

-behaviour(escalus_user_db).

%% `escalus_user_db` callbacks
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

%% Public API
-export([create_users/1,
         delete_users/1,
         get_jid/2,
         get_username/2,
         get_host/2,
         get_server/2,
         get_userspec/2,
         update_userspec/4,
         get_options/2,
         get_options/3,
         get_options/4,
         get_users/1,
         get_user_by_name/1,
         get_user_by_name/2,
         create_user/2,
         verify_creation/1,
         delete_user/2,
         get_usp/2,
         is_mod_register_enabled/1
        ]).

%% Public types
-export_type([user_name/0,
              user_spec/0,
              who/0]).

%% Public types
-type who() :: all | {by_name, [escalus_config:key()]}.
-type user_spec() :: [{user_option(), any()}].

%% Internal types
-type user() :: user_name() | user_spec().
-type named_user() :: {user_name(), user_spec()}.
-type user_name() :: atom().
-type host() :: inet:hostname() | inet:ip4_address() | binary().
-type xmpp_domain() :: inet:hostname() | binary().

-include("escalus.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% `escalus_user_db` callbacks
%%--------------------------------------------------------------------

-spec start(escalus:config()) -> any().
start(Config) ->
    case auth_type(Config) of
        {escalus_user_db, {module, M, Opts}} ->
            M:start(Opts);
        _ ->
            ok
    end.

-spec stop(escalus:config()) -> any().
stop(Config) ->
    case auth_type(Config) of
        {escalus_user_db, {module, M, Opts}} ->
            M:stop(Opts);
        _ ->
            ok
    end.

-spec create_users(escalus:config(), who()) -> escalus:config().
create_users(Config, Who) ->
    case auth_type(Config) of
        {escalus_user_db, xmpp} ->
            create_users_via_xmpp(Config, Who);
        {escalus_user_db, {module, M, _}} ->
            M:create_users(Config, Who)
    end.

-spec create_users_via_xmpp(escalus:config(), who()) -> escalus:config().
create_users_via_xmpp(Config, Who) ->
    Users = get_users(Who),
    CreationResults = [create_user(Config, User) || User <- Users],
    lists:foreach(fun verify_creation/1, CreationResults),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

-spec delete_users(escalus:config(), who()) -> escalus:config().
delete_users(Config, Who) ->
    case auth_type(Config) of
        {escalus_user_db, xmpp} ->
            Users = get_users(Who),
            [delete_user(Config, User) || User <- Users];
        {escalus_user_db, {module, M, _}} ->
            M:delete_users(Config, Who)
    end.

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec create_users(escalus:config()) -> escalus:config().
create_users(Config) ->
    create_users(Config, all).

-spec delete_users(escalus:config()) -> escalus:config().
delete_users(Config) ->
    delete_users(Config, all).

-spec get_jid(escalus:config(), user()) -> binary().
get_jid(Config, User) ->
    Username = get_username(Config, User),
    Server = get_server(Config, User),
    <<Username/binary, "@", Server/binary>>.

-spec get_username(escalus:config(), user()) -> binary().
get_username(Config, User) ->
    get_defined_option(Config, User, username, escalus_username).

-spec get_password(escalus:config(), user()) -> binary().
get_password(Config, User) ->
    get_defined_option(Config, User, password, escalus_password).

-spec get_host(escalus:config(), user()) -> host().
get_host(Config, User) ->
    get_user_option(host, User, escalus_host, Config, get_server(Config, User)).

-spec get_port(escalus:config(), user()) -> inet:port_number().
get_port(Config, User) ->
    get_user_option(port, User, escalus_port, Config, 5222).

-spec get_server(escalus:config(), user()) -> xmpp_domain().
get_server(Config, User) ->
    get_user_option(server, User, escalus_server, Config, <<"localhost">>).

-spec get_wspath(escalus:config(), user()) -> binary() | 'undefined'.
get_wspath(Config, User) ->
    get_user_option(wspath, User, escalus_wspath, Config, undefined).

-spec get_auth_method(escalus:config(), user()) -> {module(), atom()}.
get_auth_method(Config, User) ->
    AuthMethod = get_user_option(auth_method, User,
                                 escalus_auth_method, Config,
                                 <<"PLAIN">>),
    get_auth_method(AuthMethod).

-spec get_auth_method(binary() | {module(), atom()}) -> {module(), atom()}.
get_auth_method(<<"PLAIN">>) ->
    {escalus_auth, auth_plain};
get_auth_method(<<"DIGEST-MD5">>) ->
    {escalus_auth, auth_digest_md5};
get_auth_method(<<"SASL-ANON">>) ->
    {escalus_auth, auth_sasl_anon};
get_auth_method(<<"SCRAM-SHA-1">>) ->
    {escalus_auth, auth_sasl_scram_sha1};
get_auth_method({Mod, Fun}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun}.

-spec get_usp(escalus:config(), user()) -> [binary() | xmpp_domain()].
get_usp(Config, User) ->
    [get_username(Config, User),
     get_server(Config, User),
     get_password(Config, User)].

%% TODO: get_options/2 and get_userspec/2 are redundant - remove one
%% TODO: this list of options should be complete and formal!
-spec get_options(escalus:config(), user()) -> escalus:config().
get_options(Config, User) ->
    [{username, get_username(Config, User)},
     {server, get_server(Config, User)},
     {host, get_host(Config, User)},
     {port, get_port(Config, User)},
     {auth, get_auth_method(Config, User)},
     {wspath, get_wspath(Config, User)}
     | get_userspec(Config, User)].

-spec get_options(escalus:config(), user(), binary()) -> escalus:config().
get_options(Config, User, Resource) ->
    [{resource, Resource} | get_options(Config, User)].

-spec get_options(escalus:config(), user(),
                  binary(), escalus_event:event_client()) -> escalus:config().
get_options(Config, User, Resource, EventClient) ->
    [{event_client, EventClient} | get_options(Config, User, Resource)].

-spec get_userspec(escalus:config(), user_name() | user_spec())
    -> user_spec().
get_userspec(Config, Username) when is_atom(Username) ->
    Users = escalus_config:get_config(escalus_users, Config),
    {Username, UserSpec} = lists:keyfind(Username, 1, Users),
    UserSpec;
get_userspec(_Config, UserSpec) when is_list(UserSpec) ->
    UserSpec.

-spec update_userspec(escalus:config(), atom(), atom(), any()) ->
      escalus:config().
update_userspec(Config, UserName, Option, Value) ->
    UserSpec = [{Option, Value}
                | escalus_users:get_userspec(Config, UserName)],
    Users = escalus_config:get_config(escalus_users, Config),
    NewUsers = lists:keystore(UserName, 1, Users, {UserName, UserSpec}),
    lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers}).

-spec get_users(who()) -> [named_user()].
get_users(all) ->
    escalus_ct:get_config(escalus_users);
get_users({by_name, Names}) ->
    All = get_users(all),
    [get_user_by_name(Name, All) || Name <- Names].

-spec get_user_by_name(user_name(), escalus:config()) -> {user_name(), escalus:config()}.
get_user_by_name(Name, Users) ->
    {Name, _} = proplists:lookup(Name, Users).

-spec get_user_by_name(user_name()) -> {user_name(), escalus:config()}.
get_user_by_name(Name) ->
    get_user_by_name(Name, get_users(all)).

-spec create_user(escalus:config(), named_user()) -> any().
create_user(Config, {_Name, Options}) ->
    ClientProps = get_options(Config, Options),
    {ok, Conn, ClientProps, _} = escalus_connection:start(ClientProps,
                                                          [start_stream,
                                                           stream_features,
                                                           maybe_use_ssl]),
    escalus_connection:send(Conn, escalus_stanza:get_registration_fields()),
    {ok, result, RegisterInstrs} = wait_for_result(Conn),
    Answers = get_answers(ClientProps, RegisterInstrs),
    escalus_connection:send(Conn, escalus_stanza:register_account(Answers)),
    Result = wait_for_result(Conn),
    escalus_connection:stop(Conn),
    Result.

-spec verify_creation({ok, _, _} | {error, _, _}) -> ok.
verify_creation({ok, result, _}) ->
    ok;
verify_creation({ok, conflict, Raw}) ->
    RawStr = exml:to_iolist(Raw),
    error_logger:info_msg("user already existed: ~s~n", [RawStr]);
verify_creation({error, Error, Raw}) ->
    RawStr = exml:to_iolist(Raw),
    error_logger:error_msg("error when trying to register user: ~s~n", [RawStr]),
    error(Error).

-spec delete_user(escalus:config(), {atom(), user()}) ->
      {ok, _, _} | {error, _, _}.
delete_user(Config, {_Name, UserSpec}) ->
    Options = get_options(Config, UserSpec),
    {ok, Conn, _, _} = escalus_connection:start(Options),
    escalus_connection:send(Conn, escalus_stanza:remove_account()),
    Result = wait_for_result(Conn),
    escalus_connection:stop(Conn),
    Result.

-spec auth_type([proplists:property()]) -> {escalus_user_db, {module, atom(), list()} | xmpp}.
auth_type(Config) ->
    Type = case {escalus_config:get_config(escalus_user_db, Config, undefined),
                 try_check_mod_register(Config)} of
               {{module, M, Args}, _} -> {module, M, Args};
               {{module, M}, _} -> {module, M, []};
               {_, false} -> {module, escalus_ejabberd, []};
               {_, true} -> xmpp
           end,
    {escalus_user_db, Type}.

try_check_mod_register(Config) ->
    try is_mod_register_enabled(Config)
    catch _:_ -> false
    end.

-spec is_mod_register_enabled(escalus:config()) -> boolean().
is_mod_register_enabled(Config) ->
    Server = escalus_config:get_config(escalus_server, Config, <<"localhost">>),
    Host = escalus_config:get_config(escalus_host, Config, Server),
    Port = escalus_config:get_config(escalus_port, Config, 5222),
    ClientProps = [{server, Server},
                   {host, Host},
                   {port, Port}],
    {ok, Conn, ClientProps, _} = escalus_connection:start(ClientProps,
                                                          [start_stream,
                                                           stream_features,
                                                           maybe_use_ssl]),
    escalus_connection:send(Conn, escalus_stanza:get_registration_fields()),
    case wait_for_result(Conn) of
        {error, _, _} ->
            false;
        _ ->
            true
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-type user_option() :: 'username'    %% binary()
                     | 'server'      %% binary()
                     | 'password'    %% binary()
                     | 'compression' %% <<"zlib">> | false
                     | 'ssl'         %% 'false' | 'optional',
                                     %% shouldn't there also be 'required'?
                     | 'transport'   %% 'tcp' | 'bosh' | 'ws', anything else?
                     | 'path'        %% BOSH path
                     | 'port'        %% TCP port
                     | 'wspath'      %% WebSocket path - unify with `path`?
                     | 'host'        %% IP address? DNS name?
                     | 'auth_method' %% <<"PLAIN">> | <<"DIGETS-MD5">>
                                     %% | <<"SASL-ANON">> | <<"SCRAM-SHA-1">>
                                     %% | Other
                     .

-type ejabberd_option() :: 'ejabberd_node'
                         | 'ejabberd_cookie'
                         | 'ejabberd_domain'.

-type escalus_option() :: 'escalus_server'
                        | 'escalus_username'
                        | 'escalus_password'
                        | 'escalus_host'
                        | 'escalus_port'
                        | 'escalus_auth_method'
                        | 'escalus_wspath'
                        .

-type long_option() :: ejabberd_option() | escalus_option().

-type option_value() :: any().

%% get_user_option is a wrapper on escalus_config:get_config/5,
%% which can take either UserSpec (a proplist) or user name (atom)
%% as the second argument
-spec get_user_option(user_option(), user(), long_option(),
                      escalus:config(), option_value()) -> option_value().
get_user_option(Short, Name, Long, Config, Default) when is_atom(Name) ->
    {Name, Spec} = case lists:keysearch(escalus_users, 1, Config) of
        false ->
            get_user_by_name(Name);
        {value, {_, Users}} ->
            get_user_by_name(Name, Users)
    end,
    get_user_option(Short, Spec, Long, Config, Default);
get_user_option(Short, Spec, Long, Config, Default) ->
    escalus_config:get_config(Short, Spec, Long, Config, Default).

-spec get_defined_option(escalus:config(), user(),
                         user_option(), long_option()) -> option_value().
get_defined_option(Config, Name, Short, Long) ->
    case get_user_option(Short, Name, Long, Config, undefined) of
        undefined ->
            escalus_ct:fail({undefined_option, Short, Name});
        Value ->
            Value
    end.

-spec wait_for_result(escalus:client()) -> {ok, result, exml:element()}
                                         | {ok, conflict, exml:element()}
                                         | {error, Error, exml:cdata()}
      when Error :: 'failed_to_register' | 'bad_response' | 'timeout'.
wait_for_result(Conn) ->
    receive
        {stanza, Conn, Stanza} ->
            case response_type(Stanza) of
                result ->
                    {ok, result, Stanza};
                conflict ->
                    {ok, conflict, Stanza};
                error ->
                    {error, failed_to_register, Stanza};
                _ ->
                    {error, bad_response, Stanza}
            end
    after 3000 ->
            {error, timeout, exml:escape_cdata(<<"timeout">>)}
    end.

response_type(#xmlel{name = <<"iq">>} = IQ) ->
    case exml_query:attr(IQ, <<"type">>) of
        <<"result">> ->
            result;
        <<"error">> ->
            case exml_query:path(IQ, [{element, <<"error">>},
                                      {attr, <<"code">>}]) of
                <<"409">> ->
                    conflict;
                _ ->
                    error
            end;
        _ ->
            other
    end;
response_type(_) ->
    other.

get_answers(UserSpec, InstrStanza) ->
    BinSpec = [{list_to_binary(atom_to_list(K)), V} || {K, V} <- UserSpec],
    Query = exml_query:subelement(InstrStanza, <<"query">>),
    ChildrenNames = [N || #xmlel{name = N} <- Query#xmlel.children],
    NoInstr = ChildrenNames -- [<<"instructions">>],
    [#xmlel{name=K,
            children=[#xmlcdata{content = proplists:get_value(K, BinSpec)}]}
     || K <- NoInstr].
