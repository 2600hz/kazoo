%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_auth_backend_ldap_app).

-behaviour(application).
-export([start/2, stop/1]).

%% Dummy supervisor to get this application behaviour working
-behaviour(supervisor).
-export([init/1]).

start(_Type, _StartArgs) ->
    {ok, Backends} = application:get_env(rabbit, auth_backends),
    case configured(rabbit_auth_backend_ldap, Backends) of
        true  -> ok;
        false -> rabbit_log:warning(
                   "LDAP plugin loaded, but rabbit_auth_backend_ldap is not "
                   "in the list of auth_backends. LDAP auth will not work.~n")
    end,
    {ok, SSL} = application:get_env(rabbitmq_auth_backend_ldap, use_ssl),
    case SSL of
        true  -> rabbit_networking:ensure_ssl();
        false -> ok
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

configured(_M, [])        -> false;
configured(M,  [M    |_]) -> true;
configured(M,  [{M,_}|_]) -> true;
configured(M,  [{_,M}|_]) -> true;
configured(M,  [_    |T]) -> configured(M, T).

%%----------------------------------------------------------------------------

init([]) -> {ok, {{one_for_one, 3, 10}, []}}.
