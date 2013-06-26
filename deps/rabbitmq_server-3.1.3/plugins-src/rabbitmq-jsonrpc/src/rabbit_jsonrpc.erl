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
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
%%

-module(rabbit_jsonrpc).

-behaviour(application).
-export([start/2, stop/1, listener/0]).

%% Dummy supervisor - see Ulf Wiger's comment at
%% http://erlang.2086793.n4.nabble.com/initializing-library-applications-without-processes-td2094473.html
-behaviour(supervisor).
-export([init/1]).

start(_Type, _StartArgs) ->
    RpcContext = case application:get_env(?MODULE, context) of
                     undefined -> "rpc";
                     {ok, V} -> V
                 end,
    Listener = listener(),
    if_redirect(
      fun () ->
              rabbit_web_dispatch:register_port_redirect(
                jsonrpc_redirect, [{port,          55670},
                                   {ignore_in_use, true}], "", port(Listener))
      end),
    rabbit_web_dispatch:register_context_handler(
        jsonrpc, Listener, RpcContext,
        fun(Req) ->
            case rfc4627_jsonrpc_mochiweb:handle("/" ++ RpcContext, Req) of
                no_match ->
                    Req:not_found();
                {ok, Response} ->
                    Req:respond(Response)
            end
        end, "JSON-RPC: RPC endpoint"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    if_redirect(
      fun () -> rabbit_web_dispatch:unregister_context(jsonrpc_redirect) end),
    rabbit_web_dispatch:unregister_context(jsonrpc),
    ok.

init([]) -> {ok, {{one_for_one, 3, 10}, []}}.

listener() ->
    {ok, Listener} = application:get_env(rabbitmq_jsonrpc, listener),
    Listener.

if_redirect(Thunk) ->
    {ok, Redir} = application:get_env(rabbitmq_jsonrpc, redirect_old_port),
    case Redir of
        true  -> Thunk();
        false -> ok
    end.

port(Listener) ->
    proplists:get_value(port, Listener).
