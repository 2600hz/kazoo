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

-module(rabbit_jsonrpc_channel_factory).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init(_Args) ->
    Service = rfc4627_jsonrpc:service(<<"rabbitmq">>,
                                      <<"urn:uuid:f98a4235-20a9-4321-a15c-94878a6a14f3">>,
                                      <<"1.2">>,
                                      [{<<"open">>, [{"username", str},
                                                     {"password", str},
                                                     {"sessionTimeout", num},
                                                     {"virtualHost", str}]}]),
    rfc4627_jsonrpc:register_service(self(), Service),
    {ok, nostate}.

handle_call({jsonrpc, <<"open">>, _RequestInfo, Args}, _From, State) ->
    {ok, Oid} = rabbit_jsonrpc_channel:open(Args),
    {reply,
     {result, {obj, [{service, Oid}]}},
     State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
