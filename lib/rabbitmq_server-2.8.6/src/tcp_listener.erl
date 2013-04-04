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

-module(tcp_listener).

-behaviour(gen_server).

-export([start_link/8]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock, on_startup, on_shutdown, label}).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-type(mfargs() :: {atom(), atom(), [any()]}).

-spec(start_link/8 ::
        (inet:ip_address(), inet:port_number(), [gen_tcp:listen_option()],
         integer(), atom(), mfargs(), mfargs(), string()) ->
                           rabbit_types:ok_pid_or_error()).

-endif.

%%--------------------------------------------------------------------

start_link(IPAddress, Port, SocketOpts,
           ConcurrentAcceptorCount, AcceptorSup,
           OnStartup, OnShutdown, Label) ->
    gen_server:start_link(
      ?MODULE, {IPAddress, Port, SocketOpts,
                ConcurrentAcceptorCount, AcceptorSup,
                OnStartup, OnShutdown, Label}, []).

%%--------------------------------------------------------------------

init({IPAddress, Port, SocketOpts,
      ConcurrentAcceptorCount, AcceptorSup,
      {M,F,A} = OnStartup, OnShutdown, Label}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, SocketOpts ++ [{ip, IPAddress},
                                             {active, false}]) of
        {ok, LSock} ->
            lists:foreach(fun (_) ->
                                  {ok, _APid} = supervisor:start_child(
                                                  AcceptorSup, [LSock])
                          end,
                          lists:duplicate(ConcurrentAcceptorCount, dummy)),
            {ok, {LIPAddress, LPort}} = inet:sockname(LSock),
            error_logger:info_msg(
              "started ~s on ~s:~p~n",
              [Label, rabbit_misc:ntoab(LIPAddress), LPort]),
            apply(M, F, A ++ [IPAddress, Port]),
            {ok, #state{sock = LSock,
                        on_startup = OnStartup, on_shutdown = OnShutdown,
                        label = Label}};
        {error, Reason} ->
            error_logger:error_msg(
              "failed to start ~s on ~s:~p - ~p (~s)~n",
              [Label, rabbit_misc:ntoab(IPAddress), Port,
               Reason, inet:format_error(Reason)]),
            {stop, {cannot_listen, IPAddress, Port, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=LSock, on_shutdown = {M,F,A}, label=Label}) ->
    {ok, {IPAddress, Port}} = inet:sockname(LSock),
    gen_tcp:close(LSock),
    error_logger:info_msg("stopped ~s on ~s:~p~n",
                          [Label, rabbit_misc:ntoab(IPAddress), Port]),
    apply(M, F, A ++ [IPAddress, Port]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
