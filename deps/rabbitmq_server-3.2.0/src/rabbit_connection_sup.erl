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
%% Copyright (c) 2007-2013 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_connection_sup).

-behaviour(supervisor2).

-export([start_link/0, reader/1]).

-export([init/1]).

-include("rabbit.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-spec(start_link/0 :: () -> {'ok', pid(), pid()}).
-spec(reader/1 :: (pid()) -> pid()).

-endif.

%%--------------------------------------------------------------------------

start_link() ->
    {ok, SupPid} = supervisor2:start_link(?MODULE, []),
    %% We need to get channels in the hierarchy here so they get shut
    %% down after the reader, so the reader gets a chance to terminate
    %% them cleanly. But for 1.0 readers we can't start the real
    %% ch_sup_sup (because we don't know if we will be 0-9-1 or 1.0) -
    %% so we add another supervisor into the hierarchy.
    %%
    %% This supervisor also acts as an intermediary for heartbeaters and
    %% the queue collector process, since these must not be siblings of the
    %% reader due to the potential for deadlock if they are added/restarted
    %% whilst the supervision tree is shutting down.
    {ok, HelperSup} =
        supervisor2:start_child(
          SupPid,
          {helper_sup, {rabbit_connection_helper_sup, start_link, []},
           intrinsic, infinity, supervisor, [rabbit_connection_helper_sup]}),
    {ok, ReaderPid} =
        supervisor2:start_child(
          SupPid,
          {reader, {rabbit_reader, start_link, [HelperSup]},
           intrinsic, ?MAX_WAIT, worker, [rabbit_reader]}),
    {ok, SupPid, ReaderPid}.

reader(Pid) ->
    hd(supervisor2:find_child(Pid, reader)).

%%--------------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.
