%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2010-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_prequeue).

%% This is the initial gen_server that all queue processes start off
%% as. It handles the decision as to whether we need to start a new
%% slave, a new master/unmirrored, or whether we are restarting (and
%% if so, as what). Thus a crashing queue process can restart from here
%% and always do the right thing.

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-behaviour(gen_server2).

-include("rabbit.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([start_mode/0]).

-type(start_mode() :: 'declare' | 'recovery' | 'slave').

-spec(start_link/3 :: (rabbit_types:amqqueue(), start_mode(), pid())
                      -> rabbit_types:ok_pid_or_error()).

-endif.

%%----------------------------------------------------------------------------

start_link(Q, StartMode, Marker) ->
    gen_server2:start_link(?MODULE, {Q, StartMode, Marker}, []).

%%----------------------------------------------------------------------------

init({Q, StartMode, Marker}) ->
    init(Q, case {is_process_alive(Marker), StartMode} of
                {true,  slave} -> slave;
                {true,  _}     -> master;
                {false, _}     -> restart
            end).

init(Q, master) -> rabbit_amqqueue_process:init(Q);
init(Q, slave)  -> rabbit_mirror_queue_slave:init(Q);

init(#amqqueue{name = QueueName}, restart) ->
    {ok, Q = #amqqueue{pid        = QPid,
                       slave_pids = SPids}} = rabbit_amqqueue:lookup(QueueName),
    LocalOrMasterDown = node(QPid) =:= node()
        orelse not rabbit_mnesia:on_running_node(QPid),
    Slaves = [SPid || SPid <- SPids, rabbit_mnesia:is_process_alive(SPid)],
    case rabbit_mnesia:is_process_alive(QPid) of
        true  -> false = LocalOrMasterDown, %% assertion
                 rabbit_mirror_queue_slave:go(self(), async),
                 rabbit_mirror_queue_slave:init(Q); %% [1]
        false -> case LocalOrMasterDown andalso Slaves =:= [] of
                     true  -> crash_restart(Q);     %% [2]
                     false -> timer:sleep(25),
                              init(Q, restart)      %% [3]
                 end
    end.
%% [1] There is a master on another node. Regardless of whether we
%%     were originally a master or a slave, we are now a new slave.
%%
%% [2] Nothing is alive. We are the last best hope. Try to restart as a master.
%%
%% [3] The current master is dead but either there are alive slaves to
%%     take over or it's all happening on a different node anyway. This is
%%     not a stable situation. Sleep and wait for somebody else to make a
%%     move.

crash_restart(Q = #amqqueue{name = QueueName}) ->
    rabbit_log:error("Restarting crashed ~s.~n", [rabbit_misc:rs(QueueName)]),
    gen_server2:cast(self(), init),
    rabbit_amqqueue_process:init(Q#amqqueue{pid = self()}).

%%----------------------------------------------------------------------------

%% This gen_server2 always hands over to some other module at the end
%% of init/1.
handle_call(_Msg, _From, _State)     -> exit(unreachable).
handle_cast(_Msg, _State)            -> exit(unreachable).
handle_info(_Msg, _State)            -> exit(unreachable).
terminate(_Reason, _State)           -> exit(unreachable).
code_change(_OldVsn, _State, _Extra) -> exit(unreachable).

