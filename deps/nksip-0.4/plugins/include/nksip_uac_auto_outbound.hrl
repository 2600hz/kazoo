%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------


-record(sipreg_ob, {
    id :: term(),
    pos :: integer(),
    cseq :: nksip:cseq(),
    conn_monitor :: reference(),
    conn_pid :: pid(),
    fails :: non_neg_integer()
}).


-record(state_ob, {
    outbound :: boolean(),
    pos :: integer(),
    regs = [#sipreg_ob{}]
}).


