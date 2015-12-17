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


-record(sipreg, {
    id :: term(),
    ruri :: nksip:uri(),
    opts :: nksip:optslist(),
    call_id :: nksip:call_id(),
    interval :: non_neg_integer(),
    from :: any(),
    cseq :: nksip:cseq(),
    next :: nksip_lib:timestamp(),
    ok :: boolean()
}).


-record(state, {
    pings :: [#sipreg{}],
    regs :: [#sipreg{}]
}).


