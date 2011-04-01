%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-define(DEFAULT_TIMEOUT, 60000).

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].
%% In R13B bool() is now called boolean()
%% Uncomment if it's not compiling.
%% -type boolean() :: bool()


-record(server, {
    host :: string(),
    port :: integer(),
    prefix :: string(),
    options = [{max_sessions, 2040}] :: list()
}).

-type server() :: #server{}.

% record to keep database information
-record(db, {
    server :: server(),
    name :: string(),
    options = [{max_sessions, 2040}] :: list()
}).

-type db() :: #db{}.

-record(server_uuids, {
    host_port,
    uuids
}).

-record(view, {
    db :: db(),
    name :: string(),
    options :: iolist(),
    method :: atom(),
    body :: iolist(),
    headers :: list(),
    url_parts :: list(),
    url :: string()
}).

-type view() :: #view{}.

-record(gen_changes_state, {
    req_id,
    mod,
    modstate,
    seq,
    db,
    options,
    partial_chunk = <<"">>,
    row,
    complete=false
}).
    
-define(USER_AGENT, "couchbeam/0.5.0").

