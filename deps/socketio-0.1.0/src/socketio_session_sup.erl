%% @author Kirill Trofimov <sinnus@gmail.com>
%% @copyright 2012 Kirill Trofimov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(socketio_session_sup).
-author('Kirill Trofimov <sinnus@gmail.com>').
-behaviour(supervisor).

-export([start_link/0, start_child/4]).
-export([init/1]).

%% --------------------------------------------------------------------------

-spec start_link() -> ignore | {'ok', pid()} | {'error', any()}.
start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {socketio_session, start_link, []},
            temporary, 5000, worker, [socketio_session]}]}}.

start_child(SessionId, SessionTimeout, Callback, Opts) ->
   supervisor:start_child(?MODULE, [SessionId, SessionTimeout, Callback, Opts]).
