%%% @doc Contains util functions.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(inaka_apns_os).

                                                % API
-export([cmd/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec cmd(string()) -> {0 | 1, string()}.
cmd(Cmd) ->
    NewCmd = "
    R=$(" ++ Cmd ++ ")\n
    if [ $? -eq 0 ];then\n
      echo \"0::$R\"\n
    fi",
  case string:tokens(os:cmd(NewCmd), "::") of
    ["0", Result] -> {0, Result};
    Error         -> {1, Error}
  end.
