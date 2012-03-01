%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc Syslog backend for lager.

-module(lager_syslog_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level}).

-include_lib("lager/include/lager.hrl").

%% @private
init([Ident, Facility, Level]) ->
    case application:start(syslog) of
        ok ->
            init2(Ident, Facility, Level);
        {error, {already_started, _}} ->
            init2(Ident, Facility, Level);
        Error ->
            Error
    end.

init2(Ident, Facility, Level) ->
    case syslog:open(Ident, [pid], Facility) of
        ok ->
            {ok, #state{level=lager_util:level_to_num(Level)}};
        Error ->
            Error
    end.

%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{level=lager_util:level_to_num(Level)}};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {_Date, _Time}, [_LevelStr, Location, Message]}, State) ->
    syslog:log(convert_level(Level), [Location, Message]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    application:stop(syslog),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

convert_level(?DEBUG) -> debug;
convert_level(?INFO) -> info;
convert_level(?NOTICE) -> notice;
convert_level(?WARNING) -> warning;
convert_level(?ERROR) -> err;
convert_level(?CRITICAL) -> crit;
convert_level(?ALERT) -> alert;
convert_level(?EMERGENCY) -> emergency.

