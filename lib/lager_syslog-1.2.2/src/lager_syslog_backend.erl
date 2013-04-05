%% Copyright (c) 2011-2012 Basho Technologies, Inc.  All Rights Reserved.
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

-export([config_to_id/1]).

-record(state, {level, handle, id, formatter,format_config}).

-include_lib("lager/include/lager.hrl").

-define(DEFAULT_FORMAT,["[", severity, "] ",
        {pid, ""},
        {module, [
                {pid, ["@"], ""},
                module,
                {function, [":", function], ""},
                {line, [":",line], ""}], ""},
        " ", message]).


%% @private
init([Ident, Facility, Level]) when is_atom(Level) ->
    init([Ident, Facility, Level, {lager_default_formatter, ?DEFAULT_FORMAT}]);
init([Ident, Facility, Level, {Formatter, FormatterConfig}]) when is_atom(Level), is_atom(Formatter) ->
    case application:start(syslog) of
        ok ->
            init2([Ident, Facility, Level, {Formatter, FormatterConfig}]);
        {error, {already_started, _}} ->
            init2([Ident, Facility, Level, {Formatter, FormatterConfig}]);
        Error ->
            Error
    end.

%% @private
init2([Ident, Facility, Level, {Formatter, FormatterConfig}]) ->
    case syslog:open(Ident, [pid], Facility) of
        {ok, Log} ->
            try parse_level(Level) of
                Lvl ->
                    {ok, #state{level=Lvl,
                            id=config_to_id([Ident, Facility, Level]),
                            handle=Log,
                            formatter=Formatter,
                            format_config=FormatterConfig}}
                catch
                    _:_ ->
                        {error, bad_log_level}
                end;
        Error ->
            Error
    end.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    try parse_level(Level) of
        Lvl ->
            {ok, ok, State#state{level=Lvl}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {_Date, _Time}, [_LevelStr, Location, Message]},
        #state{level=LogLevel} = State) when Level =< LogLevel ->
    syslog:log(State#state.handle, convert_level(Level), [Location, Message]),
    {ok, State};
handle_event({log, Message}, #state{level=Level,formatter=Formatter,format_config=FormatConfig} = State) ->
    case lager_util:is_loggable(Message, Level, State#state.id) of
        true ->
            syslog:log(State#state.handle, convert_level(lager_msg:severity_as_int(Message)), [Formatter:format(Message, FormatConfig)]),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% convert the configuration into a hopefully unique gen_event ID
config_to_id([Ident, Facility, _Level]) ->
    {?MODULE, {Ident, Facility}};
config_to_id([Ident, Facility, _Level, _Formatter]) ->
    {?MODULE, {Ident, Facility}}.

convert_level(?DEBUG) -> debug;
convert_level(?INFO) -> info;
convert_level(?NOTICE) -> notice;
convert_level(?WARNING) -> warning;
convert_level(?ERROR) -> err;
convert_level(?CRITICAL) -> crit;
convert_level(?ALERT) -> alert;
convert_level(?EMERGENCY) -> emerg.

parse_level(Level) ->
    try lager_util:config_to_mask(Level) of
        Res ->
            Res
    catch
        error:undef ->
            %% must be lager < 2.0
            lager_util:level_to_num(Level)
    end.

