%% Copyright (c) 2010 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc A memory-limited info/error/warning event handler.
%%
%% Replace the OTP default error_logger's event handler (which
%% can cause memory use problems when handling very large messages)
%% with a handler that will use a limited amount of RAM but is
%% otherwise equivalent.
%%
%% Strictly speaking, this library need be a "code only" application,
%% i.e. an OTP application without any support processes (like the
%% <tt>stdlib</tt> OTP application).  However, there's one significant
%% reason why <tt>riak_err</tt> would want to have a small monitoring
%% server running.  The reason is as follows:
%%
%% The OTP <tt>kernel</tt> application's has a gen_event-based process
%% with the registered name <tt>error_logger</tt> that handles all
%% system info/error/warning messages, e.g., submitted by
%% <tt>error_logger:info_msg()</tt> and related functions.  If there
%% is a problem with an <tt>error_logger</tt> event handler (e.g.,
%% throws an exception), the handler is silently removed from handling
%% further events.
%%
%% We wish to know about all events, good and bad, despite software
%% bugs.  Therefore, if there's an error in an event handler, we want
%% a monitor process to find out about the error and act to re-install
%% the handler.  If we don't re-install the handler, we will
%% <em>never</em> see another event logged.  We want happy event
%% logging, but we do not want to travel the "Ignorance is bliss" road
%% to find that happiness.
%%
%% <ol>
%%
%% <li> We use a long-lived gen_server-based process (running code in
%% this module) to be our monitor. </li>
%%
%% <li> We install the handler using
%% <tt>gen_event:add_sup_handler/3</tt>.  If the handler exits or
%% throws an exception, we'll be sent a <tt>{gen_event_EXIT, ...}</tt>
%% message. </li>
%%
%% <li> If we receive the <tt>{gen_event_EXIT, ...}</tt> bad news
%% message, we exit.  Our supervisor will restart us, and the
%% side-effect of running our <tt>init()</tt> function is
%% re-installing our custom event handler. </li>
%%
%%</ol>
%%
%% === Configuration ===
%%
%% There are two config knobs may be specified on the command line
%% via "-riak_err KnobName Integer" on the command line or (in a
%% Basho application like Riak) via the same "-riak_err KnobName Integer"
%% line in the <tt>etc/vm.args</tt> file).  Alternatively, these properties
%% may be set using application environment variables by the same name.
%%
%% <ol>
%% <li> <tt>term_max_size</tt> For arguments formatted in FormatString &amp;
%% ArgList style, if the total size of ArgList is more than term_max_size,
%% then we'll ignore FormatString and log the message with a well-known
%% (and therefore safe) formatting string.  The default is 10KBytes. </li>
%%
%% <li> <tt>fmt_max_bytes</tt> When formatting a log-related term that might
%% be "big", limit the term's formatted output to a maximum of
%% <tt>fmt_max_bytes</tt> bytes.  The default is 12KBytes. </li>
%%
%% <li> <tt>console_error_type</tt> Defines the
%% handler's choices for writing formatted events to the console/tty.
%% config values are not quite the same as the <tt>sasl</tt> application's
%% <tt>errlog_type</tt> parameter: only <tt>error</tt> and <tt>all</tt>
%% are valid.  The default is <tt>all</tt>. </li>
%%
%% <li> <tt>console_sasl_reports</tt> Defines whether or not SASL
%% reports are written to the console/tty.  Valid values are Erlang
%% boolean values.  The default is <tt>false</tt>. </li>
%%
%% </ol>
%%
%% For example, <tt>erl -riak_err term_max_size 8192 fmt_max_bytes 9000</tt>.

-module(riak_err_app).

-behaviour(application).

%% Application callbacks
-export([start/0,
         start/2,
         stop/1]).

%% @doc  Starts the application
-spec start() -> 'ok' | {'error', term()}.
start() ->
    application:start(riak_err).

%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    riak_err_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop(_State) ->
    ok.
