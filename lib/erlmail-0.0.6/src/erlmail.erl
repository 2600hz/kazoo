%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail package start and stop funcations
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.3
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stuart Jackson, Simple Enigma, Inc. All Righs Reserved
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%
%%%---------------------------------------------------------------------------------------
-module(erlmail).
-author('sjackson@simpleenigma.com').

-export([start/0,stop/0,restart/0,reload/0]).

%%-------------------------------------------------------------------------
%% @spec () -> any()
%% @doc Starts Application
%% @end
%%-------------------------------------------------------------------------
start() -> 
	io:format("Starting ErlMail ...~n"),
	application:start(?MODULE).

%%-------------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops application
%% @end
%%-------------------------------------------------------------------------
stop() ->
	io:format("Stopping ErlMail ...~n"),
	application:stop(?MODULE).

%%-------------------------------------------------------------------------
%% @spec () -> any()
%% @doc Stops, reloads and then starts application
%% @end
%%-------------------------------------------------------------------------
restart() ->
	stop(),
	reload(),
	start().

%%-------------------------------------------------------------------------
%% @spec () -> any()
%% @doc Reloads modules
%% @end
%%-------------------------------------------------------------------------
reload() ->
	io:format("Reloading ErlMail Modules ...~n"),
	reload:reload([
		erlmail_app,erlmail_sup,erlmail_util,erlmail_store,erlmail_store_sup,erlmail_suite,
		gen_store,dets_store,mnesia_store,mysql_store,aws_store,
		imapd,imapd_listener,imapd_fsm,imapd_app,imapd_sup,imapd_util,imapd_cmd,imapd_util,imapd_ext,imapd_fetch,imapd_search,imapd_resp,
		imap_parser,imap_scan,imapc,imapc_fsm,imapc_util,
		mime,
		popc,popc_fsm,
		popd_app,popd_cmd,popd_fsm,popd_listener,popd_sum,popd_util,
		smtpd_app,smtpd_sup,smtpd_fsm,smtpd_listener,smtpd_util,smtpd_cmd,smtpd_queue,
		smtpc,smtpc_fsm,smtpc_util,
		erlmail_test,erml_erlmail
		]).
	
