%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       EMail Common Test Suite. This common test suite is designed to work with ErlMail as well as to test any general SMTP, IMAP4 or POP3 server.
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.6
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
-module(email_suite).
-author('sjackson@simpleenigma.com').

%%% If you get an error about ct.hrl not being able to include test_server.hrl then
%%% Change the ct.hrl that that shows:
%%% -include("test_server.hrl").
%%% so that it reads
%%% -include_lib("test_Server/include/test_server.hrl").
%%%
-include_lib("common_test/include/ct.hrl").

-export([all/0,suite/0]).
-export([init_per_suite/2,end_per_suite/2]).

-export([smtp/0,smtp/1,imap/0,imap/1,pop/0,pop/1]).


all() -> [smtp,imap,pop].

init_per_suite(_TestCaseName,Config) -> Config.
end_per_suite(_TestCaseName,_Config) -> ok.

suite() ->
	[
	  {timetrap,{minutes,1}},
	  {require,server}
	].



smtp() -> 
	[
	  {userdata,[{doc,"Perform all SMTP commands"}]}
	].
smtp(Config) -> 
	_Server = ?config(server,Config),
	ok.


imap() -> 
	[
	  {userdata,[{doc,"Perform all IMAP commands"}]}
	].
imap(Config) -> 
	Server = ?config(server,Config),
	{ok,Fsm} = imapc:connect(Server),
	{ok,noop_successfull} = imapc:noop(Fsm),
	{ok,capability_successful,_Capabilities} = imapc:capability(Fsm),
	{ok,logout_successful} = imapc:logout(Fsm),	
	ok.


pop() -> 
	[
	  {userdata,[{doc,"Perform all POP commands"}]}
	].
pop(Config) -> 
	_Server = ?config(server,Config),
	ok.





