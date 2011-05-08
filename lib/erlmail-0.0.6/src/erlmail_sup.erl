%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail Supervisor definition file
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
-module(erlmail_sup).
-author('sjackson@simpleenigma.com').

-include("../include/erlmail.hrl").

-behaviour(supervisor).

-export([init/1]).
-export([start_link/0,child_spec/1,children/1]).


start_link() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).


init(_Args) -> 
	{ok,Servers} = application:get_env(erlmail,server_list),
	RestartStrategy = one_for_one,
	MaxR = 1,
	MaxT = 60,
	Children = [child_spec(erlmail_store) | children(Servers)],
	case supervisor:check_childspecs(Children) of
		ok -> {ok, {{RestartStrategy,MaxR,MaxT}, Children}};
		{error,_Reason} -> ignore
	end.


children(List) -> 
	Specs = lists:map(fun(C) -> 
		S = "server_" ++ atom_to_list(C) ++ "_start", 
		case application:get_env(erlmail,list_to_atom(S)) of
			{ok,true} -> child_spec(C);
			_ -> []
		end
		end,List),
	lists:flatten(Specs).


child_spec(erlmail_store) -> {erlmail_store,{erlmail_store_sup,start_link,[]},permanent,5000,supervisor,[erlmail_store_sup]};
child_spec(smtp)          -> {smtpd,{smtpd_sup,start_link,[]},permanent,5000,supervisor,[smtpd_sup]};
child_spec(imap)          -> {imapd,{imapd_sup,start_link,[]},permanent,5000,supervisor,[imapd_sup]};
child_spec(pop)           -> {popd, {popd_sup, start_link,[]},permanent,5000,supervisor,[popd_sup]};
child_spec(_)             -> [].













