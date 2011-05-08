%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP Server OTP supervisor file
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
-module(imapd_sup).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").

-behaviour(supervisor).

-export([init/1,start_link/0]).
-export([start_client/0]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

start_link() ->
    ListenPort = erlmail_util:get_app_env(server_imap_port, 143),
	FSM = erlmail_util:get_app_env(server_imap_fsm, imapd_fsm),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, FSM]).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() -> supervisor:start_child(imapd_client_sup, []).



init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [% IMAP Response Server
              {imapd_resp,
               {imapd_resp,start_link,[]},
               permanent,
               2000,
               worker,
               [imapd_resp]
              },% IMAP TCP Listener
              {imapd_listener,
               {imapd_listener,start_link,[Port,Module]},
               permanent,
               2000,
               worker,
               [imapd_listener]
              },% Client instance supervisor
              {imapd_client_sup,
               {supervisor,start_link,[{local, imapd_client_sup}, ?MODULE, [Module]]},
               permanent,
               infinity,
               supervisor,
               []
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [% IMAPD Client
              {undefined,
               {Module,start_link,[]},
               temporary,
               2000,
               worker,
               []
              }
            ]
        }
    }.