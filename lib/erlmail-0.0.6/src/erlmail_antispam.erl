%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       ErlMail Anti-spam module
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
-module(erlmail_antispam).
-author('sjackson@simpleenigma.com').


-export([dnsbl/1,pre_deliver/1,post_deliver/1]).

%%-------------------------------------------------------------------------
%% @spec (IP::tuple()) -> {ok,Responses::list()}
%% @doc Takes an IP address and processes it through a list of
%% DNS blackhole lists. Each respones is a tuple key/value pair that the 
%% key is the name of hte DNS blackhole list and the value is a boolean,
%% stating if the IP address is in the blackhole list (true) or not (false)
%% @end
%%-------------------------------------------------------------------------
dnsbl(_IP) -> {ok,[]}.

%%-------------------------------------------------------------------------
%% @spec (Message::record()) -> {ok,NewMessage::record()}
%% @doc Takes a message and processes it before it is delivered to the 
%% message store.
%% @end
%%-------------------------------------------------------------------------
pre_deliver(Message) -> {ok,Message}.

%%-------------------------------------------------------------------------
%% @spec (Message::record()) -> {ok,NewMessage::record()}
%% @doc Takes a message and processes it after it is delivered to the 
%% message store.
%% @end
%%-------------------------------------------------------------------------
post_deliver(Message) -> {ok,Message}.

















