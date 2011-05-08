%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       Generalized Message Store behavior defination
%%% @reference See <a href="http://erlsoft.org/modules/erlmail" target="_top">Erlang Software Framework</a> for more information
%%% @reference See <a href="http://erlmail.googlecode.com" target="_top">ErlMail Google Code Repository</a> for more information
%%% @version   0.0.6
%%% @since     0.0.5
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
-module(gen_store).
-author('sjackson@simpleenigma.com').
-include("../include/erlmail.hrl").
-include("../include/imap.hrl").

-export([message_name/1,lookup/1]).
-export([select/2,select/3,update/2]).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{create,1},{drop,1},{insert,1},{update,1},{delete,1},{select,2},
								{list,0},{list,1},
								{message_name,1},{ensure_inbox,1},{deliver,1},{check,1},{mlist,3}
								];
behaviour_info(_Other) -> undefined.




select(MailBox,_State) when is_record(MailBox,mailbox_store) -> 
	Store = erlmail_util:get_app_env(store_type_mailbox_store,mnesia_store),
	Store:select(MailBox).


select(message,MessageName,#imapd_fsm{user = User} = _State) ->
	Store = erlmail_util:get_app_env(store_type_message,mnesia_store),
	{UserName,DomainName} = User#user.name,
	Store:select({MessageName,UserName,DomainName}).


update(Message,_State) when is_record(Message,message) ->
	Store = erlmail_util:get_app_env(store_type_message,mnesia_store),
	Store:update(Message).














lookup(domain)        -> erlmail_util:get_app_env(store_type_domain,mnesia_store);
lookup(user)          -> erlmail_util:get_app_env(store_type_user,mnesia_store);
lookup(message)       -> erlmail_util:get_app_env(store_type_message,mnesia_store);
lookup(mailbox_store) -> erlmail_util:get_app_env(store_type_mailbox_store,mnesia_store);
lookup(all)           ->
	{lookup(domain),lookup(user),lookup(message),lookup(mailbox_store)}.




message_name(_Args) ->
	{Number,_} = random:uniform_s(9000,now()),
	Seconds = calendar:datetime_to_gregorian_seconds({date(),time()}) - 62167219200,
	Node = atom_to_list(node()),
	[_N,Host] = string:tokens(Node,[64]),
	integer_to_list(Seconds) ++ [46] ++ 						  % Seconds since UNIX Epoch (1-1-1970)
	[77] ++ integer_to_list(calendar:time_to_seconds(time())) ++  % M then Seconds in day
	[82] ++ integer_to_list(Number + 1000) ++ [46] ++             % R then random number between 1,001 and 10,000
	Host.