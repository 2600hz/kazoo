%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       MySQL Email store
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
-module(mysql_store).
-author('sjackson@simpleenigma.com').
-include("../include/erlmail.hrl").
-behavior(gen_store).

-export([create/1,drop/1,select/1,select/2,insert/1,delete/1,update/1,message_name/1]).
-export([list/0,list/1,check/1,mlist/3]).
-export([deliver/1,ensure_inbox/1]).

%%% Create all SQL statements in macros for easy replacement
-define(MYSQL_STORE_SQL_SELECT_USER,"").
%%%


%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @type store_type() = domain | user | message | mailbox_store
%% @doc Performs check command on store of Type. Commits and cleans up any
%%      actions that are not writen to disk on a regualr basis
%% @end
%%-------------------------------------------------------------------------
check(_Type) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
create(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Deletes entry in store of Type.
%% @end
%%-------------------------------------------------------------------------
delete(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Message::tuple()) -> ok | undefined | {error,string()}
%% @doc Takes a #message{} record and deilvers it to a users INBOX
%% @end
%%-------------------------------------------------------------------------
deliver(Message) when is_record(Message,message) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc  Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
drop(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec ({UserName::string(),DomainName::string()}) -> {ok,create} | {ok,exists} | undefined | {error,string()}
%% @doc  Makes sure that the specified user has an INBOX. Used in deliver command.
%% @end
%%-------------------------------------------------------------------------
ensure_inbox(User) when is_record(User,user) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Inserts Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
insert(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec () -> ok | undefined | {error,string()}
%% @doc  Lists all domains.
%% @end
%%-------------------------------------------------------------------------
list() -> undefined.
%%-------------------------------------------------------------------------
%% @spec (Type::any()) -> ok | undefined | {error,string()}
%% @doc  Lists all domains, users, messges or mailboxes depnding on 
%%       term that is given
%% @end
%%-------------------------------------------------------------------------
list(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Args::any()) -> string() | undefined | {error,string()}
%% @doc  Generates message name. May use gen_store:message_name/1 or
%%       be specific to the message store.
%% @end
%%-------------------------------------------------------------------------
message_name(Args) -> gen_store:message_name(Args).

%%-------------------------------------------------------------------------
%% @spec (MailBoxName::string(),{UserName::string(),DomainName::string()},Subscribed::bool()) -> list()
%% @doc Lists mailbox information for LIST and LSUB commands
%% @end
%%-------------------------------------------------------------------------
mlist(_MailBoxName,{_UserName,_DomainName},_Subscribed) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Name::any()) -> ok | undefined | {error,string()}
%% @doc  Retrives Record from the correct store.
%% @end
%%-------------------------------------------------------------------------
select(_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple(),TableName::list()) -> ok | undefined | {error,string()}
%% @doc  MySQL Store internal SELECT implimentation
%% @hidden
%% @end
%%-------------------------------------------------------------------------
select(_,_) -> undefined.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Update Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
update(_Record) -> undefined.
