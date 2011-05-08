%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       MNESIA email store
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
-module(mnesia_store).
-author('sjackson@simpleenigma.com').
-include("../include/erlmail.hrl").
-behavior(gen_store).

% Behavior Exports
-export([create/1,drop/1,select/1,select/2,insert/1,delete/1,update/1,message_name/1]).
-export([list/0,list/1,check/1,mlist/3]).
-export([deliver/1,ensure_inbox/1]).

% Store Specific Exports
-export([init/1,create/2]).


%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @type store_type() = domain | user | message | mailbox_store
%% @doc Performs check command on store of Type. Commits and cleans up any
%%      actions that are not writen to disk on a regualar basis
%% @end
%%-------------------------------------------------------------------------
check(_Type) -> undefined.


%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
create(Type) -> create(Type,[node()]).

%%-------------------------------------------------------------------------
%% @spec (Type::store_type(),TableName::atom()) -> ok | undefined | {error,string()}
%% @doc Create command specific to Mnesia store
%% @hidden
%% @end
%%-------------------------------------------------------------------------
create(Type,NodeList) when Type =:= domain; Type =:= user; Type =:= message; Type =:= mailbox_store ->
	{TableName,TableDef} = case Type of
		domain  -> {erlmail_util:get_app_env(mnesia_table_domain,erlmail_domain), 
					[{disc_copies,NodeList},{type,set},{record_name,domain}, {attributes,record_info(fields,domain)}]};
		user    -> {erlmail_util:get_app_env(mnesia_table_user,erlmail_user), 
					[{disc_copies,NodeList},{type,set},{record_name,user},   {attributes,record_info(fields,user)}]};
		message -> {erlmail_util:get_app_env(mnesia_table_message,erlmail_message), 
					[{disc_copies,NodeList},{type,set},{record_name,message},{attributes,record_info(fields,message)}]};
		mailbox_store -> {erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store), 
					[{disc_copies,NodeList},{type,set},{record_name,mailbox_store},{attributes,record_info(fields,mailbox_store)}]}
	end,
	case mnesia:create_table(TableName,TableDef) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc Deletes entry in store of Type.
%% @end
%%-------------------------------------------------------------------------
delete(Domain)  when is_record(Domain,domain)         -> delete(Domain#domain.name,erlmail_util:get_app_env(mnesia_table_domain,erlmail_domain));
delete(User)    when is_record(User,user)             -> delete(User#user.name,erlmail_util:get_app_env(mnesia_table_user,erlmail_user));
delete(MailBox) when is_record(MailBox,mailbox_store) -> delete(MailBox#mailbox_store.name,erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store));
delete(Message) when is_record(Message,message)       -> delete(Message#message.name,erlmail_util:get_app_env(mnesia_table_message,erlmail_message)).

%%-------------------------------------------------------------------------
%% @spec (Type::store_type(),TableName::atom()) -> ok | undefined | {error,string()}
%% @doc delete command specific to Mnesia store
%% @hidden
%% @end
%%-------------------------------------------------------------------------
delete(Name,TableName) ->
	F = fun() ->
		mnesia:delete({TableName,Name})
		end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Message::tuple()) -> ok | undefined | {error,string()}
%% @doc Takes a #message{} record and deilvers it to a users INBOX
%% @end
%%-------------------------------------------------------------------------
deliver(Message) when is_record(Message,message) -> 
	{MessageName,UserName,DomainName} = Message#message.name,
	MailBoxTableName = erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store),
	MessageTableName = erlmail_util:get_app_env(mnesia_table_message,erlmail_message),
	ensure_inbox({UserName,DomainName}),
	F = fun() ->
		[MailBox] = mnesia:read({MailBoxTableName,{"INBOX",UserName,DomainName}}),
		NewMailBox = MailBox#mailbox_store{messages = lists:usort([MessageName|MailBox#mailbox_store.messages])},
		UID = MailBox#mailbox_store.uidnext,
		mnesia:write(MailBoxTableName,NewMailBox#mailbox_store{uidnext = UID + 1},write),
		mnesia:write(MessageTableName,Message#message{uid = UID},write)	
	end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Type::store_type()) -> ok | undefined | {error,string()}
%% @doc  Creates store for Type.
%% @end
%%-------------------------------------------------------------------------
drop(Type) when Type =:= domain; Type =:= user; Type =:= message; Type =:= mailbox_store -> 
	Key = list_to_atom("mnesia_table_" ++ atom_to_list(Type)),
	Default = list_to_atom("erlmail_" ++ atom_to_list(Type)),
	TableName = erlmail_util:get_app_env(Key, Default),
	case mnesia:delete_table(TableName) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec ({UserName::string(),DomainName::string()}) -> {ok,create} | {ok,exists} | undefined | {error,string()}
%% @doc  Makes sure that the specified user has an INBOX. Used in deliver command.
%% @end
%%-------------------------------------------------------------------------
ensure_inbox(User) when is_record(User,user) -> ensure_inbox(User#user.name);
ensure_inbox({UserName,DomainName}) ->
	TableName = erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store),
	F = fun() ->
		case mnesia:read({TableName,{"INBOX",UserName,DomainName}}) of
			[] -> 
				insert(#mailbox_store{name = {"INBOX",UserName,DomainName}, subscribed = true }),
				created;
			_ -> exists
		end
		
	end,
	case mnesia:sync_transaction(F) of
		{atomic,created} -> {ok,cretated};
		{atomic,exists} -> {ok,exists};
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (NodeList::list()) -> ok | undefined | {error,string()}
%% @doc  Initalizes data store. Mnesia specific command.
%% @end
%%-------------------------------------------------------------------------
init(NodeList) -> 
	case mnesia:create_schema(NodeList) of
		ok               -> ok;
		{atomic,ok}      -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Inserts Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
insert(Domain)  when is_record(Domain,domain)         -> insert(Domain,erlmail_util:get_app_env(mnesia_table_domain,erlmail_domain));
insert(User)    when is_record(User,user)             -> insert(User,erlmail_util:get_app_env(mnesia_table_user,erlmail_user));
insert(MailBox) when is_record(MailBox,mailbox_store) -> insert(MailBox,erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store));
insert(Message) when is_record(Message,message)       -> insert(Message,erlmail_util:get_app_env(mnesia_table_message,erlmail_message)).
%%-------------------------------------------------------------------------
%% @spec (Record::tuple(),TableName::list()) -> ok | undefined | {error,string()}
%% @doc  Mnesia Store internal INSERT implimentation
%% @hidden
%% @end
%%-------------------------------------------------------------------------
insert(Record,TableName) -> 
	F = fun() ->
		mnesia:write(TableName,Record,write)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,ok} -> ok;
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec () -> ok | undefined | {error,string()}
%% @doc  Lists all domains.
%% @end
%%-------------------------------------------------------------------------
list() ->
	MatchHead = #domain{name = '$1', _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_util:get_app_env(mnesia_table_domain,erlmail_domain),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end.
%%-------------------------------------------------------------------------
%% @spec (Type::any()) -> ok | undefined | {error,string()}
%% @doc  Lists all domains, users, messges or mailboxes depnding on 
%%       term that is given
%% @end
%%-------------------------------------------------------------------------
list(domain)  -> list();
list(domains) -> list();
list(DomainName) when is_list(DomainName) -> 
	MatchHead = #user{name = {'$1',DomainName}, _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_util:get_app_env(mnesia_table_user,erlmail_user),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end;
list({UserName,DomainName}) -> 
	MatchHead = #message{name = {'$1',UserName,DomainName}, _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_util:get_app_env(mnesia_table_message,erlmail_message),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> List;
		{aborted,Reason} -> {error,Reason}
	end.


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

mlist(MailBoxName,{UserName,DomainName},false) -> mlist(MailBoxName,{UserName,DomainName},'_');
mlist(MailBoxName,{UserName,DomainName},Subscribed) -> 
%	?D({MailBoxName,UserName,DomainName,Subscribed}),
	MatchHead = #mailbox_store{name = {'$1',UserName,DomainName}, subscribed = Subscribed, _ = '_'},
	Guard = [],
	Result = '$1',
	MatchSpec = [{MatchHead, Guard, [Result]}],
	TableName = erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store),
	F = fun() ->
		mnesia:select(TableName,MatchSpec)
		end,
	case mnesia:sync_transaction(F) of
		{atomic,List} -> 
			{ok,Clean,_line} = regexp:gsub(MailBoxName,"%","?"),
			 RegExp = regexp:sh_to_awk(Clean),
%			?D(RegExp),
%			?D(List),
			Filter = lists:filter(fun(Folder) -> 
				case regexp:match(Folder,RegExp) of
					{match,_,_} -> true;
					nomatch -> false
				end
				end,List),
			?D(Filter),
			lists:sort(Filter);
		{aborted,Reason} -> {error,Reason}
	end.

%%-------------------------------------------------------------------------
%% @spec (Name::any()) -> ok | undefined | {error,string()}
%% @doc  Retrives Record from the correct store.
%% @end
%%-------------------------------------------------------------------------
select(MailBox) when is_record(MailBox,mailbox_store)      ->
	{MailBoxName,UserName,DomainName} = MailBox#mailbox_store.name,
	select({MailBoxName,{UserName,DomainName}});
select(Domain)  when is_list(Domain)                       -> select(erlmail_util:get_app_env(mnesia_table_domain,erlmail_domain),Domain);
select({MailBoxName,{UserName,DomainName}})                -> select(erlmail_util:get_app_env(mnesia_table_mailbox_store,erlmail_mailbox_store),{MailBoxName,UserName,DomainName});
select(User)    when is_tuple(User), size(User) == 2       -> select(erlmail_util:get_app_env(mnesia_table_user,erlmail_user),User);
select(Message) when is_tuple(Message), size(Message) == 3 -> select(erlmail_util:get_app_env(mnesia_table_message,erlmail_message),Message).
%%-------------------------------------------------------------------------
%% @spec (Record::tuple(),TableName::list()) -> ok | undefined | {error,string()}
%% @doc  Mnesia Store internal SELECT implimentation
%% @hidden
%% @end
%%-------------------------------------------------------------------------
select(TableName,Key) ->
	F = fun() ->
		mnesia:read({TableName,Key})
		end,
	case mnesia:sync_transaction(F) of
		{atomic,[]} -> [];
		{atomic,[Record]} -> Record;
		{atomic,RecordList} when is_list(RecordList) -> RecordList;
		{aborted,Reason} -> {error,Reason}
	end.


%%-------------------------------------------------------------------------
%% @spec (Record::tuple()) -> ok | undefined | {error,string()}
%% @doc  Update Record into correct store. Records can be of types:
%%       #domain{}, #user{}, #message{} or #mailbox_store{}
%% @end
%%-------------------------------------------------------------------------
update(Record) -> insert(Record).
