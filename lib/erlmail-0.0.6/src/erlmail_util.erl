%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       Erlmail utility functions
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
-module(erlmail_util).
-author('sjackson@simpleenigma.com').

-include("../include/erlmail.hrl").
-include("../include/smtp.hrl").
-include("../include/imap.hrl").

-define(APPLICATION,erlmail).

-export([split_email/1,combine_email/1,combine_email/2]).
-export([create/4,join/1,remove/1,check/1,check/4]).
-export([split_at/1,split_at/2,rsplit_at/1,rsplit_at/2,unquote/1]).
-export([to_lower_atom/1]).
-export([get_app_env/2]).



split_email(Atom) when is_atom(Atom) -> split_email(atom_to_list(Atom));
split_email([]) -> {[],[]};
split_email(EmailAddress) ->
	case string:tokens(string:strip(EmailAddress),[64]) of
		[UserName,DomainName] -> {UserName,DomainName};
		_AnythingsElse -> {[],[]}
	end.

%%-------------------------------------------------------------------------
%% @spec (String::string()) -> {string(),string()}
%% @doc Splits the given string into two strings at the first SPACE (chr(32))
%% @end
%%-------------------------------------------------------------------------
split_at(String) -> split_at(String,32).
%%-------------------------------------------------------------------------
%% @spec (String::string(),Chr::char()) -> {string(),string()}
%% @doc Splits the given string into two strings at the first instace of Chr
%% @end
%%-------------------------------------------------------------------------
split_at(String,Chr) -> 
	case string:chr(String, Chr) of
		0 -> {String,[]};
		Pos -> 
			case lists:split(Pos,String) of
				{One,Two} -> {string:strip(One),Two};
				Other -> Other
			end
	end.
%%-------------------------------------------------------------------------
%% @spec (String::string()) -> {string(),string()}
%% @doc Splits the given string into two strings at the last SPACE (chr(32))
%% @end
%%-------------------------------------------------------------------------
rsplit_at(String) -> rsplit_at(String,32).
%%-------------------------------------------------------------------------
%% @spec (String::string(),Chr::char()) -> {string(),string()}
%% @doc Splits the given string into two strings at the last instace of Chr
%% @end
%%-------------------------------------------------------------------------
rsplit_at(String,Chr) -> 
	case string:rchr(String, Chr) of
		0 -> {String,[]};
		Pos -> 
			case lists:split(Pos,String) of
				{One,Two} -> {string:strip(One),Two};
				Other -> Other
			end
	end.



combine_email([])                                 -> [];
combine_email({[],[]})                            -> [];
combine_email({UsersName,DomainName})             -> combine_email(UsersName,DomainName).
combine_email(Atom,DomainName) when is_atom(Atom) -> combine_email(atom_to_list(Atom),DomainName);
combine_email(UsersName,Atom)  when is_atom(Atom) -> combine_email(UsersName,atom_to_list(Atom));
combine_email(UsersName,DomainName)               -> UsersName ++ [64] ++ DomainName.










get_app_env(Opt, Default) ->
	case lists:keysearch(?APPLICATION, 1, application:loaded_applications()) of
		false -> application:load(?APPLICATION);
		_ -> ok
	end,
	case application:get_env(?APPLICATION, Opt) of
	{ok, Val} -> Val;
	_ ->
		case init:get_argument(Opt) of
		[[Val | _]] -> Val;
		error		-> Default
		end
	end.


%%-------------------------------------------------------------------------
%% @spec (String::string()) -> string()
%% @doc Removes Double Quotes and white space from both sides of a string
%% @end
%%-------------------------------------------------------------------------
unquote(String) -> 
	S2 = string:strip(String,both,32),
	string:strip(S2,both,34).





to_lower_atom(Atom) when is_atom(Atom) -> to_lower_atom(atom_to_list(Atom));
to_lower_atom(String) when is_list(String) ->
	list_to_atom(string:to_lower(String)).





%%-------------------------------------------------------------------------
%% @spec (TableName::string(),RecordName::string(),Type::atom(),Keys::list()) -> ok | {error,Reason::atom()}
%% @doc  create imap_resp mnesia table
%% @end
%% @private
%%-------------------------------------------------------------------------
create(TableName,RecordName,Type,Keys) -> 
	RecordInfo = case RecordName of
		message_store -> record_info(fields, message_store);
		imap_resp -> record_info(fields, imap_resp)
	end,
	mnesia:create_table(TableName,[{ram_copies,[node()]},{attributes,RecordInfo},{type,Type}]),
	lists:map(fun(Key) -> 
		mnesia:add_table_index(TableName,Key)
		end,Keys),
	ok.
%%-------------------------------------------------------------------------
%% @spec (TableName::string()) -> ok | {error,Reason::atom()}
%% @doc  Join existing imap_resp mnesia table
%% @end
%% @private
%%-------------------------------------------------------------------------
join(TableName) -> mnesia:add_table_copy(TableName,node(),ram_copies).
%%-------------------------------------------------------------------------
%% @spec (TableName::string()) -> ok | {error,Reason::atom()}
%% @doc  Remove node from mnesia table list
%% @end
%% @private
%%-------------------------------------------------------------------------
remove(TableName) -> 
	case catch mnesia:table_info(TableName,ram_copies) of
		{'EXIT',_Reason} -> {error,table_not_found};
		[Node] when Node == node() -> 
			mnesia:delete_table(TableName),
			ok;
		NodeList -> 
			case lists:member(node(),NodeList) of
				true -> 
					mnesia:del_table_copy(TableName,node()),
					ok;
				false -> ok
			end
	end.

%%-------------------------------------------------------------------------
%% @spec (TableName::string()) -> ok | {error,Reason::atom()}
%% @doc  Checks to see if mnesia is started and imap_resp table is active.
%% @end
%% @private
%%-------------------------------------------------------------------------
check(TableName) -> 
	case lists:keysearch(mnesia,1,application:loaded_applications()) of
		{value,_} -> ok;		
		_ -> mnesia:start()
	end,
	case catch mnesia:table_info(TableName,ram_copies) of
		[] -> {error,no_ram_copies};
		NodeList when is_list(NodeList) -> 
			case lists:member(node(),NodeList) of
				true -> ok;
				false -> {error,local_node_not_in_node_list}
			end;
		{'EXIT',_Reason} -> {error,table_not_found}
	end.

%%-------------------------------------------------------------------------
%% @spec (TableName::string(),RecordName::string(),Type::atom(),Keys::list()) -> ok | {error,Reason::atom()}
%% @doc  If Type=['join'|'create'] node will run check/0 then init 
%%       imap_resp table. If Type is anything else only check/0 is run.
%% @end
%% @private
%%-------------------------------------------------------------------------
check(TableName,RecordName,Type,Keys) ->
	case check(TableName) of
		ok -> ok;
		{error,table_not_found} -> create(TableName,RecordName,Type,Keys);
		{error,local_node_not_in_node_list} -> join(TableName)
	end.
