%%%---------------------------------------------------------------------------------------
%%% @author    Stuart Jackson <sjackson@simpleenigma.com> [http://erlsoft.org]
%%% @copyright 2006 - 2007 Simple Enigma, Inc. All Rights Reserved.
%%% @doc       IMAP server extention processing
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
-module(imapd_ext).
-author('sjackson@simpleenigma.com').
-include("../include/imap.hrl").

-export([list/0,list/1,verify/2,check/1]).
-export([capability/0]).


%%-------------------------------------------------------------------------
%% @spec () -> string()
%% @doc Check and verify extentions and return capability string
%% @end
%%-------------------------------------------------------------------------
capability() ->
	Cap = lists:foldl(fun({_M,Ext},Acc) -> [32,http_util:to_upper(atom_to_list(Ext))|Acc] 
		end,[32,"IMAP4rev1"],list()),
	string:strip(lists:flatten(lists:reverse(Cap))).

%%-------------------------------------------------------------------------
%% @spec (Key::atom()) -> {ok,tuple()} | {error,atom()}
%% @doc Check to see if extention is in the config file
%% @end
%%-------------------------------------------------------------------------
check(Key) ->
	case lists:keysearch(Key,2,list()) of
		{value,{Module,Function}} -> {ok,Module,Function};
		_ -> {error,extention_not_found}
	end.

%%-------------------------------------------------------------------------
%% @spec () -> list()
%% @doc List all verified extentions from config file
%% @end
%%-------------------------------------------------------------------------

list() -> list(erlmail_util:get_app_env(server_imap_extentions,[])).
%%-------------------------------------------------------------------------
%% @spec (ExtentionList::list()) -> list()
%% @doc List all verified extentions from ExtentionList
%% @end
%%-------------------------------------------------------------------------
list(ExtentionList) -> 
	Tokens = string:tokens(ExtentionList,[32]),
	Ext = lists:map(fun(Ext) -> 
		case string:tokens(Ext,[58]) of
			[M,F] -> {list_to_atom(M),list_to_atom(F)};
			[F] -> {imapd_ext,list_to_atom(F)}
		end
		end,Tokens),
	lists:foldl(fun({Module,Function},Acc) ->
		case verify(Module,Function) of
			ok -> [{Module,Function}|Acc];
			_ -> Acc
		end
		end,[],Ext).
	

%%-------------------------------------------------------------------------
%% @spec (Module::atom(),Funcation::atom()) -> bool()
%% @doc Verify that Module:Function/2 exists in Erlang path
%% @end
%%-------------------------------------------------------------------------
verify(Module,Function) ->
	case code:get_object_code(Module) of
		{Module,_Obj,BeamPath} ->
			case beam_lib:chunks(BeamPath,[exports]) of
				{ok,{_Mod,[{exports,Exports}]}} ->
					case lists:member({Function,2},Exports) of
						true -> ok;
						false -> {error,function_not_found}
					end;
				{error,_BeamLib,Reason} -> {error,Reason}
			end;
		error -> {error,module_not_loaded}
	end.