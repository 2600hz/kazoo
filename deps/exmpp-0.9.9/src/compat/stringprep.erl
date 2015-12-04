%%%----------------------------------------------------------------------
%%% File    : stringprep.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to stringprep_drv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@proces-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

%% @doc
%% This <strong>{@module}</strong> module is for compatibility with ejabberd.

-module(stringprep).
-author('alexey@process-one.net').

-export([start/0, start_link/0,
	 tolower/1,
	 nameprep/1,
	 nodeprep/1,
	 resourceprep/1]).

-define(STRINGPREP_PORT, stringprep_port).

-define(NAMEPREP_COMMAND, 1).
-define(NODEPREP_COMMAND, 2).
-define(RESOURCEPREP_COMMAND, 3).

start() ->
    exmpp_stringprep:start().

start_link() ->
    exmpp_stringprep:start_link().

%% @doc Deprecated for {@link exmpp_stringprep:to_lower/1}.
%% ```
%% - stringprep:tolower(String)
%% + exmpp_stringprep:to_lower(String)
%% '''

tolower(String) ->
    try
	exmpp_stringprep:to_lower(String)
    catch
	_Exception ->
	    error
    end.

nameprep(String) ->
    try
	exmpp_stringprep:nameprep(String)
    catch
	_Exception ->
	    error
    end.

nodeprep(String) ->
    try
	exmpp_stringprep:nodeprep(String)
    catch
	_Exception ->
	    error
    end.

resourceprep(String) ->
    try
	exmpp_stringprep:resourceprep(String)
    catch
	_Exception ->
	    error
    end.



