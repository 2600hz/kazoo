%%%----------------------------------------------------------------------
%%% File    : xml_stream.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Parse XML streams
%%% Created : 17 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(xml_stream).
-author('alexey@process-one.net').

-export([new/1,
	 new/2,
	 parse/2,
	 close/1,
	 parse_element/1]).

-define(XML_START, 0).
-define(XML_END,   1).
-define(XML_CDATA, 2).
-define(XML_ERROR, 3).

-define(PARSE_COMMAND, 0).
-define(PARSE_FINAL_COMMAND, 1).


new(CallbackPid) ->
    new(CallbackPid, infinity).

new(CallbackPid, MaxSize) ->
    exmpp_xmlstream:start({gen_fsm, CallbackPid}, [{maxsize, MaxSize}]).

%% @doc Deprecated for {@link exmpp_xmlstream:parse/2}.
%% ```
%% - xml_stream:parse(XMLStreamState, Data)
%% + exmpp_xmlstream:parse(XMLStreamState, Data)
%% '''

parse(State, Str) ->
    try
        {ok, New_State} = exmpp_xmlstream:parse(State, Str),
        New_State
    catch
	throw:{xml_parser, parsing, stanza_too_big, _} ->
	    exmpp_xmlstream:send_events(
	      State,
	      [{xmlstreamerror, "XML stanza is too big"}]),
            State
    end.

close(State) ->
    exmpp_xmlstream:stop(State).

%% @doc Deprecated for {@link exmpp_xml:parse_document/2}.
%% ```
%% - El = xml_stream:parse_element(Text)
%% + [El] = exmpp_xml:parse_document(Text, [names_as_atom])
%% '''

parse_element(Str) ->
    exmpp_xmlstream:parse_element(Str).

