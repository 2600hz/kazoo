%%%----------------------------------------------------------------------
%%% File    : xml.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : XML utils
%%% Created : 20 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
%%
%% Some replacements to make in ejabberd source code to work with exmpp: 
%% ```
%% - {xmlelement, _, _, SubEls} = Stanza
%% + SubEls = exmpp_xml:get_child_elements(Stanza)
%% '''
%% ```
%% - {xmlelement, "presence", [{"type", "unavailable"}], []}
%% + exmpp_presence:unavailable()
%% '''

-module(xml).
-author('alexey@process-one.net').

-include("exmpp.hrl").

-export([element_to_string/1,
	 crypt/1, make_text_node/1,
	 remove_cdata/1,
	 get_cdata/1, get_tag_cdata/1,
	 get_attr/2, get_attr_s/2,
	 get_tag_attr/2, get_tag_attr_s/2,
	 get_subtag/2, get_subtag_cdata/2,
         append_subtags/2,
	 get_path_s/2,
	 replace_tag_attr/3]).

%% Select at compile time how to escape characters in binary text
%% nodes.
%% Can be choosen with ./configure --enable-full-xml
-ifdef(FULL_XML_SUPPORT).
-define(ESCAPE_BINARY(CData), make_text_node(CData)).
-else.
-define(ESCAPE_BINARY(CData), crypt(CData)).
-endif.

%% @doc Deprecated for {@link exmpp_xml:document_to_list/1}.
%% ```
%% - xml:element_to_string(El)
%% + exmpp_xml:document_to_list(El)
%% '''

element_to_string(El) ->
    exmpp_xml:document_to_list(El).

crypt(S) ->
    exmpp_xml:escape_using_entities(S).

%% Make a cdata_binary depending on what characters it contains
make_text_node(CData) ->
    exmpp_xml:escape_using_cdata(CData).

%% @doc Deprecated for {@link exmpp_xml:get_child_elements/1}.
%% ```
%% - xml:remove_cdata(Els)
%% + exmpp_xml:get_child_elements(SubEl)
%% '''

remove_cdata(L) -> exmpp_xml:remove_cdata_from_list(L).

%% @doc Deprecated for {@link exmpp_xml:get_cdata_from_list_as_list/1},
%% {@link exmpp_xml:get_cdata_as_list/1}.
%% ```
%% - xml:get_cdata(Els)
%% + exmpp_xml:get_cdata_from_list_as_list(Els)
%% '''
%% ```
%% - xml:get_cdata(Els)
%% + exmpp_xml:get_cdata_as_list(El)
%% '''

get_cdata(L) ->
    exmpp_xml:get_cdata_from_list_as_list(L).

%% @doc Deprecated for {@link exmpp_xml:get_cdata/1}.
%% ```
%% - xml:get_tag_cdata(El)
%% + exmpp_xml:get_cdata(El)
%% '''

get_tag_cdata({xmlelement, _Name, _Attrs, Els}) ->
    get_cdata(Els).

get_attr(AttrName, Attrs) ->
    case exmpp_xml:get_attribute_from_list_as_list(Attrs, AttrName,
						   undefined) of
	undefined ->
	    false;
	Val ->
	    {value, Val}
    end.

%% @doc Deprecated for {@link exmpp_xml:get_attribute_from_list_as_list/3}.
%% ```
%% - get_attr_s(AttrName, Attrs)
%% + exmpp_xml:get_attribute_from_list_as_list(Attrs, AttrName, "")
%% '''
%% ```
%% - xml:get_attr_s("to", Attrs)
%% + exmpp_stanza:get_recipient(Stanza)
%% '''
%% Deprecated for {@link proplists:get_value/3}.
%% ```
%% - xml:get_attr_s("username", KeyVals)
%% + proplists:get_value("username", KeyVals, "")
%% '''
%% Deprecated for {@link exmpp_stream:get_lang/1}.
%% ```
%% - xml:get_attr_s("xml:lang", Attrs)
%% + exmpp_stream:get_lang(Stanza)
%% '''
%% Deprecated for {@link exmpp_stream:get_version/1}.
%% ```
%% - xml:get_attr_s("version", Attrs) of
%% + exmpp_stream:get_version(Stanza)
%% '''
%% Deprecated for {@link exmpp_stanza:get_id_from_attrs/1}.
%% ```
%% - xml:get_attr_s("id", Attrs)
%% + exmpp_stanza:get_id_from_attrs(Attrs)
%% '''

get_attr_s(AttrName, Attrs) ->
    exmpp_xml:get_attribute_from_list_as_list(Attrs, AttrName, "").

%% @doc Deprecated for {@link exmpp_xml:get_attribute_as_binary/3}.
%% ```
%% - xml:get_tag_attr("affiliation", Item)
%% + exmpp_xml:get_attribute_as_binary(Item, 'affiliation', false)
%% '''

get_tag_attr(AttrName, #xmlel{attrs = Attrs}) ->
    get_attr(AttrName, Attrs);
get_tag_attr(AttrName, #xmlelement{attrs = Attrs}) ->
    get_attr(AttrName, Attrs).

%% @doc Deprecated for {@link exmpp_xml:get_attribute_as_list/3}.
%% ```
%% - xml:get_tag_attr_s("node", SubEl)
%% + exmpp_xml:get_attribute_as_list(SubEl, 'node', "")
%% '''

get_tag_attr_s(AttrName, El) ->
    exmpp_xml:get_attribute_as_list(El, AttrName, "").

%% @doc Deprecated for {@link exmpp_xml:get_element/2}.
%% ```
%% - xml:get_subtag(El, "x")
%% + exmpp_xml:get_element(El, x)
%% '''
%% ```
%% - xml:get_subtag(Packet, "status")
%% + exmpp_presence:get_status(Packet)
%% '''

get_subtag(El, Name) ->
    case exmpp_xml:get_element(El, Name) of
	undefined -> false;
	Sub_El    -> Sub_El
    end.

%% @doc Deprecated for {@link exmpp_xml:get_cdata/1},
%% {@link exmpp_xml:get_cdata_as_list/1}.
%% ```
%% - xml:get_subtag_cdata(Packet, "body")
%% + exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Packet, 'body'))
%% '''

get_subtag_cdata(Tag, Name) ->
    exmpp_xml:get_cdata(Tag, Name).

%% @doc Deprecated for {@link exmpp_xml:append_children/1}.
%% ```
%% - xml:append_subtags(Stanza, SubEls)
%% + exmpp_xml:append_children(Stanza, SubEls)
%% '''

append_subtags(Stanza, SubEls) ->
    exmpp_xml:append_children(Stanza, SubEls).

%% @doc Deprecated for {@link exmpp_presence:get_show/1}.
%% ```
%% - xml:get_path_s(Stanza, [{elem, "show"}, cdata])
%% + exmpp_presence:get_show(Stanza)
%% '''

get_path_s(El, []) ->
    El;
get_path_s(El, [{elem, Name} | Path]) ->
    case get_subtag(El, Name) of
	false ->
	    "";
	SubEl ->
	    get_path_s(SubEl, Path)
    end;
get_path_s(El, [{attr, Name}]) ->
    get_tag_attr_s(Name, El);
get_path_s(El, [cdata]) ->
    get_tag_cdata(El).

%% @doc Deprecated for {@link exmpp_stanza:set_lang/2} and others.
%% ```
%% - xml:replace_tag_attr("xml:lang", Lang, El)
%% + exmpp_stanza:set_lang(El, Lang)
%% '''

replace_tag_attr(Attr, Value, El) ->
    exmpp_xml:set_attribute(El, Attr, Value).


