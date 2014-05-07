%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> provides utilities to open and
%% close an XMPP stream, negotiate features and handle stream errors.
%%
%% {@link exmpp_client_stream} and {@link exmpp_server_stream} should be
%% prefered to {@module} because they'll set some defaults values for
%% the caller.
%%
%% <h3>Stream handling</h3>
%%
%% <p>
%% It covers these basic functions:
%% </p>
%% <ul>
%% <li>Open a stream to an XMPP server</li>
%% <li>Open a stream in reply to initiating entity</li>
%% <li>Close a stream (regardless who has initiated the stream)</li>
%% </ul>
%%
%% <p>
%% This table illustrates stream opening and closing.
%% </p>
%% <table class="illustration">
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client call `{@module}':
%% </p>
%% <pre>Opening = exmpp_stream:opening(
%%   "jabber.example.com",
%%   ?NS_JABBER_CLIENT,
%%   "1.0"<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;stream:stream xmlns:stream="http://etherx.jabber.org/streams"
%%   xmlns="jabber:client" to="jabber.example.org" version="1.0"&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% If the server accepts the client stream opening, it'll call:
%% </p>
%% <pre>Opening_Reply = exmpp_stream:opening_reply(
%%   Opening,
%%   random<br/>).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;stream:stream xmlns:stream="http://etherx.jabber.org/streams"
%%   xmlns="jabber:client" version="1.0" from="jabber.example.org"
%%   id="stream-396429316"&gt;</pre>
%% <p>
%% Note that `{@module}' generated an ID automatically; you may override
%% this.
%% </p>
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% At the end of the communication, the client close its stream:
%% </p>
%% <pre>Client_Closing = exmpp_stream:closing().</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server do the same:
%% </p>
%% <pre>Server_Closing = exmpp_stream:closing(Client_Closing).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;/stream:stream&gt;</pre>
%% <p>
%% The server may use the same function clause than the client but here,
%% it gives the client closing to the function. This is to be sure to
%% use the same XML prefix.
%% </p>
%% </td>
%% </tr>
%% </table>

-module(exmpp_stream).

-include("exmpp.hrl").

%% avoid name clash with local error/2 function
-compile({no_auto_import,[error/2]}).

%% Creating elements.
-export([
	 opening/3,
	 opening/4,
	 opening_reply/4,
	 opening_reply/5,
	 opening_reply/2,
	 opening_reply/3,
	 closing/0,
	 closing/1
	]).

%% Attributes handling.
-export([
	 get_receiving_entity/1,
	 set_receiving_entity/2,
	 get_initiating_entity/1,
	 set_initiating_entity/2,
	 get_default_ns/1,
	 set_default_ns/2,
	 get_version/1,
	 set_version/2,
	 get_id/1,
	 set_id/2,
	 get_lang/1,
	 set_lang/2
	]).

%% Version handling.
-export([
	 parse_version/1,
	 serialize_version/1
	]).

%% Features announcement.
-export([
	 set_dialback_support/1,
	 features/1
	]).

%% Error handling.
-export([
	 error/1,
	 error/2,
	 is_error/1,
	 get_condition/1,
	 get_text/1
	]).

%% Serialization wrappers.
-export([
	 to_list/1,
	 to_binary/1,
	 to_iolist/1
	]).

%% --------------------------------------------------------------------
%% Type definitions.
%% --------------------------------------------------------------------

-type(streamversion() :: {non_neg_integer(), non_neg_integer()}).

%% --------------------------------------------------------------------
%% Stream opening/closing.
%% --------------------------------------------------------------------

%% @spec (To, Default_NS, Version) -> Opening
%%     To = binary() | string() | undefined
%%     Default_NS = atom() | string()
%%     Version = binary() | string() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Opening = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% @see opening/4.

-spec opening
(binary() | string() | undefined, xmlname(),
 binary() | string() | streamversion()) ->
    xmlel().

opening(To, Default_NS, Version) ->
    opening(To, Default_NS, Version, undefined).

%% @spec (To, Default_NS, Version, Lang) -> Opening
%%     To = binary() | string() | undefined
%%     Default_NS = atom() | string()
%%     Version = binary() | string() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Lang = binary() | string() | undefined
%%     Opening = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening tag.
%%
%% This element is supposed to be sent by the initiating entity
%% to the receiving entity (for the other way around, see {@link
%% opening_reply/1}).

-spec opening
(binary() | string() | undefined, xmlname(),
 binary() | string() | streamversion(), binary() | string() | undefined) ->
    xmlel().

opening(To, Default_NS, Version, Lang) ->
    %% Prepare attributes.
    Attrs1 = set_receiving_entity_in_attrs([], To),
    Attrs2 = set_version_in_attrs(Attrs1, Version),
    Attrs3 = case Lang of
		 undefined -> Attrs2;
		 _         -> set_lang_in_attrs(Attrs2, Lang)
	     end,
    %% Create element.
    #xmlel{ns          = ?NS_XMPP,
	   declared_ns = [{?NS_XMPP, ?NS_XMPP_pfx}, {Default_NS, none}],
	   name        = 'stream',
	   attrs       = Attrs3,
	   children    = undefined
	  }.

%% @spec (From, Default_NS, Version, ID) -> Opening_Reply
%%     From = binary() | string() | undefined
%%     Default_NS = atom() | string()
%%     Version = binary() | string() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() | string() | undefined
%%     Opening_Reply = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% @see opening_reply/5.

-spec opening_reply
(binary() | string() | undefined, xmlname(),
 binary() | string() | streamversion(), binary() | string() | random) ->
    xmlel().

opening_reply(From, Default_NS, Version, ID) ->
    opening_reply(From, Default_NS, Version, ID, undefined).

%% @spec (From, Default_NS, Version, ID, Lang) -> Opening_Reply
%%     From = binary() | string() | undefined
%%     Default_NS = atom() | string()
%%     Version = binary() | string() | {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     ID = binary() | string() | random
%%     Lang = binary() | string() | undefined
%%     Opening_Reply = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening reply tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(binary() | string() | undefined, xmlname(),
 binary() | string() | streamversion(), binary() | string() | random,
 binary() | string() | undefined) ->
    xmlel().

opening_reply(From, Default_NS, Version, ID, Lang) ->
    %% Prepare attributes.
    Attrs1 = set_initiating_entity_in_attrs([], From),
    Attrs2 = set_version_in_attrs(Attrs1, Version),
    Attrs3 = set_id_in_attrs(Attrs2, ID),
    Attrs4 = case Lang of
		 undefined -> Attrs3;
		 _         -> set_lang_in_attrs(Attrs3, Lang)
	     end,
    %% Create element.
    #xmlel{ns          = ?NS_XMPP,
	   declared_ns = [{?NS_XMPP, ?NS_XMPP_pfx}, {Default_NS, none}],
	   name        = 'stream',
	   attrs       = Attrs4,
	   children    = undefined
	  }.

%% @spec (Opening, ID) -> Opening_Reply
%%     Opening = exmpp_xml:xmlel()
%%     ID = binary() | string() | random
%%     Opening_Reply = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(xmlel(), binary() | string() | random) -> xmlel().

opening_reply(#xmlel{attrs = Attrs} = Opening, ID) ->
    Attrs1 = exmpp_stanza:reply_from_attrs(Attrs),
    Attrs2 = set_id_in_attrs(Attrs1, ID),
    Opening#xmlel{attrs = Attrs2}.

%% @spec (Opening, ID, Lang) -> Opening_Reply
%%     Opening = exmpp_xml:xmlel()
%%     ID = binary() | string() | random
%%     Lang = binary() | string() | undefined
%%     Opening_Reply = exmpp_xml:xmlel()
%% @doc Make a `<stream>' opening reply tag for the given `Opening' tag.
%%
%% This element is supposed to be sent by the receiving entity in reply
%% to the initiating entity (for the other way around, see {@link
%% opening/1}).
%%
%% If `ID' is `random', one will be generated automatically.

-spec opening_reply
(xmlel(), binary() | string() | random, binary() | string() | undefined) ->
    xmlel().

opening_reply(#xmlel{attrs = Attrs} = Opening, ID, Lang) ->
    Attrs1 = exmpp_stanza:reply_from_attrs(Attrs),
    Attrs2 = set_id_in_attrs(Attrs1, ID),
    Attrs3 = case Lang of
		 undefined -> Attrs2;
		 _         -> set_lang_in_attrs(Attrs2, Lang)
	     end,
    Opening#xmlel{attrs = Attrs3}.

%% @spec () -> Closing
%%     Closing = exmpp_xml:xmlendtag()
%% @doc Make a `</stream>' closing tag.

-spec closing
() -> xmlendtag().

closing() ->
    #xmlendtag{ns = ?NS_XMPP, name = 'stream'}.

%% @spec (Opening) -> Closing
%%     Opening = exmpp_xml:xmlel()
%%     Closing = exmpp_xml:xmlendtag()
%% @doc Make a `</stream>' closing tag for the given `Opening' tag.

-spec closing
(xmlel()) -> xmlendtag().

closing(#xmlel{ns = NS, name = Name}) ->
    #xmlendtag{ns = NS, name = Name}.

%% --------------------------------------------------------------------
%% Stream standard attributes.
%% --------------------------------------------------------------------

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exmpp_xml:xmlel()
%%     Hostname = binary()
%% @doc Return the receiving entity hostname.

-spec get_receiving_entity
(xmlel()) -> binary() | undefined.

get_receiving_entity(Opening) ->
    exmpp_xml:get_attribute_as_binary(Opening, <<"to">>, undefined).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     Hostname = binary() | string()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the receiving entity in the `to' attribute.

-spec set_receiving_entity
(xmlel(), binary() | string()) -> xmlel().

set_receiving_entity(#xmlel{attrs = Attrs} = Opening, Hostname) ->
    New_Attrs = set_receiving_entity_in_attrs(Attrs, Hostname),
    Opening#xmlel{attrs = New_Attrs}.

set_receiving_entity_in_attrs(Attrs, undefined) ->
    Attrs;
set_receiving_entity_in_attrs(Attrs, Hostname) ->
    exmpp_xml:set_attribute_in_list(Attrs, <<"to">>, Hostname).

%% @spec (Opening) -> Hostname | undefined
%%     Opening = exmpp_xml:xmlel()
%%     Hostname = binary()
%% @doc Return the initiating entity hostname.

-spec get_initiating_entity
(xmlel()) -> binary() | undefined.

get_initiating_entity(Opening) ->
    exmpp_xml:get_attribute_as_binary(Opening, <<"from">>, undefined).

%% @spec (Opening, Hostname) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     Hostname = binary() | string()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the initiating entity in the `from' attribute.

-spec set_initiating_entity
(xmlel(), binary() | string()) -> xmlel().

set_initiating_entity(#xmlel{attrs = Attrs} = Opening, Hostname) ->
    New_Attrs = set_initiating_entity_in_attrs(Attrs, Hostname),
    Opening#xmlel{attrs = New_Attrs}.

set_initiating_entity_in_attrs(Attrs, undefined) ->
    Attrs;
set_initiating_entity_in_attrs(Attrs, Hostname) ->
    exmpp_xml:set_attribute_in_list(Attrs, <<"from">>, Hostname).

%% @spec (Opening) -> Default_NS | undefined
%%     Opening = exmpp_xml:xmlel()
%%     Default_NS = atom() | string()
%% @doc Return the default namespace.
%%
%% XMPP-IM defines `jabber:client' and `jabber:server'.

-spec get_default_ns
(xmlel()) -> xmlname() | undefined.

get_default_ns(#xmlel{declared_ns = Declared_NS} = _Opening) ->
    case lists:keysearch(none, 2, Declared_NS) of
        {value, {NS, _none}} -> NS;
        _                    -> undefined
    end.

%% @spec (Opening, NS) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     NS = atom() | string()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the default namespace.
%%
%% XMPP-IM defines `jabber:client' and `jabber:server'.

-spec set_default_ns
(xmlel(), xmlname()) -> xmlel().

set_default_ns(#xmlel{declared_ns = Declared_NS} = Opening, NS) ->
    Opening#xmlel{declared_ns = [{NS, none} | Declared_NS]}.

%% @spec (Opening) -> Version
%%     Opening = exmpp_xml:xmlel()
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Return the version of the stream.

-spec get_version
(xmlel()) -> streamversion().

get_version(Opening) ->
    parse_version(exmpp_xml:get_attribute_as_binary(Opening, <<"version">>, <<>>)).

%% @spec (Opening, Version) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     Version = binary() | string() | {Major, Minor} | undefined
%%     Major = integer()
%%     Minor = integer()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the protocol version.

-spec set_version
(xmlel(), binary() | string() | streamversion()) -> xmlel().

set_version(#xmlel{attrs = Attrs} = Opening, Version) ->
    New_Attrs = set_version_in_attrs(Attrs, Version),
    Opening#xmlel{attrs = New_Attrs}.

set_version_in_attrs(Attrs, Version)
  when Version == undefined;
Version == ""; Version == <<>>; Version == {0, 0} ->
    exmpp_xml:remove_attribute_from_list(Attrs, <<"version">>);
set_version_in_attrs(Attrs, {_, _} = Version) ->
    Version_B = serialize_version(Version),
    set_version_in_attrs(Attrs, Version_B);
set_version_in_attrs(Attrs, Version) ->
    exmpp_xml:set_attribute_in_list(Attrs, <<"version">>, Version).

%% @spec (Opening) -> ID | undefined
%%     Opening = exmpp_xml:xmlel()
%%     ID = binary()
%% @doc Return the stream ID.

-spec get_id
(xmlel()) -> binary() | undefined.

get_id(Opening) ->
    exmpp_xml:get_attribute_as_binary(Opening, <<"id">>, undefined).

%% @spec (Opening, ID) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     ID = binary() | string() | random
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the stream ID.

-spec set_id
(xmlel(), binary() | string() | random) -> xmlel().

set_id(#xmlel{attrs = Attrs} = Opening, ID) ->
    New_Attrs = set_id_in_attrs(Attrs, ID),
    Opening#xmlel{attrs = New_Attrs}.

set_id_in_attrs(Attrs, ID) when ID == random; ID == <<>>; ID == "" ->
    set_id_in_attrs(Attrs, exmpp_utils:random_id("stream"));
set_id_in_attrs(Attrs, ID) ->
    exmpp_xml:set_attribute_in_list(Attrs, <<"id">>, ID).

%% @spec (Opening) -> Lang | undefined
%%     Opening = exmpp_xml:xmlel()
%%     Lang = binary()
%% @doc Return the language of the stream.

-spec get_lang
(xmlel()) -> binary() | undefined.

get_lang(Opening) ->
    exmpp_xml:get_attribute_as_binary(Opening, ?NS_XML, <<"lang">>, undefined).

%% @spec (Opening, Lang) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     Lang = binary() | string()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Set the default language.

-spec set_lang
(xmlel(), binary() | string()) -> xmlel().

set_lang(#xmlel{attrs = Attrs} = Opening, Lang) ->
    New_Attrs = set_lang_in_attrs(Attrs, Lang),
    Opening#xmlel{attrs = New_Attrs}.

set_lang_in_attrs(Attrs, Lang) ->
    exmpp_xml:set_attribute_in_list(Attrs, ?NS_XML, <<"lang">>, Lang).

%% --------------------------------------------------------------------
%% Version handling.
%% --------------------------------------------------------------------

%% @spec (String) -> Version
%%     String = binary() | string() | undefined
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%% @doc Parse the stream version in `String'.

-spec parse_version
(binary() | string() | undefined) -> streamversion().

parse_version(undefined) ->
    {0, 0};
parse_version("") ->
    {0, 0};
parse_version(<<>>) ->
    {0, 0};
parse_version(String) when is_binary(String) ->
    parse_version(binary_to_list(String));
parse_version(String) ->
    case string:to_integer(String) of
        {Major, [$. | Rest]} ->
            case string:to_integer(Rest) of
                {Minor, []} -> {Major, Minor};
                _           -> {error, invalid_version}
            end;
        _ ->
            {error, invalid_version}
    end.

%% @spec (Version) -> Binary
%%     Version = {Major, Minor}
%%     Major = integer()
%%     Minor = integer()
%%     Binary = binary()
%% @doc Make a binary() for the `version' attribute of a stream element.

-spec serialize_version
(streamversion() | undefined) -> binary().

serialize_version(undefined) ->
    <<>>;
serialize_version({0, 0}) ->
    <<>>;
serialize_version({Major, Minor}) ->
    list_to_binary(lists:flatten(io_lib:format("~b.~b", [Major, Minor]))).

%% --------------------------------------------------------------------
%% Features announcement.
%% --------------------------------------------------------------------

%% @spec (Opening) -> New_Opening
%%     Opening = exmpp_xml:xmlel()
%%     New_Opening = exmpp_xml:xmlel()
%% @doc Declare server diablack support.

-spec set_dialback_support
(xmlel()) -> xmlel().

set_dialback_support(Opening) ->
    exmpp_xml:declare_ns_here(Opening, ?NS_DIALBACK, ?NS_DIALBACK_pfx).

%% @spec (Features) -> Features_Announcement
%%     Features = [exmpp_xml:xmlel()]
%%     Features_Announcement = exmpp_xml:xmlel()
%% @doc Make the features annoucement element.

-spec features
([xmlel()]) -> xmlel().

features(Features) ->
    #xmlel{
	  ns = ?NS_XMPP,
	  declared_ns = [{?NS_XMPP, ?NS_XMPP_pfx}],
	  name = 'features',
	  children = Features
	 }.

%% --------------------------------------------------------------------
%% Stream-level errors.
%% --------------------------------------------------------------------

standard_conditions() ->
    [
     {'bad-format'},
     {'bad-namespace-prefix'},
     {'conflict'},
     {'connection-timeout'},
     {'host-gone'},
     {'host-unknown'},
     {'improper-addressing'},
     {'internal-server-error'},
     {'invalid-from'},
     {'invalid-id'},
     {'invalid-namespace'},
     {'invalid-xml'},
     {'not-authorized'},
     {'policy-violation'},
     {'remote-connection-failed'},
     {'resource-constraint'},
     {'restricted-xml'},
     {'see-other-host'},
     {'system-shutdown'},
     {'undefined-condition'},
     {'unsupported-encoding'},
     {'unsupported-stanza-type'},
     {'unsupported-version'},
     {'xml-not-well-formed'},
     %% rfc3920bis
     {'not-well-formed'},
     {'reset'}
    ].

%% @spec (Condition) -> Stream_Error
%%     Condition = atom()
%%     Stream_Error = exmpp_xml:xmlel()
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition'.

-spec error
(atom()) -> xmlel().

error(Condition) ->
    error(Condition, {undefined, undefined}).

%% @spec (Condition, {Lang, Text}) -> Stream_Error
%%     Condition = atom()
%%     Stream_Error = exmpp_xml:xmlel()
%%     Lang = binary() | string() | undefined
%%     Text = binary() | string() | undefined
%% @doc Make a standard `<stream:error>' element based on the given
%% `Condition' with Text child element.

-spec error
(atom(), {binary() | string() | undefined, binary() | string() | undefined}) ->
    xmlel().

error(Condition, {Lang, Text}) ->
    case lists:keymember(Condition, 1, standard_conditions()) of
        true  -> ok;
        false -> throw({stream_error, condition, invalid, Condition})
    end,
    Condition_El = #xmlel{ns = ?NS_STREAM_ERRORS,
			  name = Condition
			 },
    Error_El0 = #xmlel{ns = ?NS_XMPP,
		       declared_ns = [{?NS_XMPP, ?NS_XMPP_pfx}],
		       name = 'error',
		       children = [Condition_El]
		      },
    case Text of
        undefined ->
            Error_El0;
        _ ->
            TextChildren = [exmpp_xml:cdata(Text)],
            Text_El0 = #xmlel{ns = ?NS_STREAM_ERRORS,
			      name = 'text',
			      children = TextChildren
			     },
            Text_El = case Lang of
			  undefined ->
			      Text_El0;
			  _ ->
			      exmpp_xml:set_attribute(Text_El0, ?NS_XML,
						      <<"lang">>, Lang)
		      end,
            exmpp_xml:append_child(Error_El0, Text_El)
    end.

%% @spec (XML_El) -> boolean()
%%     XML_El = exmpp_xml:xmlel()
%% @doc Tell if this element is a stream error.

-spec is_error
(xmlel()) -> boolean().

is_error(#xmlel{ns = ?NS_XMPP, name = 'error'}) ->
    true;
is_error(_) ->
    false.

%% @spec (Stream_Error) -> Condition | undefined
%%     Stream_Error = exmpp_xml:xmlel()
%%     Condition = atom()
%% @doc Return the child element name corresponding to the stanza error
%% condition.

-spec get_condition
(xmlel()) -> atom() | undefined.

get_condition(#xmlel{ns = ?NS_XMPP, name = 'error'} = El) ->
    case exmpp_xml:get_element_by_ns(El, ?NS_STREAM_ERRORS) of
        undefined ->
	    %% This <stream:error/> element is invalid because the
	    %% condition must be present (and first).
            undefined;
        #xmlel{name = 'text'} ->
	    %% Same as above.
            undefined;
        #xmlel{name = Condition} when is_atom(Condition) ->
            Condition;
        #xmlel{name = Condition} when is_list(Condition) ->
            list_to_atom(Condition)
    end.

%% @spec (Stream_Error) -> Text | undefined
%%     Stream_Error = exmpp_xml:xmlel()
%%     Text = binary()
%% @doc Return the text that describes the error.

-spec get_text
(xmlel()) -> binary() | undefined.

get_text(#xmlel{ns = ?NS_XMPP, name = 'error'} = El) ->
    case exmpp_xml:get_element(El, ?NS_STREAM_ERRORS, 'text') of
        undefined -> undefined;
        Text      -> exmpp_xml:get_cdata(Text)
    end.

%% --------------------------------------------------------------------
%% Serialization wrappers.
%% --------------------------------------------------------------------

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | list()
%%     XML_Text = string()
%% @doc Serialize a stream opening/closing.

-spec to_list
(xmlel() | [xmlel()]) -> string().

to_list(El) ->
    exmpp_xml:document_to_list(El).

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | list()
%%     XML_Text = binary()
%% @doc Serialize a stream opening/closing.

-spec to_binary
(xmlel() | [xmlel()]) -> binary().

to_binary(El) ->
    exmpp_xml:document_to_binary(El).

%% @spec (El) -> XML_Text
%%     El = exmpp_xml:xmlel() | list()
%%     XML_Text = iolist()
%% @doc Serialize a stream opening/closing.

-spec to_iolist
(xmlel() | [xmlel()]) -> iolist().

to_iolist(El) ->
    exmpp_xml:document_to_iolist(El).
