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
%% The module <strong>{@module}</strong> is an XML parser based on Expat.
%%
%% <p>
%% It provides a set of functions to prepare a tree of the elements from
%% an XML stream or an XML document. To ease the handling of the tree
%% produced by the parsing, it also export functions to access each
%% parts of an element.
%% </p>
%%
%% <p>
%% Namespace support is fully tested and is now ready for production use.
%% </p>
%%
%% <p>
%% A backward compatible layer, built on top of this module, is provided
%% by {@link xml}.
%% </p>

-module(exmpp_xml).

-behaviour(gen_server).

-include("exmpp.hrl").
-include("internal/exmpp_known_nss.hrl").
-include("internal/exmpp_known_elems.hrl").

%% Initialization.
-export([
	 start/0,
	 start_link/0
	]).

%% Registry handling.
-export([
	 register_engine/2,
	 register_engine/3,
	 get_engine_names/0,
	 is_engine_available/1,
	 get_engine_driver/1
	]).

%% Parser.
-export([
	 start_parser/0,
	 start_parser/1,
	 reset_parser/1,
	 reset_parser/2,
	 stop_parser/1,
	 add_known_nss/2,
	 add_known_elems/2,
	 parse/2,
	 parse_final/2,
	 parse_document/1,
	 parse_document/2,
	 parse_document_fragment/1,
	 parse_document_fragment/2,
	 port_revision/1
	]).

%% Namespace handling.
-export([
	 is_ns_declared_here/2,
	 declare_ns_here/3,
	 get_ns_as_list/1,
	 get_ns_as_atom/1
	]).

%% Attribute handling.
-export([
	 attribute_matches/2,
	 attribute_matches/3,
	 attribute/2,
	 attribute/3,
	 get_attribute_node_from_list/2,
	 get_attribute_node_from_list/3,
	 get_attribute_node/2,
	 get_attribute_node/3,
	 get_attribute_from_list/3,
	 get_attribute_from_list/4,
	 get_attribute/3,
	 get_attribute/4,
	 get_attribute_from_list_as_list/3,
	 get_attribute_from_list_as_list/4,
	 get_attribute_as_list/3,
	 get_attribute_as_list/4,
	 get_attribute_from_list_as_binary/3,
	 get_attribute_from_list_as_binary/4,
	 get_attribute_as_binary/3,
	 get_attribute_as_binary/4,
	 set_attribute_in_list/2,
	 set_attribute_in_list/3,
	 set_attribute_in_list/4,
	 set_attribute/2,
	 set_attribute/3,
	 set_attribute/4,
	 set_attributes/2,
	 has_attribute_in_list/2,
	 has_attribute_in_list/3,
	 has_attribute/2,
	 has_attribute/3,
	 remove_attribute_from_list/2,
	 remove_attribute_from_list/3,
	 remove_attribute/2,
	 remove_attribute/3
	]).

%% Element handling.
-export([
	 get_name_as_list/1,
	 get_name_as_atom/1,
	 element_matches/2,
	 element_matches/3,
	 element_matches_by_ns/2,
	 element/1,
	 element/2,
	 element/4,
	 get_element/2,
	 get_element/3,
	 get_elements/3,
	 get_elements/2,
	 get_element_by_ns/2,
	 has_element/2,
	 has_element/3,
	 has_element_by_ns/2,
	 get_child_elements/1,
	 remove_element/2,
	 remove_element/3,
	 remove_element_by_ns/2,
	 remove_elements/2,
	 remove_elements/3,
	 remove_elements_by_ns/2
	]).
-export([
	 prepend_child/2,
	 prepend_children/2,
	 append_child/2,
	 append_children/2,
	 replace_child/3,
	 set_children/2,
	 filter/2,
	 fold/3,
	 foreach/2,
	 map/2
	]).

%% Character data handling.
-export([
	 cdata/1,
	 get_cdata_from_list/1,
	 get_cdata_from_list_as_list/1,
	 get_cdata/1,
	 get_cdata_as_list/1,
	 normalize_cdata_in_list/1,
	 normalize_cdata/1,
	 set_cdata_in_list/2,
	 set_cdata/2,
	 append_cdata_to_list/2,
	 append_cdata/2,
	 remove_cdata_from_list/1,
	 remove_cdata/1,
	 is_whitespace/1,
	 remove_whitespaces_from_list/1,
	 remove_whitespaces/1,
	 remove_whitespaces_deeply/1
	]).

%% Misc. functions on the whole XML tree.
-export([
	 get_path/2,
	 xmlel_to_xmlelement/1,
	 xmlel_to_xmlelement/3,
	 xmlelement_to_xmlel/1,
	 xmlelement_to_xmlel/3,
	 xmlelement_to_xmlel_and_nss_tables/3,
	 node_to_list/3,
	 document_to_list/1,
	 node_to_binary/3,
	 document_to_binary/1,
	 node_to_iolist/3,
	 document_to_iolist/1,
	 deindent_document/1,
	 indent_document/2,
	 indent_document/3,
	 clear_endtag_tuples/1,
	 escape_using_entities/1,
	 escape_using_cdata/1,
	 internal_escaping_function_name/0
	]).

%% gen_server(3erl) callbacks.
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% --------------------------------------------------------------------
%% Constants and macros.
%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(ENGINES_REGISTRY, exmpp_xml_engines_registry).
-define(DEFAULT_ENGINE, expat).

-define(COMMAND_ADD_KNOWN_NSS,     1).
-define(COMMAND_ADD_KNOWN_ELEMS,   2).
-define(COMMAND_SET_MAX_SIZE,      3).
-define(COMMAND_SET_ROOT_DEPTH,    4).
-define(COMMAND_SET_NAMES_AS_ATOM, 5).
-define(COMMAND_SET_CHECK_NSS,     6).
-define(COMMAND_SET_CHECK_ELEMS,   7).
-define(COMMAND_SET_EMIT_ENDTAG,   8).
-define(COMMAND_PARSE,             9).
-define(COMMAND_PARSE_FINAL,      10).
-define(COMMAND_RESET_PARSER,     11).
-define(COMMAND_PORT_REVISION,    12).

-define(DEFAULT_PARSER_OPTIONS, [
				 {max_size, infinity},
				 {root_depth, 0},
				 {names_as_atom, true},
				 {emit_endtag, false}
				]).

-define(PREFIXED_NAME(P, N), P ++ ":" ++ N).

-ifdef(ESCAPE_USING_CDATA_SECTIONS).
-define(ESCAPE(CData), escape_using_cdata(CData)).
-else.
-define(ESCAPE(CData), escape_using_entities(CData)).
-endif.

-define(IMPLICIT_PREFIXED_NS, [
			       {?NS_XML, ?NS_XML_pfx}
			      ]).

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type xmlparseroption() = Engine | Namespace_Option | Names_Format | Checks | Stanza_Max_Size | Root_Depth | Send_End_Element | Autoload_Known
%%     Engine = {engine, atom()}
%%     Stanza_Max_Size  = {max_size, infinity} | {max_size, Size}
%%     Root_Depth = {root_depth, none} | {root_depth, Depth}
%%     Name_Format = {names_as_atom, boolean()}
%%     Checks = NS_Check | Elems_Check | Attrs_Check
%%       NS_Check = {check_nss, Known_List_Name | boolean()}
%%       Elems_Check = {check_elems, Known_List_Name | boolean()}
%%     Known_List_Name = atom()
%%     Send_End_Element = {emit_endtag, boolean()}.
%% Options of the form `{Key, boolean()}' can be specified as `Key'. See
%% {@link proplists}.
%%
%% <br/><br/>
%% The `engine' option allows one to choose the engine to use. Available
%% engines list can be retrived with {@link get_engine_names/0}.
%%
%% <br/><br/>
%% The `max_size' option limits the size in bytes of a stanza to avoid
%% deny of service at the parser level. Actually, this limit is only
%% verified against the length of the data provided and the counter is
%% reset to zero when an element is found. The caveats is that if the
%% limits is, eg., 15 and the data is `<foo></foo><bar></bar>', the
%% parser will return an error because the whole chunk is 22 bytes,
%% despite each stanza contains 11 bytes.
%% TODO: Fix the max_size beahavior.
%%
%% <br/><br/>
%% The `root_depth' option specicifies at which level the parser stops
%% to split each node and start to produce trees. For example, if the
%% root depth is 0, the parser will return a unique tree for the whole
%% document. If the root depth is 1, then `<stream>' will produce an
%% element without any children and `<presence>' will produce a tree
%% with all its children. With `{root_depth, none}', no tree will be
%% made, ie, each opening tag will produce an element without any
%% children.
%%
%% <br/><br/>
%% The `names_as_atom' option sets if element and attribute names should
%% be encoded as an {@link atom()} or a {@link string()} respectively.
%% "Should" because if names or attributes checks fail, a name will be
%% encoded as a `string()' (see next option).
%%
%% <br/><br/>
%% The `Checks' options enable or disable the control of a namespace,
%% an element name or an attribute name if `names_as_atom' is set. If
%% `false' is set, no check will be made. If a known list name is
%% specified, the checks will be based on this list. If `true' is set,
%% the previous selected known list will be used. The known list must
%% exist already. This is to avoid atom() table pollution and overflow.
%% If a check says that the verified string is known, it'll be encoded
%% as an atom() in the tuple; otherwise it'll be encoded as a string().
%% It's highly recommended to keep these checks enabled.
%%
%% <br/><br/>
%% The `emit_endtag' option selects if the parser must produce {@link
%% xmlendtag()} when it encouters an end tag above `root_depth'.

-type(xmlparseroption() ::
      {engine, atom()}                         |
      {max_size, infinity | non_neg_integer()} |
      {root_depth, none | non_neg_integer()}   |
      {names_as_atom, boolean()}                  | names_as_atom |
      {check_nss, atom() | boolean()}             | check_nss     |
      {check_elems, atom() | boolean()}           | check_elems   |
      {emit_endtag, boolean()}                    | emit_endtag
     ).

%% @type xmlparser().
%% Handler for the Expat parser, initialized with a call to {@link
%% start_parser/0}.

-record(xml_parser, {
	  options = [] :: [xmlparseroption()],
	  port         :: port()
	 }).
-type(xmlparser() :: #xml_parser{}).

%% @type xmlel() = {xmlel, NS, Declared_NS, Name, Attrs, Children}
%%     NS = atom() | string() | undefined
%%     Declared_NS = [{NS, Prefix} | {NS, none}]
%%     Name = atom() | string()
%%     Attrs = [xmlattr()]
%%     Children = [xmlel() | xmlcdata()] | undefined.
%% Record representing an XML element (or only the opening tag).
%%
%% <br/><br/>
%% Declared_NS lists all the namespaces declared in this element, even
%% if they're not used by it.

%% @type xmlattr() = {xmlattr, NS, Name, Value}
%%     NS = atom() | string()
%%     Name = binary()
%%     Value = binary().
%% Represents an tag attribute.

%% @type xmlcdata() = {xmlcdata, CData}
%%     CData = binary().
%% Record representing characters data inside an XML element.

%% @type xmlendtag() = {xmlendtag, NS, Name}
%%     NS = atom() | string()
%%     Name = atom() | string().
%% Record representing an XML end tag, for nodes above the configured
%% `root_depth' (see {@link xmlparseroption()}).

%% @type xmlel_old() = {xmlelement, Name, Attrs, Children}
%%     Name = string()
%%     Attrs = [xmlattr_old()]
%%     Children = [xmlel_old() | xmlcdata()] | undefined.
%% Record representing an XML tag.

%% @type xmlattr_old() = {Name, Value}
%%     Name = atom() | string()
%%     Value = string().
%% Represents an tag attribute.

%% @type pathcomponent() = {element, Elem_Name} | {element, NS, Elem_Name} | {attribute, Attr_Name} | {attribute, NS, Attr_Name} | cdata | cdata_as_list
%%     NS = atom() | string()
%%     Elem_Name = atom() | string()
%%     Attr_Name = binary().
%% Represents a path component. The `elem' tuple points to an XML
%% element named `Elem_Name'. The `attr' tuple points to the value of
%% the `Attr_Name' attribute. cdata asks for the character data of a
%% node.

%% Internal types.
-record(state, {
	  known_nss_lists,   % These are #dict{} but I have no idea how to write the
	  known_elems_lists  % contract when the type isn't public (it's internal to
	                     % the 'dict' module in stdlib).
	 }).

-record(xml_engine, {
	  name        :: atom(),
	  %% '_' is used in ETS requests
	  driver_path :: string() | undefined | '_',
	  %% but it is an invalid value
	  driver      :: atom(),
	  %% otherwise.
	  port        :: port() | '_'
	 }).

%% --------------------------------------------------------------------
%% Initialization.
%% --------------------------------------------------------------------

%% @hidden

start() ->
    Ret = gen_server:start({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    load_builtin_known_lists(),
    Ret.

%% @hidden

start_link() ->
    Ret = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    register_builtin_engines(),
    load_builtin_known_lists(),
    Ret.

-define(REGISTER_EXPAT,
	register_builtin_engine(expat, exmpp_xml_expat)).
-define(REGISTER_EXPAT_LEGACY,
	register_builtin_engine(expat_legacy, exmpp_xml_expat_legacy)).

-define(REGISTER_LIBXML2,
	register_builtin_engine(libxml2, exmpp_xml_libxml2)).

register_builtin_engines() ->
    ?REGISTER_EXPAT,
    ?REGISTER_EXPAT_LEGACY,
    ?REGISTER_LIBXML2,
    ok.

register_builtin_engine(Name, Driver) ->
    try
        register_engine(Name, Driver)
    catch
        throw:{port_driver, load, Reason, Driver_Name} ->
            error_logger:warning_msg("Failed to load driver \"~s\": ~s~n",
				     [Driver_Name, erl_ddll:format_error(Reason)])
    end.

load_builtin_known_lists() ->
    [_ | Known_NSs] = lists:reverse(?XMPP_KNOWN_NSS),
    [_ | Known_Elems] = lists:reverse(?XMPP_KNOWN_ELEMS),
    add_known_nss(xmpp, Known_NSs),
    add_known_elems(xmpp, Known_Elems).

%% --------------------------------------------------------------------
%% Registry handling.
%% --------------------------------------------------------------------

%% @spec (Name, Driver) -> ok
%%     Name = atom()
%%     Driver = atom()
%% @doc Add a new XML engine.

-spec(register_engine/2 :: (atom(), atom()) -> ok).

register_engine(Name, Driver) ->
    register_engine(Name, undefined, Driver).

%% @spec (Name, Driver_Path, Driver) -> ok
%%     Name = atom()
%%     Driver_Path = string() | undefined
%%     Driver = atom()
%% @doc Add a new XML engine.

-spec(register_engine/3 :: (atom(), string() | undefined, atom()) -> ok).

register_engine(Name, Driver_Path, Driver)
  when is_atom(Name) ->
    Engine = #xml_engine{
      name = Name,
      driver_path = Driver_Path,
      driver = Driver
     },
    case gen_server:call(?SERVER, {register_engine, Engine}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.

%% @spec () -> [Engine_Name]
%%     Engine_Name = atom()
%% @doc Return the list of XML engines.

-spec(get_engine_names/0 :: () -> [atom()]).

get_engine_names() ->
    ets:safe_fixtable(?ENGINES_REGISTRY, true),
    Keys = get_engine_names2(ets:first(?ENGINES_REGISTRY), []),
    ets:safe_fixtable(?ENGINES_REGISTRY, false),
    Keys.

get_engine_names2('$end_of_table', Keys) ->
    lists:reverse(Keys);
get_engine_names2(Prev_Key, Keys) ->
    get_engine_names2(ets:next(?ENGINES_REGISTRY, Prev_Key),
		      [Prev_Key | Keys]).

%% @spec (Engine_Name) -> boolean()
%%     Engine_Name = atom()
%% @doc Tell if `Engine_Name' is available.

-spec(is_engine_available/1 :: (atom()) -> boolean()).

is_engine_available(Engine_Name) ->
    ets:member(?ENGINES_REGISTRY, Engine_Name).

%% @spec (Engine_Name) -> Driver_Name
%%     Engine_Name = atom()
%%     Driver_Name = atom() | undefined
%% @doc Return the port driver name associated to the given engine.

-spec(get_engine_driver/1 :: (atom()) -> atom() | undefined).

get_engine_driver(Engine_Name) ->
    case ets:match(?ENGINES_REGISTRY,
		   {xml_engine, Engine_Name, '_', '$1', '_'}) of
        [[Engine_Driver]] ->
            Engine_Driver;
        _ ->
            undefined
    end.

%% --------------------------------------------------------------------
%% Global known lists.
%% --------------------------------------------------------------------

%% @spec (List_Name, List) -> ok
%%     List_Name = atom()
%%     List = [NS]
%%     NS = atom()
%% @doc Tell parsers that `NS_List' are known namespaces.
%%
%% If `check_nss' is enabled, all occurences of these namespaces will be
%% represented as an atom().

-spec(add_known_nss/2 :: (atom(), [atom()]) -> ok).

add_known_nss(List_Name, List) ->
    case gen_server:call(?SERVER, {add_known, nss, List_Name, List}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.

%% @spec (List_Name, List) -> ok
%%     List_Name = atom()
%%     List = [Name]
%%     Name = atom()
%% @doc Tell parsers that `Names_List' are known element names.
%%
%% If `check_elems' is enabled, all occurences of these names will be
%% represented as an atom().

-spec(add_known_elems/2 :: (atom(), [atom()]) -> ok).

add_known_elems(List_Name, List) ->
    case gen_server:call(?SERVER, {add_known, names, List_Name, List}) of
        ok                 -> ok;
        {error, Exception} -> throw(Exception)
    end.


%% --------------------------------------------------------------------
%% Parsing functions (interface to the Expat port driver).
%% --------------------------------------------------------------------

%% @spec () -> Parser
%%     Parser = xmlparser()
%% @doc Initialize the Expat port driver with default options.
%%
%% Default options are:
%% ```
%% [
%%   {max_size, infinity},
%%   {root_depth, 0},
%%   names_as_atom,
%%   {emit_endtag, false}
%% ].
%% '''
%%
%% @see start_parser/1.
%% @see xmlparseroption().

-spec(start_parser/0 :: () -> xmlparser()).

start_parser() ->
    start_parser([]).

%% @spec (Options) -> Parser
%%     Options = [xmlparseroption()]
%%     Parser = xmlparser()
%% @throws {xml_parser, options, Reason, Infos}
%% @doc Initialize the Expat port driver with given `Options'.
%%
%% You must call this function before any use of functions {@link
%% parse/2} or {@link parse_final/2}. The returned `Parser' must be
%% given as the first argument for those functions. When finished, you
%% must free this parser with the {@link stop_parser/1}. Here is an
%% example:
%% ```
%% fun() ->
%%     Parser = xml:start_parser(),
%%     xml:parse(Parser, "<stream version='1.0'><presence/></stream>"),
%%     xml:stop_parser(Parser).
%% '''

-spec(start_parser/1 :: ([xmlparseroption()]) -> xmlparser()).

start_parser(Options) ->
    %% Start a port driver instance.
    Driver_Name = get_engine_from_options(Options),
    Port = exmpp_internals:open_port(Driver_Name),

    %% Initialize port.
    try
	%% Check options.
        Parser = #xml_parser{port = Port},
        New_Options = merge_options(?DEFAULT_PARSER_OPTIONS, Options),
        reset_parser2(Parser, New_Options)
    catch
        _:Exception ->
            exmpp_internals:close_port(Port),
            throw(Exception)
    end.

%% @spec (Parser) -> New_Parser
%%     Parser = xmlparser()
%% @doc Reset the parser with the same previous options.

-spec(reset_parser/1 :: (xmlparser()) -> xmlparser()).

reset_parser(Parser) ->
    reset_parser(Parser, []).

%% @spec (Parser, Options) -> New_Parser
%%     Parser = xmlparser()
%%     Options = [xmlparseroption()]
%% @doc Reset the parser and update its options.

-spec(reset_parser/2 :: (xmlparser(), [xmlparseroption()]) -> xmlparser()).

reset_parser(#xml_parser{port = Port} = Parser, Options) ->
    New_Options = merge_options(Parser#xml_parser.options, Options),
    port_control(Port, ?COMMAND_RESET_PARSER, <<>>),
    reset_parser2(Parser, New_Options).

reset_parser2(Parser, Options) ->
    case handle_options(Parser, Options) of
        {error, Reason, Infos} ->
            throw({xml_parser, options, Reason, Infos});
        New_Parser ->
            New_Parser
    end.

%% @spec (Parser) -> ok
%%     Parser = xmlparser()
%% @doc Stop the Expat port driver.
%%
%% This must be called when you are done with the `Parser' returned by
%% {@link start_parser/0}.
%%
%% @see start_parser/0. `start_parser/0' for an example

-spec(stop_parser/1 :: (xmlparser()) -> ok).

stop_parser(#xml_parser{port = Port} = _Parser) ->
    unlink(Port),
    exmpp_internals:close_port(Port),
    ok.

%% @spec (Parser, Data) -> [XML_Element] | continue
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlel_old() | xmlel() | xmlendtag() | xmlcdata()
%% @throws {xml_parser, parsing, Reason, Details}
%% @doc Parse a chunk from an XML stream.
%%
%% This may be called multiple times with a new chunk of data. However
%% the entire data must represent at most one and only one XML document.
%% If you want to process the last chunk of data, you should call {@link
%% parser_final/2}. If you can't know when the end of the document
%% occurs, you may use this function to process data, then you call
%% {@link parse_final/2} with an empty string. Here is an example:
%% ```
%% fun (Parser) ->
%%     xml:parse(Parser, "<stream ver"),
%%     xml:parse(Parser, "sion='1."),
%%     xml:parse(Parser, "0'></stream>"),
%%     xml:parser_final(Parser, "").
%% '''

-spec(parse/2 ::
      (xmlparser(), binary() | string()) -> [xmlnode() | xmlendtag()] | continue).

parse(Parser, Data) when is_list(Data) ->
    parse(Parser, list_to_binary(Data));

parse(#xml_parser{port = Port} = _Parser, Data) when is_binary(Data) ->
    engine_parse(Port, Data).

%% @spec (Parser, Data) -> [XML_Element] | done
%%     Parser = xmlparser()
%%     Data = string() | binary()
%%     XML_Element = xmlel_old() | xmlel() | xmlendtag() | xmlcdata()
%% @throws {xml_parser, parsing, Reason, Details}
%% @doc Parse the last chunk from an XML stream.
%%
%% This is used when you know there won't be any more data to process.
%% This last chunk must provide the end of the XML document or the
%% parser will return an error. This function may also be used to
%% process an entire XML document in one pass.
%%
%% @see parse/2. `parse/2' for an example

-spec(parse_final/2 ::
      (xmlparser(), binary() | string()) -> [xmlnode() | xmlendtag()] | done).

parse_final(Parser, Data) when is_list(Data) ->
    parse_final(Parser, list_to_binary(Data));

parse_final(#xml_parser{port = Port} = _Parser, Data) when is_binary(Data) ->
    engine_parse_final(Port, Data).

%% @spec (Document) -> [XML_Element] | done
%%     Document = string() | binary()
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options; see
%% {@link start_parser/1} for any related informations.

-spec(parse_document/1 ::
      (binary() | string()) -> [xmlnode() | xmlendtag()] | done).

parse_document(Document) ->
    parse_document(Document, []).

%% @spec (Document, Parser_Options) -> [XML_Element] | done
%%     Document = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata()
%% @doc Parse an entire XML document at once.
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser; see {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

-spec(parse_document/2 ::
      (binary() | string(), [xmlparseroption()]) ->
	     [xmlnode() | xmlendtag()] | done).

parse_document(Document, Parser_Options) ->
    Parser = start_parser(Parser_Options),
    try
        parse_final(Parser, Document)
    catch
        throw:Exception ->
            throw(Exception)
    after
        stop_parser(Parser)
    end.

%% @spec (Fragment) -> [XML_Element] | continue
%%     Fragment = string() | binary()
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata()
%% @doc Parse a fragment of an XML document at once.
%%
%% This function is useful if you do not have a complete and valid XML
%% document. For instance, something like this:
%% ```
%% <element>content</elem
%% '''
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. It'll use default options, but
%% will set `{root_depth, none}' (which can be overriden); see {@link
%% start_parser/1} for any related informations.

-spec(parse_document_fragment/1 ::
      (binary() | string()) -> [xmlnode() | xmlendtag()] | continue).

parse_document_fragment(Fragment) ->
    parse_document_fragment(Fragment, []).

%% @spec (Fragment, Parser_Options) -> [XML_Element] | continue
%%     Fragment = string() | binary()
%%     Parser_Options = [xmlparseroption()]
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata()
%% @doc Parse a fragment of an XML document at once.
%%
%% This function is useful if you do not have a complete and valid XML
%% document. For instance, something like this:
%% ```
%% <element>content</elem
%% '''
%%
%% Initializing a parser with {@link start_parser/1} isn't necessary,
%% this function will take care of it. `Parser_Options' is passed to the
%% parser but `{root_depth, none}' is prepended (this can be overriden);
%% see {@link start_parser/1} for any related informations.
%%
%% Return values are the same as {@link parse_final/2}.

-spec(parse_document_fragment/2 ::
      (binary() | string(), [xmlparseroption()]) ->
	     [xmlnode() | xmlendtag()] | continue).

parse_document_fragment(Fragment, Parser_Options) ->
    Parser = start_parser([{root_depth, none} | Parser_Options]),
    try
        parse(Parser, Fragment)
    catch
        throw:Exception ->
            throw(Exception)
    after
        stop_parser(Parser)
    end.

%% @hidden

port_revision(#xml_parser{port = Port} = _Parser) ->
    engine_port_revision(Port).

%% --------------------------------------------------------------------
%% Functions to handle namespaces in XML elements and attributes.
%% --------------------------------------------------------------------

%% @spec (XML_Element, NS) -> boolean()
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%% @doc Tell if `NS' was declared within this element.
%%
%% @todo Like for elements and attributes, implement a more flexible
%% matching (`string()' vs. `atom()').

-spec(is_ns_declared_here/2 :: (xmlel(), xmlname()) -> boolean()).

is_ns_declared_here(#xmlel{declared_ns = Declared_NS}, NS) ->
    lists:keymember(NS, 1, Declared_NS).

%% @spec (XML_Element, NS, Prefix) -> New_XML_Element
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%%     Prefix = string() | none
%%     New_XML_Element = xmlel()
%% @doc Declare the given namespace in this element.
%%
%% @todo Like for elements and attributes, implement a more flexible
%% matching (`string()' vs. `atom()').

-spec(declare_ns_here/3 :: (xmlel(), xmlname(), string() | none) -> xmlel()).

declare_ns_here(#xmlel{declared_ns = Declared_NS} = XML_Element,
		NS, Prefix) ->
    New_Declared_NS = lists:keystore(NS, 1,
				     Declared_NS, {NS, Prefix}),
    XML_Element#xmlel{declared_ns = New_Declared_NS}.

%% @spec (XML_Element) -> NS | undefined
%%     XML_Element = xmlel()
%%     NS = string()
%% @doc Return the namespace as a string, regardless of the original
%% encoding.

-spec(get_ns_as_list/1 :: (xmlel()) -> string() | undefined).

get_ns_as_list(#xmlel{ns = undefined}) ->
    undefined;
get_ns_as_list(#xmlel{ns = NS}) ->
    as_list(NS).

as_list(V) when is_atom(V) -> atom_to_list(V);
as_list(V) when is_list(V) -> V.

%% @spec (XML_Element) -> NS | undefined
%%     XML_Element = xmlel()
%%     NS = atom()
%% @doc Return the namespace as an atom, regardless of the original
%% encoding.

-spec(get_ns_as_atom/1 :: (xmlel()) -> atom() | undefined).

get_ns_as_atom(#xmlel{ns = undefined}) ->
    undefined;
get_ns_as_atom(#xmlel{ns = NS}) ->
    as_atom(NS).

as_atom(V) when is_atom(V) -> V;
as_atom(V) when is_list(V) -> list_to_atom(V).

%% --------------------------------------------------------------------
%% Functions to handle XML attributes (xmlattr() & xmlattr_old()).
%% This is similar to the DOM interface but NOT compliant.
%% --------------------------------------------------------------------

%% @spec (Name, Value) -> Attr
%%     Name = binary()
%%     Value = binary() | string() | atom() | integer()
%%     Attr = xmlattr()
%% @doc Create an XML attribute with the name `Name'.
%%
%% This is almost the same as:
%% ```
%% Attr = #xmlattr{name = Name, value = Value}.
%% '''

-spec(attribute/2 ::
      (attributename(), binary() | string() | atom() | integer()) ->
	     xmlattr()).

attribute(Name, Value) when is_binary(Name) ->
    set_attr_value(#xmlattr{name = Name}, Value).

set_attr_value(#xmlattr{} = Attr, Value) ->
    Attr#xmlattr{value = exmpp_utils:any_to_binary(Value)};
set_attr_value({Name, _}, Value) ->
    {Name, exmpp_utils:any_to_list(Value)}.

%% @spec (NS, Name, Value) -> Attr
%%     NS = atom() | string() | undefined
%%     Name = binary()
%%     Value = binary() | string() | atom() | integer()
%%     Attr = xmlattr()
%% @doc Create an XML attribute with the name `Name' in the namespace `NS'.
%%
%% This is almost the same as:
%% ```
%% Attr = #xmlattr{ns = NS, name = Name, value = Value}.
%% '''

-spec(attribute/3 ::
      (xmlname(), attributename(), binary() | string() | atom() | integer()) ->
	     xmlattr()).

attribute(NS, Name, Value) when is_binary(Name) ->
    set_attr_value(#xmlattr{ns = NS, name = Name}, Value).

%% @spec (Attr, Name) -> boolean()
%%     Attr = xmlattr() | xmlattr_old()
%%     Name = string() | binary()
%% @doc Tell if `Attr' is named `Name'.
%%
%% It takes care of comparison between string and atom.

-spec(attribute_matches/2 :: (xmlattr_any(), xmlname() | attributename()) -> boolean()).

attribute_matches(#xmlattr{name = Name}, Name) ->
    true;
attribute_matches({Name, _Value}, Name) ->
    true;

attribute_matches({Name_B, _Value}, Name)
  when is_binary(Name_B), is_list(Name) ->
    Name_B == list_to_binary(Name);
attribute_matches({Name, _Value}, Name_B)
  when is_binary(Name_B), is_list(Name) ->
    Name_B == list_to_binary(Name);

attribute_matches(_Attr, _Name) ->
    false.

%% @spec (Attr, NS, Name) -> boolean()
%%     Attr = xmlattr()
%%     NS = atom() | string()
%%     Name = string() | binary()
%% @doc Tell if `Attr' has the namespace `NS' and is named `Name'.
%%
%% It takes care of comparison between string and atom.

-spec(attribute_matches/3 ::
      (xmlattr(), xmlname(), xmlname() | attributename()) -> boolean()).

attribute_matches(Attr, NS, Name) when is_list(Name) ->
    attribute_matches(Attr, NS, list_to_binary(Name));


attribute_matches(#xmlattr{ns = NS} = Attr, NS2, Name) when is_atom(NS), is_list(NS2) ->
    attribute_matches(Attr, list_to_atom(NS2), Name);
attribute_matches(#xmlattr{ns = NS} = Attr, NS2, Name) when is_list(NS),  is_atom(NS2) ->
    attribute_matches(Attr, atom_to_list(NS2), Name);

attribute_matches(#xmlattr{ns = NS, name = Name}, NS, Name) ->
    true;

attribute_matches(_Attr, _NS, _Name) ->
    false.

%% @spec (Attrs, Attr_Name) -> Attr | undefined
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     Attr = xmlattr() | xmlattr_old()
%% @doc Return the attribute named `Attr_Name' from the list.
%%
%% Return `undefined' if the attribute isn't found.

%% XXX Dialyzer doesn't support this multiple-clause contract because
%% both clauses take a list() as a first argument. So until it can look
%% inside those list(), we specify a less strict contract.
%%
%% -spec(get_attribute_node_from_list/2 ::
%%   ([], xmlname())              -> undefined;
%%   ([xmlattr()], xmlname())     -> xmlattr() | undefined;
%%   ([xmlattr_old()], xmlname()) -> xmlattr_old() | undefined).

-spec(get_attribute_node_from_list/2 ::
      ([xmlattr() | xmlattr_old()], attributename()) ->
	     xmlattr() | xmlattr_old() | undefined).

get_attribute_node_from_list([Attr | Rest], Name) ->
    case attribute_matches(Attr, Name) of
        true  -> Attr;
        false -> get_attribute_node_from_list(Rest, Name)
    end;
get_attribute_node_from_list([], _Name) ->
    undefined.

%% @spec (Attrs, NS, Attr_Name) -> Attr | undefined
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Attr = xmlattr()
%% @doc Return the attribute named `Attr_Name' from the list with the
%% `NS' namespace URI.
%%
%% Return `undefined' if the attribute isn't found.

-spec(get_attribute_node_from_list/3 ::
      ([xmlattr()], xmlname(), attributename()) -> xmlattr() | undefined).

get_attribute_node_from_list([Attr | Rest], NS, Name) ->
    case attribute_matches(Attr, NS, Name) of
        true  -> Attr;
        false -> get_attribute_node_from_list(Rest, NS, Name)
    end;
get_attribute_node_from_list([], _NS, _Name) ->
    undefined.

%% @spec (XML_Element, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Attr_Name = binary()
%%     Attr = xmlattr() | xmlattr_old()
%% @doc Return the attribute named `Attr_Name'.
%%
%% Return `undefined' if the attribute isn't found.

-spec(get_attribute_node/2 ::
      (xmlel(), attributename())     -> xmlattr() | undefined;
      (xmlel_old(), attributename()) -> xmlattr_old() | undefined;
      (undefined, attributename())   -> undefined).

get_attribute_node(#xmlel{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    get_attribute_node_from_list(Attrs, Name);
get_attribute_node(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Attr_Name) -> Attr | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Attr = xmlattr()
%% @doc Return the attribute named `Attr_Name' with the `NS' namespace URI.
%%
%% Return `undefined' if the attribute isn't found.

-spec(get_attribute_node/3 ::
      (xmlel(), xmlname(), attributename())   -> xmlattr() | undefined;
      (undefined, xmlname(), attributename()) -> undefined).

get_attribute_node(#xmlel{attrs = Attrs} = _XML_Element, NS, Name) ->
    get_attribute_node_from_list(Attrs, NS, Name);
get_attribute_node(undefined, _NS, _Name) ->
    undefined.

%% @spec (Attrs, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary() | string()
%% @doc Return the value of the attribute named `Attr_Name' from the list.
%%
%% The return type depends on `Attrs' type:
%% <ul>
%% <li>`binary()' with {@link xmlattr()}</li>
%% <li>`list()' with {@link xmlattr_old()}</li>
%% </ul>
%%
%% Return `Default' if the attribute isn't found.

%% XXX Dialyzer doesn't support this multiple-clause contract because
%% both clauses take a list() as a first argument. So until it can look
%% inside those list(), we specify a less strict contract.
%%
%% -spec(get_attribute_from_list/3 ::
%%   ([], attributename(), Default)              -> Default;
%%   ([xmlattr()], attributename(), Default)     -> binary() | Default;
%%   ([xmlattr_old()], attributename(), Default) -> string() | Default).

-spec(get_attribute_from_list/3 ::
      ([xmlattr() | xmlattr_old()], attributename(), Default) ->
	     binary() | string() | Default).

get_attribute_from_list(Attrs, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        {_Name, Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (Attrs, NS, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name' from the
%% list with the `NS' namespace URI.
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_from_list/4 ::
      ([xmlattr()], xmlname(), attributename(), Default) -> binary() | Default).

get_attribute_from_list(Attrs, NS, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, NS, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (XML_Element, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary() | string()
%% @doc Return the value of the attribute named `Attr_Name'.
%%
%% The return type depends on attributes type in `XML_Element':
%% <ul>
%% <li>`binary()' with {@link xmlattr()}</li>
%% <li>`list()' with {@link xmlattr_old()}</li>
%% </ul>
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute/3 ::
      (xmlel(), attributename(), Default)     -> binary() | Default;
      (xmlel_old(), attributename(), Default) -> string() | Default;
      (undefined, attributename(), Default)   -> Default).

get_attribute(#xmlel{attrs = Attrs} = _XML_Element, Name, Default) ->
    get_attribute_from_list(Attrs, Name, Default);
get_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name, Default) ->
    get_attribute_from_list(Attrs, Name, Default);
get_attribute(undefined, _Name, Default) ->
    Default.

%% @spec (XML_Element, NS, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI.
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute/4 ::
      (xmlel(), xmlname(), attributename(), Default)   -> binary() | Default;
      (undefined, xmlname(), attributename(), Default) -> Default).

get_attribute(#xmlel{attrs = Attrs} = _XML_Element, NS, Name, Default) ->
    get_attribute_from_list(Attrs, NS, Name, Default);
get_attribute(undefined, _NS, _Name, Default) ->
    Default.

%% @spec (Attrs, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = list()
%% @doc Return the value of the attribute named `Attr_Name' from the
%% list, as a list().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_from_list_as_list/3 ::
      ([xmlattr()] | [xmlattr_old()], attributename(), Default) -> string() | Default).

get_attribute_from_list_as_list(Attrs, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, Attr_Name) of
        #xmlattr{value = Value} ->
            binary_to_list(Value);
        {_Name, Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (Attrs, NS, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = list()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI from the list, as a list().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_from_list_as_list/4 ::
      ([xmlattr()], xmlname(), attributename(), Default) -> string() | Default).

get_attribute_from_list_as_list(Attrs, NS, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, NS, Attr_Name) of
        #xmlattr{value = Value} ->
            binary_to_list(Value);
        _ ->
            Default
    end.

%% @spec (XML_Element, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = list()
%% @doc Return the value of the attribute named `Attr_Name', as a
%% list().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_as_list/3 ::
      (xmlel_any(), attributename(), Default) -> string() | Default;
      (undefined, attributename(), Default)   -> Default).

get_attribute_as_list(#xmlel{attrs = Attrs} = _XML_Element, Name,
		      Default) ->
    get_attribute_from_list_as_list(Attrs, Name, Default);
get_attribute_as_list(#xmlelement{attrs = Attrs} = _XML_Element, Name,
		      Default) ->
    get_attribute_from_list_as_list(Attrs, Name, Default);
get_attribute_as_list(undefined, _Name, Default) ->
    Default.

%% @spec (XML_Element, NS, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = list()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI, as a list().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_as_list/4 ::
      (xmlel() | undefined, xmlname(), attributename(), Default) -> string() | Default).

get_attribute_as_list(#xmlel{attrs = Attrs} = _XML_Element, NS, Name,
		      Default) ->
    get_attribute_from_list_as_list(Attrs, NS, Name, Default);
get_attribute_as_list(undefined, _NS, _Name, Default) ->
    Default.

%% @spec (Attrs, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name' from the
%% list, as a binary().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_from_list_as_binary/3 ::
      ([xmlattr()] | [xmlattr_old()], attributename(), Default) -> binary() | Default).

get_attribute_from_list_as_binary(Attrs, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        {_Name, Value} ->
            list_to_binary(Value);
        _ ->
            Default
    end.

%% @spec (Attrs, NS, Attr_Name, Default) -> Attr_Value | Default
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI from the list, as a binary().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_from_list_as_binary/4 ::
      ([xmlattr()], xmlname(), attributename(), Default) -> binary() | Default).

get_attribute_from_list_as_binary(Attrs, NS, Attr_Name, Default) ->
    case get_attribute_node_from_list(Attrs, NS, Attr_Name) of
        #xmlattr{value = Value} ->
            Value;
        _ ->
            Default
    end.

%% @spec (XML_Element, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name', as a
%% binary().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_as_binary/3 ::
      (xmlel_any(), attributename(), Default) -> binary() | Default;
      (undefined, attributename(), Default)   -> Default).

get_attribute_as_binary(#xmlel{attrs = Attrs} = _XML_Element, Name,
			Default) ->
    get_attribute_from_list_as_binary(Attrs, Name, Default);
get_attribute_as_binary(#xmlelement{attrs = Attrs} = _XML_Element, Name,
			Default) ->
    get_attribute_from_list_as_binary(Attrs, Name, Default);
get_attribute_as_binary(undefined, _Name, Default) ->
    Default.

%% @spec (XML_Element, NS, Attr_Name, Default) -> Attr_Value | Default
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Default = term()
%%     Attr_Value = binary()
%% @doc Return the value of the attribute named `Attr_Name' with the
%% `NS' namespace URI, as a binary().
%%
%% Return `Default' if the attribute isn't found.

-spec(get_attribute_as_binary/4 ::
      (xmlel(), xmlname(), attributename(), Default)   -> binary() | Default;
      (undefined, xmlname(), attributename(), Default) -> Default).

get_attribute_as_binary(#xmlel{attrs = Attrs} = _XML_Element, NS, Name,
			Default) ->
    get_attribute_from_list_as_binary(Attrs, NS, Name, Default);
get_attribute_as_binary(undefined, _NS, _Name, Default) ->
    Default.

%% @spec (Attrs, Attr_Name) -> boolean()
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%% @doc Check the presence for attribute `Attr_Name' in the list.

-spec(has_attribute_in_list/2 ::
      ([xmlattr()] | [xmlattr_old()], attributename()) -> boolean()).

has_attribute_in_list(Attrs, Name) ->
    case get_attribute_node_from_list(Attrs, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (Attrs, NS, Attr_Name) -> boolean()
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%% @doc Check the presence for attribute `Attr_Name' with namespace `NS'
%% in the list.

-spec(has_attribute_in_list/3 ::
      ([xmlattr()], xmlname(), attributename()) -> boolean()).

has_attribute_in_list(Attrs, NS, Name) ->
    case get_attribute_node_from_list(Attrs, NS, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, Attr_Name) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Attr_Name = binary()
%% @doc Check the presence for attribute `Attr_Name' in the XML element.

-spec(has_attribute/2 ::
      (xmlel_any() | undefined, attributename()) -> boolean()).

has_attribute(#xmlel{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(#xmlelement{attrs = Attrs} = _XML_Element, Name) ->
    has_attribute_in_list(Attrs, Name);
has_attribute(undefined, _Name) ->
    false.

%% @spec (XML_Element, NS, Attr_Name) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     NS = atom() | string()
%%     Attr_Name = binary()
%% @doc Check the presence for attribute `Attr_Name' with namespace `NS'
%% in the XML element.

-spec(has_attribute/3 ::
      (xmlel_any() | undefined, xmlname(), attributename()) -> boolean()).

has_attribute(#xmlel{attrs = Attrs} = _XML_Element, NS, Name) ->
    has_attribute_in_list(Attrs, NS, Name);
has_attribute(undefined, _NS, _Name) ->
    false.

%% @spec (Attrs, Attr) -> New_Attrs
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr = xmlattr() | xmlattr_old()
%%     Attr_Name = binary()
%%     Attr_Value = binary() | string() | atom() | integer()
%%     New_Attrs = [xmlattr()] | [xmlattr_old()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If a match is found, `Attr' will replace the old attribute as is,
%% regardless of the format of the latter.

%% XXX Dialyzer doesn't support this multiple-clause contract because
%% both clauses take a list() as a first argument. So until it can look
%% inside those list(), we specify a less strict contract.
%%
%% -spec(set_attribute_in_list/2 ::
%%   ([], xmlattr()                   -> [xmlattr()];
%%   ([], xmlattr_old())              -> [xmlattr_old()];
%%   ([xmlattr()], xmlattr()          -> [xmlattr()];
%%   ([xmlattr_old()], xmlattr_old()) -> [xmlattr_old()]).

-spec(set_attribute_in_list/2 ::
      ([xmlattr() | xmlattr_old()], xmlattr() | xmlattr_old()) ->
	     [xmlattr() | xmlattr_old()]).

set_attribute_in_list(Attrs, {Name, Value}) when is_binary(Name) ->
    set_attribute_in_list(Attrs, Name, Value);
set_attribute_in_list(Attrs, #xmlattr{} = Attr) ->
    set_attribute_in_list2(Attrs, Attr, []).

set_attribute_in_list2([Attr | Rest],
		       #xmlattr{ns = undefined, name = Name} = New_Attr, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true ->
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, New_Attr, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([Attr | Rest],
		       #xmlattr{ns = NS, name = Name} = New_Attr, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true ->
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, New_Attr, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([], New_Attr, New_Attrs) ->
    New_Attrs ++ [New_Attr].

%% @spec (Attrs, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     Attr_Value = binary() | string() | atom() | integer()
%%     New_Attrs = [xmlattr()] | [xmlattr_old()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlattr()} record if it can't determine the type from the
%% other attributes.

%% XXX Dialyzer doesn't support this multiple-clause contract because
%% both clauses take a list() as a first argument. So until it can look
%% inside those list(), we specify a less strict contract.
%%
%% -spec(set_attribute_in_list/3 ::
%%   ([xmlattr()], attributename(), binary() | string() | atom() | integer()) ->
%%       [xmlattr()];
%%   ([xmlattr_old()], attributename(), binary() | string() | atom() | integer()) ->
%%       [xmlattr_old()]).

-spec(set_attribute_in_list/3 ::
      ([xmlattr() | xmlattr_old()],
       attributename(), binary() | string() | atom() | integer()) ->
	     [xmlattr() | xmlattr_old()]).

set_attribute_in_list(Attrs, Name, Value) when is_binary(Name) ->
    set_attribute_in_list2(Attrs, Name, Value, []).

set_attribute_in_list2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true ->
            New_Attr = set_attr_value(Attr, Value),
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, Name, Value,
				   New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([], Name, Value, New_Attrs) ->
    New_Attr = case New_Attrs of
		   [#xmlattr{} | _] ->
		       attribute(Name, Value);
		   [{_, _} | _] ->
		       set_attr_value({Name, undefined}, Value);
		   _ ->
		       attribute(Name, Value)
	       end,
    New_Attrs ++ [New_Attr].

%% @spec (Attrs, NS, Attr_Name, Attr_Value) -> New_Attrs
%%     Attrs = [xmlattr()]
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Attr_Value = binary() | string() | atom() | integer()
%%     New_Attrs = [xmlattr()]
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.
%%
%% If the attribute is to be added, this function use the {@link
%% xmlattr()} record.

-spec(set_attribute_in_list/4 ::
      ([xmlattr()], xmlname(), attributename(),
       binary() | string() | atom() | integer()) ->
	     [xmlattr()]).

set_attribute_in_list(Attrs, NS, Name, Value) when is_binary(Name) ->
    set_attribute_in_list2(Attrs, NS, Name, Value, []).

set_attribute_in_list2([Attr | Rest], NS, Name, Value, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true ->
            New_Attr = set_attr_value(Attr, Value),
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_in_list2(Rest, NS, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_in_list2([], NS, Name, Value, New_Attrs) ->
    New_Attrs ++ [attribute(NS, Name, Value)].

%% @spec (XML_Element, Attr) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Attr = xmlattr() | xmlattr_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name.
%%
%% If a match is found, `Attr' will replace the old attribute as is,
%% regardless of the format of the latter.

-spec(set_attribute/2 ::
      (xmlel(), xmlattr() | xmlattr_old())     -> xmlel();
      (xmlel_old(), xmlattr() | xmlattr_old()) -> xmlel_old()).

set_attribute(#xmlel{attrs = Attrs} = XML_Element, Attr) ->
    New_Attrs = set_attribute_in_list(Attrs, Attr),
    XML_Element#xmlel{attrs = New_Attrs};
set_attribute(#xmlelement{attrs = Attrs} = XML_Element, Attr) ->
    New_Attrs = set_attribute_in_list(Attrs, Attr),
    XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (XML_Element, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Attr_Name = binary()
%%     Attr_Value = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Add a new attribute or change the value of an existing attribute.

-spec(set_attribute/3 ::
      (xmlel(), attributename(), binary() | string() | atom() | integer()) ->
	     xmlel();
      (xmlel_old(), attributename(), binary() | string() | atom() | integer()) ->
	     xmlel_old()).

set_attribute(#xmlel{attrs = Attrs} = XML_Element, Name, Value) when is_binary(Name)->
    New_Attrs = set_attribute_ns2(Attrs, Name, Value, []),
    XML_Element#xmlel{attrs = New_Attrs};
set_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name, Value) ->
    New_Attrs = set_attribute2(Attrs, Name, Value, []),
    XML_Element#xmlelement{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true ->
            New_Attr = set_attr_value(Attr, Value),
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_ns2(Rest, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_ns2([], Name, Value, New_Attrs) ->
    New_Attrs ++ [attribute(Name, Value)].

set_attribute2([Attr | Rest], Name, Value, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true ->
            New_Attr = set_attr_value(Attr, Value),
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute2(Rest, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute2([], Name, Value, New_Attrs) ->
    New_Attrs ++ [set_attr_value({Name, undefined}, Value)].

%% @spec (XML_Element, NS, Attr_Name, Attr_Value) -> New_XML_Element
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     Attr_Value = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel()
%% @doc Add a new attribute or change the value of an existing attribute
%% with the same name and the `NS' namespace URI.

-spec(set_attribute/4 ::
      (xmlel(), xmlname(), attributename(), binary() | string() | atom() | integer()) ->
	     xmlel()).

set_attribute(#xmlel{attrs = Attrs} = XML_Element, NS, Name, Value) when is_binary(Name) ->
    New_Attrs = set_attribute_ns2(Attrs, NS, Name, Value, []),
    XML_Element#xmlel{attrs = New_Attrs}.

set_attribute_ns2([Attr | Rest], NS, Name, Value, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true ->
            New_Attr = set_attr_value(Attr, Value),
            New_Attrs ++ [New_Attr] ++ Rest;
        false ->
            set_attribute_ns2(Rest, NS, Name, Value, New_Attrs ++ [Attr])
    end;
set_attribute_ns2([], NS, Name, Value, New_Attrs) ->
    New_Attrs ++ [attribute(NS, Name, Value)].

%% @spec (XML_Element, Attrs_Spec) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Attrs_Spec = [{Name, Value} | {NS, Name, Value} | xmlattr_old() | xmlattr()]
%%       NS = atom() | string()
%%       Name = binary()
%%       Value = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Set multiple attributes at a time.
%%
%% Existing attributes are not completly overwritten by the ones present
%% in `Attrs_Spec'. They are simply updated.

-spec(set_attributes/2 ::
      (xmlel(),
       [xmlattr() |
	{attributename(), binary() | string() | atom() | integer()} |
	{xmlname(), attributename(), binary() | string() | atom() | integer()}]) ->
	     xmlel();
      (xmlel_old(),
       [xmlattr_old() |
	{attributename(), binary() | string() | atom() | integer()}]) ->
	     xmlel_old()).

set_attributes(XML_Element, [{Name, Value} | Rest]) ->
    New_XML_Element = set_attribute(XML_Element, Name, Value),
    set_attributes(New_XML_Element, Rest);

set_attributes(XML_Element, [{NS, Name, Value} | Rest]) ->
    New_XML_Element = set_attribute(XML_Element, NS, Name, Value),
    set_attributes(New_XML_Element, Rest);

set_attributes(XML_Element, [#xmlattr{} = Attr | Rest]) ->
    New_XML_Element = set_attribute(XML_Element, Attr),
    set_attributes(New_XML_Element, Rest);

set_attributes(XML_Element, []) ->
    XML_Element.

%% @spec (Attrs, Attr_Name) -> New_Attrs
%%     Attrs = [xmlattr()] | [xmlattr_old()]
%%     Attr_Name = binary()
%%     New_Attrs = [xmlattr()] | [xmlattr_old()]
%% @doc Remove attribute named `Attr_Name' and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

%% XXX Dialyzer doesn't support this multiple-clause contract because
%% both clauses take a list() as a first argument. So until it can look
%% inside those list(), we specify a less strict contract.
%%
%% -spec(remove_attribute_from_list/2 ::
%%   ([], attributename())              -> [];
%%   ([xmlattr()], attributename())     -> [xmlattr()];
%%   ([xmlattr_old()], attributename()) -> [xmlattr_old()]).

-spec(remove_attribute_from_list/2 ::
      ([xmlattr() | xmlattr_old()], attributename()) -> [xmlattr() | xmlattr_old()]).

remove_attribute_from_list(Attrs, Name)  when is_binary(Name) ->
    remove_attribute_from_list2(Attrs, Name, []).

remove_attribute_from_list2([Attr | Rest], Name, New_Attrs) ->
    case attribute_matches(Attr, Name) of
        true when is_record(Attr, xmlattr) ->
            lists:reverse(New_Attrs) ++ Rest;
        true when is_tuple(Attr), size(Attr) == 2 ->
            lists:reverse(New_Attrs) ++ Rest;
        false ->
            remove_attribute_from_list2(Rest, Name,
					[Attr | New_Attrs])
    end;
remove_attribute_from_list2([], _Name, New_Attrs) ->
    lists:reverse(New_Attrs).

%% @spec (Attrs, NS, Attr_Name) -> New_Attrs
%%     Attrs = [xmlattr()]
%%     Attr_Name = binary()
%%     New_Attrs = [xmlattr()]
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new list.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

-spec(remove_attribute_from_list/3 ::
      ([xmlattr()], xmlname(), attributename()) -> [xmlattr()]).

remove_attribute_from_list(Attrs, NS, Name) when is_binary(Name) ->
    remove_attribute_from_list2(Attrs, NS, Name, []).

remove_attribute_from_list2([Attr | Rest], NS, Name, New_Attrs) ->
    case attribute_matches(Attr, NS, Name) of
        true when is_record(Attr, xmlattr) ->
            lists:reverse(New_Attrs) ++ Rest;
        false ->
            remove_attribute_from_list2(Rest, NS, Name,
					[Attr | New_Attrs])
    end;
remove_attribute_from_list2([], _NS, _Name, New_Attrs) ->
    lists:reverse(New_Attrs).

%% @spec (XML_Element, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Attr_Name = binary()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove attribute named `Attr_Name' and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

-spec(remove_attribute/2 ::
      (xmlel(), attributename())     -> xmlel();
      (xmlel_old(), attributename()) -> xmlel_old()).

remove_attribute(#xmlel{attrs = Attrs} = XML_Element, Name) when is_binary(Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, Name),
    XML_Element#xmlel{attrs = New_Attrs};

remove_attribute(#xmlelement{attrs = Attrs} = XML_Element, Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, Name),
    XML_Element#xmlelement{attrs = New_Attrs}.

%% @spec (XML_Element, NS, Attr_Name) -> New_XML_Element
%%     XML_Element = xmlel()
%%     NS = atom() | string()
%%     Attr_Name = binary()
%%     New_XML_Element = xmlel()
%% @doc Remove attribute named `Attr_Name' with the `NS' namespace URI
%% and return the new element.
%%
%% If `Attr_Name' doesn't exist, this function has no effect (it won't
%% return an error).

-spec(remove_attribute/3 ::
      (xmlel(), xmlname(), attributename()) -> xmlel()).

remove_attribute(#xmlel{attrs = Attrs} = XML_Element, NS, Name) when is_binary(Name) ->
    New_Attrs = remove_attribute_from_list(Attrs, NS, Name),
    XML_Element#xmlel{attrs = New_Attrs}.

%% --------------------------------------------------------------------
%% Functions to handle XML elements (xmlel() & xmlel_old()).
%% This is similar to the DOM interface but NOT compliant.
%% --------------------------------------------------------------------

%% @spec (Name) -> XML_Element
%%     Name = atom() | string()
%%     XML_Element = xmlel()
%% @doc Create an XML element with the name `Name' but no namespace.
%%
%% Caution: be sure you do not want to set a namespace: it won't be
%% inherited from the parent node!
%%
%% This is the same as:
%% ```
%% XML_Element = #xmlel{name = Name}.
%% '''

-spec(element/1 :: (xmlname()) -> xmlel()).

element(Name) ->
    #xmlel{name = Name}.

%% @spec (NS, Name) -> XML_Element
%%     NS = atom() | string() | undefined
%%     Name = atom() | string()
%%     XML_Element = xmlel()
%% @doc Create an XML element with the name `Name' in the namespace `NS'.
%%
%% This is the same as:
%% ```
%% XML_Element = #xmlel{ns = NS, name = Name}.
%% '''

-spec(element/2 :: (xmlname(), xmlname()) -> xmlel()).

element(NS, Name) ->
    #xmlel{ns = NS, name = Name}.

%% @spec (NS, Name, Attrs, Children) -> XML_Element
%%     NS = atom() | string() | undefined
%%     Name = atom() | string()
%%     Attrs = [xmlattr()]
%%     Children = [xmlcdata()]
%%     XML_Element = xmlel()
%% @doc Create an XML element with the name `Name' in the namespace `NS'.
%%
%% This is the same as:
%% ```
%% XML_Element = #xmlel{ns = NS, name = Name}.
%% '''

-spec(element/4 ::
      (xmlname(), xmlname(), [xmlattr()], [xmlel() | xmlcdata()]) -> xmlel()).

element(NS, Name, Attrs, Children) ->
    #xmlel{ns = NS, name = Name, attrs = Attrs, children = Children}.

%% @spec (XML_Element) -> Name
%%     XML_Element = xmlel() | xmlel_old()
%%     Name = list()
%% @doc Return the name of an element as list, regardless of the
%% original encoding.

-spec(get_name_as_list/1 :: (xmlel_any()) -> string()).

get_name_as_list(#xmlel{name = Name}) ->
    as_list(Name);
get_name_as_list(#xmlelement{name = Name}) ->
    as_list(Name).

%% @spec (XML_Element) -> Name
%%     XML_Element = xmlel() | xmlel_old()
%%     Name = atom()
%% @doc Return the name of an element as atom, regardless of the
%% original encoding.

-spec(get_name_as_atom/1 :: (xmlel_any()) -> atom()).

get_name_as_atom(#xmlel{name = Name}) ->
    as_atom(Name);
get_name_as_atom(#xmlelement{name = Name}) ->
    as_atom(Name).

%% @spec (XML_Element, Name) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%% @doc Tell if `XML_Element' is named `Name'.
%%
%% It takes care of comparison between string and atom.

-spec(element_matches/2 :: (xmlel_any(), xmlname()) -> boolean()).

element_matches(#xmlel{name = Name}, Name) ->
    true;
element_matches(#xmlelement{name = Name}, Name) ->
    true;

element_matches(#xmlel{name = Name_A}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlel{name = Name}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(#xmlelement{name = Name_A}, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlelement{name = Name}, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(_XML_Element, _Name) ->
    false.

%% @spec (XML_Element, NS, Name) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%% @doc Tell if `XML_Element' has the namespace `NS' and is named `Name'.
%%
%% It takes care of comparison between string and atom.

-spec(element_matches/3 :: (xmlel(), xmlname(), xmlname()) -> boolean()).

element_matches(#xmlel{ns = NS, name = Name}, NS, Name) ->
    true;

element_matches(#xmlel{ns = NS_A, name = Name_A}, NS, Name)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);
element_matches(#xmlel{ns = NS, name = Name}, NS_A, Name_A)
  when is_atom(NS_A), is_list(NS), is_atom(Name_A), is_list(Name) ->
    NS_A == list_to_atom(NS) andalso Name_A == list_to_atom(Name);

element_matches(#xmlel{ns = NS_A, name = Name}, NS, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);
element_matches(#xmlel{ns = NS, name = Name}, NS_A, Name)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);

element_matches(#xmlel{ns = NS, name = Name_A}, NS, Name)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);
element_matches(#xmlel{ns = NS, name = Name}, NS, Name_A)
  when is_atom(Name_A), is_list(Name) ->
    Name_A == list_to_atom(Name);

element_matches(_XML_Element, _NS, _Name) ->
    false.

%% @spec (XML_Element, NS) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     NS = atom() | string()
%% @doc Tell if `XML_Element' has the namespace `NS'.
%%
%% It takes care of comparison between string and atom.

-spec(element_matches_by_ns/2 :: (xmlel(), xmlname()) -> boolean()).

element_matches_by_ns(#xmlel{ns = NS}, NS) ->
    true;

element_matches_by_ns(#xmlel{ns = NS_A}, NS)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);
element_matches_by_ns(#xmlel{ns = NS}, NS_A)
  when is_atom(NS_A), is_list(NS) ->
    NS_A == list_to_atom(NS);

element_matches_by_ns(_XML_Element, _NS) ->
    false.

%% @spec (XML_Element, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlel_old()
%% @doc Search in the children of `XML_Element' an element named `Name'.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

-spec(get_element/2 ::
      (xmlel_any() | undefined, xmlname()) -> xmlel_any() | undefined).

get_element(#xmlel{children = Children}, Name) ->
    get_element2(Children, Name);
get_element(#xmlelement{children = Children}, Name) ->
    get_element2(Children, Name);
get_element(undefined, _Name) ->
    undefined.

get_element2([Node | Rest], Name) ->
    case element_matches(Node, Name) of
        true  -> Node;
        false -> get_element2(Rest, Name)
    end;
get_element2([], _Name) ->
    undefined;
get_element2(undefined, _Name) ->
    undefined.

%% @spec (XML_Element, NS, Name) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' an element named `Name'
%% with `NS' namespace URI.
%%
%% If no element with the given name is found, it returns `undefined'.
%% This will only search among direct children.

-spec(get_element/3 ::
      (xmlel() | undefined, xmlname(), xmlname()) -> xmlel() | undefined).

get_element(#xmlel{children = Children}, NS, Name) ->
    get_element2(Children, NS, Name);
get_element(undefined, _NS, _Name) ->
    undefined.

get_element2([Node | Rest], NS, Name) ->
    case element_matches(Node, NS, Name) of
        true  -> Node;
        false -> get_element2(Rest, NS, Name)
    end;
get_element2([], _NS, _Name) ->
    undefined;
get_element2(undefined, _NS, _Name) ->
    undefined.

%% @spec (XML_Element, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%%     XML_Subelement = xmlel() | xmlel_old()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name'
%%
%% This will only search among direct children.

-spec(get_elements/2 ::
      (xmlel_any() | undefined, xmlname()) -> [xmlel_any()]).

get_elements(#xmlel{children = Children}, Name) ->
    get_elements2(Children, Name);
get_elements(#xmlelement{children = Children}, Name) ->
    get_elements2(Children, Name);
get_elements(undefined, _Name) ->
    [].

get_elements2(undefined, _Name) ->
    [];
get_elements2([], _Name) ->
    [];
get_elements2(Children, Name) ->
    lists:filter(filter_by_name(Name), Children).

filter_by_name(Searched_Name) ->
    fun(XML_Element) ->
	    element_matches(XML_Element, Searched_Name)
    end.

%% @spec (XML_Element, NS, Name) -> [XML_Subelement]
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' for all the elements
%% named `Name' with `NS' namespace URI.
%%
%% This will only search among direct children.

-spec(get_elements/3 ::
      (xmlel() | undefined, xmlname(), xmlname()) -> [xmlel()]).

get_elements(#xmlel{children = Children}, NS, Name) ->
    get_elements2(Children, NS, Name);
get_elements(undefined, _NS, _Name) ->
    [].

get_elements2(undefined, _NS, _Name) ->
    [];
get_elements2([], _NS, _Name) ->
    [];
get_elements2(Children, NS, Name) ->
    lists:filter(filter_by_name(NS, Name), Children).

filter_by_name(Searched_NS, Searched_Name) ->
    fun(XML_Element) ->
	    element_matches(XML_Element, Searched_NS, Searched_Name)
    end.

%% @spec (XML_Element, NS) -> XML_Subelement | undefined
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     XML_Subelement = xmlel()
%% @doc Search in the children of `XML_Element' the first element with
%% `NS' namespace URI.
%%
%% If no element with the given namespace is found, it returns
%% `undefined'. This will only search among direct children.
%%
%% This function is particularly usefull to extract XMPP error codes.

-spec(get_element_by_ns/2 ::
      (xmlel() | undefined, xmlname()) -> xmlel() | undefined).

get_element_by_ns(#xmlel{children = Children}, NS) ->
    get_element_by_ns2(Children, NS);
get_element_by_ns(undefined, _NS) ->
    undefined.

get_element_by_ns2([Node | Rest], NS) ->
    case element_matches_by_ns(Node, NS) of
        true  -> Node;
        false -> get_element_by_ns2(Rest, NS)
    end;
get_element_by_ns2([], _NS) ->
    undefined;
get_element_by_ns2(undefined, _NS) ->
    undefined.

%% @spec (XML_Element, Name) -> boolean()
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%% @doc Check the presence for element `Name' in the children.

-spec(has_element/2 ::
      (xmlel_any() | undefined, xmlname()) -> boolean()).

has_element(XML_Element, Name) ->
    case get_element(XML_Element, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, NS, Name) -> boolean()
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%% @doc Check the presence for element `Name' with `NS' namespace URI in
%% the children.

-spec(has_element/3 ::
      (xmlel() | undefined, xmlname(), xmlname()) -> boolean()).

has_element(XML_Element, NS, Name) ->
    case get_element(XML_Element, NS, Name) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element, NS) -> boolean()
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%% @doc Check the presence for any elements with `NS' namespace URI in
%% the children.

-spec(has_element_by_ns/2 ::
      (xmlel() | undefined, xmlname()) -> boolean()).

has_element_by_ns(XML_Element, NS) ->
    case get_element_by_ns(XML_Element, NS) of
        undefined -> false;
        _         -> true
    end.

%% @spec (XML_Element) -> [XML_Subelement]
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     XML_Subelement = xmlel() | xmlel_old()
%% @doc Get all the element children of the given element, skipping
%% non-element nodes likes cdata.

-spec(get_child_elements/1 :: (xmlel_any() | undefined) -> [xmlel_any()]).

get_child_elements(#xmlel{children = Children}) ->
    get_child_elements2(Children);
get_child_elements(#xmlelement{children = Children}) ->
    get_child_elements2(Children);
get_child_elements(undefined) ->
    [].

get_child_elements2(undefined) ->
    [];
get_child_elements2([]) ->
    [];
get_child_elements2(Children) ->
    lists:filter(fun is_element/1, Children).

is_element(#xmlelement{}) -> true;
is_element(#xmlel{})      -> true;
is_element(_)             -> false.

%% @spec (XML_Element, Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove the first child with the name `Name'.

-spec(remove_element/2 :: (xmlel_any(), xmlname()) -> xmlel_any()).

remove_element(#xmlel{children = Children} = XML_Element, Name) ->
    New_Children = remove_element2(Children, Name),
    XML_Element#xmlel{children = New_Children};
remove_element(#xmlelement{children = Children} = XML_Element, Name) ->
    New_Children = remove_element2(Children, Name),
    XML_Element#xmlelement{children = New_Children}.

remove_element2(undefined, _Name) ->
    undefined;
remove_element2(Children, Name) ->
    remove_element3(Children, Name, []).

remove_element3([El | Rest], Name, Result) ->
    case element_matches(El, Name) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element3(Rest, Name, [El | Result])
    end;
remove_element3([], _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS, Name) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove the first child with the name `Name' in the namespace `NS'.

-spec(remove_element/3 :: (xmlel(), xmlname(), xmlname()) -> xmlel()).

remove_element(#xmlel{children = Children} = XML_Element, NS, Name) ->
    New_Children = remove_element2(Children, NS, Name),
    XML_Element#xmlel{children = New_Children}.

remove_element2(undefined, _NS, _Name) ->
    undefined;
remove_element2(Children, NS, Name) ->
    remove_element3(Children, NS, Name, []).

remove_element3([El | Rest], NS, Name, Result) ->
    case element_matches(El, NS, Name) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element3(Rest, NS, Name, [El | Result])
    end;
remove_element3([], _NS, _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove the first child in the namespace `NS'.

-spec(remove_element_by_ns/2 :: (xmlel(), xmlname()) -> xmlel()).

remove_element_by_ns(#xmlel{children = Children} = XML_Element, NS) ->
    New_Children = remove_element_by_ns2(Children, NS),
    XML_Element#xmlel{children = New_Children}.

remove_element_by_ns2(undefined, _NS) ->
    undefined;
remove_element_by_ns2(Children, NS) ->
    remove_element_by_ns3(Children, NS, []).

remove_element_by_ns3([El | Rest], NS, Result) ->
    case element_matches_by_ns(El, NS) of
        true  -> lists:append(lists:reverse(Result), Rest);
        false -> remove_element_by_ns3(Rest, NS, [El | Result])
    end;
remove_element_by_ns3([], _NS, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, Name) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     Name = atom() | string()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove all children with the name `Name'.

-spec(remove_elements/2 :: (xmlel_any(), xmlname()) -> xmlel_any()).

remove_elements(#xmlel{children = Children} = XML_Element, Name) ->
    New_Children = remove_elements2(Children, Name),
    XML_Element#xmlel{children = New_Children};
remove_elements(#xmlelement{children = Children} = XML_Element, Name) ->
    New_Children = remove_elements2(Children, Name),
    XML_Element#xmlelement{children = New_Children}.

remove_elements2(undefined, _Name) ->
    undefined;
remove_elements2(Children, Name) ->
    remove_elements3(Children, Name, []).

remove_elements3([El | Rest], Name, Result) ->
    case element_matches(El, Name) of
        true  -> remove_elements3(Rest, Name, Result);
        false -> remove_elements3(Rest, Name, [El | Result])
    end;
remove_elements3([], _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS, Name) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     Name = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove all children with the name `Name' in the namespace `NS'.

-spec(remove_elements/3 :: (xmlel(), xmlname(), xmlname()) -> xmlel()).

remove_elements(#xmlel{children = Children} = XML_Element, NS, Name) ->
    New_Children = remove_elements2(Children, NS, Name),
    XML_Element#xmlel{children = New_Children}.

remove_elements2(undefined, _NS, _Name) ->
    undefined;
remove_elements2(Children, NS, Name) ->
    remove_elements3(Children, NS, Name, []).

remove_elements3([El | Rest], NS, Name, Result) ->
    case element_matches(El, NS, Name) of
        true  -> remove_elements3(Rest, NS, Name, Result);
        false -> remove_elements3(Rest, NS, Name, [El | Result])
    end;
remove_elements3([], _NS, _Name, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, NS) -> New_XML_Element
%%     XML_Element = xmlel() | undefined
%%     NS = atom() | string()
%%     New_XML_Element = xmlel()
%% @doc Remove all children in the namespace `NS'.

-spec(remove_elements_by_ns/2 :: (xmlel(), xmlname()) -> xmlel()).

remove_elements_by_ns(#xmlel{children = Children} = XML_Element, NS) ->
    New_Children = remove_elements_by_ns2(Children, NS),
    XML_Element#xmlel{children = New_Children}.

remove_elements_by_ns2(undefined, _NS) ->
    undefined;
remove_elements_by_ns2(Children, NS) ->
    remove_elements_by_ns3(Children, NS, []).

remove_elements_by_ns3([El | Rest], NS, Result) ->
    case element_matches_by_ns(El, NS) of
        true  -> remove_elements_by_ns3(Rest, NS, Result);
        false -> remove_elements_by_ns3(Rest, NS, [El | Result])
    end;
remove_elements_by_ns3([], _NS, Result) ->
    lists:reverse(Result).

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Child = xmlel() | xmlel_old() | xmlcdata()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Prepend `Child' to `XML_Element''s children list.

-spec(prepend_child/2 :: (xmlel_any(), xmlnode()) -> xmlel_any()).

prepend_child(#xmlel{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlel{children = New_Children};
prepend_child(#xmlelement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlelement{children = New_Children};
prepend_child(#xmlel{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlel{children = New_Children};
prepend_child(#xmlelement{children = Children} = XML_Element, Child) ->
    New_Children = [Child | Children],
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Children = [xmlel() | xmlel_old() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Prepend every `Children' to `XML_Element''s children list.

-spec(prepend_children/2 :: (xmlel_any(), [xmlnode()]) -> xmlel_any()).

prepend_children(#xmlel{children = undefined} = XML_Element,
		 New_Children) ->
    XML_Element#xmlel{children = New_Children};
prepend_children(#xmlelement{children = undefined} = XML_Element,
		 New_Children) ->
    XML_Element#xmlelement{children = New_Children};
prepend_children(#xmlel{children = Children} = XML_Element,
		 New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlel{children = Concat_Children};
prepend_children(#xmlelement{children = Children} = XML_Element,
		 New_Children) ->
    Concat_Children = New_Children ++ Children,
    XML_Element#xmlelement{children = Concat_Children}.

%% @spec (XML_Element, Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Child = xmlel() | xmlel_old() | xmlcdata()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Append `Child' to `XML_Element''s children list.

-spec(append_child/2 :: (xmlel_any(), xmlnode()) -> xmlel_any()).

append_child(#xmlel{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlel{children = New_Children};
append_child(#xmlelement{children = undefined} = XML_Element, Child) ->
    New_Children = [Child],
    XML_Element#xmlelement{children = New_Children};
append_child(#xmlel{children = Children} = XML_Element, Child) ->
    New_Children = Children ++ [Child],
    XML_Element#xmlel{children = New_Children};
append_child(#xmlelement{children = Children} = XML_Element, Child) ->
    New_Children = Children ++ [Child],
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Children = [xmlel() | xmlel_old() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Append every `Children' to `XML_Element''s children list.

-spec(append_children/2 :: (xmlel_any(), [xmlnode()]) -> xmlel_any()).

append_children(#xmlel{children = undefined} = XML_Element,
		New_Children) ->
    XML_Element#xmlel{children = New_Children};
append_children(#xmlelement{children = undefined} = XML_Element,
		New_Children) ->
    XML_Element#xmlelement{children = New_Children};
append_children(#xmlel{children = Children} = XML_Element,
		New_Children) ->
    Concat_Children = Children ++ New_Children,
    XML_Element#xmlel{children = Concat_Children};
append_children(#xmlelement{children = Children} = XML_Element,
		New_Children) ->
    Concat_Children = Children ++ New_Children,
    XML_Element#xmlelement{children = Concat_Children}.

%% @spec (XML_Element, Old_Child, New_Child) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Old_Child = xmlel() | xmlel_old() | xmlcdata()
%%     New_Child = xmlel() | xmlel_old() | xmlcdata()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Replace `Old_Child' by `New_Child' in `XML_Element' children
%% list.

-spec(replace_child/3 :: (xmlel_any(), xmlnode(), xmlnode()) -> xmlel_any()).

replace_child(#xmlel{children = Children} = XML_Element,
	      Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlel{children = New_Children};
replace_child(#xmlelement{children = Children} = XML_Element,
	      Old_Child, New_Child) ->
    New_Children = replace_child2(Children, Old_Child, New_Child),
    XML_Element#xmlelement{children = New_Children}.

replace_child2(undefined, _Old_Child, _New_Child) ->
    undefined;
replace_child2([], _Old_Child, _New_Child) ->
    [];
replace_child2(Children, Old_Child, New_Child) ->
    [
     case C of
	 Old_Child -> New_Child;
	 _         -> C
     end || C <- Children
	       ].

%% @spec (XML_Element, Children) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Children = [xmlel() | xmlel_old() | xmlcdata()]
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Set `XML_Element''s children list to `Children'.
%%
%% Any existing child is removed.

-spec(set_children/2 :: (xmlel_any(), [xmlnode()]) -> xmlel_any()).

set_children(#xmlel{} = XML_Element, New_Children)
  when is_list(New_Children) ->
    XML_Element#xmlel{children = New_Children};
set_children(#xmlelement{} = XML_Element, New_Children)
  when is_list(New_Children) ->
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Pred, XML_Element) -> New_XML_Element
%%     Pred = function()
%%     Child = xmlel() | xmlel_old()
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove any children for which `Pred(Child)' doesn't return `true'.
%%
%% `Pred' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> boolean()
%% '''
%%
%% If `children' is `undefined', the function isn't called.

-spec(filter/2 ::
      (fun((xmlel_any(), xmlnode()) -> boolean()), xmlel_any()) -> xmlel_any()).

filter(Pred, #xmlel{children = Children} = XML_Element)
  when is_function(Pred, 2) ->
    New_Children = filter2(Pred, XML_Element, Children),
    XML_Element#xmlel{children = New_Children};
filter(Pred, #xmlelement{children = Children} = XML_Element)
  when is_function(Pred, 2) ->
    New_Children = filter2(Pred, XML_Element, Children),
    XML_Element#xmlelement{children = New_Children}.

filter2(_Pred, _XML_Element, undefined) ->
    undefined;
filter2(Pred, XML_Element, Children) ->
    [C || C <- Children, Pred(XML_Element, C)].

%% @spec (Fun, Acc0, XML_Element) -> Acc1
%%     Fun = function()
%%     Acc_In = term()
%%     Child = xmlel() | xmlel_old() | undefined
%%     Acc_Out = term()
%%     Acc0 = term()
%%     XML_Element = xmlel() | xmlel_old()
%%     Acc1 = term()
%% @doc Call `Fun' for each `XML_Element''s children and return the last
%% accumulator.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(Acc_In, XML_Element, Child) -> Acc_Out
%% '''

-spec(fold/3 ::
      (fun((any(), xmlel_any(), xmlnode() | undefined) -> any()), any(), xmlel_any()) ->
	     any()).

fold(Fun, Acc0, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 3) ->
    fold2(Fun, Acc0, XML_Element, Children);
fold(Fun, Acc0, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 3) ->
    fold2(Fun, Acc0, XML_Element, Children).

fold2(Fun, Acc_In, XML_Element, undefined) ->
    Fun(Acc_In, XML_Element, undefined);
fold2(Fun, Acc_In, XML_Element, [Child | Rest]) ->
    fold2(Fun, Fun(Acc_In, XML_Element, Child), XML_Element, Rest);
fold2(_Fun, Acc_Out, _XML_Element, []) ->
    Acc_Out.

%% @spec (Fun, XML_Element) -> ok
%%     Fun = function()
%%     Child = xmlel() | xmlel_old() | undefined
%%     XML_Element = xmlel() | xmlel_old()
%% @doc Call `Fun' for each `XML_Element''s children.
%%
%% `Fun' return value is ignored.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> Ignored
%% '''

-spec(foreach/2 ::
      (fun((xmlel_any(), xmlnode() | undefined) -> any()), xmlel_any()) ->
	     ok).

foreach(Fun, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    foreach2(Fun, XML_Element, Children);
foreach(Fun, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    foreach2(Fun, XML_Element, Children).

foreach2(Fun, XML_Element, undefined) ->
    Fun(XML_Element, undefined),
    ok;
foreach2(Fun, XML_Element, [Child | Rest]) ->
    Fun(XML_Element, Child),
    foreach2(Fun, XML_Element, Rest);
foreach2(_Fun, _XML_Element, []) ->
    ok.

%% @spec(Fun, XML_Element) -> New_XML_Element
%%     Fun = function()
%%     Child = xmlel() | xmlel_old()
%%     New_Child = xmlel() | xmlel_old()
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Apply `Fun' on each child and replace the original one with the
%% function return value.
%%
%% `Fun' has the following prototype:
%% ```
%% fun(XML_Element, Child) -> New_Child
%% '''
%%
%% If `children' is `undefined', the function isn't called.

-spec(map/2 ::
      (fun((xmlel_any(), xmlnode()) -> xmlnode()), xmlel_any()) ->
	     xmlel_any()).

map(Fun, #xmlel{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    New_Children = map2(Fun, XML_Element, Children),
    XML_Element#xmlel{children = New_Children};
map(Fun, #xmlelement{children = Children} = XML_Element)
  when is_function(Fun, 2) ->
    New_Children = map2(Fun, XML_Element, Children),
    XML_Element#xmlelement{children = New_Children}.

map2(_Fun, _XML_Element, undefined) ->
    undefined;
map2(Fun, XML_Element, [Child | Rest]) ->
    [Fun(XML_Element, Child) | map2(Fun, XML_Element, Rest)];
map2(_Fun, _XML_Element, []) ->
    [].

%% --------------------------------------------------------------------
%% Functions to handle XML text nodes.
%% This is similar to the DOM interface but NOT compliant.
%% --------------------------------------------------------------------

%% @spec (Value) -> CData
%%     Value = binary() | string() | atom() | integer()
%%     CData = xmlcdata()
%% @doc Create a CData node from a value.

-spec(cdata/1 :: (binary() | string() | atom() | integer()) -> xmlcdata()).

cdata(CData) ->
    #xmlcdata{cdata = exmpp_utils:any_to_binary(CData)}.

%% @spec (Children) -> CData
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     CData = binary()
%% @doc Concatenate and return any character data from the given
%% children list.

-spec(get_cdata_from_list/1 :: ([xmlnode()] | undefined) -> binary()).

get_cdata_from_list(undefined) ->
    <<>>;
get_cdata_from_list(Children) ->
    %% The function list_to_binary/1 will concatenate every
    %% binaries in the list returned by get_cdata_from_list2/2.
    list_to_binary(get_cdata_from_list2(Children, [])).

get_cdata_from_list2([#xmlcdata{cdata = Chunk} | Rest], Data) ->
    get_cdata_from_list2(Rest, [Chunk | Data]);
get_cdata_from_list2([_ | Rest], Data) ->
    get_cdata_from_list2(Rest, Data);
get_cdata_from_list2([], Data) ->
    lists:reverse(Data).

%% @spec (Children) -> CData
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     CData = string()
%% @doc Concatenate and return any character data from the given
%% children list.

-spec(get_cdata_from_list_as_list/1 :: ([xmlnode()] | undefined) -> string()).

get_cdata_from_list_as_list(Children) ->
    binary_to_list(get_cdata_from_list(Children)).

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     CData = binary()
%% @doc Concatenate and return any character data of the given XML
%% element.
%%
%% This function is `get_tag_cdata/1' renamed in `get_cdata/1'. It
%% doesn't take a list of children like the old `get_cdata/1', use
%% {@link get_cdata_from_list/1} for this purpose!

-spec(get_cdata/1 :: (xmlel_any()) -> binary()).

get_cdata(#xmlel{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(#xmlelement{children = Children}) ->
    get_cdata_from_list(Children);
get_cdata(undefined) ->
    %% This clause makes it possible to write code like:
    %% exmpp_xml:get_cdata(exmpp_xml:get_element(XML_El, body))
    <<>>.

%% @spec (XML_Element) -> CData
%%     XML_Element = xmlel() | xmlel_old() | undefined
%%     CData = string()
%% @doc Concatenate and return any character data of the given XML
%% element.

-spec(get_cdata_as_list/1 :: (xmlel_any()) -> string()).

get_cdata_as_list(XML_Element) ->
    binary_to_list(get_cdata(XML_Element)).

%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one.

-spec(normalize_cdata_in_list/1 ::
      ([xmlnode()] | undefined) -> [xmlnode()] | undefined).

normalize_cdata_in_list(undefined) ->
    undefined;
normalize_cdata_in_list([]) ->
    [];
normalize_cdata_in_list(Children) ->
    normalize_cdata_in_list2(Children, [], []).

normalize_cdata_in_list2([], Current_CDatas, New_Children) ->
    New_Children1 = case list_to_binary(lists:reverse(Current_CDatas)) of
			<<>>  -> [New_Children];
			CData -> [#xmlcdata{cdata = CData} | New_Children]
		    end,
    lists:reverse(lists:flatten(New_Children1));
normalize_cdata_in_list2([#xmlcdata{cdata = CData} | Rest], Current_CDatas,
			 New_Children) ->
    normalize_cdata_in_list2(Rest, [CData | Current_CDatas], New_Children);
normalize_cdata_in_list2([XML_Node | Rest], Current_CDatas, New_Children) ->
    New_Children1 = case list_to_binary(lists:reverse(Current_CDatas)) of
			<<>>  -> [XML_Node | New_Children];
			CData -> [XML_Node, cdata(CData) | New_Children]
		    end,
    normalize_cdata_in_list2(Rest, [], New_Children1).

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Regroup all splitted {@link xmlcdata()} in a unique one
%% and remove empty ones.
%%
%% One caveats is the reconstructed {@link xmlcdata()} is appended at
%% the end of the children list.

-spec(normalize_cdata/1 :: (xmlel_any()) -> xmlel_any()).

normalize_cdata(#xmlel{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlel{children = New_Children};
normalize_cdata(#xmlelement{children = Children} = XML_Element) ->
    New_Children = normalize_cdata_in_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Children, CData) -> New_Children
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     CData = binary() | string() | atom() | integer()
%%     New_Children = [xmlel() | xmlel_old() | xmlcdata()]
%% @doc Replace any character data by `CData' in the list.
%%
%% The new `CData' is placed at the end of the children list.

-spec(set_cdata_in_list/2 ::
      ([xmlnode()] | undefined, binary() | string() | atom() | integer()) ->
	     [xmlnode()]).

set_cdata_in_list(undefined, CData) ->
    [cdata(CData)];
set_cdata_in_list(Children, CData) ->
    Purged_Children = remove_cdata_from_list(Children),
    Purged_Children ++ [cdata(CData)].

%% @spec (XML_Element, CData) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     CData = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Replace any character data by `CData'.
%%
%% The new `CData' is placed at the end of the children list.

-spec(set_cdata/2 ::
      (xmlel_any(), binary() | string() | atom() | integer()) -> xmlel_any()).

set_cdata(#xmlel{children = Children} = XML_Element, CData) ->
    New_Children = set_cdata_in_list(Children, CData),
    XML_Element#xmlel{children = New_Children};
set_cdata(#xmlelement{children = Children} = XML_Element, CData) ->
    New_Children = set_cdata_in_list(Children, CData),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Children, CData) -> New_Children
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     CData = binary() | string() | atom() | integer()
%%     New_Children = [xmlel() | xmlel_old() | xmlcdata()]
%% @doc Append `CData' to `Children' list.

-spec(append_cdata_to_list/2 ::
      ([xmlnode()] | undefined, binary() | string() | atom() | integer()) ->
	     [xmlnode()]).

append_cdata_to_list(undefined, CData) ->
    [cdata(CData)];
append_cdata_to_list(Children, CData) ->
    Children ++ [cdata(CData)].

%% @spec (XML_Element, CData) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     CData = binary() | string() | atom() | integer()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Append `Child' to `XML_Element''s children list.

-spec(append_cdata/2 ::
      (xmlel_any(), binary() | string() | atom() | integer()) -> xmlel_any()).

append_cdata(#xmlel{children = Children} = XML_Element, CData) ->
    New_Children = append_cdata_to_list(Children, CData),
    XML_Element#xmlel{children = New_Children};
append_cdata(#xmlelement{children = Children} = XML_Element, CData) ->
    New_Children = append_cdata_to_list(Children, CData),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlel_old()] | undefined
%% @doc Remove any character data from the given XML element children
%% list.

-spec(remove_cdata_from_list/1 ::
      ([xmlnode()] | undefined) -> [xmlnode()] | undefined).

remove_cdata_from_list(undefined) ->
    undefined;
remove_cdata_from_list(Children) ->
    [Child || Child <- Children, remove_cdata_from_list2(Child)].

remove_cdata_from_list2(#xmlcdata{}) -> false;
remove_cdata_from_list2(_)           -> true.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove any character data from the given XML element.
%%
%% This function doesn't take a list of children like the old
%% `remove_cdata/1', use {@link remove_cdata_from_list/1} for this
%% purpose!

-spec(remove_cdata/1 :: (xmlel_any()) -> xmlel_any()).

remove_cdata(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_cdata_from_list(Children),
    XML_Element#xmlel{children = New_Children};
remove_cdata(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_cdata_from_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (CData) -> boolean()
%%     CData = xmlel() | xmlel_old() | xmlcdata()
%% @doc Tell if a text node contains only whitespaces.
%%
%% Of course, if an XML element is given in argument, it will return
%%`false'.
%%
%% Whitespaces are `\s', `\t', `\n' and `\r'.

-spec(is_whitespace/1 :: (xmlnode()) -> boolean()).

is_whitespace(#xmlcdata{cdata = CData}) ->
    is_whitespace2(CData);
is_whitespace(_) ->
    false.

is_whitespace2(<<C:8, Rest/binary>>)
  when C == $\s; C == $\t; C == $\n; C == $\r ->
    is_whitespace2(Rest);
is_whitespace2(<<>>) ->
    true;
is_whitespace2(_CData) ->
    false.

%% @spec (Children) -> New_Children
%%     Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%%     New_Children = [xmlel() | xmlel_old() | xmlcdata()] | undefined
%% @doc Remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

-spec(remove_whitespaces_from_list/1 ::
      ([xmlnode()] | undefined) -> [xmlnode()] | undefined).

remove_whitespaces_from_list(undefined) ->
    undefined;
remove_whitespaces_from_list(Children) ->
    [Child || Child <- Children, not is_whitespace(Child)].

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

-spec(remove_whitespaces/1 :: (xmlel_any()) -> xmlel_any()).

remove_whitespaces(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_from_list(Children),
    XML_Element#xmlel{children = New_Children};
remove_whitespaces(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_from_list(Children),
    XML_Element#xmlelement{children = New_Children}.

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Remove text nodes containing only whitespaces in every elements
%% in the given tree.
%%
%% @see is_whitespace/1.

-spec(remove_whitespaces_deeply/1 :: (xmlel_any()) -> xmlel_any()).

remove_whitespaces_deeply(#xmlel{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_deeply2(Children),
    XML_Element#xmlel{children = New_Children};
remove_whitespaces_deeply(#xmlelement{children = Children} = XML_Element) ->
    New_Children = remove_whitespaces_deeply2(Children),
    XML_Element#xmlelement{children = New_Children}.

remove_whitespaces_deeply2(undefined) ->
    undefined;
remove_whitespaces_deeply2(Children) ->
    remove_whitespaces_deeply3(Children, []).

remove_whitespaces_deeply3([El | Rest], Result)
  when is_record(El, xmlel); is_record(El, xmlelement) ->
    New_El = remove_whitespaces_deeply(El),
    remove_whitespaces_deeply3(Rest, [New_El | Result]);
remove_whitespaces_deeply3([#xmlcdata{} = CData | Rest], Result) ->
    case is_whitespace(CData) of
        true  -> remove_whitespaces_deeply3(Rest, Result);
        false -> remove_whitespaces_deeply3(Rest, [CData | Result])
    end;
remove_whitespaces_deeply3([Other | Rest], Result) ->
    remove_whitespaces_deeply3(Rest, [Other | Result]);
remove_whitespaces_deeply3([], Result) ->
    lists:reverse(Result).

%% --------------------------------------------------------------------
%% Function to walk the tree.
%% --------------------------------------------------------------------

%% @spec (XML_Element, Path) -> XML_Subelement | Attr_Value | CData | Not_Found
%%     XML_Element = xmlel() | xmlel_old()
%%     Path = [pathcomponent()]
%%     XML_Subelement = xmlel() | xmlel_old()
%%     Attr_Value = string()
%%     CData = binary()
%%     Not_Found = nil() | binary() | undefined
%% @throws {xml, path, ending_component_not_at_the_end, Path} |
%%         {xml, path, invalid_component,               Path}
%% @doc Follow the given path and return what's pointed by the last
%% component of it.
%%
%% `Path' is a list of path components. If a component points to an
%% {@link xmlel()} or {@link xmlel_old()}, the function will
%% look for this element and will use it as a base for the next path
%% component. If a component points to an attribute, the function will
%% look for this attribute in the current element and return its value
%% (see {@link get_attribute/2} for the possible return values).
%% If a component asks for character data, the function will return
%% character data for the current element (see {@link get_cdata/1}
%% for possible return values). A path will not be followed further
%% after an attribute or a character data component. If an XML element
%% isn't found while walking through the path, an empty string is
%% returned.

-spec(get_path/2 ::
      (xmlel(), xmlpath()) -> xmlel_any() | binary() | string() | undefined).

get_path(XML_Element, [{element, Name} | Path]) ->
    case get_element(XML_Element, Name) of
        undefined      -> get_path_not_found(Path);
        XML_Subelement -> get_path(XML_Subelement, Path)
    end;
get_path(XML_Element, [{element, NS, Name} | Path]) ->
    case get_element(XML_Element, NS, Name) of
        undefined      -> get_path_not_found(Path);
        XML_Subelement -> get_path(XML_Subelement, Path)
    end;
get_path(XML_Element, [{attribute, Name}]) ->
    get_attribute(XML_Element, Name, "");
get_path(XML_Element, [{attribute, Name, Default}]) ->
    get_attribute(XML_Element, Name, Default);
get_path(XML_Element, [{attribute, NS, Name, Default}]) ->
    get_attribute(XML_Element, NS, Name, Default);
get_path(XML_Element, [cdata]) ->
    get_cdata(XML_Element);
get_path(XML_Element, [cdata_as_list]) ->
    get_cdata_as_list(XML_Element);
get_path(XML_Element, []) ->
    XML_Element;
get_path(_XML_Element, [{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [{attribute, _Name, _Default} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [{attribute, _NS, _Name, _Default} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, [cdata_as_list | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path(_XML_Element, Path) ->
    throw({xml, path, invalid_component, Path}).

get_path_not_found([{element, _Name} | Rest]) ->
    get_path_not_found(Rest);
get_path_not_found([{attribute, _Name}]) ->
    "";
get_path_not_found([{attribute, _NS, _Name}]) ->
    "";
get_path_not_found([cdata]) ->
    <<>>;
get_path_not_found([cdata_as_list]) ->
    "";
get_path_not_found([]) ->
    undefined;
get_path_not_found([{attribute, _Name} | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found([cdata | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found([cdata_as_list | _Rest] = Path) ->
    throw({xml, path, ending_component_not_at_the_end, Path});
get_path_not_found(Path) ->
    throw({xml, path, invalid_component, Path}).

%% --------------------------------------------------------------------
%% Converters.
%% --------------------------------------------------------------------

%% @spec (XML_NS_Element) -> XML_Element
%%     XML_NS_Element = xmlel() | xmlel_old() | xmlcdata()
%%     XML_Element = xmlel_old() | xmlcdata()
%% @doc Convert an {@link xmlel()} to an {@link xmlel_old()} tuple.
%%
%% Other tuples are ignored.

-spec(xmlel_to_xmlelement/1 :: (xmlel()) -> xmlel_old()).

xmlel_to_xmlelement(XML_Element) ->
    xmlel_to_xmlelement(XML_Element, [], []).

%% @spec (XML_NS_Element, Default_NS, Prefixed_NS) -> XML_Element
%%     XML_NS_Element = xmlel() | xmlel_old() | xmlcdata()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Element = xmlel_old() | xmlcdata()
%% @doc Convert an {@link xmlel()} to an {@link xmlel_old()} tuple.
%%
%% Other tuples are ignored.
%%
%% `Default_NS' and `Prefixed_NS' contain namespace declaration which
%% occured above this fragment in the tree. The order in the first list
%% is important: declarations are sorted from the most recent one to
%% the oldest one.
%%
%% This may be useful in XMPP context where a majority of clients or
%% servers expects a `stream' prefix for the `<stream>' tag and the
%% default namespace declaration in this same element.
%%
%% `Default_NS' may be a list of equivalent namespaces. This is useful
%% when stanzas go to and from streams with compatible but different
%% namespaces. Here is an example with `jabber:client', `jabber:server'
%% and `jabber:component:accept':
%% ```
%% exmpp_stanza:to_list(El,
%%   [?NS_JABBER_CLIENT, ?NS_JABBER_SERVER, ?NS_COMPONENT_ACCEPT]).
%% '''

-spec(xmlel_to_xmlelement/3 ::
      (xmlel(), xmldefaultnss(), xmlprefixednss()) -> xmlel_old()).

xmlel_to_xmlelement(#xmlel{children = Children} = El,
		    Default_NS, Prefixed_NS) ->
    %% Unresolve namespaces.
    {New_Name, New_Attrs, Default_NS1, Prefixed_NS1} = unresolve_xmlel_nss(El,
									   Default_NS, Prefixed_NS),
    %% Treat children.
    New_Children = xmlels_to_xmlelements(Children, Default_NS1, Prefixed_NS1),
    %% Now, create the final #xmlelement.
    #xmlelement{name = New_Name, attrs = New_Attrs, children = New_Children};
xmlel_to_xmlelement(#xmlendtag{} = Endtag,
		    Default_NS, Prefixed_NS) ->
    %% Unresolve namespaces.
    New_Name = unresolve_endtag_nss(Endtag, Default_NS, Prefixed_NS),
    %% Now, recreate the final #xmlendtag.
    #xmlendtag{ns = undefined, name = New_Name};
xmlel_to_xmlelement(XML_El, _Default_NS, _Prefixed_NS) ->
    %% xmlel_old() or xmlcdata().
    XML_El.

unresolve_xmlel_nss(#xmlel{ns = NS, name = Name, attrs = Attrs,
			   declared_ns = Declared_NS},
		    Default_NS, Prefixed_NS) ->
    %% First, we add namespace declarations to element attributes.
    {Prefix, Attrs1, Default_NS1, Prefixed_NS1} =
	forward_declare_ns(NS, lists:reverse(Declared_NS), Attrs,
			   Default_NS, Prefixed_NS),
    %% Then, we convert attributes ot the old format.
    {New_Attrs, Prefixed_NS2} = xmlnsattributes_to_xmlattributes(Attrs1,
								 Prefixed_NS1),
    %% We can now proceed with the modification of the name.
    Name_S = exmpp_known_elems:elem_as_list(Name),
    New_Name = case Prefix of
		   none -> Name_S;
		   _    -> Prefix ++ ":" ++ Name_S
	       end,
    {New_Name, New_Attrs, Default_NS1, Prefixed_NS2}.

unresolve_endtag_nss(#xmlendtag{ns = NS, name = Name},
		     Default_NS, Prefixed_NS) ->
    Name_S = exmpp_known_nss:ns_as_list(Name),
    Use_Default_NS = use_default_ns(NS, Default_NS),
    case Use_Default_NS of
        true ->
	    %% This end tag uses the default namespace.
            Name_S;
        false ->
	    %% Search a prefix in already declared namespaces.
            case search_in_prefixed_ns(NS, Prefixed_NS) of
                undefined ->
		    %% Too late to declare something; the
		    %% namespace should have been provided
		    %% by the caller.
                    Name_S;
                Prefix ->
                    Prefix ++ ":" ++ Name_S
            end
    end.

%% Function called to convert element attributes.
xmlnsattributes_to_xmlattributes(Attrs, Prefixed_NS) ->
    xmlnsattributes_to_xmlattributes2(Attrs, Prefixed_NS, []).

xmlnsattributes_to_xmlattributes2([#xmlattr{ns = NS, name = Name,
					    value = Value} | Rest],
				  Prefixed_NS, Converted_Attrs) ->
    Name_S = binary_to_list(Name),
    {New_Name, Converted_Attrs1, Prefixed_NS1} =
	case NS of
	    undefined ->
		{Name_S,
		 Converted_Attrs,
		 Prefixed_NS
		};
	    _ ->
		case search_in_prefixed_ns(NS, Prefixed_NS) of
		    undefined ->
			%% Never declared. A prefix must be generated.
			Prefix = new_auto_prefix(Prefixed_NS),
			NS_S = exmpp_known_nss:ns_as_list(NS),
			NS_Decl = {"xmlns:" ++ Prefix, NS_S},
			{Prefix ++ ":" ++ Name_S,
			 [NS_Decl | Converted_Attrs],
			 [{NS, Prefix} | Prefixed_NS]
			};
		    Prefix ->
			%% Use an already declared prefix.
			{Prefix ++ ":" ++ Name_S,
			 Converted_Attrs,
			 Prefixed_NS
			}
		end
	end,
    %% XXX The value is converted from binary() to list() because
    %% the old format required it. But this function is also used by
    %% document_to_iolist/1 & friends. In this context, the call to
    %% binary_to_list/1 is a waste of time.
    %% TODO: the old format is not used anymore.. fix this conversion stuff
    %% that is ugly and redundant.     
    xmlnsattributes_to_xmlattributes2(Rest, Prefixed_NS1,
				      [{New_Name, binary_to_list(Value)}
				       | Converted_Attrs1]);
xmlnsattributes_to_xmlattributes2([], Prefixed_NS, Converted_Attrs) ->
    {lists:reverse(Converted_Attrs), Prefixed_NS}.

%% Function called to convert element's children.
xmlels_to_xmlelements(undefined, _Default_NS, _Prefixed_NS) ->
    undefined;
xmlels_to_xmlelements([], _Default_NS, _Prefixed_NS) ->
    [];
xmlels_to_xmlelements(XML_Elements, Default_NS, Prefixed_NS) ->
    xmlels_to_xmlelements2(XML_Elements, [], Default_NS, Prefixed_NS).

xmlels_to_xmlelements2([XML_NS_Element | Rest], XML_Elements,
		       Default_NS, Prefixed_NS) ->
    XML_Element = xmlel_to_xmlelement(XML_NS_Element,
				      Default_NS, Prefixed_NS),
    xmlels_to_xmlelements2(Rest, [XML_Element | XML_Elements],
			   Default_NS, Prefixed_NS);
xmlels_to_xmlelements2([], XML_Elements, _Default_NS, _Prefixed_NS) ->
    lists:reverse(XML_Elements).

%% Helpers.
use_default_ns(NS, Default_NS) ->
    case Default_NS of
        [NS | _] ->
            true;
        [[X | _] = Default_NS1 | _] when is_atom(X); is_list(X) ->
            lists:member(NS, Default_NS1);
        _ ->
            false
    end.

search_in_prefixed_ns(NS, Prefixed_NS) ->
    case lists:keysearch(NS, 1, Prefixed_NS) of
        {value, {_NS, Prefix}} ->
            Prefix;
        _ ->
            case lists:keysearch(NS, 1, ?IMPLICIT_PREFIXED_NS) of
                {value, {_NS, Prefix}} ->
                    Prefix;
                _ ->
                    undefined
            end
    end.

forward_declare_ns(Curr_NS, [{undefined = NS, none} | Rest],
		   Attrs, [], Prefixed_NS) ->
    New_Default_NS = [NS],
    forward_declare_ns(Curr_NS, Rest,
		       Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(undefined = Curr_NS, Declared_NS,
		   Attrs, [], Prefixed_NS) ->
    New_Default_NS = [Curr_NS],
    forward_declare_ns(Curr_NS, Declared_NS,
		       Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(Curr_NS, [{NS, none} | Rest],
		   Attrs, Default_NS, Prefixed_NS) ->
    %% Forward-declare a default namespace.
    %% XXX This would benefit the addition of exmpp_known_*:*_as_binary/1.
    NS_S = exmpp_known_nss:ns_as_list(NS),
    NS_Decl = attribute(<<"xmlns">>, NS_S),
    New_Attrs = [NS_Decl | Attrs],
    New_Default_NS = [NS | Default_NS],
    forward_declare_ns(Curr_NS, Rest, New_Attrs, New_Default_NS, Prefixed_NS);
forward_declare_ns(Curr_NS, [{NS, Prefix} = PNS | Rest],
		   Attrs, Default_NS, Prefixed_NS) ->
    case lists:member(PNS, ?IMPLICIT_PREFIXED_NS) of
        true ->
	    %% This is an implicitly declared namespace (with the same
	    %% prefix). We do not re-declare it.
            forward_declare_ns(Curr_NS, Rest, Attrs,
			       Default_NS, Prefixed_NS);
        _ ->
	    %% Forward-declare a prefixed namespace.
	    %% XXX This would benefit the addition of
	    %% exmpp_known_*:*_as_binary/1.
            NS_S = exmpp_known_nss:ns_as_list(NS),
            NS_Decl = attribute(list_to_binary("xmlns:" ++ Prefix), NS_S),
            New_Attrs = [NS_Decl | Attrs],
            Prefixed_NS1 = [PNS | Prefixed_NS],
            forward_declare_ns(Curr_NS, Rest, New_Attrs,
			       Default_NS, Prefixed_NS1)
    end;
forward_declare_ns(Curr_NS, [], Attrs, Default_NS, Prefixed_NS) ->
    %% We finish with the current namespace of the element.
    Use_Default_NS = use_default_ns(Curr_NS, Default_NS),
    case Use_Default_NS of
        true ->
	    %% The element belongs to the current default namespace.
	    %% There's nothing to do.
            {none, Attrs, Default_NS, Prefixed_NS};
        false ->
	    %% We look for a prefixed namespace.
            case search_in_prefixed_ns(Curr_NS, Prefixed_NS) of
                undefined ->
		    %% This element uses a new namespace: it'll become
		    %% the new default one.
		    %% XXX This would benefit the addition of
		    %% exmpp_known_*:*_as_binary/1.
                    Curr_NS_S = exmpp_known_nss:ns_as_list(Curr_NS),
                    NS_Decl = attribute(<<"xmlns">>, Curr_NS_S),
                    New_Attrs = [NS_Decl | Attrs],
                    Default_NS1 = [Curr_NS | Default_NS],
                    {none, New_Attrs, Default_NS1, Prefixed_NS};
                Prefix ->
		    %% Found one: we return the corresponding prefix.
                    {Prefix, Attrs, Default_NS, Prefixed_NS}
            end
    end.

new_auto_prefix(Prefixed_NS) ->
    new_auto_prefix2(Prefixed_NS, 1).

new_auto_prefix2(Prefixed_NS, Seq) ->
    Prefix = "ns" ++ integer_to_list(Seq),
    case lists:keymember(Prefix, 2, Prefixed_NS) of
        true  -> new_auto_prefix2(Prefixed_NS, Seq + 1);
        false -> Prefix
    end.

%% @spec (XML_Element) -> XML_NS_Element
%%     XML_Element = xmlel_old() | xmlcdata()
%%     XML_NS_Element = xmlel() | xmlel_old() | xmlcdata()
%% @doc Convert an {@link xmlel_old()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.

-spec(xmlelement_to_xmlel/1 :: (xmlel_old()) -> xmlel()).

xmlelement_to_xmlel(XML_Element) ->
    xmlelement_to_xmlel(XML_Element, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_NS_Element
%%     XML_Element = xmlel_old() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlel() | xmlel_old() | xmlcdata()
%% @doc Convert an {@link xmlel_old()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlel_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.

-spec(xmlelement_to_xmlel/3 ::
      (xmlel_old(), xmldefaultnss(), xmlprefixednss()) -> xmlel()).

xmlelement_to_xmlel(XML_El, Default_NS, Prefixed_NS) ->
    {New_XML_El, _, _} = xmlelement_to_xmlel_and_nss_tables(XML_El, Default_NS,
							    Prefixed_NS),
    New_XML_El.

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> {XML_NS_Element, New_Default_NS, New_Prefixed_NS}
%%     XML_Element = xmlel_old() | xmlcdata()
%%     Default_NS = [NS]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom() | string()
%%     Prefix = string()
%%     XML_NS_Element = xmlel() | xmlel_old() | xmlcdata()
%%     New_Default_NS = [NS]
%%     New_Prefixed_NS = [{NS, Prefix}]
%% @doc Convert an {@link xmlel_old()} to an {@link xmlel()}
%% tuple.
%%
%% Other tuples are ignored.
%%
%% See {@link xmlel_to_xmlelement/3} for a description of
%% `Default_NS' and `Prefixed_NS'.
%%
%% This function will returned updated namespaces tables
%% `New_Default_NS' and `New_Prefixed_NS' which can be used for future
%% calls.

-spec(xmlelement_to_xmlel_and_nss_tables/3 ::
      (xmlel_old(), xmldefaultnss(), xmlprefixednss()) ->
	     {xmlel(), xmldefaultnss(), xmlprefixednss()}).

xmlelement_to_xmlel_and_nss_tables(
  #xmlelement{name = Name, attrs = Attrs, children = Children},
  Default_NS, Prefixed_NS) ->
    %% Udpate NS tables by looking at each attribute for NS declarations.
    %% These later are removed at the same time.
    {Declared_NS, Attrs1, Default_NS1, Prefixed_NS1} =
	update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS),
    %% Convert attributes and children to the new format.
    New_Attrs = xmlattributes_to_xmlnsattributes(Attrs1, Prefixed_NS1),
    New_Children = xmlelements_to_xmlels(Children,
					 Default_NS1, Prefixed_NS1),
    %% Check the element namespace and convert it to the new format.
    Name_S = exmpp_known_elems:elem_as_list(Name),
    XML_NS_Element =
	case string:tokens(Name_S, ":") of
	    [Prefix, Real_Name] ->
		Real_Name_A = list_to_atom(Real_Name),
		case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS1) of
		    undefined ->
			%% Namespace never declared.
			#xmlel{ns = undefined,
			       declared_ns = Declared_NS,
			       name = Real_Name_A,
			       attrs = New_Attrs,
			       children = New_Children
			      };
		    NS ->
			#xmlel{ns = NS,
			       declared_ns = Declared_NS,
			       name = Real_Name_A,
			       attrs = New_Attrs,
			       children = New_Children
			      }
		end;
	    [Real_Name] ->
		Real_Name_A = list_to_atom(Real_Name),
		case Default_NS1 of
		    [NS | _] ->
			%% Uses the current default namespace.
			#xmlel{ns = NS,
			       declared_ns = lists:delete({NS, none},
							  Declared_NS),
			       name = Real_Name_A,
			       attrs = New_Attrs,
			       children = New_Children
			      };
		    _ ->
			%% No default namespace declared.
			#xmlel{ns = undefined,
			       declared_ns = Declared_NS,
			       name = Real_Name_A,
			       attrs = New_Attrs,
			       children = New_Children
			      }
		end
	end,
    {XML_NS_Element, Default_NS1, Prefixed_NS1};
xmlelement_to_xmlel_and_nss_tables(
  #xmlendtag{name = Name}, Default_NS, Prefixed_NS) ->
    Name_S = exmpp_known_elems:elem_as_list(Name),
    XML_NS_Element =
	case string:tokens(Name_S, ":") of
	    [Prefix, Real_Name] ->
		Real_Name_A = list_to_atom(Real_Name),
		case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) of
		    undefined ->
			%% Namespace never declared.
			#xmlendtag{ns = undefined,
				   name = Real_Name_A
				  };
		    NS ->
			#xmlendtag{ns = NS,
				   name = Real_Name_A
				  }
		end;
	    [Real_Name] ->
		Real_Name_A = list_to_atom(Real_Name),
		case Default_NS of
		    [NS | _] ->
			%% Uses the current default namespace.
			#xmlendtag{ns = NS,
				   name = Real_Name_A
				  };
		    _ ->
			%% No default namespace declared.
			#xmlendtag{ns = undefined,
				   name = Real_Name_A
				  }
		end
	end,
    {XML_NS_Element, Default_NS, Prefixed_NS};
xmlelement_to_xmlel_and_nss_tables(XML_El, Default_NS, Prefixed_NS) ->
    %% xmlnslement() ot xmlcdata().
    {XML_El, Default_NS, Prefixed_NS}.

%% Function called to extract namespaces and their prefix (if any).
update_ns_from_xmlattributes(Attrs, Default_NS, Prefixed_NS) ->
    update_ns_from_xmlattributes2(Attrs, Default_NS, Prefixed_NS, [], []).

update_ns_from_xmlattributes2([{Name, Value} = Attr | Rest],
			      Default_NS, Prefixed_NS, Declared_NS,
			      Purged_Attrs) ->
    case string:tokens(Name, ":") of
        ["xmlns"] ->
	    %% Default NS declaration.
            update_ns_from_xmlattributes2(Rest,
					  [list_to_atom(Value) | Default_NS],
					  Prefixed_NS,
					  [{list_to_atom(Value), none}
					   | Declared_NS],
					  Purged_Attrs);
        ["xmlns", Prefix] ->
	    %% Prefixed NS declaration.
            update_ns_from_xmlattributes2(Rest,
					  Default_NS,
					  [{list_to_atom(Value), Prefix}
					   | Prefixed_NS],
					  [{list_to_atom(Value), Prefix}
					   | Declared_NS],
					  Purged_Attrs);
        _ ->
	    %% Irrelevant attribute.
            update_ns_from_xmlattributes2(Rest,
					  Default_NS, Prefixed_NS, Declared_NS,
					  [Attr | Purged_Attrs])
    end;
update_ns_from_xmlattributes2([], Default_NS, Prefixed_NS,
			      Declared_NS, Purged_Attrs) ->
    {Declared_NS, lists:reverse(Purged_Attrs), Default_NS, Prefixed_NS}.

%% Function called to convert element's attributes.
xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS) ->
    xmlattributes_to_xmlnsattributes(Attrs, Prefixed_NS, []).

xmlattributes_to_xmlnsattributes([{Name, Value} | Rest],
				 Prefixed_NS, Converted_Attrs) ->
    Name_S = exmpp_known_elems:elem_as_list(Name),
    New_Attr = case string:tokens(Name_S, ":") of
		   [Prefix, Real_Name] ->
		       Real_Name_A = list_to_binary(Real_Name),
		       case search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) of
			   undefined ->
			       %% Namespace never declared.
			       attribute(Real_Name_A, Value);
			   NS ->
			       attribute(NS, Real_Name_A, Value)
		       end;
		   [Real_Name] ->
		       %% Not attached to any namespace.
		       Real_Name_A = list_to_binary(Real_Name),
		       attribute(Real_Name_A, Value)
	       end,
    xmlattributes_to_xmlnsattributes(Rest, Prefixed_NS,
				     Converted_Attrs ++ [New_Attr]);
xmlattributes_to_xmlnsattributes([], _Prefixed_NS, Converted_Attrs) ->
    Converted_Attrs.

%% Function called to convert element's children.
xmlelements_to_xmlels(undefined, _Default_NS, _Prefixed_NS) ->
    undefined;
xmlelements_to_xmlels([], _Default_NS, _Prefixed_NS) ->
    [];
xmlelements_to_xmlels(XML_Elements, Default_NS, Prefixed_NS) ->
    xmlelements_to_xmlels2(XML_Elements, [],
			   Default_NS, Prefixed_NS).

xmlelements_to_xmlels2([XML_Element | Rest], XML_NS_Elements,
		       Default_NS, Prefixed_NS) ->
    XML_NS_Element = xmlelement_to_xmlel(XML_Element,
					 Default_NS, Prefixed_NS),
    xmlelements_to_xmlels2(Rest, [XML_NS_Element | XML_NS_Elements],
			   Default_NS, Prefixed_NS);
xmlelements_to_xmlels2([], XML_NS_Elements, _Default_NS, _Prefixed_NS) ->
    lists:reverse(XML_NS_Elements).

%% Helpers.
search_prefix_in_prefixed_ns(Prefix, Prefixed_NS) ->
    case lists:keysearch(Prefix, 2, Prefixed_NS) of
        {value, {NS, _Prefix}} ->
            NS;
        _ ->
            case lists:keysearch(Prefix, 2, ?IMPLICIT_PREFIXED_NS) of
                {value, {NS, _Prefix}} ->
                    NS;
                _ ->
                    undefined
            end
    end.

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = string()
%% @doc Serialize an XML node to text.
%%
%% `Default_NS' and `Prefixed_NS' contain namespace declaration which
%% occured above this node in the tree. The order in the first list is
%% important: declarations are sorted from the most recent one to the
%% oldest one.

-spec(node_to_list/3 ::
      (xmlel_any() | [xmlel_any()], xmldefaultnss(), xmlprefixednss()) -> string()).

node_to_list(El, Default_NS, Prefixed_NS) ->
    Binary = node_to_binary(El, Default_NS, Prefixed_NS),
    binary_to_list(Binary).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | list()
%%     XML_Text = string()
%% @doc Serialize an XML document to text.

-spec(document_to_list/1 :: (xmlel_any()) -> string()).

document_to_list(El) ->
    node_to_list(El, [], []).

%% @spec (El, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = binary()
%% @doc Serialize an XML node to text.
%%
%% Converting to binary is about 15% to 20% faster than converting to a
%% list.

-spec(node_to_binary/3 ::
      (xmlel_any() | [xmlel_any()]| #xmlendtag{}, xmldefaultnss(), xmlprefixednss()) -> binary()).

node_to_binary(El, Default_NS, Prefixed_NS) ->
    IO_List = node_to_iolist(El, Default_NS, Prefixed_NS),
    iolist_to_binary(IO_List).

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | list()
%%     XML_Text = binary()
%% @doc Serialize an XML document to text.
%%
%% Converting to binary is about 15% to 20% faster than converting to a
%% list.

-spec(document_to_binary/1 :: (xmlel_any()) -> binary()).

document_to_binary(El) ->
    node_to_binary(El, [], []).

%% @spec (XML_Element, Default_NS, Prefixed_NS) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | xmlendtag() | xmlcdata() | list()
%%     Default_NS = [NS | Equivalent_NSs]
%%     Prefixed_NS = [{NS, Prefix}]
%%     NS = atom()
%%     Equivalent_NSs = [NS]
%%     Prefix = string()
%%     XML_Text = iolist()
%% @doc Serialize an XML node to text.
%%
%% Converting to iolist is about 40% to 50% faster than converting to a
%% list.
%%
%% TODO: transform directly to text without converting it to the old
%%       xmlelement() first.

-spec(node_to_iolist/3 ::
      (xmlel_any() | [xmlel_any()]| #xmlendtag{}, xmldefaultnss(), xmlprefixednss()) -> iolist()).

node_to_iolist(El, Default_NS, Prefixed_NS) when is_list(El) ->
    Els = normalize_cdata_in_list(El),
    node_to_iolist2(Els, Default_NS, Prefixed_NS, []);
node_to_iolist(El, Default_NS, Prefixed_NS) ->
    node_to_iolist2([El], Default_NS, Prefixed_NS, []).

node_to_iolist2([El | Rest], Default_NS, Prefixed_NS, IO_List) ->
    Sub_IO_List =
	case El of
	    #xmlel{children = Children} ->
		{Name, Attrs, Default_NS1, Prefixed_NS1} =
		    unresolve_xmlel_nss(El, Default_NS, Prefixed_NS),
		element_to_iolist(Name, Attrs, Children,
				  Default_NS1, Prefixed_NS1);
	    #xmlelement{name = Name, attrs = Attrs, children = Children} ->
		element_to_iolist(Name, Attrs, Children,
				  Default_NS, Prefixed_NS);
	    #xmlendtag{ns = undefined, name = Name} ->
		endtag_to_iolist(Name);
	    #xmlendtag{} ->
		Name = unresolve_endtag_nss(El, Default_NS, Prefixed_NS),
		endtag_to_iolist(Name);
	    #xmlpi{target = Target, value = Value} ->
		pi_to_iolist(Target, Value);
	    #xmlcdata{cdata = CData} ->
		?ESCAPE(CData)
	end,
    node_to_iolist2(Rest, Default_NS, Prefixed_NS, [Sub_IO_List | IO_List]);
node_to_iolist2([], _Default_NS, _Prefixed_NS, IO_List) ->
    lists:reverse(IO_List).

element_to_iolist(Name, Attrs, Children, Default_NS, Prefixed_NS)
  when is_atom(Name) ->
    element_to_iolist(exmpp_known_elems:elem_as_list(Name), Attrs, Children,
		      Default_NS, Prefixed_NS);
element_to_iolist(Name, Attrs, undefined, _Default_NS, _Prefixed_NS) ->
    %% Children may come later, we don't close the tag.
    [$<, Name, attrs_to_iolist(Attrs), $>];
element_to_iolist(Name, Attrs, [], _Default_NS, _Prefixed_NS) ->
    [$<, Name, attrs_to_iolist(Attrs), $/, $>];
element_to_iolist(Name, Attrs, Children, Default_NS, Prefixed_NS) ->
    Content = node_to_iolist(Children, Default_NS, Prefixed_NS),
    [$<, Name, attrs_to_iolist(Attrs), $>, Content, $<, $/, Name, $>].

endtag_to_iolist(Name) when is_atom(Name) ->
    endtag_to_iolist(exmpp_known_elems:elem_as_list(Name));
endtag_to_iolist(Name) ->
    [$<, $/, Name, $>].

pi_to_iolist(Target, Value) when is_atom(Target) ->
    pi_to_iolist(atom_to_list(Target), Value);
pi_to_iolist(Target, Value) ->
    [$<, $?, Target, $\s, Value, $?, $>].

attrs_to_iolist(Attrs) ->
    [attr_to_iolist(A) || A <- Attrs].

attr_to_iolist({Name, Value}) ->
    [$\s, Name, $=, $", escape_attr_using_entities(Value), $"].

%% @spec (XML_Element) -> XML_Text
%%     XML_Element = xmlel() | xmlel_old() | list()
%%     XML_Text = iolist()
%% @doc Serialize an XML document to text.

-spec(document_to_iolist/1 :: (xmlel_any()) -> iolist()).

document_to_iolist(El) ->
    node_to_iolist(El, [], []).

%% @spec (XML_Element) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Recursively remove text nodes containing only whitespaces.
%%
%% @see is_whitespace/1.

-spec(deindent_document/1 :: (xmlel_any()) -> xmlel_any()).

deindent_document(#xmlel{children = Children} = El) ->
    New_Children = deindent_children(remove_whitespaces_from_list(Children)),
    El#xmlel{children = New_Children};
deindent_document(#xmlelement{children = Children} = El) ->
    New_Children = deindent_children(remove_whitespaces_from_list(Children)),
    El#xmlelement{children = New_Children}.

deindent_children(undefined) ->
    undefined;
deindent_children(Children) ->
    deindent_children2(Children, []).

deindent_children2([], Result) ->
    lists:reverse(Result);
deindent_children2([#xmlcdata{cdata = CData} | Rest], Result) ->
    New_Child = cdata(exmpp_utils:strip(CData)),
    deindent_children2(Rest, [New_Child | Result]);
deindent_children2([Child | Rest], Result)
  when is_record(Child, xmlel); is_record(Child, xmlelement) ->
    New_Child = deindent_document(Child),
    deindent_children2(Rest, [New_Child | Result]).

%% @spec (XML_Element, Indent) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Indent = binary()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Add whitespaces text nodes to indent the document.
%%
%% Indentation of {@link xmlendtag()} isn't supported yet.

-spec(indent_document/2 :: (xmlel_any(), binary()) -> xmlel_any()).

indent_document(El, Indent) ->
    indent_document(El, Indent, <<>>).

%% @spec (XML_Element, Indent, Previous_Total) -> New_XML_Element
%%     XML_Element = xmlel() | xmlel_old()
%%     Indent = binary()
%%     Previous_Total = binary()
%%     New_XML_Element = xmlel() | xmlel_old()
%% @doc Add whitespaces text nodes to indent the document.
%%
%% Indentation of {@link xmlendtag()} isn't supported yet.

-spec(indent_document/3 :: (xmlel_any(), binary(), binary()) -> xmlel_any()).

indent_document(El, Indent, Previous_Total) ->
    %% First, we remove previous indentation.
    New_El = deindent_document(El),
    indent_document2(New_El, Indent, Previous_Total).

indent_document2(#xmlel{children = Children} = El,
		 Indent, Previous_Total) ->
    New_Children = indent_children(Children, Indent, Previous_Total),
    El#xmlel{children = New_Children};
indent_document2(#xmlelement{children = Children} = El,
		 Indent, Previous_Total) ->
    New_Children = indent_children(Children, Indent, Previous_Total),
    El#xmlelement{children = New_Children}.

indent_children(undefined, _Indent, _Previous_Total) ->
    undefined;
indent_children(Children, Indent, Previous_Total) ->
    New_Previous_Total = list_to_binary([Previous_Total, Indent]),
    Before = cdata(list_to_binary([<<"\n">>, New_Previous_Total])),
    End = cdata(list_to_binary([<<"\n">>, Previous_Total])),
    indent_children2(Children, Indent, New_Previous_Total, Before, End, []).

indent_children2([], _Indent, _Previous_Total, _Before, _End, []) ->
    [];
indent_children2([#xmlcdata{cdata = CData}], _Indent, _Previous_Total,
		 _Before, _End, []) ->
    [cdata(exmpp_utils:strip(CData))];
indent_children2([], _Indent, _Previous_Total, _Before, End, Result) ->
    lists:reverse([End | Result]);
indent_children2([#xmlcdata{cdata = CData} | Rest], Indent, Previous_Total,
		 Before, End, Result) ->
    New_Child = cdata(exmpp_utils:strip(CData)),
    New_Result = [New_Child, Before | Result],
    indent_children2(Rest, Indent, Previous_Total, Before, End, New_Result);
indent_children2([Child | Rest], Indent, Previous_Total, Before, End, Result)
  when is_record(Child, xmlel); is_record(Child, xmlelement) ->
    New_Child = indent_document2(Child, Indent, Previous_Total),
    New_Result = [New_Child, Before | Result],
    indent_children2(Rest, Indent, Previous_Total, Before, End, New_Result).

%% @spec (XML_Elements) -> Cleaned_XML_Elements
%%     XML_Elements = [xmlel() | xmlel_old() | xmlcdata() |
%%         xmlendtag()]
%%     Cleaned_XML_Elements = [xmlel() | xmlel_old() | xmlcdata()]
%% @doc Remove any {@link xmlendtag()}
%% from the list of XML elements.
%%
%% This is primarily designed to work on returned value of {@link
%% parse/2} and {@link parse_final/2} when the `no_endtag' parser
%% option (see {@link xmlparseroption()}) wasn't specified at {@link
%% start_parser/1} time.

-spec(clear_endtag_tuples/1 :: ([xmlnode() | xmlendtag()]) -> [xmlnode()]).

clear_endtag_tuples(XML_Elements) ->
    clear_endtag_tuples2(XML_Elements, []).

clear_endtag_tuples2([#xmlendtag{} | Rest], Result) ->
    clear_endtag_tuples2(Rest, Result);
clear_endtag_tuples2([XML_Element | Rest], Result) ->
    clear_endtag_tuples2(Rest, [XML_Element | Result]);
clear_endtag_tuples2([], Result) ->
    lists:reverse(Result).

%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @doc Replace sensible characters with entities.
%%
%% Processed characters are <tt>&amp;</tt>, <tt>&lt;</tt>,
%% <tt>&gt;</tt>, <tt>&quot;</tt>, <tt>&apos;</tt>.

-spec(escape_using_entities/1 :: (binary() | string()) -> binary() | string()).

escape_using_entities(CData) when is_list(CData) ->
    lists:flatten([case C of
		       $& -> "&amp;";
		       $< -> "&lt;";
		       $> -> "&gt;";
		       $" -> "&quot;";
		       $' -> "&apos;";
		       _  -> C
		   end || C <- CData]);

escape_using_entities(CData) when is_binary(CData) ->
    escape_using_entities2(CData, []).

escape_using_entities2(<<C:8, Rest/binary>>, New_CData) ->
    New_C = case C of
		$& -> <<"&amp;">>;
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$" -> <<"&quot;">>; % "
		$' -> <<"&apos;">>; % '
		_  -> C
	    end,
    escape_using_entities2(Rest, [New_C | New_CData]);
escape_using_entities2(<<>>, New_CData) ->
    list_to_binary(lists:reverse(New_CData)).



-spec(escape_attr_using_entities/1 :: (binary() | string()) -> binary() | string()).

escape_attr_using_entities(CData) when is_list(CData) ->
    lists:flatten([case C of
		       $& -> "&amp;";
		       $< -> "&lt;";
		       $> -> "&gt;";
		       $" -> "&quot;";
		       $' -> "&apos;";
            	       $\n  -> "&#xA;";
            	       $\t  -> "&#x9;";
            	       $\r  -> "&#xD;";
		       _  -> C
		   end || C <- CData]);

escape_attr_using_entities(CData) when is_binary(CData) ->
    escape_attr_using_entities2(CData, []).

escape_attr_using_entities2(<<C:8, Rest/binary>>, New_CData) ->
    New_C = case C of
		$& -> <<"&amp;">>;
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$" -> <<"&quot;">>; % "
		$' -> <<"&apos;">>; % '
	       $\n  -> <<"&#xA;">>;
	       $\t  -> <<"&#x9;">>;
	       $\r  -> <<"&#xD;">>;
		_  -> C
	    end,
    escape_attr_using_entities2(Rest, [New_C | New_CData]);
escape_attr_using_entities2(<<>>, New_CData) ->
    list_to_binary(lists:reverse(New_CData)).


%% @spec (CData) -> Escaped_CData
%%     CData = string() | binary()
%%     Escaped_CData = string() | binary()
%% @doc Escape text using CDATA sections.

-spec(escape_using_cdata/1 :: (binary() | string()) -> binary() | string()).

escape_using_cdata(CData) when is_list(CData) ->
    escape_using_cdata_list(CData, false, []);
escape_using_cdata(CData) when is_binary(CData) ->
    case cdata_need_escape(CData) of
        no ->
            CData;
        global ->
            list_to_binary([<<"<![CDATA[">>, CData, <<"]]>">>]);
        {split, End_Token_Positions} ->
            Escaped = escape_using_cdata_binary(CData, End_Token_Positions),
            list_to_binary(Escaped)
    end.

%% If a text node contains the characters '<' or '&', it must be enclosed
%% inside CDATA sections. If such a text also contains CDATA end token
%% ("]]>"), it must be split in multiple CDATA sections.
%%
%% See:
%%   http://www.w3.org/TR/xml11/#syntax
%%   http://en.wikipedia.org/wiki/CDATA#Uses_of_CDATA_sections
%%
%% For binary(), we do it in two steps (first, is it needed, then do it).
%% This is because in most cases, the text node won't have CDATA end token.
%% XXX Should we do the same for lists?

escape_using_cdata_list([], false, Escaped) ->
    lists:reverse(Escaped);
escape_using_cdata_list([], true, Escaped) ->
    "<![CDATA[" ++ lists:reverse(lists:flatten(Escaped)) ++ "]]>";
escape_using_cdata_list([$], $], $> | Rest], _Must_Escape, Escaped) ->
    escape_using_cdata_list(Rest, true, [">[ATADC[!<>]]]]" | Escaped]);
escape_using_cdata_list([C | Rest], _Must_Escape, Escaped)
  when C == $<; C == $& ->
    escape_using_cdata_list(Rest, true, [C | Escaped]);
escape_using_cdata_list([C | Rest], Must_Escape, Escaped) ->
    escape_using_cdata_list(Rest, Must_Escape, [C | Escaped]).

%% This function returns what kind of escape must be done:
%%   . 'no'
%%   . 'global' for text containing '<' and '&'
%%   . {'split, End_Token_Pos} for text containing CDATA end token(s)

cdata_need_escape(CData) ->
    cdata_need_escape2(CData, 0, false, []).

cdata_need_escape2(<<>>, _Current_Pos, false, _End_Token_Pos) ->
    no;
cdata_need_escape2(<<>>, _Current_Pos, true, []) ->
    global;
cdata_need_escape2(<<>>, _Current_Pos, true, End_Token_Pos) ->
    {split, lists:reverse(End_Token_Pos)};
cdata_need_escape2(<<$], $], $>, Rest/binary>>, Current_Pos, _Must_Escape,
		   End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 3, true,
		       [Current_Pos + 1 | End_Token_Pos]);
cdata_need_escape2(<<$<, Rest/binary>>, Current_Pos, _Must_Escape,
		   End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, true, End_Token_Pos);
cdata_need_escape2(<<$&, Rest/binary>>, Current_Pos, _Must_Escape,
		   End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, true, End_Token_Pos);
cdata_need_escape2(<<_:8, Rest/binary>>, Current_Pos, Must_Escape,
		   End_Token_Pos) ->
    cdata_need_escape2(Rest, Current_Pos + 1, Must_Escape, End_Token_Pos).

%% This function use the End_Token_Pos list returned by
%% cdata_need_escape/1 and split CDATA end tokens at those positions.

escape_using_cdata_binary(CData, End_Token_Pos) ->
    escape_using_cdata_binary2(CData, 0, End_Token_Pos, []).

escape_using_cdata_binary2(Rest, _Current_Pos, [], Escaped) ->
    lists:reverse([<<"]]>">>, Rest, <<"<![CDATA[">> | Escaped]);
escape_using_cdata_binary2(CData, Current_Pos, [Pos | End_Token_Pos],
			   Escaped) ->
    Split = Pos - Current_Pos,
    {CData1, CData2} = split_binary(CData, Split + 1),
    escape_using_cdata_binary2(CData2, Pos + 1, End_Token_Pos,
			       [<<"]]>">>, CData1, <<"<![CDATA[">> | Escaped]).

%% @spec () -> escape_using_entities | escape_using_cdata
%% @doc Tell what escaping function will be used internally.

-spec(internal_escaping_function_name/0 ::
      () -> escape_using_cdata | escape_using_entities).

-ifdef(ESCAPE_USING_CDATA_SECTIONS).
internal_escaping_function_name() ->
    escape_using_cdata.
-else.
internal_escaping_function_name() ->
    escape_using_entities.
-endif.

%% --------------------------------------------------------------------
%% Utilities.
%% --------------------------------------------------------------------

%% Choose the most appropriate engine.
get_engine_from_options(Options) ->
    Engine_Name =
	case proplists:get_value(engine, Options) of
	    undefined ->
		case is_engine_available(?DEFAULT_ENGINE) of
		    true ->
			?DEFAULT_ENGINE;
		    false ->
			case ets:first(?ENGINES_REGISTRY) of
			    '$end_of_table' ->
				throw({xml_parser, options,
				       no_engine_registered,
				       undefined});
			    Name ->
				Name
			end
		end;
	    Name ->
		case is_engine_available(Name) of
		    true ->
			Name;
		    false ->
			throw({xml_parser, options, engine_unavailable, Name})
		end
	end,
    get_engine_driver(Engine_Name).

%% Merge options to avoid duplicates and multiple initialization of the
%% parser.
merge_options(Options, [{Key, _} = Option | Rest]) ->
    New_Options = lists:keystore(Key, 1, Options, Option),
    merge_options(New_Options, Rest);
merge_options(Options, [Option | Rest]) when is_atom(Option) ->
    merge_options(Options, [{Option, true} | Rest]);
merge_options(Options, []) ->
    Options.

%% Update parser options.
handle_options(#xml_parser{options = Options, port = Port} = Parser,
	       [{Key, _} = Option | Rest]) ->
    case set_option(Port, Option) of
        ok ->
            New_Options = lists:keystore(Key, 1, Options, Option),
            New_Parser = Parser#xml_parser{options = New_Options},
            handle_options(New_Parser, Rest);
        Error ->
            Error
    end;
handle_options(_Parser, [Invalid_Option | _Rest]) ->
    {error, invalid, Invalid_Option};
handle_options(Parser, []) ->
    Parser.

set_option(_Port, {engine, Engine_Name}) when is_atom(Engine_Name) ->
    ok;

set_option(Port, {max_size, infinity}) ->
    port_control(Port, ?COMMAND_SET_MAX_SIZE, term_to_binary(-1)),
    ok;
set_option(Port, {max_size, Max}) when is_integer(Max), Max >= 0 ->
    port_control(Port, ?COMMAND_SET_MAX_SIZE, term_to_binary(Max)),
    ok;

set_option(Port, {root_depth, none}) ->
    port_control(Port, ?COMMAND_SET_ROOT_DEPTH, term_to_binary(-1)),
    ok;
set_option(Port, {root_depth, Depth}) when is_integer(Depth), Depth >= 0 ->
    port_control(Port, ?COMMAND_SET_ROOT_DEPTH, term_to_binary(Depth)),
    ok;

set_option(Port, {names_as_atom, As_Atom}) when is_boolean(As_Atom) ->
    port_control(Port, ?COMMAND_SET_NAMES_AS_ATOM, term_to_binary(As_Atom)),
    ok;

set_option(Port, {check_nss, Check}) when is_atom(Check) ->
    engine_set_things_check(Port, nss, Check);

set_option(Port, {check_elems, Check}) when is_atom(Check) ->
    engine_set_things_check(Port, names, Check);

set_option(Port, {emit_endtag, Endtag}) when is_boolean(Endtag) ->
    port_control(Port, ?COMMAND_SET_EMIT_ENDTAG, term_to_binary(Endtag)),
    ok;

set_option(_Port, Invalid_Option) ->
    {error, invalid, Invalid_Option}.

%% --------------------------------------------------------------------
%% Engine function wrappers.
%% --------------------------------------------------------------------

control(Port, Command, Data) ->
    case port_control(Port, Command, Data) of
        <<0, Result/binary>> ->
            Result;
        <<1, Error/binary>> ->
            {error, binary_to_term(Error)};
        Other ->
	    %% The port driver must have a bug somewhere: this return
	    %% value is illegal.
            {_, Driver_Name} = erlang:port_info(Port, name),
            Term = try
		       binary_to_term(Other)
		   catch
		       _:_ ->
			   Other
		   end,
            error_logger:error_msg(
              "exmpp_xml: Unexpected return value from '~s':~n~p~n",
              [Driver_Name, Term]),
            {error, {unexpected, Term}}
    end.

engine_add_known(Port, Type, List_Name, New_Items) ->
    Command = case Type of
		  nss   -> ?COMMAND_ADD_KNOWN_NSS;
		  names -> ?COMMAND_ADD_KNOWN_ELEMS
	      end,
    control(Port, Command, term_to_binary({List_Name, New_Items})).

engine_set_things_check(Port, Type, List_Name) ->
    Command = case Type of
		  nss   -> ?COMMAND_SET_CHECK_NSS;
		  names -> ?COMMAND_SET_CHECK_ELEMS
	      end,
    case control(Port, Command, term_to_binary(List_Name)) of
        {error, Reason} -> {error, failure, Reason};
        _               -> ok
    end.

engine_parse(Port, Data) ->
    engine_parse2(Port, ?COMMAND_PARSE, Data).

engine_parse_final(Port, Data) ->
    engine_parse2(Port, ?COMMAND_PARSE_FINAL, Data).

engine_parse2(Port, Command, Data) ->
    case control(Port, Command, Data) of
        {error, Error} ->
            case Error of
                {Reason, Details} ->
                    throw({xml_parser, parsing, Reason, Details});
                Reason ->
                    throw({xml_parser, parsing, Reason, undefined})
            end;
        Result ->
            binary_to_term(Result)
    end.

engine_port_revision(Port) ->
    case control(Port, ?COMMAND_PORT_REVISION, <<>>) of
        {error, Reason} ->
            throw({xml_parser, port_revision, port_revision, Reason});
        Revision ->
            binary_to_term(Revision)
    end.

%% --------------------------------------------------------------------
%% gen_server(3erl) callbacks.
%% --------------------------------------------------------------------

%% @hidden

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?ENGINES_REGISTRY, [named_table, {keypos, 2}]),
    {ok, #state{
       known_nss_lists = dict:new(),
       known_elems_lists = dict:new()
      }}.

%% @hidden

handle_call({register_engine,
	     #xml_engine{driver_path = Driver_Path,
			 driver = Driver_Name} = Engine},
	    _From,
	    #state{known_nss_lists = Known_NSs,
		   known_elems_lists = Known_Names} = State) ->
    try
	%% Load the driver now.
        case Driver_Path of
            undefined ->
                exmpp_internals:load_driver(Driver_Name);
            _ ->
                exmpp_internals:load_driver(Driver_Name, [Driver_Path])
        end,
        try
	    %% Start a port for management purpose.
            Port = exmpp_internals:open_port(Driver_Name),
	    %% Send him the known lists.
            Fun1 = fun(List_Name, List, {P, Type, Acc}) ->
			   Items = dict:fetch_keys(List),
			   Ret = engine_add_known(P, Type, List_Name, Items),
			   case Acc of
			       {error, _Reason} -> {P, Type, Acc};
			       _                -> {P, Type, Ret}
			   end
		   end,
            case dict:fold(Fun1, {Port, nss, ok}, Known_NSs) of
                {_Port1, {error, Reason1}} ->
                    exmpp_internals:close_port(Port),
                    throw(Reason1);
                _ ->
                    ok
            end,
            case dict:fold(Fun1, {Port, names, ok}, Known_Names) of
                {_Port2, {error, Reason2}} ->
                    exmpp_internals:close_port(Port),
                    throw(Reason2);
                _ ->
                    ok
            end,
	    %% Add engine to the global list.
            Engine1 = Engine#xml_engine{port = Port},
            try
                ets:insert(?ENGINES_REGISTRY, Engine1)
            catch
                _:Exception2 ->
                    exmpp_internals:close_port(Port),
                    throw(Exception2)
            end,
            {reply, ok, State}
        catch
            _:Exception1 ->
                exmpp_internals:unload_driver(Driver_Name),
                {reply, {error, Exception1}, State}
        end
    catch
        _:Exception ->
            {reply, {error, Exception}, State}
    end;

handle_call({add_known, Type, List_Name, New_Items}, _From, State) ->
    Lists = case Type of
		nss   -> State#state.known_nss_lists;
		names -> State#state.known_elems_lists
	    end,
    %% Give the new items to all drivers.
    Fun1 = fun(#xml_engine{port = Port}, Acc) ->
		   Ret = engine_add_known(Port, Type, List_Name, New_Items),
		   case Acc of
		       {error, _Reason} -> Acc;
		       _                -> Ret
		   end
	   end,
    Result = ets:foldl(Fun1, ok, ?ENGINES_REGISTRY),
    case Result of
        {error, Reason} ->
            {reply,
	     {error,
	      {xml_parser, add_known, Type, {List_Name, New_Items, Reason}}},
	     State};
        _ ->
	    %% Add new items to the list.
            List = case dict:is_key(List_Name, Lists) of
		       true  -> dict:fetch(List_Name, Lists);
		       false -> dict:new()
		   end,
            Fun2 = fun(Item, Acc) -> dict:store(Item, true, Acc) end,
            New_List = lists:foldl(Fun2, List, New_Items),
            New_Lists = dict:store(List_Name, New_List, Lists),
	    %% Update the state.
            New_State = case Type of
			    nss   -> State#state{known_nss_lists = New_Lists};
			    names -> State#state{known_elems_lists = New_Lists}
			end,
            {reply, ok, New_State}
    end;

handle_call(Request, From, State) ->
    error_logger:info_msg("~p:handle_call/3:~n- Request: ~p~n- From: ~p~n"
			  "- State: ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

%% @hidden

handle_cast(Request, State) ->
    error_logger:info_msg("~p:handle_cast/2:~n- Request: ~p~n"
			  "- State: ~p~n", [?MODULE, Request, State]),
    {noreply, State}.

%% @hidden

handle_info(Info, State) ->
    error_logger:info_msg("~p:handle_info/2:~n- Info: ~p~n"
			  "- State: ~p~n", [?MODULE, Info, State]),
    {noreply, State}.

%% @hidden

code_change(Old_Vsn, State, Extra) ->
    error_logger:info_msg("~p:code_change/3:~n- Old_Vsn: ~p~n- Extra: ~p~n"
			  "- State: ~p~n", [?MODULE, Old_Vsn, Extra, State]),
    {ok, State}.

%% @hidden

terminate(_Reason, _State) ->
    ok.
