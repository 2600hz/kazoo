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
%% The module <strong>{@module}</strong> provides functions to handle
%% JID.

-module(exmpp_jid).

-include("exmpp.hrl").
-include("exmpp_jid.hrl").

-export([binary_split/2]).

%% Conversion.
-export([
	 make/0,
	 make/1,
	 make/2,
	 make/3,
	 bare/1,
	 full/2
	]).

%% Parsing.
-export([
	 parse/1
	]).

%% Serialization.
-export([
     to_lower/1,
	 to_list/1,
	 to_list/2,
	 to_list/3,
	 prep_to_list/1,
	 bare_to_list/1,
	 bare_to_list/2,
	 prep_bare_to_list/1,
	 to_binary/1,
	 to_binary/2,
	 to_binary/3,
	 prep_to_binary/1,
	 bare_to_binary/1,
	 bare_to_binary/2,
	 prep_bare_to_binary/1
	]).

%% Comparison.
-export([
	 full_compare/2,
	 bare_compare/2,
	 compare/2,
	 compare_domains/2
	]).

%% Checks.
-export([
	 is_jid/1
	]).

%% List accessors.
-export([
	 node_as_list/1,
	 prep_node_as_list/1,
	 domain_as_list/1,
	 prep_domain_as_list/1,
	 resource_as_list/1,
	 prep_resource_as_list/1
	]).

%% Raw binary() accessors.
-export([
	 node/1,
	 prep_node/1,
	 domain/1,
	 prep_domain/1,
	 resource/1,
	 prep_resource/1
	]).

%% --------------------------------------------------------------------
%% Constants.
%% --------------------------------------------------------------------

-define(NODE_MAX_LENGTH,     1023).
-define(DOMAIN_MAX_LENGTH,   1023).
-define(RESOURCE_MAX_LENGTH, 1023).
-define(BARE_JID_MAX_LENGTH, ?NODE_MAX_LENGTH + 1 + ?DOMAIN_MAX_LENGTH).
-define(JID_MAX_LENGTH,      ?BARE_JID_MAX_LENGTH + 1 + ?RESOURCE_MAX_LENGTH).

%% --------------------------------------------------------------------
%% Documentation / type definitions.
%% --------------------------------------------------------------------

%% @type jid() = {jid, Orig_Jid, Prepd_Node, Prepd_Domain, Prepd_Resource}
%%     Orig_Jid = binary() | undefined
%%     Prepd_Node = binary() | undefined
%%     Prepd_Domain = binary() | undefined
%%     Prepd_Resource = binary() | undefined.
%% Represents a JID.
%%
%% <strong>`jid()' is an internal type and the structure documented
%% herein may be changed without notice</strong>. Please use only the
%% accessors exported by this module to get each component of a JID.
%%
%% `Prepd_Node' is set to the value of `Node' passed through the
%% NODEPREP stringprep profile.
%%
%% `Prepd_Domain' is set to the value of `Domain' passed through the
%% NAMEPREP stringprep profile.
%%
%% `Prepd_Resource' is set to the value of `Resource' passed through the
%% RESOURCEPREP stringprep profile.

-type(jid() :: #jid{}).

%% Internal type only used in contracts.
-type(node_arg()   :: binary() | string() | undefined).
-type(domain_arg() :: binary() | string()).
-type(res_arg()    :: binary() | string() | undefined).

%% --------------------------------------------------------------------
%% JID creation & conversion.
%% --------------------------------------------------------------------

%% @spec (Jid::jid()) -> {Node::binary() | undefined, Domain::binary() | undefined, Resource::binary() | undefined}
to_lower(Jid) when ?IS_JID(Jid)->
    {prep_node(Jid),
     prep_domain(Jid),
     prep_resource(Jid)};
to_lower(StringJid) when is_list(StringJid)->
    to_lower(parse(StringJid));
to_lower(StringJid) when is_binary(StringJid)->
    to_lower(parse(StringJid)).


%% @spec () -> Jid
%%     Jid = jid()
%% @doc Create a blank JID.

-spec(make/0 :: () -> jid()).

make() ->
    #jid{}.

%% @spec (Domain) -> Bare_Jid
%%     Domain = binary() | string()
%%     Bare_Jid = jid()
%% @throws {jid, make, too_long, {domain, Domain}} |
%%         {jid, make, invalid,  {domain, Domain}}
%% @doc Create a bare JID.

-spec(make/1 :: (domain_arg() | {node_arg(), domain_arg(), res_arg()}) -> jid()).
make({Node, Domain, Resource})->
    make(Node, Domain, Resource);
make(Domain) ->
    make(undefined, Domain).

%% @spec (Node, Domain) -> Bare_Jid
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Bare_Jid = jid()
%% @throws {jid, make, too_long, {domain, Domain}} |
%%         {jid, make, invalid,  {domain, Domain}} |
%%         {jid, make, too_long, {node, Node}} |
%%         {jid, make, invalid,  {node, Node}}
%% @doc Create a bare JID.

-spec(make/2 :: (node_arg(), domain_arg()) -> jid()).

make(_Node, Domain)
  when is_list(Domain), length(Domain) > ?DOMAIN_MAX_LENGTH ->
    throw({jid, make, too_long, {domain, Domain}});
make(_Node, Domain)
  when is_binary(Domain), size(Domain) > ?DOMAIN_MAX_LENGTH ->
    throw({jid, make, too_long, {domain, Domain}});
make("", Domain) ->
    %% This clause is here because ejabberd uses empty string.
    make(undefined, Domain);
make(<<>>, Domain) ->
    %% This clause is here because ejabberd uses empty string.
    make(undefined, Domain);
make(undefined, Domain) ->
    try
        LDomain = exmpp_stringprep:nameprep(Domain),
        #jid{raw = storbi2bi(Domain),
	     node = undefined,
	     domain = storbi2bi(LDomain),
	     resource = undefined
	    }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
	  throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}})
    end;
make(Node, _Domain)
  when is_list(Node), length(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, too_long, {node, Node}});
make(Node, _Domain)
  when is_binary(Node), size(Node) > ?NODE_MAX_LENGTH ->
    throw({jid, make, too_long, {node, Node}});
make(Node, Domain) ->
    try
        LNode = exmpp_stringprep:nodeprep(Node),
        LDomain = exmpp_stringprep:nameprep(Domain),
        #jid{raw =
	     <<(storbi2bi(Node))/binary, $@, (storbi2bi(Domain))/binary >>,
	     node = storbi2bi(LNode),
	     domain = storbi2bi(LDomain),
	     resource = undefined
	    }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
	  throw:{stringprep, nodeprep, invalid_string, _} ->
            throw({jid, make, invalid, {node, Node}});
	  throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}})
    end.

%% @spec (Node, Domain, Resource) -> Jid
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | random | undefined
%%     Jid = jid()
%% @doc Create a full JID.

-spec(make/3 :: (node_arg(), domain_arg(), res_arg()) -> jid()).

make(Node, Domain, undefined) ->
    make(Node, Domain);
make(Node, Domain, "") ->
    %% This clause is here because ejabberd uses empty string.
    make(Node, Domain);
make(Node, Domain, <<>>) ->
    %% This clause is here because ejabberd uses empty string.
    make(Node, Domain);
make(Node, Domain, Resource) ->
    Jid = make(Node, Domain),
    try
        full(Jid, Resource)
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
	  throw:{jid, convert, Reason, Infos} ->
            throw({jid, make, Reason, Infos})
    end.

%% @spec (Orig, Node, Domain, Resource) -> Jid
%%     Orig = binary()
%%     Node = binary() | string() | undefined
%%     Resource = binary() | string() | undefined
%%     Jid = jid()
%% @doc Create a full JID.
%%
%% The first parameter is the original JID `Node@Domain/Resource' (eg.
%% as extracted from a stanza recipient).
%%
%% We reuse this value here. The intention is to save some memory, see
%% comments on `include/internal/exmpp_xmpp.hrl'

-spec(make/4 :: (binary(), node_arg(), domain_arg(), res_arg()) -> jid()).

make(Orig, Node, Domain, Resource) ->
    try
        LNode = case Node of
		    undefined -> undefined;
		    _         -> storbi2bi(exmpp_stringprep:nodeprep(Node))
		end,
        LDomain = storbi2bi(exmpp_stringprep:nameprep(Domain)),
        LResource = case Resource of
			undefined -> undefined;
			_         -> storbi2bi(exmpp_stringprep:resourceprep(Resource))
		    end,
        #jid{raw = Orig,
	     node = LNode,
	     domain = LDomain,
	     resource = LResource
	    }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
	  throw:{stringprep, nodeprep, invalid_string, _} ->
            throw({jid, make, invalid, {node, Node}});
	  throw:{stringprep, nameprep, invalid_string, _} ->
            throw({jid, make, invalid, {domain, Domain}});
	  throw:{stringprep, resourceprep, invalid_string, _} ->
            throw({jid, make, invalid, {resource, Resource}})
    end.

%% @spec (Jid) -> Bare_Jid
%%     Jid = jid()
%%     Bare_Jid = jid()
%% @doc Convert a full JID to its bare version.

-spec(bare/1 :: (jid()) -> jid()).

bare(#jid{raw = Orig_Jid} = Jid) ->
    New_Orig_Jid = case binary_split(Orig_Jid, $/) of
               [Bare_Jid]    -> Bare_Jid;
		       [Bare_Jid | _Other] -> Bare_Jid
		   end,
    Jid#jid{
      raw = New_Orig_Jid,
      resource = undefined
     }.

%% @spec (Bare_Jid, Resource) -> Jid
%%     Bare_Jid = jid()
%%     Resource = binary() | string() | random
%%     Jid = jid()
%% @throws {jid, convert, too_long, {resource, Resource}} |
%%         {jid, convert, invalid,  {resource, Resource}}
%% @doc Convert a bare JID to its full version.

-spec(full/2 :: (jid(), res_arg()) -> jid()).

full(Jid, undefined) ->
    Jid;
full(Jid, "") ->
    %% This clause is here because ejabberd uses empty string.
    Jid;
full(Jid, <<>>) ->
    %% This clause is here because ejabberd uses empty string.
    Jid;
full(Jid, random) ->
    Resource = generate_resource(),
    full(Jid, Resource);
full(_Jid, Resource)
  when is_list(Resource), length(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, too_long, {resource, Resource}});
full(_Jid, Resource)
  when is_binary(Resource), size(Resource) > ?RESOURCE_MAX_LENGTH ->
    throw({jid, convert, too_long, {resource, Resource}});
full(#jid{raw = Orig_Jid} = Jid, Resource) ->
    try
        LResource = exmpp_stringprep:resourceprep(Resource),
        Resource_B = storbi2bi(Resource),
        New_Orig_Jid =
	case binary_split(Orig_Jid, $/) of
	    [Bare_Jid, _] -> <<Bare_Jid/binary, $/, Resource_B/binary>>;
	    [Bare_Jid]    -> <<Bare_Jid/binary, $/, Resource_B/binary>>
				 end,
        Jid#jid{raw = New_Orig_Jid,
		    resource = storbi2bi(LResource)
	       }
    catch
        throw:{stringprep, _, exmpp_not_started, _} = E ->
            throw(E);
	  throw:{stringprep, resourceprep, invalid_string, _} ->
            throw({jid, convert, invalid, {resource, Resource}})
    end.

%% --------------------------------------------------------------------
%% JID parsing.
%% --------------------------------------------------------------------

%% @spec (String) -> Jid
%%     String = binary() | string()
%%     Jid = jid()
%% @throws {jid, parse, Reason, {jid, String}}
%% @doc Parse a string and create a full JID.

-spec(parse/1 :: (binary() | string()) -> jid()).

parse(String) when is_binary(String) ->
    case parse_binary(String, String, <<>>) of
        {error, Reason} ->
            throw({jid, parse, Reason, {jid, String}});
        Jid ->
            Jid
    end;
parse(String) when is_list(String) ->
    B = list_to_binary(String),
    case parse_binary(B, B, <<>>) of
        {error, Reason} ->
            throw({jid, parse, Reason, {jid, String}});
        Jid ->
            Jid
    end.

-spec(parse_binary/3 :: (binary(), binary(), binary()) -> jid() | {error, any()}).

parse_binary(_Original, String, _) when size(String) > ?JID_MAX_LENGTH ->
    %% Invalid JID: too long.
    {error, too_long};
parse_binary(_Original, <<$@, _Rest/binary>>, <<>>) ->
    %% Invalid JID of the form "@Domain".
    {error, unexpected_node_separator};
parse_binary(Original, <<$@, Rest/binary>>, Node) ->
    %% JID of the form "Node@Domain".
    parse_binary(Original, Rest, Node, <<>>);
parse_binary(_Original, <<$/, _Rest/binary>>, <<>>) ->
    %% Invalid JID of the form "/Resource".
    {error, unexpected_resource_separator};
parse_binary(_Original, <<$/>>, _Domain) ->
    %% Invalid JID of the form "Domain/".
    {error, unexpected_end_of_string};
parse_binary(Original, <<$/ , Resource/binary>>, Domain) ->
    %% Valid JID of the form "Domain/Resource".
    make(Original, undefined, Domain, Resource);
parse_binary(Original, <<C, Rest/binary>>, Node_Or_Domain) ->
    %% JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_binary(Original, Rest, <<Node_Or_Domain/binary, C>>);
parse_binary(_Original, <<>>, <<>>) ->
    %% Invalid JID of the form "".
    {error, unexpected_end_of_string};
parse_binary(Original, <<>>, Domain) ->
    %% Valid JID of the form "Domain".
    make(Original, undefined, Domain, undefined).
parse_binary(_Original, <<$@,  _Rest/binary>>, _Node, _Domain) ->
    %% Invalid JID of the form "Node@Domain@Domain".
    {error, unexpected_node_separator};
parse_binary(_Original, <<$/, _Rest/binary>>, _Node, <<>>) ->
    %% Invalid JID of the form "Node@/Resource".
    {error, unexpected_resource_separator};
parse_binary(_Original, <<$/>>, _Node, _Domain) ->
    %% Invalid JID of the form "Node@Domain/".
    {error, unexpected_end_of_string};
parse_binary(Original, <<$/, Resource/binary>>, Node, Domain) ->
    %% Valid JID of the form "Node@Domain/Resource".
    make(Original, Node, Domain, Resource);
parse_binary(Original, <<C, Rest/binary>>, Node, Domain) ->
    %% JID of the form "Node@Domain" or "Node@Domain/Resource".
    parse_binary(Original, Rest, Node, <<Domain/binary, C>>);
parse_binary(_Original, <<>>, _Node, <<>>) ->
    %% Invalid JID of the form "Node@".
    {error, unexpected_end_of_string};
parse_binary(Original, <<>>, Node, Domain) ->
    %% Valid JID of the form "Node@Domain".
    make(Original, Node, Domain, undefined).

%% --------------------------------------------------------------------
%% JID serialization.
%% --------------------------------------------------------------------

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a full JID.

-spec(to_list/1 :: (jid()) -> string()).

to_list(#jid{} = JID) ->
    binary_to_list(to_binary(JID)).

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = string()
%% @doc Stringify a bare JID.

-spec(to_list/2 :: (node_arg(), domain_arg()) -> string()).

to_list(Node, Domain) ->
    bare_to_list(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | undefined
%%     String = string()
%% @doc Stringify a full JID.

-spec(to_list/3 :: (node_arg(), domain_arg(), res_arg()) -> string()).

to_list(Node, Domain, Resource) ->
    binary_to_list(to_binary(Node, Domain, Resource)).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a full JID with STRINGPREP profiles applied.

-spec(prep_to_list/1 :: (jid()) -> string()).

prep_to_list(
  #jid{node = Node, domain = Domain, resource = Resource}) ->
    to_list(Node, Domain, Resource).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a bare JID.

-spec(bare_to_list/1 :: (jid()) -> string()).

bare_to_list(#jid{} = JID) ->
    binary_to_list(bare_to_binary(JID)).

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = string()
%% @doc Stringify a full JID.

-spec(bare_to_list/2 :: (node_arg(), domain_arg()) -> string()).

bare_to_list(Node, Domain) ->
    binary_to_list(bare_to_binary(Node, Domain)).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = string()
%% @doc Stringify a bare JID with STRINGPREP profiles applied.

-spec(prep_bare_to_list/1 :: (jid()) -> string()).

prep_bare_to_list(
  #jid{node = Node, domain = Domain}) ->
    bare_to_list(Node, Domain).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a full JID.

-spec(to_binary/1 :: (jid()) -> binary()).

to_binary(#jid{raw = Orig_Jid}) ->
    Orig_Jid.

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = binary()
%% @doc Stringify a bare JID.

-spec(to_binary/2 :: (node_arg(), domain_arg()) -> binary()).

to_binary(Node, Domain) ->
    bare_to_binary(Node, Domain).

%% @spec (Node, Domain, Resource) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     Resource = binary() | string() | undefined
%%     String = binary()
%% @doc Stringify a full JID.

-spec(to_binary/3 :: (node_arg(), domain_arg(), res_arg()) -> binary()).

to_binary(Node, Domain, Resource) when is_list(Resource) ->
    to_binary(Node, Domain, as_binary(Resource));

to_binary(Node, Domain, Resource)
  when Resource == undefined orelse is_binary(Resource) ->
    S1 = bare_to_binary(Node, Domain),
    if
        Resource == <<>> orelse Resource == undefined ->
            S1;
        true ->
            <<S1/binary, "/", Resource/binary>>
		end.

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a full JID with STRINGPREP profiles applied.

-spec(prep_to_binary/1 :: (jid()) -> binary()).

prep_to_binary(
  #jid{node = Node, domain = Domain, resource = Resource}) ->
    to_binary(Node, Domain, Resource).

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a bare JID.

-spec(bare_to_binary/1 :: (jid()) -> binary()).

bare_to_binary(#jid{raw = Orig_Jid, resource = LResource} = Jid) ->
    case LResource of
        undefined -> Orig_Jid;
        _         -> bare_to_binary(exmpp_jid:node(Jid), domain(Jid))
    end.

%% @spec (Node, Domain) -> String
%%     Node = binary() | string() | undefined
%%     Domain = binary() | string()
%%     String = binary()
%% @doc Stringify a full JID.

-spec(bare_to_binary/2 :: (node_arg(), domain_arg()) -> binary()).

bare_to_binary(Node, Domain) when is_list(Node) ->
    bare_to_binary(as_binary(Node), Domain);
bare_to_binary(Node, Domain) when is_list(Domain) ->
    bare_to_binary(Node, as_binary(Domain));

bare_to_binary(Node, Domain)
  when (Node == undefined orelse is_binary(Node)) andalso
       (Domain == undefined orelse is_binary(Domain)) ->
    if
        Node == <<>> orelse Node == undefined ->
            Domain;
        true ->
            <<Node/binary, "@", Domain/binary>>
		end.

%% @spec (Jid) -> String
%%     Jid = jid()
%%     String = binary()
%% @doc Stringify a bare JID with STRINGPREP profiles applied.

-spec(prep_bare_to_binary/1 :: (jid()) -> binary()).

prep_bare_to_binary(#jid{node = Node, domain = Domain}) ->
    bare_to_binary(Node, Domain).

%% --------------------------------------------------------------------
%% JID comparison.
%% --------------------------------------------------------------------

%% @spec (Jid1, Jid2) -> boolean()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare full JIDs.

-spec(full_compare/2 :: (jid(), jid()) -> boolean()).

full_compare(#jid{node = LNode, domain = LDomain,
		  resource = LResource},
	     #jid{node = LNode, domain = LDomain,
		  resource = LResource}) ->
    true;
full_compare(_Jid1, _Jid2) ->
    false.

%% @spec (Jid1, Jid2) -> boolean()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare bare JIDs.

-spec(bare_compare/2 :: (jid(), jid()) -> boolean()).

bare_compare(#jid{node = LNode, domain = LDomain},
	     #jid{node = LNode, domain = LDomain}) ->
    true;
bare_compare(_Jid1, _Jid2) ->
    false.

%% @spec (Jid1, Jid2) -> boolean()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare full JIDs. This function is identical to full_compare/2.

-spec(compare/2 :: (jid(), jid()) -> boolean()).

compare(Jid1, Jid2) ->
    full_compare(Jid1, Jid2).

%% @spec (Jid1, Jid2) -> boolean()
%%     Jid1 = jid()
%%     Jid2 = jid()
%% @doc Compare JID's domain.

-spec(compare_domains/2 :: (jid(), jid()) -> boolean()).

compare_domains(#jid{domain = LDomain},
		#jid{domain = LDomain}) ->
    true;
compare_domains(_Jid1, _Jid2) ->
    false.

%% --------------------------------------------------------------------
%% JID checks.
%% --------------------------------------------------------------------

%% @spec (Jid) -> boolean()
%%     Jid = jid()
%% @doc Tell if the argument is a JID.
%%
%% You should probably use the `IS_JID(Jid)' guard expression.

-spec(is_jid/1 :: (jid()) -> boolean()).

is_jid(Jid) when ?IS_JID(Jid) ->
    true;
is_jid(_) ->
    false.

%% --------------------------------------------------------------------
%% JID members accessors.
%% --------------------------------------------------------------------

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = binary()
%% @doc Return the node part of a JID.

-spec(node/1 :: (jid()) -> binary() | undefined).

node(#jid{raw = undefined}) ->
    undefined;
node(#jid{raw = Orig_Jid}) ->
    case binary_split(Orig_Jid, $@) of
        [Node, _] -> Node;
        _         -> undefined
    end.

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = binary()
%% @doc Return the node part of a JID with NODEPREP profile applied.

-spec(prep_node/1 :: (jid()) -> binary() | undefined).

prep_node(#jid{node = N}) -> N.

%% @spec (Jid) -> Domain | undefined
%%     Jid = jid()
%%     Domain = binary()
%% @doc Return the domain part of a JID.

-spec(domain/1 :: (jid()) -> binary() | undefined).

domain(#jid{raw = undefined}) ->
    undefined;
domain(#jid{raw = Orig_Jid}) ->
    Domain_And_Resource = case binary_split(Orig_Jid, $@) of
			      [_, Domain1] -> Domain1;
			      _            -> Orig_Jid
			  end,
    case binary_split(Domain_And_Resource, $/) of
        [Domain2, _] -> Domain2;
        _            -> Domain_And_Resource
    end.

%% @spec (Jid) -> Domain | undefined
%%     Jid = jid()
%%     Domain = binary()
%% @doc Return the domain part of a JID with NAMEPREP profile applied.

-spec(prep_domain/1 :: (jid()) -> binary() | undefined).

prep_domain(#jid{domain = D}) -> D.

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = binary()
%% @doc Return the resource part of a JID.

-spec(resource/1 :: (jid()) -> binary() | undefined).

resource(#jid{raw = undefined}) ->
    undefined;
resource(#jid{raw = Orig_Jid}) ->
    case binary_split(Orig_Jid, $/) of
        [_, Resource] -> Resource;
        _             -> undefined
    end.

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = binary()
%% @doc Return the resource part of a JID with RESOURCEPREP profile applied.

-spec(prep_resource/1 :: (jid()) -> binary() | undefined).

prep_resource(#jid{resource = R}) -> R.

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = string()
%% @doc Return the node part of a JID as a list.

-spec(node_as_list/1 :: (jid()) -> string() | undefined).

node_as_list(Jid) ->
    as_list_or_undefined(exmpp_jid:node(Jid)).

%% @spec (Jid) -> Node | undefined
%%     Jid = jid()
%%     Node = string()
%% @doc Return the node part of a JID as a list with NODEPREP profile
%% applied.

-spec(prep_node_as_list/1 :: (jid()) -> string() | undefined).

prep_node_as_list(Jid) ->
    as_list_or_undefined(prep_node(Jid)).

%% @spec (Jid) -> Domain | undefined
%%     Jid = jid()
%%     Domain = string()
%% @doc Return the domain part of a JID as a list.

-spec(domain_as_list/1 :: (jid()) -> string() | undefined).

domain_as_list(Jid) ->
    as_list_or_undefined(domain(Jid)).

%% @spec (Jid) -> Domain | undefined
%%     Jid = jid()
%%     Domain = string()
%% @doc Return the domain part of a JID as a list with NAMEPREP profile
%% applied.

-spec(prep_domain_as_list/1 :: (jid()) -> string() | undefined).

prep_domain_as_list(Jid) ->
    as_list_or_undefined(prep_domain(Jid)).

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = string()
%% @doc Return the resource part of a JID as a list.

-spec(resource_as_list/1 :: (jid()) -> string() | undefined).

resource_as_list(Jid) ->
    as_list_or_undefined(resource(Jid)).

%% @spec (Jid) -> Resource | undefined
%%     Jid = jid()
%%     Resource = string()
%% @doc Return the domain part of a JID as a list with RESOURCEPREP
%% profile applied.

-spec(prep_resource_as_list/1 :: (jid()) -> string() | undefined).

prep_resource_as_list(Jid) ->
    as_list_or_undefined(prep_resource(Jid)).

as_list_or_undefined(undefined) ->
    undefined;
as_list_or_undefined(V) when is_binary(V) ->
    binary_to_list(V).

as_binary(V) when is_list(V) ->
    list_to_binary(V).

%% --------------------------------------------------------------------
%% Helper functions
%% --------------------------------------------------------------------

%% We do not use random generator to avoid having to decide when and how
%% to seed the Erlang random number generator.

-spec(generate_resource/0 :: () -> string()).

generate_resource() ->
    {A, B, C} = erlang:timestamp(),
    lists:flatten(["exmpp#",
		   integer_to_list(A),
		   integer_to_list(B),
		   integer_to_list(C)]
		 ).

%% If both lists are equal, don't waste memory creating two separate
%% binary copies.
%% "st or bi 2 bi" means: convert a STring OR a BInary TO a BInary.

-spec(storbi2bi/1 :: (binary() | string()) -> binary()).

storbi2bi(String) when is_list(String) ->
    list_to_binary(String);
storbi2bi(Binary) when is_binary(Binary) ->
    Binary.

-spec(binary_split/2 :: (binary(), char()) -> [binary()]).

binary_split(B, C) -> binary_split(B, C, <<>>, []).

binary_split(<<C, Rest/binary>>, C, Acc, Tokens) ->
    binary_split(Rest, C, <<>>, [Acc | Tokens]);
binary_split(<<C1, Rest/binary>>, C, Acc, Tokens) ->
    binary_split(Rest, C, <<Acc/binary, C1>>, Tokens);
binary_split(<<>>, _C, Acc, Tokens) ->
    lists:reverse([Acc | Tokens]).
