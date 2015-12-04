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
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of legacy authentication found in Jabber, before XMPP 1.0.
%%
%% @see exmpp_client_legacy_auth.
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_server_legacy_auth).

-include("exmpp.hrl").

%% Creating stanza.
-export([
	 fields/1,
	 fields/2,
	 success/1,
	 failure/2
	]).

%% Accessing informations.
-export([
	 want_fields/1,
	 get_credentials/1
	]).

%% Tools.
-export([
	 unhex/1
	]).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (Request_IQ) -> Fields_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Fields_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for advertising fields.
%%
%% Both authentication methods are proposed.

fields(Request_IQ) ->
    fields(Request_IQ, both).

%% @spec (Request_IQ, Auth) -> Fields_IQ
%%     Request_IQ = exmpp_xml:xmlel()
%%     Auth = plain | digest | both
%%     Fields_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for advertising fields.

fields(Request_IQ, Auth) when ?IS_IQ(Request_IQ) ->
    Path = [ {element, 'query' }, {element, 'username'}, cdata ],
    Username_Children = case exmpp_xml:get_path(Request_IQ, Path) of
                   <<>> -> [];
                   Username -> [#xmlcdata{cdata = Username}]
                end,
    Username_El = #xmlel{ns = ?NS_LEGACY_AUTH, name = 'username',
			 children = Username_Children},
    Password_El = #xmlel{ns = ?NS_LEGACY_AUTH, name = 'password'},
    Digest_El   = #xmlel{ns = ?NS_LEGACY_AUTH, name = 'digest'},
    Resource_El = #xmlel{ns = ?NS_LEGACY_AUTH, name = 'resource'},
    Children = case Auth of
		   plain  -> [Username_El, Password_El, Resource_El];
		   digest -> [Username_El, Digest_El, Resource_El];
		   both   -> [Username_El, Password_El, Digest_El, Resource_El]
	       end,
    Query = #xmlel{
      ns = ?NS_LEGACY_AUTH,
      name = 'query',
      children = Children
     },
    exmpp_iq:result(Request_IQ, Query).

%% @spec (Password_IQ) -> Success_IQ
%%     Password_IQ = exmpp_xml:xmlel()
%%     Success_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to notify a successfull authentication.

success(Password_IQ) when ?IS_IQ(Password_IQ) ->
    exmpp_iq:result(Password_IQ).

%% @spec (Password_IQ, Condition) -> Failure_IQ
%%     Password_IQ = exmpp_xml:xmlel()
%%     Condition = not_authorized | conflict | not_acceptable
%%     Failure_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to notify a successfull authentication.

failure(Password_IQ, Condition) when ?IS_IQ(Password_IQ) ->
    Code = case Condition of
	       'not-authorized' -> "401";
	       'conflict'       -> "409";
	       'not-acceptable' -> "406"
	   end,
    Error = exmpp_xml:set_attribute(
	      exmpp_stanza:error(Password_IQ#xmlel.ns, Condition),
	      <<"code">>, Code),
    exmpp_iq:error_without_original(Password_IQ, Error).

%% --------------------------------------------------------------------
%% Accessing informations.
%% --------------------------------------------------------------------

%% @spec (Request_IQ) -> boolean()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Tell if the initiating entity asks for the authentication fields.

want_fields(Request_IQ) when ?IS_IQ(Request_IQ) ->
    case exmpp_iq:get_type(Request_IQ) of
        'get' ->
            case exmpp_iq:get_request(Request_IQ) of
                #xmlel{ns = ?NS_LEGACY_AUTH, name = 'query'} -> true;
                _                                            -> false
            end;
        _ ->
            false
    end;
want_fields(_Stanza) ->
    false.

%% @spec (Password_IQ) -> Credentials
%%     Password_IQ = exmpp_xml:xmlel()
%%     Credentials = {Username, Password, Resource}
%%     Username = string()
%%     Password = {plain, string()} | {digest, string()}
%%     Resource = string()
%% @doc Extract credentials from the `Password_IQ'.
%%
%% For digest, hexadecimal content is decoded.

get_credentials(Password_IQ) when ?IS_IQ(Password_IQ) ->
    Request = exmpp_iq:get_request(Password_IQ),
    case Request of
        #xmlel{ns = ?NS_LEGACY_AUTH, name = 'query', children = Children}
	when length(Children) == 3 ->
            get_credentials2(Children, {undefined, undefined, undefined});
        _ ->
            throw({legacy_auth, get_credentials, invalid_iq, Password_IQ})
    end.

get_credentials2(
  [#xmlel{ns = ?NS_LEGACY_AUTH, name = 'username'} = Field | Rest],
  {_Username, Password, Resource}) ->
    Username = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, Password, Resource});
get_credentials2(
  [#xmlel{ns = ?NS_LEGACY_AUTH, name = 'password'} = Field | Rest],
  {Username, _Password, Resource}) ->
    Password = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, {plain, Password}, Resource});
get_credentials2(
  [#xmlel{ns = ?NS_LEGACY_AUTH, name = 'digest'} = Field | Rest],
  {Username, _Password, Resource}) ->
    Password = unhex(exmpp_xml:get_cdata_as_list(Field)),
    get_credentials2(Rest, {Username, {digest, Password}, Resource});
get_credentials2(
  [#xmlel{ns = ?NS_LEGACY_AUTH, name = 'resource'} = Field | Rest],
  {Username, Password, _Resource}) ->
    Resource = exmpp_xml:get_cdata_as_list(Field),
    get_credentials2(Rest, {Username, Password, Resource});
get_credentials2([Field | _Rest], _Credentials) ->
    throw({legacy_auth, get_credentials, invalid_field, Field});
get_credentials2([], {undefined, _Password, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'username'});
get_credentials2([], {_Username, undefined, _Resource}) ->
    throw({legacy_auth, get_credentials, missing_field, 'password'});
get_credentials2([], {_Username, _Password, undefined}) ->
    throw({legacy_auth, get_credentials, missing_field, 'resource'});
get_credentials2([], Credentials) ->
    Credentials.

%% --------------------------------------------------------------------
%% Tools.
%% --------------------------------------------------------------------

%% @spec (Hex) -> Plain
%%     Hex = string()
%%     Plain = list()
%% @doc Decode hexadecimal string.

unhex("") ->
    [];
unhex(Digest) when length(Digest) rem 2 /= 0 ->
    throw({legacy_auth, get_credentials, invalid_digest, Digest});
unhex(Digest) ->
    unhex2(Digest, []).

unhex2([C1, C2 | Rest], Plain) ->
    I1 = hexchar_to_int(C1),
    I2 = hexchar_to_int(C2),
    I = (I1 bsl 4) + I2,
    unhex2(Rest, [I | Plain]);
unhex2([], Plain) ->
    lists:reverse(Plain).

hexchar_to_int($0) ->  0;
hexchar_to_int($1) ->  1;
hexchar_to_int($2) ->  2;
hexchar_to_int($3) ->  3;
hexchar_to_int($4) ->  4;
hexchar_to_int($5) ->  5;
hexchar_to_int($6) ->  6;
hexchar_to_int($7) ->  7;
hexchar_to_int($8) ->  8;
hexchar_to_int($9) ->  9;
hexchar_to_int($a) -> 10;
hexchar_to_int($b) -> 11;
hexchar_to_int($c) -> 12;
hexchar_to_int($d) -> 13;
hexchar_to_int($e) -> 14;
hexchar_to_int($f) -> 15;
hexchar_to_int(C)  -> throw({legacy_auth, get_credentials, invalid_digest, C}).
