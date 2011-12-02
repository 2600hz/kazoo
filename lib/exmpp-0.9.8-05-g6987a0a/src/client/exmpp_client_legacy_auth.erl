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
%% The module <strong>{@module}</strong> implements the initiating
%% entity side of legacy authentication found in Jabber, before XMPP
%% 1.0.
%%
%% <p>
%% This table presents a successful legacy authentication.
%% </p>
%% <table class="illustration">
%% <tr>
%% <th>Client-side</th>
%% <th>Server-side</th>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% Once a stream is opened, the client call `{@module}':
%% </p>
%% <pre>Request = exmpp_client_legacy_auth:request("jabber.example.com").</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq type="get" to="jabber.example.com" id="auth-1905181425"&gt;
%%   &lt;query xmlns="jabber:iq:auth"/&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% The server answer with the available fields:
%% </p>
%% <pre>Fields = exmpp_server_legacy_auth:fields(Request).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-1905181425"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username/&gt;
%%               &lt;password/&gt;
%%               &lt;digest/&gt;
%%               &lt;resource/&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% </tr>
%% <tr>
%% <td>
%% <p>
%% The client can send its credentials:
%% </p>
%% <pre>Password = exmpp_client_legacy_auth:password(
%%   Fields,
%%   "johndoe",
%%   "foobar!",
%%   "home"<br/>).</pre>
%% <p>
%% The best method is chosen automatically (here, `<digest/>').
%% </p>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;q xmlns="jabber:client" type="set" id="auth-3105434037"&gt;
%%       &lt;query xmlns="jabber:iq:auth"&gt;
%%               &lt;username&gt;johndoe&lt;/username&gt;
%%               &lt;digest&gt;
%%                       93fdad2a795c59c73a6acf68a4dbdd3ddb366239
%%               &lt;/digest&gt;
%%               &lt;resource&gt;home&lt;/resource&gt;
%%       &lt;/query&gt;<br/>&lt;/iq&gt;</pre>
%% </td>
%% <td></td>
%% </tr>
%% <tr>
%% <td></td>
%% <td>
%% <p>
%% If the password is correct, the server notify the client:
%% </p>
%% <pre>Success = exmpp_server_legacy_auth:success(Password).</pre>
%% <p>
%% After serialization, this produces this XML message:
%% </p>
%% <pre>&lt;iq xmlns="jabber:client" type="result" id="auth-3105434037"/&gt;</pre>
%% </td>
%% </tr>
%% </table>
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0078.html">XEP-0078: Non-SASL Authentication</a>

-module(exmpp_client_legacy_auth).

-include("exmpp.hrl").

%% Creating stanza.
-export([
	 request/1,
	 request/2,
	 request_with_user/2,
	 request_with_user/3,
	 password/4,
	 password/5,
	 password_plain/3,
	 password_plain/4,
	 password_digest/3,
	 password_digest/4
	]).

%% Accessing informations.
-export([
	 get_fields/1,
	 get_prefered_auth/1,
	 is_success/1
	]).

%% Tools.
-export([
	 digest/2,
	 hex/1
	]).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (To) -> Request_IQ
%%     To = string()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

request(To) ->
    request(To, auth_id()).

%% @spec (To, ID) -> Request_IQ
%%     To = string()
%%     ID = string()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.

request(To, ID) ->
    Query = #xmlel{
      ns = ?NS_LEGACY_AUTH,
      name = 'query'
     },
    IQ = exmpp_iq:get(?NS_JABBER_CLIENT, Query, ID),
    exmpp_stanza:set_recipient(IQ, To).

%% @spec (To, Username) -> Request_IQ
%%     To = string()
%%     Username = string()
%%     Request_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.
%%
%% The stanza ID is generated automatically.

request_with_user(To, Username) ->
    request_with_user(To, Username, auth_id()).

%% @spec (To, Username, ID) -> Request_IQ
%%     To = string()
%%     Username = string()
%%     ID = string()
%%     Response_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' for requesting legacy authentication.

request_with_user(To, Username, ID) ->
    Username_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'username'},
		    Username),
    Query = #xmlel{
      ns = ?NS_LEGACY_AUTH,
      name = 'query',
      children = [Username_El]
     },
    IQ = exmpp_iq:get(?NS_JABBER_CLIENT, Query, ID),
    exmpp_stanza:set_recipient(IQ, To).

%% @spec (Fields_IQ, Username, Password, Resource) -> Password_IQ
%%     Fields_IQ = exmpp_xml:xmlel()
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq/>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

password(Fields_IQ, Username, Password, Resource) ->
    password(Fields_IQ, Username, Password, Resource, auth_id()).

%% @spec (Fields_IQ, Username, Password, Resource, ID) -> Password_IQ
%%     Fields_IQ = exmpp_xml:xmlel()
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     ID = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq/>' to send authentication informations.

password(Fields_IQ, Username, Password, Resource, ID) ->
    case get_prefered_auth(Fields_IQ) of
        plain  -> password_plain(Username, Password, Resource, ID);
        digest -> password_digest(Username, Password, Resource, ID)
    end.

%% @spec (Username, Password, Resource) -> Password_IQ
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

password_plain(Username, Password, Resource) ->
    password_plain(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Password_IQ
%%     Username = string()
%%     Password = string() | nil()
%%     Resource = string()
%%     ID = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is in clear plain text in the stanza.
%%
%% For an anonymous authentication, `Password' may be the empty string.

password_plain(Username, Password, Resource, ID) ->
    Username_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'username'},
		    Username),
    Password_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'password'},
		    Password),
    Resource_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'resource'},
		    Resource),
    Query = #xmlel{
      ns = ?NS_LEGACY_AUTH,
      name = 'query',
      children = [Username_El, Password_El, Resource_El]
     },
    exmpp_iq:set(?NS_JABBER_CLIENT, Query, ID).

%% @spec (Username, Password, Resource) -> Password_IQ
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% The stanza ID is generated automatically.

password_digest(Username, Password, Resource) ->
    password_digest(Username, Password, Resource, auth_id()).

%% @spec (Username, Password, Resource, ID) -> Password_IQ
%%     Username = string()
%%     Password = string()
%%     Resource = string()
%%     ID = string()
%%     Password_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq>' to send authentication informations.
%%
%% `Password' is encoded as specified in XEP-0078.

password_digest(Username, Password, Resource, ID) ->
    Username_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'username'},
		    Username),
    Digest_El = exmpp_xml:set_cdata(
		  #xmlel{ns = ?NS_LEGACY_AUTH, name = 'digest'},
		  hex(digest(ID, Password))),
    Resource_El = exmpp_xml:set_cdata(
		    #xmlel{ns = ?NS_LEGACY_AUTH, name = 'resource'},
		    Resource),
    Query = #xmlel{
      ns = ?NS_LEGACY_AUTH,
      name = 'query',
      children = [Username_El, Digest_El, Resource_El]
     },
    exmpp_iq:set(?NS_JABBER_CLIENT, Query, ID).

%% --------------------------------------------------------------------
%% Accessing informations.
%% --------------------------------------------------------------------

%% @spec (Fields_IQ) -> Fields
%%     Fields_IQ = exmpp_xml:xmlel()
%%     Fields = [atom()]
%% @throws {legacy_auth, get_fields, invalid_iq, Fields_IQ} |
%%         {legacy_auth, get_fields, invalid_field, Field}
%% @doc Return the list of fields supported by the server.

get_fields(Fields_IQ) when ?IS_IQ(Fields_IQ) ->
    case exmpp_iq:get_result(Fields_IQ) of
        undefined ->
            throw({legacy_auth, get_fields, invalid_iq, Fields_IQ});
        #xmlel{ns = ?NS_LEGACY_AUTH, name = 'query', children = Children}
	when length(Children) == 3 orelse length(Children) == 4 ->
            get_fields2(Children, []);
        _ ->
            throw({legacy_auth, get_fields, invalid_iq, Fields_IQ})
    end.

get_fields2([#xmlel{ns = ?NS_LEGACY_AUTH, name = Field} | Rest],
	    Fields) ->
    get_fields2(Rest, [Field | Fields]);
get_fields2([Field | _Rest], _Fields) ->
    throw({legacy_auth, get_fields, invalid_field, Field});
get_fields2([], Fields) ->
    lists:reverse(Fields).

%% @spec (Fields_IQ) -> Auth
%%     Fields_IQ = exmpp_xml:xmlel()
%%     Auth = digest | password
%% @doc Return the prefered authentication method.

get_prefered_auth(IQ) when ?IS_IQ(IQ) ->
    case lists:member('digest', get_fields(IQ)) of
        true -> digest;
        _    -> plain
    end.

%% @spec (IQ) -> bool()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the authentication succeeded.

is_success(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'result' -> true;
        'error'  -> false;
        _        -> throw({legacy_auth, is_success, unexpected_iq, IQ})
    end.

%% --------------------------------------------------------------------
%% Tools.
%% --------------------------------------------------------------------

%% @spec (ID, Passwd) -> Digest
%%     ID = string()
%%     Passwd = string()
%%     Digest = string()
%% @doc Produce a password digest for legacy auth, according to XEP-0078.

digest(ID, Passwd) ->
    Token = ID ++ Passwd,
    crypto:start(),
    binary_to_list(crypto:sha(Token)).

%% @spec (Plain) -> Hex
%%     Plain = string()
%%     Hex = string()
%% @doc Encode list to a hexadecimal string.

hex(Plain) ->
    lists:flatten([hex2(I) || I <- Plain]).

hex2(I) when I > 16#f ->
    [int_to_hexchar((I band 16#f0) bsr 4), int_to_hexchar((I band 16#0f))];
hex2(I) ->
    [$0, int_to_hexchar(I)].

int_to_hexchar(10) -> $a;
int_to_hexchar(11) -> $b;
int_to_hexchar(12) -> $c;
int_to_hexchar(13) -> $d;
int_to_hexchar(14) -> $e;
int_to_hexchar(15) -> $f;
int_to_hexchar(I)  -> $0 + I.

%% --------------------------------------------------------------------
%% Internal functions.
%% --------------------------------------------------------------------

%% @spec () -> Auth_ID
%%     Auth_ID = string()
%% @doc Generate a random authentication iq ID.
%%
%% @see exmpp_utils:random_id/1.

auth_id() ->
    exmpp_utils:random_id("auth").
