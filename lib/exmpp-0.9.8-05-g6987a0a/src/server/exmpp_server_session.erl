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
%% side of the Session Establishment.

-module(exmpp_server_session).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 feature/0
	]).

%% Session establishment.
-export([
	 want_establishment/1,
	 establish/1,
	 error/2
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec () -> Feature
%%     Feature = exmpp_xml:xmlel()
%% @doc Make a feature annoucement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature() ->
    #xmlel{ns = ?NS_SESSION,
	   name = 'session'
	  }.

%% --------------------------------------------------------------------
%% Session establishment.
%% --------------------------------------------------------------------

%% @spec (IQ) -> bool()
%%     IQ = exmpp_xml:xmlel()
%% @doc Tell if the initiating entity wants to establish a session.

want_establishment(IQ) when ?IS_IQ(IQ) ->
    case exmpp_iq:get_type(IQ) of
        'set' ->
            case exmpp_iq:get_request(IQ) of
                #xmlel{ns = ?NS_SESSION, name = 'session'} -> true;
                _                                          -> false
            end;
        _ ->
            false
    end;
want_establishment(_Stanza) ->
    false.

%% @spec (IQ) -> Result_IQ
%%     IQ = exmpp_xml:xmlel()
%%     Result_IQ = exmpp_xml:xmlel()
%% @doc Prepare a result IQ to inform the initiating entity that the
%% session is created.

establish(IQ) when ?IS_IQ(IQ) ->
    exmpp_iq:result(IQ).

%% @spec (IQ, Condition) -> Error_IQ
%%     IQ = exmpp_xml:xmlel()
%%     Condition = atom()
%%     Error_IQ = exmpp_xml:xmlel()
%% @doc Prepare an error reply to `IQ'.

error(IQ, Condition) when ?IS_IQ(IQ) ->
    Error = exmpp_stanza:error(IQ#xmlel.ns, Condition),
    exmpp_iq:error(IQ, Error).
