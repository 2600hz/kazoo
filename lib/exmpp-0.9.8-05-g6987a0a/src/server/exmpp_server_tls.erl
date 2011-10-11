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
%% side of the TLS feature.
%%
%% <p>
%% Note that it doesn't implement encryption, only feature negotiation
%% at the XMPP level.
%% </p>

-module(exmpp_server_tls).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 feature/0,
	 feature/1
	]).

%% TLS negotiation.
-export([
	 proceed/0,
	 failure/0
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec () -> Feature
%%     Feature = exmpp_xml:xmlel()
%% @doc Make a feature announcement child.
%%
%% TLS is announced as not required.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.
%%
%% @see feature/1.

feature() ->
    feature(false).

%% @spec (Is_Required) -> Feature
%%     Is_Required = bool()
%%     Feature = exmpp_xml:xmlel()
%% @doc Make a feature announcement child.
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature(Is_Required) ->
    Feature = #xmlel{ns = ?NS_TLS,
		     name = 'starttls'
		    },
    if
        Is_Required ->
            Required = #xmlel{
              ns = ?NS_TLS,
              name = 'required'
	     },
            exmpp_xml:append_child(Feature, Required);
        true ->
            Feature
    end.

%% --------------------------------------------------------------------
%% TLS negotiation.
%% --------------------------------------------------------------------

%% @spec () -> Proceed
%%     Proceed = exmpp_xml:xmlel()
%% @doc Make an XML element to tell the initiating entity it can proceed
%% with the TLS handshake.

proceed() ->
    #xmlel{ns = ?NS_TLS,
	   name = 'proceed'
	  }.

%% @spec () -> Failure
%%     Failure = exmpp_xml:xmlel()
%% @doc Make an XML element to tell the initiating entity that the TLS
%% handshake failed.

failure() ->
    #xmlel{ns = ?NS_TLS,
	   name = 'failure'
	  }.
