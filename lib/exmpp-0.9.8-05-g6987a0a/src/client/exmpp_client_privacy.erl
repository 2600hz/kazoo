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
%% The module <strong>{@module}</strong> implements the initiating entity
%% side of privacy lists management.

-module(exmpp_client_privacy).

-include("exmpp.hrl").

%% Creating stanza.
-export([
	 ack_list_push/1
	]).

%% --------------------------------------------------------------------
%% Creating stanza.
%% --------------------------------------------------------------------

%% @spec (Push_IQ) -> Ack_IQ
%%     Push_IQ = exmpp_xml:xmlel()
%%     Ack_IQ = exmpp_xml:xmlel()
%% @doc Make an `<iq/>' result to acknowledge the push.

ack_list_push(Push_IQ) ->
    exmpp_iq:result(Push_IQ).
