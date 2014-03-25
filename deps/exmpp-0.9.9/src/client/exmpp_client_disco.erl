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

%% @author Karim Gemayel <kgemayel@process-one.net>

-module(exmpp_client_disco).

-include("exmpp.hrl").

-define(QUERY_INFO,
  #xmlel{ns = ?NS_DISCO_INFO, name = 'query'}
).

-define(QUERY_ITEMS,
  #xmlel{ns = ?NS_DISCO_ITEMS, name = 'query'}
).

%% Creation.
-export([
	 info/1,
	 info/2,
	 items/1,
	 items/2
	]).

%% @spec (To) -> Iq
%%     To = string()
%%     Iq = exmpp_xml:xmlel()
%% @doc Make an <iq/> for a disco#info

info(To) ->
  Query = ?QUERY_INFO,
  Iq = ?IQ_GET(To, iq_id()),
  exmpp_xml:append_child(Iq, Query).

%% @spec (To, Node) -> Iq
%%     To   = string()
%%     Node = string()
%%     Iq   = exmpp_xml:xmlel()
%% @doc Make an <iq/> for a disco#info to a node

info(To, Node) ->
  Query = exmpp_xml:set_attribute(?QUERY_INFO, <<"node">>, Node),
  Iq = ?IQ_GET(To, iq_id()),
  exmpp_xml:append_child(Iq, Query).

%% @spec (To) -> Iq
%%     To = string()
%%     Iq = exmpp_xml:xmlel()
%% @doc Make an <iq/> for a disco#items

items(To) ->
  Query = ?QUERY_ITEMS,
  Iq = ?IQ_GET(To, iq_id()),
  exmpp_xml:append_child(Iq, Query).

%% @spec (To, Node) -> Iq
%%     To   = string()
%%     Node = string()
%%     Iq   = exmpp_xml:xmlel()
%% @doc Make an <iq/> for a disco#items to a node 

items(To, Node) ->
  Query = exmpp_xml:set_attribute(?QUERY_ITEMS, <<"node">>, Node),
  Iq = ?IQ_GET(To, iq_id()),
  exmpp_xml:append_child(Iq, Query).

%% @spec () -> Iq_ID
%%     Iq_ID = string()
%% @doc Generate a random iq ID.
%%
%% This function uses {@link random:uniform/1}. It's up to the caller to
%% seed the generator.

iq_id() ->
    "iq-" ++ integer_to_list(random:uniform(65536 * 65536)).
