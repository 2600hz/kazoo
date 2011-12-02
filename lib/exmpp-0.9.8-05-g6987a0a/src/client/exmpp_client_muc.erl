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

%% @author Pablo Polvorin <pablo.polvorin@process-one.net>
%% @doc Helper functions for generating MUC related stanzas. 
%%      Very incomplete, now it only generates kick and ban stanzas. 

-module(exmpp_client_muc).


-include("exmpp.hrl").

-export([kick/2, 
         kick/3,
         kick/4,
         ban/2,
         ban/3,
         ban/4,
         get_banlist/1,
         get_banlist/2,
         update_banlist/2,
         update_banlist/3
         ]).

-type(ban_item() :: {exmpp_jid:jid(), outcast | none | binary()} | {exmpp_jid:jid(), outcast | none | binary(), binary() | string()}).


-spec kick(Room :: exmpp_jid:jid(), Nick :: string() | binary()) -> #xmlel{}.
kick(Room, Nick) ->
   kick(?NS_JABBER_CLIENT, Room, Nick).

-spec kick(NS :: atom() | string(), Room :: exmpp_jid:jid(), Nick :: string() | binary()) -> #xmlel{}.
kick(NS, Room, Nick) ->
    kick(NS, Room, Nick, <<>>).

-spec kick(NS :: atom() | string(), Room :: exmpp_jid:jid(), Nick :: string() | binary(), Reason :: string() | binary()) -> #xmlel{}.
kick(NS, Room, Nick, Reason) ->
    exmpp_stanza:set_recipient(
        exmpp_iq:set(NS, 
            exmpp_xml:element(?NS_MUC_ADMIN, 'query', [], 
                [exmpp_xml:element(?NS_MUC_ADMIN, 'item', [?XMLATTR(<<"nick">>, Nick), ?XMLATTR(<<"role">>, <<"none">>)], 
                    [exmpp_xml:element(?NS_MUC_ADMIN, 'reason', [], [?XMLCDATA(Reason)])])])), Room).


-spec ban(Room :: exmpp_jid:jid(), JID :: exmpp_jid:jid() ) -> #xmlel{}.
ban(Room, JID) ->
   ban(?NS_JABBER_CLIENT, Room, JID).

-spec ban(NS :: atom() | string(), Room :: exmpp_jid:jid(), JID :: exmpp_jid:jid()) -> #xmlel{}.
ban(NS, Room, JID) ->
    ban(NS, Room, JID, <<>>).

-spec ban(NS :: atom() | string(), Room :: exmpp_jid:jid(), JID :: exmpp_jid:jid(), Reason :: string() | binary()) -> #xmlel{}.
ban(NS, Room, JID, Reason) ->
    update_banlist(NS, Room, [{JID, outcast, Reason}]).


-spec get_banlist(Room :: exmpp_jid:jid()) -> #xmlel{}.
get_banlist(Room) ->
    get_banlist(?NS_JABBER_CLIENT, Room).

-spec get_banlist(NS :: atom() | string(), Room :: exmpp_jid:jid()) -> #xmlel{}.
get_banlist(NS, Room) ->
    exmpp_stanza:set_recipient(
        exmpp_iq:get(NS, 
            exmpp_xml:element(?NS_MUC_ADMIN, 'query', [], 
                [exmpp_xml:element(?NS_MUC_ADMIN, 'item', [?XMLATTR(<<"affiliation">>, <<"outcast">>)], [])])), Room ).
    

-spec update_banlist(Room :: exmpp_jid:jid(), BanList :: [ban_item()]) -> #xmlel{}.
update_banlist(Room, BanList) ->
    update_banlist(?NS_JABBER_CLIENT, Room, BanList).

-spec update_banlist(NS :: atom() | string(), Room :: exmpp_jid:jid(), BanList :: [ban_item()]) -> #xmlel{}.
update_banlist(NS, Room, BanList) ->
     exmpp_stanza:set_recipient(
        exmpp_iq:set(NS, 
            exmpp_xml:element(?NS_MUC_ADMIN, 'query', [], [ban_to_item(Ban) || Ban <- BanList])), Room).


ban_to_item({JID, Affiliation}) ->
    ban_to_item({JID, Affiliation, <<>>});
ban_to_item({JID, Affiliation, Reason}) ->
    exmpp_xml:element(?NS_MUC_ADMIN, 'item', [?XMLATTR(<<"jid">>, exmpp_jid:to_binary(JID)), ?XMLATTR(<<"affiliation">>, affiliation_to_binary(Affiliation))], 
        [exmpp_xml:element(?NS_MUC_ADMIN, 'reason', [], [?XMLCDATA(Reason)])]).


affiliation_to_binary(outcast) -> <<"outcast">>;
affiliation_to_binary(none) -> <<"none">>;
affiliation_to_binary(A) when is_binary(A) -> A.
