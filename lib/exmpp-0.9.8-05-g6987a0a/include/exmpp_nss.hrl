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

% --------------------------------------------------------------------
% Namespace and prefix macros.
% --------------------------------------------------------------------

% Defined by XML.
-define(NS_XML,                      'http://www.w3.org/XML/1998/namespace').
-define(NS_XML_s,                    "http://www.w3.org/XML/1998/namespace").
-define(NS_XML_b,                    <<"http://www.w3.org/XML/1998/namespace">>).
-define(NS_XML_pfx,                  "xml").

% Defined by XMPP Core (RFC 3920).
-define(NS_XMPP,                     'http://etherx.jabber.org/streams').
-define(NS_XMPP_s,                   "http://etherx.jabber.org/streams").
-define(NS_XMPP_b,                   <<"http://etherx.jabber.org/streams">>).

-define(NS_XMPP_pfx,                 "stream").
-define(NS_STREAM_ERRORS,            'urn:ietf:params:xml:ns:xmpp-streams').
-define(NS_TLS,                      'urn:ietf:params:xml:ns:xmpp-tls').
-define(NS_SASL,                     'urn:ietf:params:xml:ns:xmpp-sasl').
-define(NS_BIND,                     'urn:ietf:params:xml:ns:xmpp-bind').
-define(NS_STANZA_ERRORS,            'urn:ietf:params:xml:ns:xmpp-stanzas').

% Defined by XMPP-IM (RFC 3921).
-define(NS_JABBER_CLIENT,            'jabber:client').
-define(NS_JABBER_CLIENT_s,          "jabber:client").
-define(NS_JABBER_CLIENT_b,          <<"jabber:client">>).

-define(NS_JABBER_SERVER,            'jabber:server').
-define(NS_JABBER_SERVER_s,          "jabber:server").
-define(NS_JABBER_SERVER_b,          <<"jabber:server">>).

-define(NS_SESSION,                  'urn:ietf:params:xml:ns:xmpp-session').

-define(NS_ROSTER,                   'jabber:iq:roster').
-define(NS_ROSTER_s,                 "jabber:iq:roster").
-define(NS_ROSTER_b,                 <<"jabber:iq:roster">>).

% Defined by End-to-End Signing and Object Encryption for XMPP (RFC 3923).
-define(NS_E2E,                      'urn:ietf:params:xml:ns:xmpp-e2e').
-define(NS_E2E_s,                    "urn:ietf:params:xml:ns:xmpp-e2e").
-define(NS_E2E_b,                    <<"urn:ietf:params:xml:ns:xmpp-e2e">>).

% Defined by XEP-0003: Proxy Accept Socket Service (PASS).
-define(NS_PASS,                     'jabber:iq:pass').
-define(NS_PASS_s,                   "jabber:iq:pass").
-define(NS_PASS_b,                   <<"jabber:iq:pass">>).

% Defined by XEP-0004: Data Forms.
-define(NS_DATA_FORMS,               'jabber:x:data').
-define(NS_DATA_FORMS_s,             "jabber:x:data").
-define(NS_DATA_FORMS_b,             <<"jabber:x:data">>).

% Defined by XEP-0009: Jabber-RPC.
-define(NS_RPC,                      'jabber:iq:rpc').
-define(NS_RPC_s,                    "jabber:iq:rpc").
-define(NS_RPC_b,                    <<"jabber:iq:rpc">>).

% Defined by XEP-0011: Jabber Browsing.
-define(NS_BROWSE,                   'jabber:iq:browse').
-define(NS_BROWSE_s,                 "jabber:iq:browse").
-define(NS_BROWSE_b,                 <<"jabber:iq:browse">>).

% Defined by XEP-0012: Last Activity.
-define(NS_LAST_ACTIVITY,            'jabber:iq:last').
-define(NS_LAST_ACTIVITY_s,          "jabber:iq:last").
-define(NS_LAST_ACTIVITY_b,          <<"jabber:iq:last">>).

% Defined by XEP-0013: Flexible Offline Message Retrieval.
-define(NS_OFFLINE,                  'http://jabber.org/protocol/offline').
-define(NS_OFFLINE_s,                "http://jabber.org/protocol/offline").
-define(NS_OFFLINE_b,                <<"http://jabber.org/protocol/offline">>).

% Defined by XEP-0016: Privacy Lists.
-define(NS_PRIVACY,                  'jabber:iq:privacy').
-define(NS_PRIVACY_s,                "jabber:iq:privacy").
-define(NS_PRIVACY_b,                <<"jabber:iq:privacy">>).

% Defined by XEP-0020: Feature Negotiation.
-define(NS_FEATURE_NEG,              'http://jabber.org/protocol/feature-neg').
-define(NS_FEATURE_NEG_s,            "http://jabber.org/protocol/feature-neg").
-define(NS_FEATURE_NEG_b,            <<"http://jabber.org/protocol/feature-neg">>).

% Defined by XEP-0022: Message Events.
-define(NS_MESSAGE_EVENT,            'jabber:x:event').
-define(NS_MESSAGE_EVENT_s,          "jabber:x:event").
-define(NS_MESSAGE_EVENT_b,          <<"jabber:x:event">>).

% Defined by XEP-0023: Message Expiration.
-define(NS_MESSAGE_EXPIRE,           'jabber:x:expire').
-define(NS_MESSAGE_EXPIRE_s,         "jabber:x:expire").
-define(NS_MESSAGE_EXPIRE_b,         <<"jabber:x:expire">>).

% Defined by XEP-0027: Current Jabber OpenPGP Usage.
-define(NS_PGP_ENCRYPTED,            'jabber:x:encrypted').
-define(NS_PGP_SIGNED,               'jabber:x:signed').

-define(NS_PGP_ENCRYPTED_s,          "jabber:x:encrypted").
-define(NS_PGP_SIGNED_s,             "jabber:x:signed").

-define(NS_PGP_ENCRYPTED_b,          <<"jabber:x:encrypted">>).
-define(NS_PGP_SIGNED_b,             <<"jabber:x:signed">>).

% Defined by XEP-0030: Service Discovery.
-define(NS_DISCO_INFO,               'http://jabber.org/protocol/disco#info').
-define(NS_DISCO_ITEMS,              'http://jabber.org/protocol/disco#items').

-define(NS_DISCO_INFO_s,             "http://jabber.org/protocol/disco#info").
-define(NS_DISCO_ITEMS_s,            "http://jabber.org/protocol/disco#items").

-define(NS_DISCO_INFO_b,             <<"http://jabber.org/protocol/disco#info">>).
-define(NS_DISCO_ITEMS_b,            <<"http://jabber.org/protocol/disco#items">>).

% Defined by XEP-0033: Extended Stanza Addressing.
-define(NS_ADDRESS,                  'http://jabber.org/protocol/address').
-define(NS_ADDRESS_s,                "http://jabber.org/protocol/address").
-define(NS_ADDRESS_b,                <<"http://jabber.org/protocol/address">>).

% Defined by XEP-0039: Statistics Gathering.
-define(NS_STATS,                    'http://jabber.org/protocol/stats').
-define(NS_STATS_s,                  "http://jabber.org/protocol/stats").
-define(NS_STATS_b,                  <<"http://jabber.org/protocol/stats">>).

% Defined by XEP-0045: Multi-User Chat.
-define(NS_MUC,                      'http://jabber.org/protocol/muc').
-define(NS_MUC_ADMIN,                'http://jabber.org/protocol/muc#admin').
-define(NS_MUC_OWNER,                'http://jabber.org/protocol/muc#owner').
-define(NS_MUC_UNIQUE,               'http://jabber.org/protocol/muc#unique').
-define(NS_MUC_USER,                 'http://jabber.org/protocol/muc#user').

-define(NS_MUC_s,                    "http://jabber.org/protocol/muc").
-define(NS_MUC_ADMIN_s,              "http://jabber.org/protocol/muc#admin").
-define(NS_MUC_OWNER_s,              "http://jabber.org/protocol/muc#owner").
-define(NS_MUC_UNIQUE_s,             "http://jabber.org/protocol/muc#unique").
-define(NS_MUC_USER_s,               "http://jabber.org/protocol/muc#user").

-define(NS_MUC_b,                    <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_ADMIN_b,              <<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER_b,              <<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE_b,             <<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_USER_b,               <<"http://jabber.org/protocol/muc#user">>).

% Defined by XEP-0047: In-Band Bytestreams.
-define(NS_IBB,                      'http://jabber.org/protocol/ibb').
-define(NS_IBB_s,                    "http://jabber.org/protocol/ibb").
-define(NS_IBB_b,                    <<"http://jabber.org/protocol/ibb">>).

% Defined by XEP-0048: Bookmarks.
-define(NS_BOOKMARKS,                'storage:bookmarks').
-define(NS_BOOKMARKS_s,              "storage:bookmarks").
-define(NS_BOOKMARKS_b,              <<"storage:bookmarks">>).

% Defined by XEP-0049: Private XML Storage.
-define(NS_PRIVATE,                  'jabber:iq:private').
-define(NS_PRIVATE_s,                "jabber:iq:private").
-define(NS_PRIVATE_b,                <<"jabber:iq:private">>).

% Defined by XEP-0050: Ad-Hoc Commands.
-define(NS_ADHOC,                    'http://jabber.org/protocol/commands').
-define(NS_ADHOC_s,                  "http://jabber.org/protocol/commands").
-define(NS_ADHOC_b,                  <<"http://jabber.org/protocol/commands">>).

% Defined by XEP-0054: vcard-temp.
-define(NS_VCARD,                    'vcard-temp').
-define(NS_VCARD_s,                  "vcard-temp").
-define(NS_VCARD_b,                  <<"vcard-temp">>).

% Defined by XEP-0055: Jabber Search.
-define(NS_SEARCH,                   'jabber:iq:search').
-define(NS_SEARCH_s,                 "jabber:iq:search").
-define(NS_SEARCH_b,                 <<"jabber:iq:search">>).

% Defined by XEP-0059: Result Set Management.
-define(NS_RSM,                      'http://jabber.org/protocol/rsm').
-define(NS_RSM_s,                    "http://jabber.org/protocol/rsm").
-define(NS_RSM_b,                    <<"http://jabber.org/protocol/rsm">>).

% Defined by XEP-0060: Publish-Subscribe.
-define(NS_PUBSUB,
  'http://jabber.org/protocol/pubsub').
-define(NS_PUBSUB_ERRORS,
  'http://jabber.org/protocol/pubsub#errors').
-define(NS_PUBSUB_EVENT,
  'http://jabber.org/protocol/pubsub#event').
-define(NS_PUBSUB_OWNER,
  'http://jabber.org/protocol/pubsub#owner').
-define(NS_PUBSUB_SUBSCRIBE_AUTH,
  'http://jabber.org/protocol/pubsub#subscribe_authorization').
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS,
  'http://jabber.org/protocol/pubsub#subscribe_options').
-define(NS_PUBSUB_NODE_CONFIG,
  'http://jabber.org/protocol/pubsub#node_config').


-define(NS_PUBSUB_ACCESS_AUTH,
  'http://jabber.org/protocol/pubsub#access-authorize').
-define(NS_PUBSUB_ACCESS_OPEN,
  'http://jabber.org/protocol/pubsub#access-open').
-define(NS_PUBSUB_ACCESS_PRESENCE,
  'http://jabber.org/protocol/pubsub#access-presence').
-define(NS_PUBSUB_ACCESS_ROSTER,
  'http://jabber.org/protocol/pubsub#access-roster').
-define(NS_PUBSUB_ACCESS_WHITELIST,
  'http://jabber.org/protocol/pubsub#access-whitelist').
-define(NS_PUBSUB_AUTO_CREATE,
  'http://jabber.org/protocol/pubsub#auto-create').
-define(NS_PUBSUB_AUTO_SUBSCRIBE,
  'http://jabber.org/protocol/pubsub#auto-subscribe').
-define(NS_PUBSUB_COLLECTIONS,
  'http://jabber.org/protocol/pubsub#collections').
-define(NS_PUBSUB_CONFIG_NODE,
  'http://jabber.org/protocol/pubsub#config-node').
-define(NS_PUBSUB_CREATE_CONFIGURE,
  'http://jabber.org/protocol/pubsub#create-and-configure').
-define(NS_PUBSUB_CREATE_NODES,
  'http://jabber.org/protocol/pubsub#create-nodes').
-define(NS_PUBSUB_DELETE_ITEMS,
  'http://jabber.org/protocol/pubsub#delete-items').
-define(NS_PUBSUB_DELETE_NODES,
  'http://jabber.org/protocol/pubsub#delete-nodes').
-define(NS_PUBSUB_FILTERED_NOTIFICATIONS,
  'http://jabber.org/protocol/pubsub#filtered-notifications').
-define(NS_PUBSUB_GET_PENDING,
  'http://jabber.org/protocol/pubsub#get-pending').
-define(NS_PUBSUB_INSTANT_NODES,
  'http://jabber.org/protocol/pubsub#instant-nodes').
-define(NS_PUBSUB_ITEM_IDS,
  'http://jabber.org/protocol/pubsub#item-ids').
-define(NS_PUBSUB_LAST_PUBLISHED,
  'http://jabber.org/protocol/pubsub#last-published').
-define(NS_PUBSUB_LEASED_SUBSCRIPTION,
  'http://jabber.org/protocol/pubsub#leased-subscription').
-define(NS_PUBSUB_MANAGE_SUBSCRIPTIONS,
  'http://jabber.org/protocol/pubsub#manage-subscriptions').
-define(NS_PUBSUB_MEMBER_AFFILIATION,
  'http://jabber.org/protocol/pubsub#member-affiliation').
-define(NS_PUBSUB_META_DATA,
  'http://jabber.org/protocol/pubsub#meta-data').
-define(NS_PUBSUB_MODIFY_AFFILIATIONS,
  'http://jabber.org/protocol/pubsub#modify-affiliations').
-define(NS_PUBSUB_MULTI_COLLECTION,
  'http://jabber.org/protocol/pubsub#multi-collection').
-define(NS_PUBSUB_MULTI_SUBSCRIBE,
  'http://jabber.org/protocol/pubsub#multi-subscribe').
-define(NS_PUBSUB_OUTCAST_AFFILIATION,
  'http://jabber.org/protocol/pubsub#outcast-affiliation').
-define(NS_PUBSUB_PERSISTENT_ITEMS,
  'http://jabber.org/protocol/pubsub#persistent-items').
-define(NS_PUBSUB_PRESENCE_NOTIFICATIONS,
  'http://jabber.org/protocol/pubsub#presence-notifications').
-define(NS_PUBSUB_PRESENCE_SUBSCRIBE,
  'http://jabber.org/protocol/pubsub#presence-subscribe').
-define(NS_PUBSUB_PUBLISH,
  'http://jabber.org/protocol/pubsub#publish').
-define(NS_PUBSUB_PUBLISH_OPTIONS,
  'http://jabber.org/protocol/pubsub#publish-options').
-define(NS_PUBSUB_PUBLISH_ONLY_AFFILIATION,
  'http://jabber.org/protocol/pubsub#publish-only-affiliation').
-define(NS_PUBSUB_PUBLISHER_AFFILIATION,
  'http://jabber.org/protocol/pubsub#publisher-affiliation').
-define(NS_PUBSUB_PURGE_NODES,
  'http://jabber.org/protocol/pubsub#purge-nodes').
-define(NS_PUBSUB_RETRACT_ITEMS,
  'http://jabber.org/protocol/pubsub#retract-items').
-define(NS_PUBSUB_RETRIEVE_AFFILIATIONS,
  'http://jabber.org/protocol/pubsub#retrieve-affiliations').
-define(NS_PUBSUB_RETRIEVE_DEFAULT,
  'http://jabber.org/protocol/pubsub#retrieve-default').
-define(NS_PUBSUB_RETRIEVE_ITEMS,
  'http://jabber.org/protocol/pubsub#retrieve-items').
-define(NS_PUBSUB_RETRIEVE_SUBSCRIPTIONS,
  'http://jabber.org/protocol/pubsub#retrieve-subscriptions').
-define(NS_PUBSUB_SUBSCRIBE,
  'http://jabber.org/protocol/pubsub#subscribe').
-define(NS_PUBSUB_SUBSCRIPTION_OPTIONS,
  'http://jabber.org/protocol/pubsub#subscription-options').
-define(NS_PUBSUB_SUBSCRIPTION_NOTIFICATIONS,
  'http://jabber.org/protocol/pubsub#subscription-notifications').


-define(NS_PUBSUB_s,
  "http://jabber.org/protocol/pubsub").
-define(NS_PUBSUB_ERRORS_s,
  "http://jabber.org/protocol/pubsub#errors").
-define(NS_PUBSUB_EVENT_s,
  "http://jabber.org/protocol/pubsub#event").
-define(NS_PUBSUB_OWNER_s,
  "http://jabber.org/protocol/pubsub#owner").
-define(NS_PUBSUB_SUBSCRIBE_AUTH_s,
  "http://jabber.org/protocol/pubsub#subscribe_authorization").
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS_s,
  "http://jabber.org/protocol/pubsub#subscribe_options").
-define(NS_PUBSUB_NODE_CONFIG_s,
  "http://jabber.org/protocol/pubsub#node_config").


-define(NS_PUBSUB_ACCESS_AUTH_s,
  "http://jabber.org/protocol/pubsub#access-authorize").
-define(NS_PUBSUB_ACCESS_OPEN_s,
  "http://jabber.org/protocol/pubsub#access-open").
-define(NS_PUBSUB_ACCESS_PRESENCE_s,
  "http://jabber.org/protocol/pubsub#access-presence").
-define(NS_PUBSUB_ACCESS_ROSTER_s,
  "http://jabber.org/protocol/pubsub#access-roster").
-define(NS_PUBSUB_ACCESS_WHITELIST_s,
  "http://jabber.org/protocol/pubsub#access-whitelist").
-define(NS_PUBSUB_AUTO_CREATE_s,
  "http://jabber.org/protocol/pubsub#auto-create").
-define(NS_PUBSUB_AUTO_SUBSCRIBE_s,
  "http://jabber.org/protocol/pubsub#auto-subscribe").
-define(NS_PUBSUB_COLLECTIONS_s,
  "http://jabber.org/protocol/pubsub#collections").
-define(NS_PUBSUB_CONFIG_NODE_s,
  "http://jabber.org/protocol/pubsub#config-node").
-define(NS_PUBSUB_CREATE_CONFIGURE_s,
  "http://jabber.org/protocol/pubsub#create-and-configure").
-define(NS_PUBSUB_CREATE_NODES_s,
  "http://jabber.org/protocol/pubsub#create-nodes").
-define(NS_PUBSUB_DELETE_ITEMS_s,
  "http://jabber.org/protocol/pubsub#delete-items").
-define(NS_PUBSUB_DELETE_NODES_s,
  "http://jabber.org/protocol/pubsub#delete-nodes").
-define(NS_PUBSUB_FILTERED_NOTIFICATIONS_s,
  "http://jabber.org/protocol/pubsub#filtered-notifications").
-define(NS_PUBSUB_GET_PENDING_s,
  "http://jabber.org/protocol/pubsub#get-pending").
-define(NS_PUBSUB_INSTANT_NODES_s,
  "http://jabber.org/protocol/pubsub#instant-nodes").
-define(NS_PUBSUB_ITEM_IDS_s,
  "http://jabber.org/protocol/pubsub#item-ids").
-define(NS_PUBSUB_LAST_PUBLISHED_s,
  "http://jabber.org/protocol/pubsub#last-published").
-define(NS_PUBSUB_LEASED_SUBSCRIPTION_s,
  "http://jabber.org/protocol/pubsub#leased-subscription").
-define(NS_PUBSUB_MANAGE_SUBSCRIPTIONS_s,
  "http://jabber.org/protocol/pubsub#manage-subscriptions").
-define(NS_PUBSUB_MEMBER_AFFILIATION_s,
  "http://jabber.org/protocol/pubsub#member-affiliation").
-define(NS_PUBSUB_META_DATA_s,
  "http://jabber.org/protocol/pubsub#meta-data").
-define(NS_PUBSUB_MODIFY_AFFILIATIONS_s,
  "http://jabber.org/protocol/pubsub#modify-affiliations").
-define(NS_PUBSUB_MULTI_COLLECTION_s,
  "http://jabber.org/protocol/pubsub#multi-collection").
-define(NS_PUBSUB_MULTI_SUBSCRIBE_s,
  "http://jabber.org/protocol/pubsub#multi-subscribe").
-define(NS_PUBSUB_OUTCAST_AFFILIATION_s,
  "http://jabber.org/protocol/pubsub#outcast-affiliation").
-define(NS_PUBSUB_PERSISTENT_ITEMS_s,
  "http://jabber.org/protocol/pubsub#persistent-items").
-define(NS_PUBSUB_PRESENCE_NOTIFICATIONS_s,
  "http://jabber.org/protocol/pubsub#presence-notifications").
-define(NS_PUBSUB_PRESENCE_SUBSCRIBE_s,
  "http://jabber.org/protocol/pubsub#presence-subscribe").
-define(NS_PUBSUB_PUBLISH_s,
  "http://jabber.org/protocol/pubsub#publish").
-define(NS_PUBSUB_PUBLISH_OPTIONS_s,
  "http://jabber.org/protocol/pubsub#publish-options").
-define(NS_PUBSUB_PUBLISH_ONLY_AFFILIATION_s,
  "http://jabber.org/protocol/pubsub#publish-only-affiliation").
-define(NS_PUBSUB_PUBLISHER_AFFILIATION_s,
  "http://jabber.org/protocol/pubsub#publisher-affiliation").
-define(NS_PUBSUB_PURGE_NODES_s,
  "http://jabber.org/protocol/pubsub#purge-nodes").
-define(NS_PUBSUB_RETRACT_ITEMS_s,
  "http://jabber.org/protocol/pubsub#retract-items").
-define(NS_PUBSUB_RETRIEVE_AFFILIATIONS_s,
  "http://jabber.org/protocol/pubsub#retrieve-affiliations").
-define(NS_PUBSUB_RETRIEVE_DEFAULT_s,
  "http://jabber.org/protocol/pubsub#retrieve-default").
-define(NS_PUBSUB_RETRIEVE_ITEMS_s,
  "http://jabber.org/protocol/pubsub#retrieve-items").
-define(NS_PUBSUB_RETRIEVE_SUBSCRIPTIONS_s,
  "http://jabber.org/protocol/pubsub#retrieve-subscriptions").
-define(NS_PUBSUB_SUBSCRIBE_s,
  "http://jabber.org/protocol/pubsub#subscribe").
-define(NS_PUBSUB_SUBSCRIPTION_OPTIONS_s,
  "http://jabber.org/protocol/pubsub#subscription-options").
-define(NS_PUBSUB_SUBSCRIPTION_NOTIFICATIONS_s,
  "http://jabber.org/protocol/pubsub#subscription-notifications").


-define(NS_PUBSUB_b,
  <<"http://jabber.org/protocol/pubsub">>).
-define(NS_PUBSUB_ERRORS_b,
  <<"http://jabber.org/protocol/pubsub#errors">>).
-define(NS_PUBSUB_EVENT_b,
  <<"http://jabber.org/protocol/pubsub#event">>).
-define(NS_PUBSUB_OWNER_b,
  <<"http://jabber.org/protocol/pubsub#owner">>).
-define(NS_PUBSUB_SUBSCRIBE_AUTH_b,
  <<"http://jabber.org/protocol/pubsub#subscribe_authorization">>).
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS_b,
  <<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_NODE_CONFIG_b,
  <<"http://jabber.org/protocol/pubsub#node_config">>).


-define(NS_PUBSUB_ACCESS_AUTH_b,
  <<"http://jabber.org/protocol/pubsub#access-authorize">>).
-define(NS_PUBSUB_ACCESS_OPEN_b,
  <<"http://jabber.org/protocol/pubsub#access-open">>).
-define(NS_PUBSUB_ACCESS_PRESENCE_b,
  <<"http://jabber.org/protocol/pubsub#access-presence">>).
-define(NS_PUBSUB_ACCESS_ROSTER_b,
  <<"http://jabber.org/protocol/pubsub#access-roster">>).
-define(NS_PUBSUB_ACCESS_WHITELIST_b,
  <<"http://jabber.org/protocol/pubsub#access-whitelist">>).
-define(NS_PUBSUB_AUTO_CREATE_b,
  <<"http://jabber.org/protocol/pubsub#auto-create">>).
-define(NS_PUBSUB_AUTO_SUBSCRIBE_b,
  <<"http://jabber.org/protocol/pubsub#auto-subscribe">>).
-define(NS_PUBSUB_COLLECTIONS_b,
  <<"http://jabber.org/protocol/pubsub#collections">>).
-define(NS_PUBSUB_CONFIG_NODE_b,
  <<"http://jabber.org/protocol/pubsub#config-node">>).
-define(NS_PUBSUB_CREATE_CONFIGURE_b,
  <<"http://jabber.org/protocol/pubsub#create-and-configure">>).
-define(NS_PUBSUB_CREATE_NODES_b,
  <<"http://jabber.org/protocol/pubsub#create-nodes">>).
-define(NS_PUBSUB_DELETE_ITEMS_b,
  <<"http://jabber.org/protocol/pubsub#delete-items">>).
-define(NS_PUBSUB_DELETE_NODES_b,
  <<"http://jabber.org/protocol/pubsub#delete-nodes">>).
-define(NS_PUBSUB_FILTERED_NOTIFICATIONS_b,
  <<"http://jabber.org/protocol/pubsub#filtered-notifications">>).
-define(NS_PUBSUB_GET_PENDING_b,
  <<"http://jabber.org/protocol/pubsub#get-pending">>).
-define(NS_PUBSUB_INSTANT_NODES_b,
  <<"http://jabber.org/protocol/pubsub#instant-nodes">>).
-define(NS_PUBSUB_ITEM_IDS_b,
  <<"http://jabber.org/protocol/pubsub#item-ids">>).
-define(NS_PUBSUB_LAST_PUBLISHED_b,
  <<"http://jabber.org/protocol/pubsub#last-published">>).
-define(NS_PUBSUB_LEASED_SUBSCRIPTION_b,
  <<"http://jabber.org/protocol/pubsub#leased-subscription">>).
-define(NS_PUBSUB_MANAGE_SUBSCRIPTIONS_b,
  <<"http://jabber.org/protocol/pubsub#manage-subscriptions">>).
-define(NS_PUBSUB_MEMBER_AFFILIATION_b,
  <<"http://jabber.org/protocol/pubsub#member-affiliation">>).
-define(NS_PUBSUB_META_DATA_b,
  <<"http://jabber.org/protocol/pubsub#meta-data">>).
-define(NS_PUBSUB_MODIFY_AFFILIATIONS_b,
  <<"http://jabber.org/protocol/pubsub#modify-affiliations">>).
-define(NS_PUBSUB_MULTI_COLLECTION_b,
  <<"http://jabber.org/protocol/pubsub#multi-collection">>).
-define(NS_PUBSUB_MULTI_SUBSCRIBE_b,
  <<"http://jabber.org/protocol/pubsub#multi-subscribe">>).
-define(NS_PUBSUB_OUTCAST_AFFILIATION_b,
  <<"http://jabber.org/protocol/pubsub#outcast-affiliation">>).
-define(NS_PUBSUB_PERSISTENT_ITEMS_b,
  <<"http://jabber.org/protocol/pubsub#persistent-items">>).
-define(NS_PUBSUB_PRESENCE_NOTIFICATIONS_b,
  <<"http://jabber.org/protocol/pubsub#presence-notifications">>).
-define(NS_PUBSUB_PRESENCE_SUBSCRIBE_b,
  <<"http://jabber.org/protocol/pubsub#presence-subscribe">>).
-define(NS_PUBSUB_PUBLISH_b,
  <<"http://jabber.org/protocol/pubsub#publish">>).
-define(NS_PUBSUB_PUBLISH_OPTIONS_b,
  <<"http://jabber.org/protocol/pubsub#publish-options">>).
-define(NS_PUBSUB_PUBLISH_ONLY_AFFILIATION_b,
  <<"http://jabber.org/protocol/pubsub#publish-only-affiliation">>).
-define(NS_PUBSUB_PUBLISHER_AFFILIATION_b,
  <<"http://jabber.org/protocol/pubsub#publisher-affiliation">>).
-define(NS_PUBSUB_PURGE_NODES_b,
  <<"http://jabber.org/protocol/pubsub#purge-nodes">>).
-define(NS_PUBSUB_RETRACT_ITEMS_b,
  <<"http://jabber.org/protocol/pubsub#retract-items">>).
-define(NS_PUBSUB_RETRIEVE_AFFILIATIONS_b,
  <<"http://jabber.org/protocol/pubsub#retrieve-affiliations">>).
-define(NS_PUBSUB_RETRIEVE_DEFAULT_b,
  <<"http://jabber.org/protocol/pubsub#retrieve-default">>).
-define(NS_PUBSUB_RETRIEVE_ITEMS_b,
  <<"http://jabber.org/protocol/pubsub#retrieve-items">>).
-define(NS_PUBSUB_RETRIEVE_SUBSCRIPTIONS_b,
  <<"http://jabber.org/protocol/pubsub#retrieve-subscriptions">>).
-define(NS_PUBSUB_SUBSCRIBE_b,
  <<"http://jabber.org/protocol/pubsub#subscribe">>).
-define(NS_PUBSUB_SUBSCRIPTION_OPTIONS_b,
  <<"http://jabber.org/protocol/pubsub#subscription-options">>).
-define(NS_PUBSUB_SUBSCRIPTION_NOTIFICATIONS_b,
  <<"http://jabber.org/protocol/pubsub#subscription-notifications">>).



% Defined by XEP-0065: SOCKS5 Bytestreams.
-define(NS_BYTESTREAMS,              'http://jabber.org/protocol/bytestreams').
-define(NS_BYTESTREAMS_s,            "http://jabber.org/protocol/bytestreams").
-define(NS_BYTESTREAMS_b,            <<"http://jabber.org/protocol/bytestreams">>).

% Defined by XEP-0066: Out of Band Data.

%% How about NS_OOB instead ?
-define(NS_OOBD_IQ,                  'jabber:iq:oob').
-define(NS_OOBD_X,                   'jabber:x:oob').

-define(NS_OOBD_IQ_s,                "jabber:iq:oob").
-define(NS_OOBD_X_s,                 "jabber:x:oob").

-define(NS_OOBD_IQ_b,                <<"jabber:iq:oob">>).
-define(NS_OOBD_X_b,                 <<"jabber:x:oob">>).

% Defined by XEP-0070: Verifying HTTP Requests via XMPP.
-define(NS_HTTP_AUTH,                'http://jabber.org/protocol/http-auth').
-define(NS_HTTP_AUTH_s,              "http://jabber.org/protocol/http-auth").
-define(NS_HTTP_AUTH_b,              <<"http://jabber.org/protocol/http-auth">>).

% Defined by XEP-0071: XHTML-IM.
-define(NS_XHTML_IM,                 'http://jabber.org/protocol/xhtml-im').
-define(NS_XHTML_IM_s,               "http://jabber.org/protocol/xhtml-im").
-define(NS_XHTML_IM_b,               <<"http://jabber.org/protocol/xhtml-im">>).

% Defined by XEP-0072: SOAP Over XMPP.
-define(NS_SOAP_FAULT,               'http://jabber.org/protocol/soap#fault').
-define(NS_SOAP_FAULT_s,             "http://jabber.org/protocol/soap#fault").
-define(NS_SOAP_FAULT_b,             <<"http://jabber.org/protocol/soap#fault">>).

% Defined by XEP-0077: In-Band Registration.
-define(NS_INBAND_REGISTER,          'jabber:iq:register').
-define(NS_INBAND_REGISTER_FEAT,     'http://jabber.org/features/iq-register').

-define(NS_INBAND_REGISTER_s,        "jabber:iq:register").
-define(NS_INBAND_REGISTER_FEAT_s,   "http://jabber.org/features/iq-register").

-define(NS_INBAND_REGISTER_b,        <<"jabber:iq:register">>).
-define(NS_INBAND_REGISTER_FEAT_b,   <<"http://jabber.org/features/iq-register">>).

% Defined by XEP-0078: Non-SASL Authentication.
-define(NS_LEGACY_AUTH,              'jabber:iq:auth').
-define(NS_LEGACY_AUTH_FEAT,         'http://jabber.org/features/iq-aut').

-define(NS_LEGACY_AUTH_s,            "jabber:iq:auth").
-define(NS_LEGACY_AUTH_FEAT_s,       "http://jabber.org/features/iq-aut").

-define(NS_LEGACY_AUTH_b,            <<"jabber:iq:auth">>).
-define(NS_LEGACY_AUTH_FEAT_b,       <<"http://jabber.org/features/iq-aut">>).


% Defined by XEP-0079: Advanced Message Processing.
-define(NS_AMP,                      'http://jabber.org/protocol/amp').
-define(NS_AMP_ERRORS,               'http://jabber.org/protocol/amp#error').
-define(NS_AMP_FEAT,                 'http://jabber.org/features/amp').

-define(NS_AMP_s,                    "http://jabber.org/protocol/amp").
-define(NS_AMP_ERRORS_s,             "http://jabber.org/protocol/amp#error").
-define(NS_AMP_FEAT_s,               "http://jabber.org/features/amp").

-define(NS_AMP_b,                    <<"http://jabber.org/protocol/amp">>).
-define(NS_AMP_ERRORS_b,             <<"http://jabber.org/protocol/amp#error">>).
-define(NS_AMP_FEAT_b,               <<"http://jabber.org/features/amp">>).

% Defined by XEP-0080: User Location.
-define(NS_GEOLOC,                   'http://jabber.org/protocol/geoloc').
-define(NS_GEOLOC_s,                 "http://jabber.org/protocol/geoloc").
-define(NS_GEOLOC_b,                 <<"http://jabber.org/protocol/geoloc">>).

% Defined by XEP-0083: Nested Roster Groups.
-define(NS_ROSTER_DELIMITER,         'roster:delimiter').
-define(NS_ROSTER_DELIMITER_s,       "roster:delimiter").
-define(NS_ROSTER_DELIMITER_b,       <<"roster:delimiter">>).

% Defined by XEP-0084: User Avatar.
-define(NS_USER_AVATAR_DATA,         'urn:xmpp:avatar:data').
-define(NS_USER_AVATAR_DATA_s,       "urn:xmpp:avatar:data").
-define(NS_USER_AVATAR_DATA_b,       <<"urn:xmpp:avatar:data">>).

-define(NS_USER_AVATAR_METADATA,     'urn:xmpp:avatar:metadata').
-define(NS_USER_AVATAR_METADATA_s,   "urn:xmpp:avatar:metadata").
-define(NS_USER_AVATAR_METADATA_b,   <<"urn:xmpp:avatar:metadata">>).

% Defined by XEP-0085: Chat State Notifications
-define(NS_CHATSTATES,               'http://jabber.org/protocol/chatstates').
-define(NS_CHATSTATES_s,             "http://jabber.org/protocol/chatstates").
-define(NS_CHATSTATES_b,             <<"http://jabber.org/protocol/chatstates">>).

% Defined by XEP-0090: Entity Time.
-define(NS_TIME_OLD,                 'jabber:iq:time').
-define(NS_TIME_OLD_s,               "jabber:iq:time").
-define(NS_TIME_OLD_b,               <<"jabber:iq:time">>).

% Defined by XEP-0091: Delayed Delivery.
-define(NS_DELAY_OLD,                'jabber:x:delay').
-define(NS_DELAY_OLD_s,              "jabber:x:delay").
-define(NS_DELAY_OLD_b,              <<"jabber:x:delay">>).

% Defined by XEP-0092: Software Version.
-define(NS_SOFT_VERSION,             'jabber:iq:version').
-define(NS_SOFT_VERSION_s,           "jabber:iq:version").
-define(NS_SOFT_VERSION_b,           <<"jabber:iq:version">>).

% Defined by XEP-0093: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE_OLD,      'jabber:x:roster').
-define(NS_ROSTER_EXCHANGE_OLD_s,    "jabber:x:roster").
-define(NS_ROSTER_EXCHANGE_OLD_b,    <<"jabber:x:roster">>).

% Defined by XEP-0095: Stream Initiation.
-define(NS_SI,                       'http://jabber.org/protocol/si').
-define(NS_SI_s,                     "http://jabber.org/protocol/si").
-define(NS_SI_b,                     <<"http://jabber.org/protocol/si">>).

% Defined by XEP-0096: File Transfer.
-define(NS_FILE_TRANSFERT,
  'http://jabber.org/protocol/si/profile/file-transfer').
-define(NS_FILE_TRANSFERT_s,
  "http://jabber.org/protocol/si/profile/file-transfer").
-define(NS_FILE_TRANSFERT_b,
  <<"http://jabber.org/protocol/si/profile/file-transfer">>).

% Defined by XEP-0100: Gateway Interaction.
-define(NS_GATEWAY,                  'jabber:iq:gateway').
-define(NS_GATEWAY_s,                "jabber:iq:gateway").
-define(NS_GATEWAY_b,                <<"jabber:iq:gateway">>).

% Defined by XEP-0107: User Mood.
-define(NS_USER_MOOD,                'http://jabber.org/protocol/mood').
-define(NS_USER_MOOD_s,              "http://jabber.org/protocol/mood").
-define(NS_USER_MOOD_b,              <<"http://jabber.org/protocol/mood">>).

% Defined by XEP-0108: User Activity.
-define(NS_USER_ACTIVITY,            'http://jabber.org/protocol/activity').
-define(NS_USER_ACTIVITY_s,          "http://jabber.org/protocol/activity").
-define(NS_USER_ACTIVITY_b,          <<"http://jabber.org/protocol/activity">>).

% Defined by XEP-0112: User Physical Location (Deferred).
-define(NS_USER_PHYSLOC,             'http://jabber.org/protocol/physloc').
-define(NS_USER_PHYSLOC_s,           "http://jabber.org/protocol/physloc").
-define(NS_USER_PHYSLOC_b,           <<"http://jabber.org/protocol/physloc">>).

% Defined by XEP-0114: Jabber Component Protocol.
-define(NS_COMPONENT_ACCEPT,         'jabber:component:accept').
-define(NS_COMPONENT_CONNECT,        'jabber:component:connect').

-define(NS_COMPONENT_ACCEPT_s,       "jabber:component:accept").
-define(NS_COMPONENT_CONNECT_s,      "jabber:component:connect").

-define(NS_COMPONENT_ACCEPT_b,       <<"jabber:component:accept">>).
-define(NS_COMPONENT_CONNECT_b,      <<"jabber:component:connect">>).

% Defined by XEP-0115: Entity Capabilities.
-define(NS_CAPS,                     'http://jabber.org/protocol/caps').
-define(NS_CAPS_s,                   "http://jabber.org/protocol/caps").
-define(NS_CAPS_b,                   <<"http://jabber.org/protocol/caps">>).

% Defined by XEP-0118: User Tune.
-define(NS_USER_TUNE,                'http://jabber.org/protocol/tune').
-define(NS_USER_TUNE_s,              "http://jabber.org/protocol/tune").
-define(NS_USER_TUNE_b,              <<"http://jabber.org/protocol/tune">>).

% Defined by XEP-0122: Data Forms Validation.
-define(NS_DATA_FORMS_VALIDATE,
  'http://jabber.org/protocol/xdata-validate').
-define(NS_DATA_FORMS_VALIDATE_s,
  "http://jabber.org/protocol/xdata-validate").
-define(NS_DATA_FORMS_VALIDATE_b,
  <<"http://jabber.org/protocol/xdata-validate">>).

% Defined by XEP-0124: Bidirectional-streams Over Synchronous HTTP.
-define(NS_BOSH,                     'urn:xmpp:xbosh').
-define(NS_BOSH_s,                   "urn:xmpp:xbosh").
-define(NS_BOSH_b,                   <<"urn:xmpp:xbosh">>).

-define(NS_HTTP_BIND,                'http://jabber.org/protocol/httpbind').
-define(NS_HTTP_BIND_s,              "http://jabber.org/protocol/httpbind").
-define(NS_HTTP_BIND_b,              <<"http://jabber.org/protocol/httpbind">>).

% Defined by XEP-0130: Waiting Lists.
-define(NS_WAITING_LIST,             'http://jabber.org/protocol/waitinglist').
-define(NS_WAITING_LIST_s,           "http://jabber.org/protocol/waitinglist").
-define(NS_WAITING_LIST_b,           <<"http://jabber.org/protocol/waitinglist">>).

% Defined by XEP-0131: Stanza Headers and Internet Metadata (SHIM).
-define(NS_SHIM,                     'http://jabber.org/protocol/shim').
-define(NS_SHIM_s,                   "http://jabber.org/protocol/shim").
-define(NS_SHIM_b,                   <<"http://jabber.org/protocol/shim">>).

% Defined by XEP-0133: Service Administration.
-define(NS_ADMIN,                     'http://jabber.org/protocol/admin').
-define(NS_ADMIN_s,                   "http://jabber.org/protocol/admin").
-define(NS_ADMIN_b,                   <<"http://jabber.org/protocol/admin">>).

% Defined by XEP-0136: Message Archiving.
-define(NS_ARCHIVING,                'urn:xmpp:archive').
-define(NS_ARCHIVING_s,              "urn:xmpp:archive").
-define(NS_ARCHIVING_b,              <<"urn:xmpp:archive">>).

% Defined by XEP-0137: Publishing Stream Initiation Requests.
-define(NS_SI_PUB,                   'http://jabber.org/protocol/sipub').
-define(NS_SI_PUB_s,                 "http://jabber.org/protocol/sipub").
-define(NS_SI_PUB_b,                 <<"http://jabber.org/protocol/sipub">>).

% Defined by XEP-0138: Stream Compression.
-define(NS_COMPRESS,                 'http://jabber.org/protocol/compress').
-define(NS_COMPRESS_FEAT,            'http://jabber.org/features/compress').

-define(NS_COMPRESS_s,               "http://jabber.org/protocol/compress").
-define(NS_COMPRESS_FEAT_s,          "http://jabber.org/features/compress").

-define(NS_COMPRESS_b,               <<"http://jabber.org/protocol/compress">>).
-define(NS_COMPRESS_FEAT_b,          <<"http://jabber.org/features/compress">>).

% Defined by XEP-0141: Data Forms Layout.
-define(NS_DATA_FORMS_LAYOUT,
  'http://jabber.org/protocol/xdata-layout').
-define(NS_DATA_FORMS_LAYOUT_s,
  "http://jabber.org/protocol/xdata-layout").
-define(NS_DATA_FORMS_LAYOUT_b,
  <<"http://jabber.org/protocol/xdata-layout">>).

% Defined by XEP-0144: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE,          'http://jabber.org/protocol/rosterx').
-define(NS_ROSTER_EXCHANGE_s,        "http://jabber.org/protocol/rosterx").
-define(NS_ROSTER_EXCHANGE_b,        <<"http://jabber.org/protocol/rosterx">>).

% Defined by XEP-0145: Annotations.
-define(NS_ROSTER_NOTES,             'storage:rosternotes').
-define(NS_ROSTER_NOTES_s,           "storage:rosternotes").
-define(NS_ROSTER_NOTES_b,           <<"storage:rosternotes">>).

% Defined by XEP-0153: vCard-Based Avatars.
-define(NS_VCARD_UPDATE,             'vcard-temp:x:update').
-define(NS_VCARD_UPDATE_s,           "vcard-temp:x:update").
-define(NS_VCARD_UPDATE_b,           <<"vcard-temp:x:update">>).

% Defined by XEP-0154: User Profile.
-define(NS_USER_PROFILE,             'urn:xmpp:tmp:profile').
-define(NS_USER_PROFILE_s,           "urn:xmpp:tmp:profile").
-define(NS_USER_PROFILE_b,           <<"urn:xmpp:tmp:profile">>).

% Defined by XEP-0155: Stanza Session Negotiation.
-define(NS_SSN,                      'urn:xmpp:ssn').
-define(NS_SSN_s,                    "urn:xmpp:ssn").
-define(NS_SSN_b,                    <<"urn:xmpp:ssn">>).

% Defined by XEP-0157: Contact Addresses for XMPP Services.
-define(NS_SERVERINFO,               'http://jabber.org/network/serverinfo').
-define(NS_SERVERINFO_s,             "http://jabber.org/network/serverinfo").
-define(NS_SERVERINFO_b,             <<"http://jabber.org/network/serverinfo">>).

% Defined by XEP-0158: CAPTCHA Forms.

-define(NS_CAPTCHA,                  'urn:xmpp:captcha').
-define(NS_CAPTCHA_s,                "urn:xmpp:captcha").
-define(NS_CAPTCHA_b,                <<"urn:xmpp:captcha">>).

%% Deferred : XEP-0158: Robot Challenges
-define(NS_ROBOT_CHALLENGE,          'urn:xmpp:tmp:challenge').
-define(NS_ROBOT_CHALLENGE_s,        "urn:xmpp:tmp:challenge").
-define(NS_ROBOT_CHALLENGE_b,        <<"urn:xmpp:tmp:challenge">>).

% Defined by XEP-0160: Best Practices for Handling Offline Messages.
-define(NS_MSGOFFLINE,               'msgoffline').
-define(NS_MSGOFFLINE_s,             "msgoffline").
-define(NS_MSGOFFLINE_b,             <<"msgoffline">>).

% Defined by XEP-0161: Abuse Reporting.
-define(NS_ABUSE_REPORTING,          'urn:xmpp:tmp:abuse').
-define(NS_ABUSE_REPORTING_s,        "urn:xmpp:tmp:abuse").
-define(NS_ABUSE_REPORTING_b,        <<"urn:xmpp:tmp:abuse">>).

% Defined by XEP-0166: Jingle.
-define(NS_JINGLE,                   'urn:xmpp:tmp:jingle').
-define(NS_JINGLE_ERRORS,            'urn:xmpp:tmp:jingle:errors').

-define(NS_JINGLE_s,                 "urn:xmpp:tmp:jingle").
-define(NS_JINGLE_ERRORS_s,          "urn:xmpp:tmp:jingle:errors").

-define(NS_JINGLE_b,                 <<"urn:xmpp:tmp:jingle">>).
-define(NS_JINGLE_ERRORS_b,          <<"urn:xmpp:tmp:jingle:errors">>).

% Defined by XEP-0167: Jingle RTP Sessions.
-define(NS_JINGLE_RPT,               'urn:xmpp:tmp:jingle:apps:rtp').
-define(NS_JINGLE_RPT_INFO,          'urn:xmpp:tmp:jingle:apps:rtp:info').

-define(NS_JINGLE_RPT_s,             "urn:xmpp:tmp:jingle:apps:rtp").
-define(NS_JINGLE_RPT_INFO_s,        "urn:xmpp:tmp:jingle:apps:rtp:info").

-define(NS_JINGLE_RPT_b,             <<"urn:xmpp:tmp:jingle:apps:rtp">>).
-define(NS_JINGLE_RPT_INFO_b,        <<"urn:xmpp:tmp:jingle:apps:rtp:info">>).

% Defined by XEP-0168: Resource Application Priority.
-define(NS_RAP,
  'http://www.xmpp.org/extensions/xep-0168.html#ns').
-define(NS_RAP_ROUTE,
  'http://www.xmpp.org/extensions/xep-0168.html#ns-route').

-define(NS_RAP_s,
  "http://www.xmpp.org/extensions/xep-0168.html#ns").
-define(NS_RAP_ROUTE_s,
  "http://www.xmpp.org/extensions/xep-0168.html#ns-route").

-define(NS_RAP_b,
  <<"http://www.xmpp.org/extensions/xep-0168.html#ns">>).
-define(NS_RAP_ROUTE_b,
  <<"http://www.xmpp.org/extensions/xep-0168.html#ns-route">>).

% Defined by XEP-0171: Language Translation.
-define(NS_LANG_TRANS,               'urn:xmpp:langtrans').
-define(NS_LANG_TRANS_ITEMS,         'urn:xmpp:langtrans#items').
-define(NS_LANG_TRANS_s,             "urn:xmpp:langtrans").
-define(NS_LANG_TRANS_ITEMS_s,       "urn:xmpp:langtrans#items").
-define(NS_LANG_TRANS_b,             <<"urn:xmpp:langtrans">>).
-define(NS_LANG_TRANS_ITEMS_b,       <<"urn:xmpp:langtrans#items">>).

% Defined by XEP-0172: User Nickname.
-define(NS_USER_NICKNAME,            'http://jabber.org/protocol/nick').
-define(NS_USER_NICKNAME_s,          "http://jabber.org/protocol/nick").
-define(NS_USER_NICKNAME_b,          <<"http://jabber.org/protocol/nick">>).

% Defined by XEP-0176: Jingle ICE-UDP Transport Method.
-define(NS_JINGLE_ICE_UDP,           'urn:xmpp:tmp:jingle:transports:ice-udp').
-define(NS_JINGLE_ICE_UDP_s,         "urn:xmpp:tmp:jingle:transports:ice-udp").
-define(NS_JINGLE_ICE_UDP_b,         <<"urn:xmpp:tmp:jingle:transports:ice-udp">>).

% Defined by XEP-0177: Jingle Raw UDP Transport Method.
-define(NS_JINGLE_RAW_UDP,
  'urn:xmpp:tmp:jingle:transports:raw-udp').
-define(NS_JINGLE_RAW_UDP_INFO,
  'urn:xmpp:tmp:jingle:transports:raw-udp:info').
-define(NS_JINGLE_RAW_UDP_s,
  "urn:xmpp:tmp:jingle:transports:raw-udp").
-define(NS_JINGLE_RAW_UDP_INFO_s,
  "urn:xmpp:tmp:jingle:transports:raw-udp:info").
-define(NS_JINGLE_RAW_UDP_b,
  <<"urn:xmpp:tmp:jingle:transports:raw-udp">>).
-define(NS_JINGLE_RAW_UDP_INFO_b,
  <<"urn:xmpp:tmp:jingle:transports:raw-udp:info">>).

% Defined by XEP-0181: Jingle DTMF.
-define(NS_JINGLE_DTMF_0,            'urn:xmpp:jingle:dtmf:0').
-define(NS_JINGLE_DTMF_0_s,          "urn:xmpp:jingle:dtmf:0").
-define(NS_JINGLE_DTMF_0_b,          <<"urn:xmpp:jingle:dtmf:0">>).

%% Deferred
-define(NS_JINGLE_DTMF,              'urn:xmpp:tmp:jingle:dtmf').
-define(NS_JINGLE_DTMF_s,            "urn:xmpp:tmp:jingle:dtmf").
-define(NS_JINGLE_DTMF_b,            <<"urn:xmpp:tmp:jingle:dtmf">>).

% Defined by XEP-0184: Message Receipts.
-define(NS_RECEIPTS,                 'urn:xmpp:receipts').
-define(NS_RECEIPTS_s,               "urn:xmpp:receipts").
-define(NS_RECEIPTS_b,               <<"urn:xmpp:receipts">>).

% Defined by XEP-0186: Invisible Command.
-define(NS_INVISIBLE_COMMAND_0,      'urn:xmpp:invisible:0').
-define(NS_INVISIBLE_COMMAND_0_s,    "urn:xmpp:invisible:0").
-define(NS_INVISIBLE_COMMAND_0_b,    <<"urn:xmpp:invisible:0">>).

%% Deferred
-define(NS_INVISIBLE_COMMAND,        'urn:xmpp:tmp:invisible').
-define(NS_INVISIBLE_COMMAND_s,      "urn:xmpp:tmp:invisible").
-define(NS_INVISIBLE_COMMAND_b,      <<"urn:xmpp:tmp:invisible">>).

% Defined by XEP-0189: Public Key Publishing.
-define(NS_PUBKEY_1,                 'urn:xmpp:pubkey:1').
-define(NS_PUBKEY_1_s,               "urn:xmpp:pubkey:1").
-define(NS_PUBKEY_1_b,               <<"urn:xmpp:pubkey:1">>).

-define(NS_ATTEST_1,                 'urn:xmpp:attest:1').
-define(NS_ATTEST_1_s,               "urn:xmpp:attest:1").
-define(NS_ATTEST_1_b,               <<"urn:xmpp:attest:1">>).

-define(NS_REVOKE_1,                 'urn:xmpp:revoke:1').
-define(NS_REVOKE_1_s,               "urn:xmpp:revoke:1").
-define(NS_REVOKE_1_b,               <<"urn:xmpp:revoke:1">>).

%% Deferred
-define(NS_PUBKEY_TMP,               'urn:xmpp:tmp:pubkey').
-define(NS_PUBKEY_TMP_s,             "urn:xmpp:tmp:pubkey").
-define(NS_PUBKEY_TMP_b,             <<"urn:xmpp:tmp:pubkey">>).

% Defined by XEP-0191: Simple Communications Blocking.
-define(NS_BLOCKING,                 'urn:xmpp:blocking').
-define(NS_BLOCKING_ERRORS,          'urn:xmpp:blocking:errors').
-define(NS_BLOCKING_s,               "urn:xmpp:blocking").
-define(NS_BLOCKING_ERRORS_s,        "urn:xmpp:blocking:errors").
-define(NS_BLOCKING_b,               <<"urn:xmpp:blocking">>).
-define(NS_BLOCKING_ERRORS_b,        <<"urn:xmpp:blocking:errors">>).

% Defined by XEP-0194: User Chatting.

-define(NS_USER_CHATTING_0,          'urn:xmpp:chatting:0').
-define(NS_USER_CHATTING_0_s,        "urn:xmpp:chatting:0").
-define(NS_USER_CHATTING_0_b,        <<"urn:xmpp:chatting:0">>).

%% Deferred
-define(NS_USER_CHATTING,
  'http://www.xmpp.org/extensions/xep-0194.html#ns').
-define(NS_USER_CHATTING_s,
  "http://www.xmpp.org/extensions/xep-0194.html#ns").
-define(NS_USER_CHATTING_b,
  <<"http://www.xmpp.org/extensions/xep-0194.html#ns">>).

% Defined by XEP-0195: User Browsing.
-define(NS_USER_BROWSING_0,          'urn:xmpp:browsing:0').
-define(NS_USER_BROWSING_0_s,        "urn:xmpp:browsing:0").
-define(NS_USER_BROWSING_0_b,        <<"urn:xmpp:browsing:0">>).

%% Deferred
-define(NS_USER_BROWSING,
  'http://www.xmpp.org/extensions/xep-0195.html#ns').
-define(NS_USER_BROWSING_s,
  "http://www.xmpp.org/extensions/xep-0195.html#ns").
-define(NS_USER_BROWSING_b,
  <<"http://www.xmpp.org/extensions/xep-0195.html#ns">>).

% Defined by XEP-0196: User Gaming.
-define(NS_USER_GAMING_0,            'urn:xmpp:gaming:0').
-define(NS_USER_GAMING_0_s,          "urn:xmpp:gaming:0").
-define(NS_USER_GAMING_0_b,          <<"urn:xmpp:gaming:0">>).

%% Deferred
-define(NS_USER_GAMING,
  'http://www.xmpp.org/extensions/xep-0196.html#ns').
-define(NS_USER_GAMING_s,
  "http://www.xmpp.org/extensions/xep-0196.html#ns").
-define(NS_USER_GAMING_b,
  <<"http://www.xmpp.org/extensions/xep-0196.html#ns">>).

% Defined by XEP-0197: User Viewing.
-define(NS_USER_VIEWING_0,           'urn:xmpp:viewing:0').
-define(NS_USER_VIEWING_0_s,         "urn:xmpp:viewing:0").
-define(NS_USER_VIEWING_0_b,         <<"urn:xmpp:viewing:0">>).

%% Deferred
-define(NS_USER_VIEWING,
  'http://www.xmpp.org/extensions/xep-0197.html#ns').
-define(NS_USER_VIEWING_s,
  "http://www.xmpp.org/extensions/xep-0197.html#ns").

% Defined by XEP-0198: Stanza Acknowledgements.
-define(NS_STREAM_MGNT_2,            'urn:xmpp:sm:2').
-define(NS_STREAM_MGNT_2_s,          "urn:xmpp:sm:2").
-define(NS_STREAM_MGNT_2_b,          <<"urn:xmpp:sm:2">>).

%% Deferred
-define(NS_STREAM_MGNT_1,            'urn:xmpp:sm:1').
-define(NS_STREAM_MGNT_1_s,          "urn:xmpp:sm:1").
-define(NS_STREAM_MGNT_1_b,          <<"urn:xmpp:sm:1">>).

-define(NS_STREAM_MGNT_0,            'urn:xmpp:sm:0').
-define(NS_STREAM_MGNT_0_s,          "urn:xmpp:sm:0").
-define(NS_STREAM_MGNT_0_b,          <<"urn:xmpp:sm:0">>).

-define(NS_STANZA_ACK,
  'http://www.xmpp.org/extensions/xep-0198.html#ns').
-define(NS_STANZA_ACK_s,
  "http://www.xmpp.org/extensions/xep-0198.html#ns").

% Defined by XEP-0199: XMPP Ping.
-define(NS_PING,                     'urn:xmpp:ping').
-define(NS_PING_s,                   "urn:xmpp:ping").
-define(NS_PING_b,                   <<"urn:xmpp:ping">>).

% Defined by XEP-0202: Entity Time.
-define(NS_TIME,                     'urn:xmpp:time').
-define(NS_TIME_s,                   "urn:xmpp:time").
-define(NS_TIME_b,                   <<"urn:xmpp:time">>).

% Defined by XEP-0203: Delayed Delivery.
-define(NS_DELAY,                    'urn:xmpp:delay').
-define(NS_DELAY_s,                  "urn:xmpp:delay").
-define(NS_DELAY_b,                  <<"urn:xmpp:delay">>).

% Defined by XEP-0206: XMPP Over BOSH.
-define(NS_XBOSH,                    'urn:xmpp:xbosh').
-define(NS_XBOSH_s,                  "urn:xmpp:xbosh").
-define(NS_XBOSH_b,                  <<"urn:xmpp:xbosh">>).
-define(NS_XBOSH_pfx,                "xmpp").

% Defined by XEP-0208: Bootstrapping Implementation of Jingle.
-define(NS_JINGLE_BOOTSTRAPING,
  'http://www.xmpp.org/extensions/xep-0208.html#ns').
-define(NS_JINGLE_BOOTSTRAPING_s,
  "http://www.xmpp.org/extensions/xep-0208.html#ns").
-define(NS_JINGLE_BOOTSTRAPING_b,
  <<"http://www.xmpp.org/extensions/xep-0208.html#ns">>).

% Defined by XEP-0209: Metacontacts.
-define(NS_METACONTACTS,             'storage:metacontacts').
-define(NS_METACONTACTS_s,           "storage:metacontacts").
-define(NS_METACONTACTS_b,           <<"storage:metacontacts">>).

% Defined by XEP-0215: External Service Discovery.
-define(NS_EXTERNAL_DISCO_0,           'urn:xmpp:extdisco:0').
-define(NS_EXTERNAL_DISCO_0_s,         "urn:xmpp:extdisco:0").
-define(NS_EXTERNAL_DISCO_0_b,         <<"urn:xmpp:extdisco:0">>).

%% Deferred
-define(NS_EXTERNAL_DISCO,
  'http://www.xmpp.org/extensions/xep-0215.html#ns').
-define(NS_EXTERNAL_DISCO_s,
  "http://www.xmpp.org/extensions/xep-0215.html#ns").
-define(NS_EXTERNAL_DISCO_b,
  <<"http://www.xmpp.org/extensions/xep-0215.html#ns">>).

% Defined by XEP-0220: Server Dialback.
-define(NS_DIALBACK,                 'jabber:server:dialback').
-define(NS_DIALBACK_FEAT,            'urn:xmpp:features:dialback').
-define(NS_DIALBACK_pfx,             "db").
-define(NS_DIALBACK_s,               "jabber:server:dialback").
-define(NS_DIALBACK_FEAT_s,          "urn:xmpp:features:dialback").
-define(NS_DIALBACK_b,               <<"jabber:server:dialback">>).
-define(NS_DIALBACK_FEAT_b,          <<"urn:xmpp:features:dialback">>).

% Defined by XEP-0221: Data Forms Media Element.
%% How about NS_DATA ?
-define(NS_DATA_FORMS_MEDIA,         'urn:xmpp:media-element').
-define(NS_DATA_FORMS_MEDIA_s,       "urn:xmpp:media-element").
-define(NS_DATA_FORMS_MEDIA_b,       <<"urn:xmpp:media-element">>).

%% Deferred
-define(NS_DATA_FORMS_MEDIA_TMP,     'urn:xmpp:tmp:media-element').
-define(NS_DATA_FORMS_MEDIA_TMP_s,   "urn:xmpp:tmp:media-element").
-define(NS_DATA_FORMS_MEDIA_TMP_b,   <<"urn:xmpp:tmp:media-element">>).

% Defined by XEP-0224: Attention.
-define(NS_ATTENTION_0,              'urn:xmpp:attention:0').
-define(NS_ATTENTION_0_s,            "urn:xmpp:attention:0").
-define(NS_ATTENTION_0_b,            <<"urn:xmpp:attention:0">>).

%% Deferred
-define(NS_ATTENTION,
  'http://www.xmpp.org/extensions/xep-0224.html#ns').
-define(NS_ATTENTION_s,
  "http://www.xmpp.org/extensions/xep-0224.html#ns").
-define(NS_ATTENTION_b,
  <<"http://www.xmpp.org/extensions/xep-0224.html#ns">>).

% Defined by XEP-0225: Component Connections.
-define(NS_COMPONENT_CONNECTION_0,   'urn:xmpp:component:0').
-define(NS_COMPONENT_CONNECTION_0_s, "urn:xmpp:component:0").
-define(NS_COMPONENT_CONNECTION_0_b, <<"urn:xmpp:component:0">>).

%% Deferred
-define(NS_COMPONENT_CONNECTION,     'urn:xmpp:tmp:component').
-define(NS_COMPONENT_CONNECTION_s,   "urn:xmpp:tmp:component").
-define(NS_COMPONENT_CONNECTION_b,   <<"urn:xmpp:tmp:component">>).

% Defined by XEP-0227: Portable Import/Export Format for XMPP-IM Servers.
-define(NS_SERVER_IMPORT_EXPORT,
  'http://www.xmpp.org/extensions/xep-0227.html#ns').
-define(NS_SERVER_IMPORT_EXPORT_s,
  "http://www.xmpp.org/extensions/xep-0227.html#ns").
-define(NS_SERVER_IMPORT_EXPORT_b,
  <<"http://www.xmpp.org/extensions/xep-0227.html#ns">>).

% Defined by XEP-0231: Data Element.
-define(NS_BOB,                      'urn:xmpp:bob').
-define(NS_BOB_s,                    "urn:xmpp:bob").
-define(NS_BOB_b,                    <<"urn:xmpp:bob">>).

%% Deferred
-define(NS_DATA,                     'urn:xmpp:tmp:data-element').
-define(NS_DATA_s,                   "urn:xmpp:tmp:data-element").
-define(NS_DATA_b,                   <<"urn:xmpp:tmp:data-element">>).

% Defined by XEP-0233: Use of Domain-Based Service Names in XMPP SASL
% Negotiation.
-define(NS_DOMAIN_BASED_NAME,        'urn:xmpp:tmp:domain-based-name').
-define(NS_DOMAIN_BASED_NAME_s,      "urn:xmpp:tmp:domain-based-name").
-define(NS_DOMAIN_BASED_NAME_B,      <<"urn:xmpp:tmp:domain-based-name">>).

% Defined by XEP-0234: Jingle File Transfer.
-define(NS_JINGLE_FT_1,              'urn:xmpp:jingle:apps:file-transfer:1').
-define(NS_JINGLE_FT_1_s,            "urn:xmpp:jingle:apps:file-transfer:1").
-define(NS_JINGLE_FT_1_b,            <<"urn:xmpp:jingle:apps:file-transfer:1">>).

%% Deferred
-define(NS_JINGLE_FILE_TRANSFERT,    'urn:xmpp:tmp:jingle:apps:file-transfer').
-define(NS_JINGLE_FILE_TRANSFERT_s,  "urn:xmpp:tmp:jingle:apps:file-transfer").
-define(NS_JINGLE_FILE_TRANSFERT_b,  <<"urn:xmpp:tmp:jingle:apps:file-transfer">>).

% Defined by XEP-0235: Authorization Tokens.
-define(NS_OAUTH_0,                  'urn:xmpp:oauth:0').
-define(NS_OAUTH_0_s,                "urn:xmpp:oauth:0").
-define(NS_OAUTH_0_b,                <<"urn:xmpp:oauth:0">>).

-define(NS_OAUTH_ERRORS_0,           'urn:xmpp:oauth:0:errors').
-define(NS_OAUTH_ERRORS_0_s,         "urn:xmpp:oauth:0:errors").
-define(NS_OAUTH_ERRORS_0_b,         <<"urn:xmpp:oauth:0:errors">>).

%% Deferred : XEP-0235: Authorization Tokens.
-define(NS_AUTH_TOKEN,               'urn:xmpp:tmp:auth-token').
-define(NS_AUTH_TOKEN_s,             "urn:xmpp:tmp:auth-token").
-define(NS_AUTH_TOKEN_b,             <<"urn:xmpp:tmp:auth-token">>).

% Defined by XEP-0237: Roster Versioning.
-define(NS_ROSTER_VER,               'urn:xmpp:features:rosterver').
-define(NS_ROSTER_VER_s,             "urn:xmpp:features:rosterver").
-define(NS_ROSTER_VER_b,             <<"urn:xmpp:features:rosterver">>).

%% Deferred : XEP-0237: Roster Sequencing.
-define(NS_ROSTER_SEQ,               'urn:xmpp:tmp:roster-sequencing').
-define(NS_ROSTER_SEQ_s,             "urn:xmpp:tmp:roster-sequencing").
-define(NS_ROSTER_SEQ_b,             <<"urn:xmpp:tmp:roster-sequencing">>).

% Defined by XEP-0244: IO Data.
-define(NS_IO_DATA_TMP,              'urn:xmpp:tmp:io-data').
-define(NS_IO_DATA_TMP_s,            "urn:xmpp:tmp:io-data").
-define(NS_IO_DATA_TMP_b,            <<"urn:xmpp:tmp:io-data">>).

% Defined by XEP-0247: Jingle XML Streams.
-define(NS_JINGLE_XML_STREAM_0,      'urn:xmpp:jingle:apps:xmlstream:0').
-define(NS_JINGLE_XML_STREAM_0_s,    "urn:xmpp:jingle:apps:xmlstream:0").
-define(NS_JINGLE_XML_STREAM_0_b,    <<"urn:xmpp:jingle:apps:xmlstream:0">>).

% Deferred
-define(NS_JINGLE_XML_STREAM,        'urn:xmpp:tmp:jingle:apps:xmlstream').
-define(NS_JINGLE_XML_STREAM_s,      "urn:xmpp:tmp:jingle:apps:xmlstream").
-define(NS_JINGLE_XML_STREAM_b,      <<"urn:xmpp:tmp:jingle:apps:xmlstream">>).

% Defined by XEP-0249: Direct MUC Invitations.
-define(NS_JABBER_X_CONF,            'jabber:x:conference').
-define(NS_JABBER_X_CONF_s,          "jabber:x:conference").
-define(NS_JABBER_X_CONF_b,          <<"jabber:x:conference">>).

% Defined by XEP-0251: Jingle Session Transfer.
-define(NS_JINGLE_TRANSFER_0,        'urn:xmpp:jingle:transfer:0').
-define(NS_JABBER_TRANSFER_0_s,      "urn:xmpp:jingle:transfer:0").
-define(NS_JABBER_TRANSFER_0_b,      <<"urn:xmpp:jingle:transfer:0">>).

% Defined by XEP-0253: PubSub Chaining.
-define(NS_PUBSUB_CHAINING,          'http://jabber.org/protocol/pubsub#chaining').
-define(NS_PUBSUB_CHAINING_s,        "http://jabber.org/protocol/pubsub#chaining").
-define(NS_PUBSUB_CHAINING_b,        <<"http://jabber.org/protocol/pubsub#chaining">>).

% Defined by XEP-0254: PubSub Queueing.
-define(NS_PUBSUB_QUEUEING_0,        'urn:xmpp:pubsub:queueing:0').
-define(NS_PUBSUB_QUEUEING_0_s,      "urn:xmpp:pubsub:queueing:0").
-define(NS_PUBSUB_QUEUEING_0_b,      <<"urn:xmpp:pubsub:queueing:0">>).

% Defined by XEP-0255: Location Query.
-define(NS_LOCATION_QUERY_0,         'urn:xmpp:locationquery:0').
-define(NS_LOCATION_QUERY_0_s,       "urn:xmpp:locationquery:0").
-define(NS_LOCATION_QUERY_0_b,       <<"urn:xmpp:locationquery:0">>).

% Defined by XEP-0257: Client Certificate Management for SASL EXTERNAL.
-define(NS_SASL_CERT_0,              'urn:xmpp:saslcert:0').
-define(NS_SASL_CERT_0_s,            "urn:xmpp:saslcert:0").
-define(NS_SASL_CERT_0_b,            <<"urn:xmpp:saslcert:0">>).

% Defined by XEP-0258: Security Labels in XMPP.
-define(NS_SEC_LABEL_0,              'urn:xmpp:sec-label:0').
-define(NS_SEC_LABEL_0_s,            "urn:xmpp:sec-label:0").
-define(NS_SEC_LABEL_0_b,            <<"urn:xmpp:sec-label:0">>).

-define(NS_SEC_LABEL_CATALOG_1,      'urn:xmpp:sec-label:catalog:1').
-define(NS_SEC_LABEL_CATALOG_1_s,    "urn:xmpp:sec-label:catalog:1").
-define(NS_SEC_LABEL_CATALOG_1_b,    <<"urn:xmpp:sec-label:catalog:1">>).

-define(NS_SEC_LABEL_ESS_0,          'urn:xmpp:sec-label:ess:0').
-define(NS_SEC_LABEL_ESS_0_s,        "urn:xmpp:sec-label:ess:0").
-define(NS_SEC_LABEL_ESS_0_b,        <<"urn:xmpp:sec-label:ess:0">>).

% Defined by XEP-0259: Message Mine-ing.
-define(NS_MINE_TMP_0,               'urn:xmpp:tmp:mine:0').
-define(NS_MINE_TMP_0_s,             "urn:xmpp:tmp:mine:0").
-define(NS_MINE_TMP_0_b,             <<"urn:xmpp:tmp:mine:0">>).

% Defined by XEP-0260: Jingle SOCKS5 Bytestreams Transport Method.
-define(NS_JINGLE_TRANSPORTS_S5B_1,
  'urn:xmpp:jingle:transports:s5b:1').
-define(NS_JINGLE_TRANSPORTS_S5B_1_s,
  "urn:xmpp:jingle:transports:s5b:1").
-define(NS_JINGLE_TRANSPORTS_S5B_1_b,
  <<"urn:xmpp:jingle:transports:s5b:1">>).

% Defined by XEP-0261: Jingle In-Band Bytestreams Transport Method.
-define(NS_JINGLE_TRANSPORTS_S5B_0,
  'urn:xmpp:jingle:transports:s5b:0').
-define(NS_JINGLE_TRANSPORTS_S5B_0_s,
  "urn:xmpp:jingle:transports:s5b:0").
-define(NS_JINGLE_TRANSPORTS_S5B_0_b,
  <<"urn:xmpp:jingle:transports:s5b:0">>).

% Defined by XEP-0262: Use of ZRTP in Jingle RTP Sessions.
-define(NS_JINGLE_APPS_RTP_ZRTP_0,
  'urn:xmpp:jingle:apps:rtp:zrtp:0').
-define(NS_JINGLE_APPS_RTP_ZRTP_0_s,
  "urn:xmpp:jingle:apps:rtp:zrtp:0").
-define(NS_JINGLE_APPS_RTP_ZRTP_0_b,
  <<"urn:xmpp:jingle:apps:rtp:zrtp:0">>).

% Defined by XEP-0264: File Transfer Thumbnails.
-define(NS_FT_THUMBS_0,              'urn:xmpp:thumbs:0').
-define(NS_FT_THUMBS_0_s,            "urn:xmpp:thumbs:0").
-define(NS_FT_THUMBS_0_b,            <<"urn:xmpp:thumbs:0">>).

% Defined by XEP-0265: Out-of-Band Stream Data.
-define(NS_JINGLE_APPS_OOB_0,        'urn:xmpp:jingle:apps:out-of-band:0').
-define(NS_JINGLE_APPS_OOB_0_s,      "urn:xmpp:jingle:apps:out-of-band:0").
-define(NS_JINGLE_APPS_OOB_0_b,      <<"urn:xmpp:jingle:apps:out-of-band:0">>).

% Defined by XEP-0268: Incident Reporting.
-define(NS_INCIDENT_REPORT_0,        'urn:xmpp:incident:0').
-define(NS_INCIDENT_REPORT_0_s,      "urn:xmpp:incident:0").
-define(NS_INCIDENT_REPORT_0_b,      <<"urn:xmpp:incident:0">>).

% Defined by XEP-0272: Multiparty Jingle (Muji).
-define(NS_TELEPATHY_MUJI,           'http://telepathy.freedesktop.org/muji').
-define(NS_TELEPATHY_MUJI_s,         "http://telepathy.freedesktop.org/muji").
-define(NS_TELEPATHY_MUJI_b,         <<"http://telepathy.freedesktop.org/muji">>).

% Defined by XEP-0273: Stanza Interception and Filtering Technology (SIFT).
-define(NS_SIFT_1,                   'urn:xmpp:sift:1').
-define(NS_SIFT_1_s,                 "urn:xmpp:sift:1").
-define(NS_SIFT_1_b,                 <<"urn:xmpp:sift:1">>).

% Defined by XEP-0275: Entity Reputation.
-define(NS_REPUTATION_0,             'urn:xmpp:reputation:0').
-define(NS_REPUTATION_0_s,           "urn:xmpp:reputation:0").
-define(NS_REPUTATION_0_b,           <<"urn:xmpp:reputation:0">>).

% Defined by XEP-0276: Temporary Presence Sharing.
-define(NS_TEMPPRES_0,               'urn:xmpp:temppres:0').
-define(NS_TEMPPRES_0_s,             "urn:xmpp:temppres:0").
-define(NS_TEMPPRES_0_b,             <<"urn:xmpp:temppres:0">>).

% Defined by XEP-0277: Microblogging over XMPP.
-define(NS_MUBLOG_0,                 'urn:xmpp:microblog:0').
-define(NS_MUBLOG_0_s,               "urn:xmpp:microblog:0").
-define(NS_MUBLOG_0_b,               <<"urn:xmpp:microblog:0">>).

% Defined by XEP-0278: Jingle Relay Nodes.
-define(NS_JINGLE_RELAY_NODES,       'http://jabber.org/protocol/jinglenodes').
-define(NS_JINGLE_RELAY_NODES_s,     "http://jabber.org/protocol/jinglenodes").
-define(NS_JINGLE_RELAY_NODES_b,     <<"http://jabber.org/protocol/jinglenodes">>).

% Defined by XEP-0279: Server IP Check.
-define(NS_SIC_0,                    'urn:xmpp:sic:0').
-define(NS_SIC_0_s,                  "urn:xmpp:sic:0").
-define(NS_SIC_0_b,                  <<"urn:xmpp:sic:0">>).

% Defined by XHTML 1.0.
-define(NS_XHTML,                    'http://www.w3.org/1999/xhtml').
-define(NS_XHTML_s,                  "http://www.w3.org/1999/xhtml").
-define(NS_XHTML_b,                  <<"http://www.w3.org/1999/xhtml">>).
