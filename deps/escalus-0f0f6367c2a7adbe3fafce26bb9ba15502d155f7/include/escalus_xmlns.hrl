%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Defines for XML namespaces commonly used in XMPP
%%%
%%% This file is based on exmpp_nss.hrl from EXMPP library
%%% @end
%%%===================================================================

% Defined by XML.
-define(NS_XML, <<"http://www.w3.org/XML/1998/namespace">>).

% Defined by XMPP Core (RFC 3920).
-define(NS_XMPP, <<"http://etherx.jabber.org/streams">>).

-define(NS_STREAM_ERRORS, <<"urn:ietf:params:xml:ns:xmpp-streams">>).
-define(NS_TLS, <<"urn:ietf:params:xml:ns:xmpp-tls">>).
-define(NS_SASL, <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_BIND, <<"urn:ietf:params:xml:ns:xmpp-bind">>).
-define(NS_STANZA_ERRORS, <<"urn:ietf:params:xml:ns:xmpp-stanzas">>).

% Defined by XMPP-IM (RFC 3921).
-define(NS_JABBER_CLIENT, <<"jabber:client">>).

-define(NS_JABBER_SERVER, <<"jabber:server">>).

-define(NS_SESSION, <<"urn:ietf:params:xml:ns:xmpp-session">>).

-define(NS_ROSTER, <<"jabber:iq:roster">>).

% Defined by End-to-End Signing and Object Encryption for XMPP (RFC 3923).
-define(NS_E2E, <<"urn:ietf:params:xml:ns:xmpp-e2e">>).

% Defined by XEP-0003: Proxy Accept Socket Service (PASS).
-define(NS_PASS, <<"jabber:iq:pass">>).

% Defined by XEP-0004: Data Forms.
-define(NS_DATA_FORMS, <<"jabber:x:data">>).

% Defined by XEP-0009: Jabber-RPC.
-define(NS_RPC, <<"jabber:iq:rpc">>).

% Defined by XEP-0011: Jabber Browsing.
-define(NS_BROWSE, <<"jabber:iq:browse">>).

% Defined by XEP-0012: Last Activity.
-define(NS_LAST_ACTIVITY, <<"jabber:iq:last">>).

% Defined by XEP-0013: Flexible Offline Message Retrieval.
-define(NS_OFFLINE, <<"http://jabber.org/protocol/offline">>).

% Defined by XEP-0016: Privacy Lists.
-define(NS_PRIVACY, <<"jabber:iq:privacy">>).

% Defined by XEP-0020: Feature Negotiation.
-define(NS_FEATURE_NEG, <<"http://jabber.org/protocol/feature-neg">>).

% Defined by XEP-0022: Message Events.
-define(NS_MESSAGE_EVENT, <<"jabber:x:event">>).

% Defined by XEP-0023: Message Expiration.
-define(NS_MESSAGE_EXPIRE, <<"jabber:x:expire">>).

% Defined by XEP-0027: Current Jabber OpenPGP Usage.
-define(NS_PGP_ENCRYPTED, <<"jabber:x:encrypted">>).
-define(NS_PGP_SIGNED, <<"jabber:x:signed">>).

% Defined by XEP-0030: Service Discovery.
-define(NS_DISCO_INFO, <<"http://jabber.org/protocol/disco#info">>).
-define(NS_DISCO_ITEMS, <<"http://jabber.org/protocol/disco#items">>).

% Defined by XEP-0033: Extended Stanza Addressing.
-define(NS_ADDRESS, <<"http://jabber.org/protocol/address">>).

% Defined by XEP-0039: Statistics Gathering.
-define(NS_STATS, <<"http://jabber.org/protocol/stats">>).

% Defined by XEP-0045: Multi-User Chat.
-define(NS_MUC, <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_ADMIN, <<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER, <<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE, <<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_USER, <<"http://jabber.org/protocol/muc#user">>).

% Defined by XEP-0047: In-Band Bytestreams.
-define(NS_IBB, <<"http://jabber.org/protocol/ibb">>).

% Defined by XEP-0048: Bookmarks.
-define(NS_BOOKMARKS, <<"storage:bookmarks">>).

% Defined by XEP-0049: Private XML Storage.
-define(NS_PRIVATE, <<"jabber:iq:private">>).

% Defined by XEP-0050: Ad-Hoc Commands.
-define(NS_ADHOC, <<"http://jabber.org/protocol/commands">>).

% Defined by XEP-0054: vcard-temp.
-define(NS_VCARD, <<"vcard-temp">>).

% Defined by XEP-0055: Jabber Search.
-define(NS_SEARCH, <<"jabber:iq:search">>).

% Defined by XEP-0059: Result Set Management.
-define(NS_RSM, <<"http://jabber.org/protocol/rsm">>).

% Defined by XEP-0060: Publish-Subscribe.
-define(NS_PUBSUB, <<"http://jabber.org/protocol/pubsub">>).
-define(NS_PUBSUB_ERRORS, <<"http://jabber.org/protocol/pubsub#errors">>).
-define(NS_PUBSUB_EVENT, <<"http://jabber.org/protocol/pubsub#event">>).
-define(NS_PUBSUB_OWNER, <<"http://jabber.org/protocol/pubsub#owner">>).
-define(NS_PUBSUB_SUBSCRIBE_AUTH, <<"http://jabber.org/protocol/pubsub#subscribe_authorization">>).
-define(NS_PUBSUB_SUBSCRIBE_OPTIONS, <<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_NODE_CONFIG, <<"http://jabber.org/protocol/pubsub#node_config">>).

-define(NS_PUBSUB_ACCESS_AUTH, <<"http://jabber.org/protocol/pubsub#access-authorize">>).
-define(NS_PUBSUB_ACCESS_OPEN, <<"http://jabber.org/protocol/pubsub#access-open">>).
-define(NS_PUBSUB_ACCESS_PRESENCE, <<"http://jabber.org/protocol/pubsub#access-presence">>).
-define(NS_PUBSUB_ACCESS_ROSTER, <<"http://jabber.org/protocol/pubsub#access-roster">>).
-define(NS_PUBSUB_ACCESS_WHITELIST, <<"http://jabber.org/protocol/pubsub#access-whitelist">>).
-define(NS_PUBSUB_AUTO_CREATE, <<"http://jabber.org/protocol/pubsub#auto-create">>).
-define(NS_PUBSUB_AUTO_SUBSCRIBE, <<"http://jabber.org/protocol/pubsub#auto-subscribe">>).
-define(NS_PUBSUB_COLLECTIONS, <<"http://jabber.org/protocol/pubsub#collections">>).
-define(NS_PUBSUB_CONFIG_NODE, <<"http://jabber.org/protocol/pubsub#config-node">>).
-define(NS_PUBSUB_CREATE_CONFIGURE, <<"http://jabber.org/protocol/pubsub#create-and-configure">>).
-define(NS_PUBSUB_CREATE_NODES, <<"http://jabber.org/protocol/pubsub#create-nodes">>).
-define(NS_PUBSUB_DELETE_ITEMS, <<"http://jabber.org/protocol/pubsub#delete-items">>).
-define(NS_PUBSUB_DELETE_NODES, <<"http://jabber.org/protocol/pubsub#delete-nodes">>).
-define(NS_PUBSUB_FILTERED_NOTIFICATIONS, <<"http://jabber.org/protocol/pubsub#filtered-notifications">>).
-define(NS_PUBSUB_GET_PENDING, <<"http://jabber.org/protocol/pubsub#get-pending">>).
-define(NS_PUBSUB_INSTANT_NODES, <<"http://jabber.org/protocol/pubsub#instant-nodes">>).
-define(NS_PUBSUB_ITEM_IDS, <<"http://jabber.org/protocol/pubsub#item-ids">>).
-define(NS_PUBSUB_LAST_PUBLISHED, <<"http://jabber.org/protocol/pubsub#last-published">>).
-define(NS_PUBSUB_LEASED_SUBSCRIPTION, <<"http://jabber.org/protocol/pubsub#leased-subscription">>).
-define(NS_PUBSUB_MANAGE_SUBSCRIPTIONS, <<"http://jabber.org/protocol/pubsub#manage-subscriptions">>).
-define(NS_PUBSUB_MEMBER_AFFILIATION, <<"http://jabber.org/protocol/pubsub#member-affiliation">>).
-define(NS_PUBSUB_META_DATA, <<"http://jabber.org/protocol/pubsub#meta-data">>).
-define(NS_PUBSUB_MODIFY_AFFILIATIONS, <<"http://jabber.org/protocol/pubsub#modify-affiliations">>).
-define(NS_PUBSUB_MULTI_COLLECTION, <<"http://jabber.org/protocol/pubsub#multi-collection">>).
-define(NS_PUBSUB_MULTI_SUBSCRIBE, <<"http://jabber.org/protocol/pubsub#multi-subscribe">>).
-define(NS_PUBSUB_OUTCAST_AFFILIATION, <<"http://jabber.org/protocol/pubsub#outcast-affiliation">>).
-define(NS_PUBSUB_PERSISTENT_ITEMS, <<"http://jabber.org/protocol/pubsub#persistent-items">>).
-define(NS_PUBSUB_PRESENCE_NOTIFICATIONS, <<"http://jabber.org/protocol/pubsub#presence-notifications">>).
-define(NS_PUBSUB_PRESENCE_SUBSCRIBE, <<"http://jabber.org/protocol/pubsub#presence-subscribe">>).
-define(NS_PUBSUB_PUBLISH, <<"http://jabber.org/protocol/pubsub#publish">>).
-define(NS_PUBSUB_PUBLISH_OPTIONS, <<"http://jabber.org/protocol/pubsub#publish-options">>).
-define(NS_PUBSUB_PUBLISH_ONLY_AFFILIATION, <<"http://jabber.org/protocol/pubsub#publish-only-affiliation">>).
-define(NS_PUBSUB_PUBLISHER_AFFILIATION, <<"http://jabber.org/protocol/pubsub#publisher-affiliation">>).
-define(NS_PUBSUB_PURGE_NODES, <<"http://jabber.org/protocol/pubsub#purge-nodes">>).
-define(NS_PUBSUB_RETRACT_ITEMS, <<"http://jabber.org/protocol/pubsub#retract-items">>).
-define(NS_PUBSUB_RETRIEVE_AFFILIATIONS, <<"http://jabber.org/protocol/pubsub#retrieve-affiliations">>).
-define(NS_PUBSUB_RETRIEVE_DEFAULT, <<"http://jabber.org/protocol/pubsub#retrieve-default">>).
-define(NS_PUBSUB_RETRIEVE_ITEMS, <<"http://jabber.org/protocol/pubsub#retrieve-items">>).
-define(NS_PUBSUB_RETRIEVE_SUBSCRIPTIONS, <<"http://jabber.org/protocol/pubsub#retrieve-subscriptions">>).
-define(NS_PUBSUB_SUBSCRIBE, <<"http://jabber.org/protocol/pubsub#subscribe">>).
-define(NS_PUBSUB_SUBSCRIPTION_OPTIONS, <<"http://jabber.org/protocol/pubsub#subscription-options">>).
-define(NS_PUBSUB_SUBSCRIPTION_NOTIFICATIONS, <<"http://jabber.org/protocol/pubsub#subscription-notifications">>).

% Defined by XEP-0065: SOCKS5 Bytestreams.
-define(NS_BYTESTREAMS, <<"http://jabber.org/protocol/bytestreams">>).

% Defined by XEP-0066: Out of Band Data.
%% How about NS_OOB instead ?
-define(NS_OOBD_IQ, <<"jabber:iq:oob">>).
-define(NS_OOBD_X, <<"jabber:x:oob">>).

% Defined by XEP-0070: Verifying HTTP Requests via XMPP.
-define(NS_HTTP_AUTH, <<"http://jabber.org/protocol/http-auth">>).

% Defined by XEP-0071: XHTML-IM.
-define(NS_XHTML_IM, <<"http://jabber.org/protocol/xhtml-im">>).

% Defined by XEP-0072: SOAP Over XMPP.
-define(NS_SOAP_FAULT, <<"http://jabber.org/protocol/soap#fault">>).

% Defined by XEP-0077: In-Band Registration.
-define(NS_INBAND_REGISTER, <<"jabber:iq:register">>).
-define(NS_INBAND_REGISTER_FEAT, <<"http://jabber.org/features/iq-register">>).

% Defined by XEP-0078: Non-SASL Authentication.
-define(NS_LEGACY_AUTH, <<"jabber:iq:auth">>).
-define(NS_LEGACY_AUTH_FEAT, <<"http://jabber.org/features/iq-aut">>).

% Defined by XEP-0079: Advanced Message Processing.
-define(NS_AMP, <<"http://jabber.org/protocol/amp">>).
-define(NS_AMP_ERRORS, <<"http://jabber.org/protocol/amp#error">>).
-define(NS_AMP_FEAT, <<"http://jabber.org/features/amp">>).

% Defined by XEP-0080: User Location.
-define(NS_GEOLOC, <<"http://jabber.org/protocol/geoloc">>).

% Defined by XEP-0083: Nested Roster Groups.
-define(NS_ROSTER_DELIMITER, <<"roster:delimiter">>).

% Defined by XEP-0084: User Avatar.
-define(NS_USER_AVATAR_DATA, <<"urn:xmpp:avatar:data">>).

-define(NS_USER_AVATAR_METADATA, <<"urn:xmpp:avatar:metadata">>).

% Defined by XEP-0085: Chat State Notifications
-define(NS_CHATSTATES, <<"http://jabber.org/protocol/chatstates">>).

% Defined by XEP-0090: Entity Time.
-define(NS_TIME_OLD, <<"jabber:iq:time">>).

% Defined by XEP-0091: Delayed Delivery.
-define(NS_DELAY_OLD, <<"jabber:x:delay">>).

% Defined by XEP-0092: Software Version.
-define(NS_SOFT_VERSION, <<"jabber:iq:version">>).

% Defined by XEP-0093: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE_OLD, <<"jabber:x:roster">>).

% Defined by XEP-0095: Stream Initiation.
-define(NS_SI, <<"http://jabber.org/protocol/si">>).

% Defined by XEP-0096: File Transfer.
-define(NS_FILE_TRANSFERT, <<"http://jabber.org/protocol/si/profile/file-transfer">>).

% Defined by XEP-0100: Gateway Interaction.
-define(NS_GATEWAY, <<"jabber:iq:gateway">>).

% Defined by XEP-0107: User Mood.
-define(NS_USER_MOOD, <<"http://jabber.org/protocol/mood">>).

% Defined by XEP-0108: User Activity.
-define(NS_USER_ACTIVITY, <<"http://jabber.org/protocol/activity">>).

% Defined by XEP-0112: User Physical Location (Deferred).
-define(NS_USER_PHYSLOC, <<"http://jabber.org/protocol/physloc">>).

% Defined by XEP-0114: Jabber Component Protocol.
-define(NS_COMPONENT_ACCEPT, <<"jabber:component:accept">>).
-define(NS_COMPONENT_CONNECT, <<"jabber:component:connect">>).

% Defined by XEP-0115: Entity Capabilities.
-define(NS_CAPS, <<"http://jabber.org/protocol/caps">>).

% Defined by XEP-0118: User Tune.
-define(NS_USER_TUNE, <<"http://jabber.org/protocol/tune">>).

% Defined by XEP-0122: Data Forms Validation.
-define(NS_DATA_FORMS_VALIDATE, <<"http://jabber.org/protocol/xdata-validate">>).

% Defined by XEP-0124: Bidirectional-streams Over Synchronous HTTP.
-define(NS_BOSH, <<"urn:xmpp:xbosh">>).

-define(NS_HTTP_BIND, <<"http://jabber.org/protocol/httpbind">>).

% Defined by XEP-0130: Waiting Lists.
-define(NS_WAITING_LIST, <<"http://jabber.org/protocol/waitinglist">>).

% Defined by XEP-0131: Stanza Headers and Internet Metadata (SHIM).
-define(NS_SHIM, <<"http://jabber.org/protocol/shim">>).

% Defined by XEP-0133: Service Administration.
-define(NS_ADMIN, <<"http://jabber.org/protocol/admin">>).

% Defined by XEP-0136: Message Archiving.
-define(NS_ARCHIVING, <<"urn:xmpp:archive">>).

% Defined by XEP-0137: Publishing Stream Initiation Requests.
-define(NS_SI_PUB, <<"http://jabber.org/protocol/sipub">>).

% Defined by XEP-0138: Stream Compression.
-define(NS_COMPRESS, <<"http://jabber.org/protocol/compress">>).
-define(NS_COMPRESS_FEAT, <<"http://jabber.org/features/compress">>).

% Defined by XEP-0141: Data Forms Layout.
-define(NS_DATA_FORMS_LAYOUT, <<"http://jabber.org/protocol/xdata-layout">>).

% Defined by XEP-0144: Roster Item Exchange.
-define(NS_ROSTER_EXCHANGE, <<"http://jabber.org/protocol/rosterx">>).

% Defined by XEP-0145: Annotations.
-define(NS_ROSTER_NOTES, <<"storage:rosternotes">>).

% Defined by XEP-0153: vCard-Based Avatars.
-define(NS_VCARD_UPDATE, <<"vcard-temp:x:update">>).

% Defined by XEP-0154: User Profile.
-define(NS_USER_PROFILE, <<"urn:xmpp:tmp:profile">>).

% Defined by XEP-0155: Stanza Session Negotiation.
-define(NS_SSN, <<"urn:xmpp:ssn">>).

% Defined by XEP-0157: Contact Addresses for XMPP Services.
-define(NS_SERVERINFO, <<"http://jabber.org/network/serverinfo">>).

% Defined by XEP-0158: CAPTCHA Forms.
-define(NS_CAPTCHA, <<"urn:xmpp:captcha">>).

%% Deferred : XEP-0158: Robot Challenges
-define(NS_ROBOT_CHALLENGE, <<"urn:xmpp:tmp:challenge">>).

% Defined by XEP-0160: Best Practices for Handling Offline Messages.
-define(NS_MSGOFFLINE, <<"msgoffline">>).

% Defined by XEP-0161: Abuse Reporting.
-define(NS_ABUSE_REPORTING, <<"urn:xmpp:tmp:abuse">>).

% Defined by XEP-0166: Jingle.
-define(NS_JINGLE, <<"urn:xmpp:tmp:jingle">>).
-define(NS_JINGLE_ERRORS, <<"urn:xmpp:tmp:jingle:errors">>).

% Defined by XEP-0167: Jingle RTP Sessions.
-define(NS_JINGLE_RPT, <<"urn:xmpp:tmp:jingle:apps:rtp">>).
-define(NS_JINGLE_RPT_INFO, <<"urn:xmpp:tmp:jingle:apps:rtp:info">>).

% Defined by XEP-0168: Resource Application Priority.
-define(NS_RAP, <<"http://www.xmpp.org/extensions/xep-0168.html#ns">>).
-define(NS_RAP_ROUTE, <<"http://www.xmpp.org/extensions/xep-0168.html#ns-route">>).

% Defined by XEP-0171: Language Translation.
-define(NS_LANG_TRANS, <<"urn:xmpp:langtrans">>).
-define(NS_LANG_TRANS_ITEMS, <<"urn:xmpp:langtrans#items">>).

% Defined by XEP-0172: User Nickname.
-define(NS_USER_NICKNAME, <<"http://jabber.org/protocol/nick">>).

% Defined by XEP-0176: Jingle ICE-UDP Transport Method.
-define(NS_JINGLE_ICE_UDP, <<"urn:xmpp:tmp:jingle:transports:ice-udp">>).

% Defined by XEP-0177: Jingle Raw UDP Transport Method.
-define(NS_JINGLE_RAW_UDP, <<"urn:xmpp:tmp:jingle:transports:raw-udp">>).
-define(NS_JINGLE_RAW_UDP_INFO, <<"urn:xmpp:tmp:jingle:transports:raw-udp:info">>).

% Defined by XEP-0181: Jingle DTMF.
-define(NS_JINGLE_DTMF_0, <<"urn:xmpp:jingle:dtmf:0">>).

%% Deferred
-define(NS_JINGLE_DTMF, <<"urn:xmpp:tmp:jingle:dtmf">>).

% Defined by XEP-0184: Message Receipts.
-define(NS_RECEIPTS, <<"urn:xmpp:receipts">>).

% Defined by XEP-0186: Invisible Command.
-define(NS_INVISIBLE_COMMAND_0, <<"urn:xmpp:invisible:0">>).

%% Deferred
-define(NS_INVISIBLE_COMMAND, <<"urn:xmpp:tmp:invisible">>).

% Defined by XEP-0189: Public Key Publishing.
-define(NS_PUBKEY_1, <<"urn:xmpp:pubkey:1">>).

-define(NS_ATTEST_1, <<"urn:xmpp:attest:1">>).

-define(NS_REVOKE_1, <<"urn:xmpp:revoke:1">>).

%% Deferred
-define(NS_PUBKEY_TMP, <<"urn:xmpp:tmp:pubkey">>).

% Defined by XEP-0191: Simple Communications Blocking.
-define(NS_BLOCKING, <<"urn:xmpp:blocking">>).
-define(NS_BLOCKING_ERRORS, <<"urn:xmpp:blocking:errors">>).

% Defined by XEP-0194: User Chatting.
-define(NS_USER_CHATTING_0, <<"urn:xmpp:chatting:0">>).

%% Deferred
-define(NS_USER_CHATTING, <<"http://www.xmpp.org/extensions/xep-0194.html#ns">>).

% Defined by XEP-0195: User Browsing.
-define(NS_USER_BROWSING_0, <<"urn:xmpp:browsing:0">>).

%% Deferred
-define(NS_USER_BROWSING, <<"http://www.xmpp.org/extensions/xep-0195.html#ns">>).

% Defined by XEP-0196: User Gaming.
-define(NS_USER_GAMING_0, <<"urn:xmpp:gaming:0">>).

%% Deferred
-define(NS_USER_GAMING, <<"http://www.xmpp.org/extensions/xep-0196.html#ns">>).

% Defined by XEP-0197: User Viewing.
-define(NS_USER_VIEWING_0, <<"urn:xmpp:viewing:0">>).

%% Deferred
-define(NS_USER_VIEWING, <<"http://www.xmpp.org/extensions/xep-0197.html#ns">>).

% Defined by XEP-0198: Stream Management.
-define(NS_STREAM_MGNT_3, <<"urn:xmpp:sm:3">>).

%% Deferred
-define(NS_STREAM_MGNT_2, <<"urn:xmpp:sm:2">>).
-define(NS_STREAM_MGNT_1, <<"urn:xmpp:sm:1">>).
-define(NS_STREAM_MGNT_0, <<"urn:xmpp:sm:0">>).
-define(NS_STANZA_ACK, <<"http://www.xmpp.org/extensions/xep-0198.html#ns">>).

% Defined by XEP-0199: XMPP Ping.
-define(NS_PING, <<"urn:xmpp:ping">>).

% Defined by XEP-0202: Entity Time.
-define(NS_TIME, <<"urn:xmpp:time">>).

% Defined by XEP-0203: Delayed Delivery.
-define(NS_DELAY, <<"urn:xmpp:delay">>).

% Defined by XEP-0206: XMPP Over BOSH.
-define(NS_XBOSH, <<"urn:xmpp:xbosh">>).

% Defined by XEP-0208: Bootstrapping Implementation of Jingle.
-define(NS_JINGLE_BOOTSTRAPING, <<"http://www.xmpp.org/extensions/xep-0208.html#ns">>).

% Defined by XEP-0209: Metacontacts.
-define(NS_METACONTACTS, <<"storage:metacontacts">>).

% Defined by XEP-0215: External Service Discovery.
-define(NS_EXTERNAL_DISCO_0, <<"urn:xmpp:extdisco:0">>).

%% Deferred
-define(NS_EXTERNAL_DISCO, <<"http://www.xmpp.org/extensions/xep-0215.html#ns">>).

% Defined by XEP-0220: Server Dialback.
-define(NS_DIALBACK, <<"jabber:server:dialback">>).
-define(NS_DIALBACK_FEAT, <<"urn:xmpp:features:dialback">>).

% Defined by XEP-0221: Data Forms Media Element.
%% How about NS_DATA ?
-define(NS_DATA_FORMS_MEDIA, <<"urn:xmpp:media-element">>).

%% Deferred
-define(NS_DATA_FORMS_MEDIA_TMP, <<"urn:xmpp:tmp:media-element">>).

% Defined by XEP-0224: Attention.
-define(NS_ATTENTION_0, <<"urn:xmpp:attention:0">>).

%% Deferred
-define(NS_ATTENTION, <<"http://www.xmpp.org/extensions/xep-0224.html#ns">>).

% Defined by XEP-0225: Component Connections.
-define(NS_COMPONENT_CONNECTION_0, <<"urn:xmpp:component:0">>).

%% Deferred
-define(NS_COMPONENT_CONNECTION, <<"urn:xmpp:tmp:component">>).

% Defined by XEP-0227: Portable Import/Export Format for XMPP-IM Servers.
-define(NS_SERVER_IMPORT_EXPORT, <<"http://www.xmpp.org/extensions/xep-0227.html#ns">>).

% Defined by XEP-0231: Data Element.
-define(NS_BOB, <<"urn:xmpp:bob">>).

%% Deferred
-define(NS_DATA, <<"urn:xmpp:tmp:data-element">>).

% Defined by XEP-0233: Use of Domain-Based Service Names in XMPP SASL
% Negotiation.
-define(NS_DOMAIN_BASED_NAME, <<"urn:xmpp:tmp:domain-based-name">>).
-define(NS_DOMAIN_BASED_NAME_B, <<"urn:xmpp:tmp:domain-based-name">>).

% Defined by XEP-0234: Jingle File Transfer.
-define(NS_JINGLE_FT_1, <<"urn:xmpp:jingle:apps:file-transfer:1">>).

%% Deferred
-define(NS_JINGLE_FILE_TRANSFERT, <<"urn:xmpp:tmp:jingle:apps:file-transfer">>).

% Defined by XEP-0235: Authorization Tokens.
-define(NS_OAUTH_0, <<"urn:xmpp:oauth:0">>).

-define(NS_OAUTH_ERRORS_0, <<"urn:xmpp:oauth:0:errors">>).

%% Deferred : XEP-0235: Authorization Tokens.
-define(NS_AUTH_TOKEN, <<"urn:xmpp:tmp:auth-token">>).

% Defined by XEP-0237: Roster Versioning.
-define(NS_ROSTER_VER, <<"urn:xmpp:features:rosterver">>).

%% Deferred : XEP-0237: Roster Sequencing.
-define(NS_ROSTER_SEQ, <<"urn:xmpp:tmp:roster-sequencing">>).

% Defined by XEP-0244: IO Data.
-define(NS_IO_DATA_TMP, <<"urn:xmpp:tmp:io-data">>).

% Defined by XEP-0247: Jingle XML Streams.
-define(NS_JINGLE_XML_STREAM_0, <<"urn:xmpp:jingle:apps:xmlstream:0">>).

% Deferred
-define(NS_JINGLE_XML_STREAM, <<"urn:xmpp:tmp:jingle:apps:xmlstream">>).

% Defined by XEP-0249: Direct MUC Invitations.
-define(NS_JABBER_X_CONF, <<"jabber:x:conference">>).

% Defined by XEP-0251: Jingle Session Transfer.
-define(NS_JINGLE_TRANSFER_0, <<"urn:xmpp:jingle:transfer:0">>).

% Defined by XEP-0253: PubSub Chaining.
-define(NS_PUBSUB_CHAINING, <<"http://jabber.org/protocol/pubsub#chaining">>).

% Defined by XEP-0254: PubSub Queueing.
-define(NS_PUBSUB_QUEUEING_0, <<"urn:xmpp:pubsub:queueing:0">>).

% Defined by XEP-0255: Location Query.
-define(NS_LOCATION_QUERY_0, <<"urn:xmpp:locationquery:0">>).

% Defined by XEP-0257: Client Certificate Management for SASL EXTERNAL.
-define(NS_SASL_CERT_0, <<"urn:xmpp:saslcert:0">>).

% Defined by XEP-0258: Security Labels in XMPP.
-define(NS_SEC_LABEL_0, <<"urn:xmpp:sec-label:0">>).

-define(NS_SEC_LABEL_CATALOG_1, <<"urn:xmpp:sec-label:catalog:1">>).

-define(NS_SEC_LABEL_ESS_0, <<"urn:xmpp:sec-label:ess:0">>).

% Defined by XEP-0259: Message Mine-ing.
-define(NS_MINE_TMP_0, <<"urn:xmpp:tmp:mine:0">>).

% Defined by XEP-0260: Jingle SOCKS5 Bytestreams Transport Method.
-define(NS_JINGLE_TRANSPORTS_S5B_1, <<"urn:xmpp:jingle:transports:s5b:1">>).

% Defined by XEP-0261: Jingle In-Band Bytestreams Transport Method.
-define(NS_JINGLE_TRANSPORTS_S5B_0, <<"urn:xmpp:jingle:transports:s5b:0">>).

% Defined by XEP-0262: Use of ZRTP in Jingle RTP Sessions.
-define(NS_JINGLE_APPS_RTP_ZRTP_0, <<"urn:xmpp:jingle:apps:rtp:zrtp:0">>).

% Defined by XEP-0264: File Transfer Thumbnails.
-define(NS_FT_THUMBS_0, <<"urn:xmpp:thumbs:0">>).

% Defined by XEP-0265: Out-of-Band Stream Data.
-define(NS_JINGLE_APPS_OOB_0, <<"urn:xmpp:jingle:apps:out-of-band:0">>).

% Defined by XEP-0268: Incident Reporting.
-define(NS_INCIDENT_REPORT_0, <<"urn:xmpp:incident:0">>).

% Defined by XEP-0272: Multiparty Jingle (Muji).
-define(NS_TELEPATHY_MUJI, <<"http://telepathy.freedesktop.org/muji">>).

% Defined by XEP-0273: Stanza Interception and Filtering Technology (SIFT).
-define(NS_SIFT_1, <<"urn:xmpp:sift:1">>).

% Defined by XEP-0275: Entity Reputation.
-define(NS_REPUTATION_0, <<"urn:xmpp:reputation:0">>).

% Defined by XEP-0276: Temporary Presence Sharing.
-define(NS_TEMPPRES_0, <<"urn:xmpp:temppres:0">>).

% Defined by XEP-0277: Microblogging over XMPP.
-define(NS_MUBLOG_0, <<"urn:xmpp:microblog:0">>).

% Defined by XEP-0278: Jingle Relay Nodes.
-define(NS_JINGLE_RELAY_NODES, <<"http://jabber.org/protocol/jinglenodes">>).

% Defined by XEP-0279: Server IP Check.
-define(NS_SIC_0, <<"urn:xmpp:sic:0">>).

% Defined by XEP-0280: Message Carbons
-define(NS_CARBONS_2, <<"urn:xmpp:carbons:2">>).

% Defined by XEP-0297: Stanza Forwarding
-define(NS_FORWARD_0, <<"urn:xmpp:forward:0">>).

% Defined by XEP-0313: Message Archive Management (MAM)
-define(NS_MAM, <<"urn:xmpp:mam:tmp">>).

% Defined by XHTML 1.0.
-define(NS_XHTML, <<"http://www.w3.org/1999/xhtml">>).


