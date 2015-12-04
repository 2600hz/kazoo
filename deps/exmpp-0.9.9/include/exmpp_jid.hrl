% WARNING: This header is internal to Exmpp. DO NOT include it in your
% applications!

% --------------------------------------------------------------------
% Records to represent XMPP/Jabber specific structures.
% --------------------------------------------------------------------

% Instead of storing the three raw components separately, we keep the
% original JID in one chunk. The intention is to save some memory.
% Most of the time we already have the raw JID (for example coming
% from a stanza), so we avoid the need of allocating a new binary when
% serializing (we can return the original, see make/4). Also we
% allocate less pointers by keeping the JID in one chunk instead of
% in its three separate components.
%
% Tradeoff: we consume more CPU and memory for accessing a single
% raw component (see exmpp_jid:node/1, exmpp_jid:domain/1,
% exmpp_jid:resource/1), but these are less frequently used than their
% prepared versions (exmpp_jid:prep_node/1, exmpp_jid:prep_domain/1). Load tests
% performed on ejabberd so far indicate an observable memory win when
% using this representation, with no significant CPU cost.

% JID.
-record(jid, {
  raw  :: binary() | undefined,   %% original JID
  node     :: binary() | undefined, %% prepared node
  domain   :: binary() | undefined, %% prepared domain
  resource :: binary() | undefined  %% prepared resource
}).
