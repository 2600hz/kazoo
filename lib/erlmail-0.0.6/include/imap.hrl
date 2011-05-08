-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.

-define(IMAP_PORT,143).
-define(server_idle_timeout, 30*1000).

-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-record(imapd_fsm,{
	socket     = [],
	addr       = [],
	options    = [],
	state      = not_authenticated,  % [not_authenticated,authenticated,selected,logout]
	user       = [],
	mailbox    = [],
	mailbox_rw = false,
	line       = [],
	buff       = <<>>
	}).

-record(imapd, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module          % FSM handling module
               }).

-record(imapc_fsm,{
	socket = [],
	state = not_authenticated,  % [not_authenticated,authenticated,selected,logout]
	capability = [],
	mailbox = [],
	mail = [],
	encrypt = plain,
	peer = []
	}).

-record(imap_resp,{
	pid       = [], % needed for IMAP response server
	mailbox   = [], % needed for IMAP response server
	timestamp = [], % needed for IMAP response server
	tag       = [],
	status    = [], % OK, NO, BAD, PREAUTH, BYE
	code      = [], % ALERT, BADCHARSET, CAPABILITY, PARSE, PERMANENTFLAGS, READ_ONLY, READ_WRITE, TRYCREATE, UIDNEXT, UIDVALIDITY, UNSEEN - found inside []
	data      = [],
	cmd       = [],
	info      = []
	}).

-record(imap_cmd,{
	tag  = [],
	cmd  = [],
	data = [],
	line = []
	}).

-record(imap_fetch_cmd,{
	name     = [],
	sections = [],
	parts    = [],
	start    = 0,
	count    = 0,
	string   = []
	}).

-record(mailbox,{
	name        = [],
	flags       = [],
	permflags   = [],
	exists      = 0,
	messages    = 0,
	recent      = 0,
	unseen      = 0,
	uidvalidity = 0,
	uidnext     = 0,
	myrights    = [],
	readwrite   = false
	}).

-record(address,{
	addr_name    = [],
	addr_adl     = [],
	addr_mailbox = [],
	addr_host    = []
	}).

-record(envelope,{
	date = [],
	subject = [],
	from = [],
	sender = [],
	reply_to = [],
	to = [],
	cc = [],
	bcc = [],
	in_reply_to = [],
	message_id = []
	}).

-record(fetch,{
	seqnum = 0,
	uid = [],
	size = 0,
	flags = [],
	internaldate = [],
	date = [],
	subject = [],
	from = [],
	sender = [],
	reply_to = [],
	to = [],
	cc = [],
	bcc = [],
	in_reply_to = [],
	message_id = [],
	body = [],
	body_structure = [],
	rfc822 = [],
	rfc822_header = [],
	rfc822_text = [],
	parts = []
	}).

-record(body,{
	type = [],
	parts = [],
	md5 = [],
	dsp = [],
	lang = [],
	loc = []
	}).

-record(part,{
	type = [],
	subtype = [],
	params = [],
	id = [],
	desc = [],
	encoding = [],
	octets = [],
	lines = [],
	md5 = [],
	dsp = [],
	lang = [],
	loc = []
	}).

-record(status,{
	mailbox = [],
	messages = [],
	recent = [],
	uidnext = [],
	uidvalidity = [],
	unseen = []
	}).



-record(folder,{
	name = [],
	delim = ".",
	flags = []
	}).

-record(tran,{
	id   = [], % tag for request
	from = [], % Process request is from
	type = []  % client or server
	}).

-record(imapd_connection,{
	sock = [],
    port = ?IMAP_PORT,
    peer_addr = [],
    peer_port = ?IMAP_PORT
	}).


% This record characterises the connection from the browser to our server
% it is intended to be a consistent view derived from a bunch of different headers
-record(req, {connection=keep_alive,	        % keep_alive | close
	      content_length,                   % Integer
	      vsn,                              % {Maj,Min}
	      method,                           % 'GET'|'POST'
	      uri,				% Truncated URI /index.html
              args="",                          % Part of URI after ?
	      headers,				% [{Tag, Val}]
	      body = <<>>}).			% Content Body
