-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.
-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-define(ERLMAIL_VERSION,"0.0.6").

-record(store,{
	name = [],
	options = []
	}).

-record(domain,{
	name    = [], % Domain Name
	options = []  % Key/Value list of options
	}).

-record(user,{
	name       = [], % Tuple {Users Name, Domain Name}
	password   = [], % User Password - check options for HASH type, otherwise plain text
	options    = []  % key/Value list of options
	}).

-record(message,{
	name         = [], % Tuple {Message Name, User Name, Doamin Name}
	from         = [], % single address for sender
	to           = [], % address list for recepient
	cc           = [], % address list for carbon copy
	bcc          = [], % address list for blind carbon copy
	internaldate = [], % date message was received
	size         = 0,  % integer() size of message
	options      = [], % Key/Value list of options
	uid          = 0,  % Unique Identifier
	flags        = [], % IMAP flags in proplist
	message      = []  % Whole Mail Message
	}).

-record(mailbox_store,{
	name       = [], % Tuple {MailBoxName,UserName,DomainName}
	subscribed = false,
	uidnext     = 1,
	uidvalidity = 0,
	options    = [],
	messages   = []
	}).

-record(message_store,{
	client  = [], % PID for FSM of the client
	server  = [], % node name of the server that has the mailbox open
	mailbox = [], % name of the mailbox
	state   = []  % current state of the mailbox; [open|active|closed]
	}).

-record(erlmail_store,{
	system  = mnesia_store, 
	domain  = mnesia_store, 
	user    = mnesia_store, 
	message = mnesia_store, 
	mailbox = mnesia_store
	}).