-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.
-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.


-define(SMTPD_PORT,25).
-define(SMTPD_MAX_CONN,25).
-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(TIMEOUT,   300000).

-record(smtpc,{
	socket = [],
	features = [],
	type = smtp, % smtp server type: [smtp:esmtp]
	state = helo % State of command, [helo,mail,rcpt,data]
	}).

-record(smtpd_fsm,{
	socket      = [],
	addr        = [],
	relay       = false,
	options     = [],
	buff        = <<>>,
	line        = [],
	cmd         = undefined,
	param       = undefined,
	host        = undefined,
	mail        = undefined,
	rcpt        = undefined,
	to          = undefined,
	messagename = undefined,
	data        = undefined
	}).

-record(smtpd_state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module          % FSM handling module
               }).

-record(outgoing_smtp,{
	rcpt       = [],
	tries      = 0,
	next_retry = [],
	response   = []
	}).

