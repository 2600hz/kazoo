-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.

-define(POP_PORT,110).

-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.

-record(popd, {
	listener, % Listening socket
	acceptor, % Asynchronous acceptor's internal reference
	module    % FSM handling module
	}).

-record(popd_fsm,{
	socket  = [],
	addr    = [],
	options = [],
	line    = [],
	state   = authorization,
	user    = undefined,
	pass    = undefined,
	buff    = <<>>
	}).

-record(popd_cmd,{
	line = [],
	cmd  = [],
	data = []
	}).