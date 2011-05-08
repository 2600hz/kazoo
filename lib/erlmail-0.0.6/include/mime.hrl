-ifndef(D).
-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).
-endif.

-ifndef(CRLF).
-define(CRLF,[13,10]).
-endif.
-ifndef(CRLF_BIN).
-define(CRLF_BIN, <<13,10>>).
-endif.


-record(mime,{
	header      = [],
	header_text = [],
	body        = [],
	body_text   = [],
	message     = []
	}).

-record(addr,{
	username    = [],
	domainname  = [],
	description = []
	}).