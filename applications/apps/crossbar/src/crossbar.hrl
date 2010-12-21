-type proplist() :: list(tuple(binary(), term())) | [].

-type crossbar_status() :: success | error | fatal.
-type crossbar_module_result() :: tuple(crossbar_status(), proplist())
				  | tuple(crossbar_status(), proplist(), string())
				  | tuple(crossbar_status(), proplist(), string(), integer()).

-type crossbar_content_handler() :: tuple(atom(), list(string())).

-record(session, {
          '_id' = undefined :: binary() | undefined
	  ,'_rev' = undefined :: binary() | undefined
          ,account_id = <<>> :: binary()
          ,expires = 0 :: integer() % secs
          ,created = 0 :: integer() % timestamp
          ,storage = [] :: proplist() % proplist
         }).
