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

-type rest_verb() :: 'POST' | 'GET' | 'PUT' | 'DELETE'.
-type rest_verbs() :: [rest_verb()].

%% -type iolist() :: [char() | binary() | iolist()].
%% -type iodata() :: iolist() | binary().
-type json_string() :: atom | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_iolist() :: {json, iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().
