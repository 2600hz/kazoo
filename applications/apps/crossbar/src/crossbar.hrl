-type proplist() :: list(tuple(binary(), term())) | [].

-type crossbar_status() :: success | error | fatal.
-type crossbar_module_result() :: tuple(crossbar_status(), proplist())
				  | tuple(crossbar_status(), proplist(), string())
				  | tuple(crossbar_status(), proplist(), string(), integer()).

-type crossbar_content_handler() :: tuple(atom(), list(string())).

-type http_method() :: 'POST' | 'GET' | 'PUT' | 'DELETE'.
-type http_methods() :: [http_method()].

%% -type iolist() :: [char() | binary() | iolist()].
%% -type iodata() :: iolist() | binary().
-type json_string() :: atom | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json_term()].
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_iolist() :: {json, iolist()}.
-type json_term() :: json_string() | json_number() | json_array() | json_object() | json_iolist().


-define(CONTENT_PROVIDED, [
                            {to_json, ["application/json","application/x-json"]}
                           ,{to_xml, ["application/xml"]}
			  ]).

-define(CONTENT_ACCEPTED, [
                            {from_xml, ["application/xml"]}
                           ,{from_json, ["application/json","application/x-json"]}
			   ,{from_form, ["application/x-www-form-urlencoded"]}
			  ]).

-define(ALLOWED_METHODS, [
			   'GET'
			  ,'POST'
			  ,'PUT'
			  ,'DELETE'
			 ]).

-record(session, {
          '_id' = undefined :: binary() | undefined
	  ,'_rev' = undefined :: binary() | undefined
          ,account_id = <<>> :: binary()
          ,expires = 0 :: integer() % secs
          ,created = 0 :: integer() % timestamp
          ,storage = [] :: proplist() % proplist
         }).

-record(cb_context, {
           content_types_provided = ?CONTENT_PROVIDED :: list(crossbar_content_handler()) | []
          ,content_types_accepted = ?CONTENT_ACCEPTED :: list(crossbar_content_handler()) | []
	  ,allowed_methods = ?ALLOWED_METHODS :: list() | []
	  ,session = #session{} :: #session{}
          ,auth_token = <<"">> :: binary()
          ,req_verb = undefined :: binary() | undefined
          ,req_nouns = [{<<"404">>, []}|[]] :: list() | []
          ,req_json = [] :: proplist()
          ,req_data = [] :: proplist()
          ,db_name = undefined :: string() | undefined
          ,doc = undefined :: json_object() | undefinded
          ,resp_expires = {{1999,1,1},{0,0,0}}
          ,resp_etag = undefined :: string() | automatic | undefined
	  ,resp_status = success :: crossbar_status()
	  ,resp_error_msg = undefined :: string() | undefined
	  ,resp_error_code = undefined :: integer() | undefined
	  ,resp_data = [] :: list() | []
          ,storage = [] :: proplist()
	 }).


