-include("../../../utils/src/whistle_types.hrl").
-include("../src/crossbar_types.hrl").

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
	  ,session = undefined :: undefined | #session{}
          ,auth_token = <<"">> :: binary()
          ,req_verb = undefined :: binary() | undefined
          ,req_nouns = [{<<"404">>, []}|[]] :: list() | []
          ,req_json = [] :: proplist()
          ,req_data = [] :: proplist()
          ,db_name = undefined :: string() | undefined
          ,doc = undefined :: json_object() | undefinded
          ,resp_expires = {{1999,1,1},{0,0,0}}
          ,resp_etag = undefined :: string() | automatic | undefined
	  ,resp_status = error :: crossbar_status()
	  ,resp_error_msg = undefined :: string() | undefined
	  ,resp_error_code = undefined :: integer() | undefined
	  ,resp_data = [] :: list() | []
          ,storage = [] :: proplist()
          ,start = undefined
	 }).