-include("../../../src/whistle_types.hrl").
-include("../../../src/whistle_amqp.hrl").
-include("../include/amqp_client/include/amqp_client.hrl").
-include("../src/crossbar_types.hrl").

-define(CONTENT_PROVIDED, [
                            {to_json, ["application/json","application/x-json"]}
                           ,{to_xml, ["application/xml"]}
			  ]).

-define(CONTENT_ACCEPTED, [
                            {from_xml, ["application/xml"]}
                           ,{from_json, ["application/json","application/x-json"]}
			   ,{from_form, ["application/x-www-form-urlencoded"]}
			   ,{from_binary, []}
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
          ,req_verb = <<"get">> :: binary() % <<"get">>, <<"post">>, <<"put">>, <<"delete">>
          ,req_nouns = [{<<"404">>, []}|[]] :: list() | []
          ,req_json = {struct, []} :: json_object() | tuple(malformed, binary())
	  ,req_files = [] :: list(tuple(binary(), json_object())) | []
          ,req_data = [] :: mochijson()
          ,db_name = undefined :: string() | undefined
          ,doc = undefined :: json_object() | json_objects() | undefined
          ,resp_expires = {{1999,1,1},{0,0,0}}
          ,resp_etag = undefined :: undefined | automatic | string()
	  ,resp_status = error :: crossbar_status()
	  ,resp_error_msg = undefined :: json_string()
	  ,resp_error_code = undefined :: json_number() | undefined
	  ,resp_data = [] :: mochijson()
	  ,resp_headers = [] :: proplist() %% allow the modules to set headers (like Location: XXX to get a 201 response code)
          ,storage = [] :: proplist()
          ,start = undefined
	 }).
