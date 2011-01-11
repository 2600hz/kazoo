%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Account REST point
%%%
%%%
%%% @end
%%% Created :  05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(account_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2, from_form/2]).
%%-export([generate_etag/2, encodings_provided/2, finish_request/2, is_authorized/2]).
-export([encodings_provided/2, finish_request/2, is_authorized/2]).
-export([content_types_provided/2, content_types_accepted/2, resource_exists/2, allowed_methods/2]).
-export([process_post/2, expires/2]). %, post_is_create/2, create_path/2]).

-import(logger, [format_log/3]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, "account").

-define(CONTENT_PROVIDED, [
                            {to_json, ["application/json","application/x-json"]}
                           ,{to_xml, ["application/xml"]}
			   ,{to_html, ["text/html"]}
			   ,{to_text, ["text/plain"]}
			  ]).

-define(CONTENT_ACCEPTED, [
			    {from_html, ["text/html"]}
			   ,{from_text, ["text/plain"]}
                           ,{from_xml, ["application/xml"]}
                           ,{from_json, ["application/json","application/x-json"]}
			   ,{from_form, ["application/x-www-form-urlencoded"]}
			  ]).

-define(ALLOWED_METHODS, [
			   'GET'
			  ,'POST'
			  ,'PUT'
			  ,'DELETE'
			 ]).

-define(XML_FORMAT, "<field name=\"~s\">~s</field>").

-define(HTML_FORMAT, "<ul><li><span class=\"field\">~s</span> <span class=\"value\">~s</span></li></ul>").

-define(TEXT_FORMAT, "~s ( ~s )~n").

-record(context, {
           content_types_provided = ?CONTENT_PROVIDED :: list(crossbar_content_handler()) | []
          ,content_types_accepted = ?CONTENT_ACCEPTED :: list(crossbar_content_handler()) | []
	  ,allowed_methods = ?ALLOWED_METHODS :: list() | []
	  ,session = #session{} :: #session{}
	  ,req_params = [] :: proplist()
          ,request = undefined :: undefined | atom()
	  ,resp_status = success :: crossbar_status()
	  ,resp_error_msg = undefined :: string() | undefined
	  ,resp_error_code = undefined :: integer() | undefined
	  ,resp_data = [] :: list() | []
	 }).

%%%===================================================================
%%% WebMachine API
%%%===================================================================
init(Opts) ->
    Event = list_to_binary([?SERVER, <<".init">>]),
    crossbar_bindings:run(Event, Opts),
    {ok, #context{}}.
    %% {{wmtrace, "/tmp"}, #context}.

resource_exists(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".resource_exists">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    find_and_auth(RD, Context).
		     
content_types_provided(RD, #context{content_types_provided=CTP}=Context) ->
    Event = list_to_binary([?SERVER, <<".content_types_provided">>]),
    Responses = crossbar_bindings:run(Event, CTP),
    CTP1 = allow_content_types(Responses, CTP),
    {CTP1, RD, Context}.

content_types_accepted(RD, #context{content_types_accepted=CTA}=Context) ->
    Event = list_to_binary([?SERVER, <<".content_types_accepted">>]),
    Responses = crossbar_bindings:run(Event, CTA),
    CTA1 = allow_content_types(Responses, CTA),
    {CTA1, RD, Context}.

allowed_methods(RD, #context{allowed_methods=Methods}=Context) ->
    Event = list_to_binary([?SERVER, <<".allowed_methods">>]),
    Responses = crossbar_bindings:run(Event, Methods),
    Methods1 = allow_methods(Responses, Methods),
    {Methods1, RD, Context}.

is_authorized(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".is_authorized">>]),
    Auth = crossbar_bindings:any(crossbar_bindings:run(Event, {RD, Context})),
    {Auth, RD, Context}.

%generate_etag(RD, Context) ->
%    Event = list_to_binary([?SERVER, <<".generate_etag">>]),
%    crossbar_bindings:run(Event, {RD, Context}),
%    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".encodings_provided">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    { [ {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}]
      ,RD, Context}.

expires(ReqData, Context) ->
     {{{1999,1,1},{0,0,0}}, ReqData, Context}.

process_post(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".process_post">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Req_Params = crossbar_util:get_request_params(RD),
    {true, RD, Context#context{req_params=Req_Params}}.

finish_request(RD, #context{session=S}=Context) ->
    Event = list_to_binary([?SERVER, <<".finish_request">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, crossbar_session:finish_session(S, RD), Context}.

%%%===================================================================
%%% Content Acceptors
%%%===================================================================
from_text(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".from_text">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.
 
from_json(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".from_json">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {struct, Prop} = mochijson2:decode(wrq:req_body(RD)),
    QS = wrq:req_qs(RD),
    Req_Params = lists:ukeymerge(1, lists:ukeysort(1, Prop), lists:ukeysort(1, QS)),
    {true, RD, Context#context{req_params=Req_Params}}.

from_xml(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".from_xml">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.

from_form(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".process_post">>]),    
    crossbar_bindings:run(Event, {RD, Context}),
    Req_Params = crossbar_util:get_request_params(RD),
    {true, RD, Context#context{req_params=Req_Params}}.

%%%===================================================================
%%% Content Providers
%%%===================================================================
to_html(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".to_html">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    Body = format_json(?HTML_FORMAT, create_resp_prop(Context1)), 
    Page = io_lib:format("<html><body>~s</body></html>", [Body]),
    {Page, RD, Context1}.

to_json(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".to_json">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    Resp = mochijson2:encode({struct, create_resp_prop(Context1)}),
    {Resp, RD, Context1}.

to_xml(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".to_xml">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    Body = format_json(?XML_FORMAT, create_resp_prop(Context1)),
    Page = io_lib:format("<xml>~s</xml>", [Body]),
    {Page, RD, Context1}.

to_text(RD, Context) ->
    Event = list_to_binary([?SERVER, <<".to_text">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    Resp = format_json(?TEXT_FORMAT, create_resp_prop(Context1)),
    {Resp, RD, Context1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will find the intersection of the content_type lists 
%% for both provided and accepts returned by bindings to the events.
%% The resulting list is then reformated into the expected proplist
%% for webmachine.  Notice, this allows the event bindings to veto
%% (remove) a content type, but they can not add one that this module
%% can not handle.
%% @end
%%--------------------------------------------------------------------
-spec(allow_content_types/2 :: (Reponses :: proplist(), Avaliable :: proplist()) -> proplist()).
allow_content_types(Responses, Available) ->
    Results = case crossbar_bindings:succeeded(Responses) of
        [] ->	    
            Available;
	Succeeded ->            
	    lists:foldr(fun({true, Response}, Acc) ->
                lists:map(fun({Fun, Mutual}) ->
                    Set1 = sets:from_list(Mutual),
                    Set2 = sets:from_list(props:get_value(Fun, Response, [])),
                    {Fun, sets:to_list(sets:intersection(Set1, Set2))}
                end, Acc)
            end, Available, Succeeded)
    end,
    lists:foldr(fun({Fun, L}, Acc) ->    				   
        lists:foldr(fun(EncType, Acc1) ->[ {EncType, Fun} | Acc1 ] end, Acc, L)
    end, [], Results).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will extract the function and parameters from the 
%% context record.  It will then call that function and wait for a 
%% response.  If no response is recieved with in the allotted time
%% a generic error is produced.  Otherwise the reply from the function
%% is loaded into the appropriate record fields for use in the response. 
%% @end
%%--------------------------------------------------------------------
-spec(allow_methods/2  :: (Reponses :: proplist(), Avaliable :: proplist()) -> #context{}).
allow_methods(Responses, Available) ->    
    case crossbar_bindings:succeeded(Responses) of
        [] ->
	    Available;
	Succeeded -> 
            lists:foldr(fun({true, Response}, Acc) ->
                Set1 = sets:from_list(Acc),
                Set2 = sets:from_list(Response),
                sets:to_list(sets:intersection(Set1, Set2))
            end, Available, Succeeded)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will extract the function and parameters from the 
%% context record.  It will then call that function and wait for a 
%% response.  If no response is recieved with in the allotted time
%% a generic error is produced.  Otherwise the reply from the function
%% is loaded into the appropriate record fields for use in the response. 
%% @end
%%--------------------------------------------------------------------
-spec(do_request/2 :: (RD :: #wm_reqdata{}, Context :: #context{}) -> #context{}).
do_request(RD, #context{request=Fun, req_params=Params}=Context) ->
    Mod = list_to_existing_atom(?SERVER),
    apply(Mod, Fun, [Params, RD]),
    receive 
	Resp ->
	  Context#context{
	      resp_data = proplists:get_value(data, Resp, [])
	     ,resp_status = proplists:get_value(status, Resp, error)
             ,resp_error_msg = proplists:get_value(message, Resp, <<"unspecified">>)
             ,resp_error_code = proplists:get_value(error, Resp, 500)
          }
    after
        10000 ->
          Context#context{
              resp_data = []
             ,resp_status = fatal
             ,resp_error_msg = <<"Request processing timed out">>
             ,resp_error_code = 500
          }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function maps the request to a function exported by the 
%% appropriate module.  If the request has not specified path other than
%% the module, then we will look for an index function.  If the
%% function has been exported then we will validate the request and
%% load it into the record, for later execution by do_request. Any
%% failure at this level will return 404 or 417
%% @end
%%--------------------------------------------------------------------
-spec(find_and_auth/2 :: (RD :: #wm_reqdata{}, Context :: #context{}) -> tuple(boolean(), #wm_reqdata{}, #context{})).
find_and_auth(RD, Context) ->
    try
	Mod = list_to_existing_atom(?SERVER),           
	case erlang:whereis(Mod) of 
	    undefined ->
		throw(unknown_module);
	    _->
		ok
	end,
	Fun = case wrq:path_info(request, RD) of
		  undefined ->
		      index;
		  Req when is_list(Req) ->
		      list_to_existing_atom(Req);
		  _ ->
		      throw(unknown_fun)
	      end,  
	case erlang:function_exported(Mod, Fun, 2) andalso is_valid(RD, Context) of
	    {true, RD0, Context1} ->		
		format_log(info, "ACCOUNT_R: ~p:~p/1 found and authed.~n", [Mod, Fun]),
		{true, RD0, Context1#context{request=Fun}};
	    {false, RD1, Context2} ->
		format_log(info, "ACCOUNT_R: ~p:~p/1 found but not authed.~n", [Mod, Fun]),
		{{error, 417}, RD1, Context2};
            false ->
		throw(not_exported)
       end
    catch
	Exception:Reason ->
	    format_log(info, "ACCOUNT_R: Could not satify req (~p:~p)~n", [Exception, Reason]),
	    {false, RD, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will authenticate and check the authorization of the
%% the request with regards to the user.  It also is responsible for
%% maintaining the users session.  
%% @end
%%--------------------------------------------------------------------
-spec(is_valid/2 :: (RD :: #wm_reqdata{}, Context :: #context{}) -> tuple(boolean(), #wm_reqdata{}, #context{})).
is_valid(RD, Context) ->
    S0 = crossbar_session:start_session(RD),
    S = S0#session{account_id = <<"test">>},
    io:format("~p~n", [wrq:req_body(RD)]),
    Params = [{<<"account_id">>, S#session.account_id} | crossbar_util:get_request_params(RD)],
    
    Event = list_to_binary([?SERVER, <<".validate">>]),
    ValidatedResults = crossbar_bindings:run(Event, {wrq:path_info(request, RD), Params}),
        case crossbar_bindings:failed(ValidatedResults) of
        [] ->
		Event1 = list_to_binary([?SERVER, <<".authenticate">>]),
		IsAuthenticated = crossbar_bindings:any(crossbar_bindings:run(Event1, {S, Params})),
		Event2 = list_to_binary([?SERVER, <<".authorize">>]),
		IsAuthorized = crossbar_bindings:any(crossbar_bindings:run(Event2, {S, Params})),
		{IsAuthenticated andalso IsAuthorized, RD, Context#context{session=S, req_params=Params}};
	            [{false, FailedKeys} | _] -> %% some listener returned false, take the first one
            Resp = crossbar_util:winkstart_envelope(error
                                                    ,[{<<"failed_keys">>, FailedKeys}]
                                                    ,"Some keys failed to validate"),
            {false, wrq:append_to_response_body(Resp, RD), Context#context{session=S}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function applies the format string to json objects.  This is
%% used to parse JSON into xml, plain text, and html.  The expected
%% JSON structure is that of mochiweb.  
%% @end
%%--------------------------------------------------------------------
-spec(format_json/2 :: (Format :: string(), Json :: json_object()) -> iolist()).
format_json(Format, Json) ->
    {Result, _} = lists:foldl(fun({K, V}, {Acc, Count}) ->
			Str = case V of
			    {struct, []} ->
				      io_lib:format(Format, [K, ""]);
			    {struct, V1} ->
				      io_lib:format(Format, [K, format_json(Format, V1)]);                           
			    V1 when is_list(V1) ->
				      case is_proplist(V1) of
					  true ->
					      case K of
						  struct ->
						      io_lib:format(Format, [integer_to_list(Count), format_json(Format, V1)]);
						  _ ->
						      io_lib:format(Format, [K, format_json(Format, V1)])
					      end;
					  _->
					      {SubStr, _} = lists:foldl(fun(V2, {Acc2, Count2}) -> 
								  Str2 = io_lib:format(Format, [integer_to_list(Count2), whistle_util:to_list(V2)]),
								  {string:concat(Acc2, Str2), Count2 + 1}
							  end, {"", 0}, V1),
					      io_lib:format(Format, [K, SubStr])
				     end;
			    _ ->
				      io_lib:format(Format, [K, whistle_util:to_list(V)])
			end,
			{string:concat(Acc, Str), Count + 1}
		end, {"", 0}, Json),
	Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if a list is a proplist, this is used
%% above to distinquish lists in the JSON payload from recursive 
%% structs
%% @end
%%--------------------------------------------------------------------
-spec(is_proplist/1 :: (Thing :: term()) -> boolean()).
is_proplist(Thing) when is_list(Thing) ->
    lists:all(fun(Elem) ->
		       case Elem of 
			   {_,_} ->
			       true;
			   _ -> 
			       false 
		       end
	      end, Thing);
is_proplist(_) ->
    false.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse atoms, and creates the JSON
%% proplist format.
%% @end
%%--------------------------------------------------------------------
-spec(create_resp_prop/1 :: (Context :: #context{}) -> proplist()).
create_resp_prop(#context{resp_data=D, resp_status=S, resp_error_msg=E, resp_error_code=C}) ->
    case {S, C} of
	{success, _} ->	     
	    [{"status", S}, {"data", {struct, D}}];
	{_, undefined} ->
	    [{"status", S}, {"message", E}, {"data", {struct, D}}];
	_ ->
	    [{"status", S}, {"message", E}, {"error", C}, {"data", {struct, D}}]
    end.
