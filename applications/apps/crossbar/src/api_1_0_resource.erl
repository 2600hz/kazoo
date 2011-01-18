%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% API resource
%%%
%%%
%%% @end
%%% Created :  05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(api_1_0_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_text/2, from_form/2]).
-export([encodings_provided/2, finish_request/2, is_authorized/2, allowed_methods/2]).
-export([known_content_type/2, content_types_provided/2, content_types_accepted/2, resource_exists/2]).
-export([expires/2, generate_etag/2]).
-export([process_post/2, delete_resource/2]).

-import(logger, [format_log/3]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

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
	  ,cb_module = undefined :: undefined | atom()
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
    crossbar_bindings:run(<<"api_1_0_resource.init">>, Opts),
    {ok, #context{}}.
    %% {{wmtrace, "/tmp"}, #context}.

allowed_methods(RD, #context{allowed_methods=Methods}=Context) ->
    CbM = wrq:path_info(cb_module, RD),
    Event = list_to_binary([CbM, <<".allowed_methods">>]),
    Responses = crossbar_bindings:run(Event, Methods),
    Methods1 = allow_methods(Responses, Methods),
    {Methods1, RD, Context#context{cb_module=CbM}}.

is_authorized(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".is_authorized">>]),
    %%Auth = crossbar_bindings:any(crossbar_bindings:run(Event, {RD, Context})),
    crossbar_bindings:run(Event, {RD, Context}),
    S0 = crossbar_session:start_session(RD),
    S = S0#session{account_id = <<"test">>},
%%    {Auth, RD, Context#context{session=S}}.
    {true, RD, Context#context{session=S}}.

known_content_type(RD, #context{cb_module=CbM, session=S}=Context) ->
    Event = list_to_binary([CbM, <<".known_content_type">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    try
	QS = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- wrq:req_qs(RD)],	
        Prop = case wrq:req_body(RD) of
		   <<>> ->
		       [{<<"data">>, {struct, []}}];
		   Content ->
		       {struct, Json} = mochijson2:decode(Content),
		       lists:ukeymerge(1, lists:ukeysort(1, Json), [{<<"data">>, {struct,[]}}])
	       end,
	Prop1 = [{<<"account_id">>, S#session.account_id} | Prop],
        Req_Params = wrq:path_tokens(RD) ++ [lists:ukeymerge(1, lists:ukeysort(1, Prop1), lists:ukeysort(1, QS))],
	{true, RD, Context#context{req_params=Req_Params}}
    catch
        Exception:Reason ->
            format_log(info, "API_1_0_R: Unknown content type (~p:~p)~n", [Exception, Reason]),
	    Context1 = Context#context{
			  resp_status = error
			 ,resp_error_msg = <<"Invalid or malformed content">>
			 ,resp_error_code = 400
			},
	    RD1 = wrq:set_resp_body(create_body(Context1), RD),
            {{halt, 400}, RD1, Context1}
    end.

resource_exists(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".resource_exists">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    find_and_auth(RD, Context).
		     
content_types_provided(RD, #context{content_types_provided=CTP, cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".content_types_provided">>]),
    Responses = crossbar_bindings:run(Event, CTP),
    CTP1 = allow_content_types(Responses, CTP),
    {CTP1, RD, Context}.

content_types_accepted(RD, #context{content_types_accepted=CTA, cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".content_types_accepted">>]),
    Responses = crossbar_bindings:run(Event, CTA),
    CTA1 = allow_content_types(Responses, CTA),
    {CTA1, RD, Context}.

generate_etag(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".generate_etag">>]),
    Responses = crossbar_bindings:run(Event, {undefined, RD, Context}),
    case crossbar_bindings:succeeded(Responses) of
	[] ->
	    { undefined, RD, Context };
	[{true, Response} | _] ->
	    Response
    end.

encodings_provided(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".encodings_provided">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    { [ {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}]
      ,RD, Context}.

expires(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".expires">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {{{1999,1,1},{0,0,0}}, RD, Context}.

process_post(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".process_post">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    RD1 = wrq:set_resp_body(create_body(Context1), RD),
    {true, RD1, Context1}.

delete_resource(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".delete_resource">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    RD1 = wrq:set_resp_body(create_body(Context1), RD),
    case Context1#context.resp_status of
	success ->
	    {true, RD1, Context1};
	_ ->
	    {false, RD1, Context1}
    end.

finish_request(RD, #context{session=S, cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".finish_request">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    RD1 = crossbar_session:finish_session(S, RD),
    {true, RD1, Context}.

%%%===================================================================
%%% Content Acceptors
%%%===================================================================
from_text(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".from_text">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.
 
from_json(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".from_json">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.

from_xml(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".from_xml">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.

from_form(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".from_form">>]),    
    crossbar_bindings:run(Event, {RD, Context}),
    {true, RD, Context}.

%%%===================================================================
%%% Content Providers
%%%===================================================================
to_html(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".to_html">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    {create_body(Context1, html), RD, Context1}.

to_json(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".to_json">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    {create_body(Context1), RD, Context1}.

to_xml(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".to_xml">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    {create_body(Context1, xml), RD, Context1}.

to_text(RD, #context{cb_module=CbM}=Context) ->
    Event = list_to_binary([CbM, <<".to_text">>]),
    crossbar_bindings:run(Event, {RD, Context}),
    Context1 = do_request(RD, Context),
    {create_body(Context1, plain), RD, Context1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will create the content body from the resp_* fields in the 
%% context record.  Currently the type parameter is used to choose the
%% the format but in the future this may be drawn from the req headers
%% @end
%%--------------------------------------------------------------------
-spec(create_body/2 :: (Context :: #context{}, Type :: atom) -> string()).
create_body(Context) ->
    create_body(Context, json).
create_body(Context, Type) ->
    case Type of
	json ->
	    mochijson2:encode({struct, create_resp_prop(Context)});
	xml ->
	    Xml = format_json(?XML_FORMAT, create_resp_prop(Context)),
	    io_lib:format("<xml>~s</xml>", [Xml]);
	html ->
	    Html = format_json(?HTML_FORMAT, create_resp_prop(Context)),	    
	    io_lib:format("<html><body>~s</body></html>", [Html]);
	plain ->
	    format_json(?TEXT_FORMAT, create_resp_prop(Context))
    end.
	    
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
%% This method will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, it can not add.
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
%% This function maps the request to a function exported by the 
%% appropriate module.  If the request has not specified path other than
%% the module, then we will look for an index function.  If the
%% function has been exported then we will validate the request and
%% load it into the record, for later execution by do_request. Any
%% failure at this level will return 404 or 417
%% @end
%%--------------------------------------------------------------------
-spec(find_and_auth/2 :: (RD :: #wm_reqdata{}, Context :: #context{}) -> tuple(boolean(), #wm_reqdata{}, #context{})).
find_and_auth(RD, #context{cb_module=CbM, req_params=Params}=Context) ->
    try
	Mod = list_to_existing_atom(CbM),
	case erlang:whereis(Mod) of 
	    undefined ->
		throw(unknown_module);
	    Pid when is_pid(Pid) ->
		ok
	end,
	Fun = case wrq:path_tokens(RD) of
		  [] ->
		      index;
		  Tokens when is_list(Tokens) ->
		      try
			  list_to_existing_atom(hd(Tokens))
		      catch
			  _:_ ->
			      list_to_atom("http_" ++ string:to_lower(atom_to_list(wrq:method(RD))))
		      end
	      end,  
	Find = atom_to_list(Fun),
	Params1 = lists:filter(fun(Elem) ->  Find /= Elem end, Params),	
	Arity = length(Params1) + 1,
	case erlang:function_exported(Mod, Fun, Arity) andalso is_valid(RD, Context#context{request=Fun, req_params=Params1}) of
	    {true, _RD, _Context} ->		
		format_log(info, "API_1_0_R: ~p:~p/~p found and authed.~n", [Mod, Fun, Arity]),
		{true, RD, Context#context{request=Fun, req_params=Params1}};
	    {false, RD1, Context1} ->
		format_log(info, "API_1_0_R: ~p:~p/~p found but not authed.~n", [Mod, Fun, Arity]),
		{{halt, 400}, wrq:remove_resp_header("Content-Encoding", RD1), Context1};
            false ->
		format_log(info, "API_1_0_R: ~p:~p/~p not exported.~n", [Mod, Fun, Arity]),
		throw(not_exported)
       end
    catch
	Exception:Reason ->
	    format_log(info, "API_1_0_R: Could not satify req (~p:~p)~n", [Exception, Reason]),
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
is_valid(RD, #context{cb_module=CbM, request=Fun, req_params=Params, session=S}=Context) ->
    Event = list_to_binary([CbM, <<".validate">>]),
    ValidatedResults = crossbar_bindings:run(Event, {Fun, Params}),
    case crossbar_bindings:failed(ValidatedResults) of
	[] ->
	    Event1 = list_to_binary([CbM, <<".authenticate">>]),
	    IsAuthenticated = crossbar_bindings:any(crossbar_bindings:run(Event1, {S, Params})),
	    Event2 = list_to_binary([CbM, <<".authorize">>]),
	    IsAuthorized = crossbar_bindings:any(crossbar_bindings:run(Event2, {S, Params})),
	    {IsAuthenticated andalso IsAuthorized, RD, Context};
	[{false, FailedKeys} | _] -> %% some listener returned false, take the first one
	    Context1 = Context#context{
			  resp_status = error
			 ,resp_error_msg = <<"Invalid Request">>
                         ,resp_error_code = 400
			 ,resp_data = [{<<"failed">>, FailedKeys}]
			},
	    RD1 = wrq:set_resp_body(create_body(Context1), RD),
            {false, RD1, Context1}
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
do_request(RD, #context{cb_module=CbM, request=Fun, req_params=Params}=Context) ->
    Mod = list_to_existing_atom(CbM),
    format_log(info, "API_1_0_R: Execute ~p:~p(~p)~n", [Mod, Fun, Params]),
    apply(Mod, Fun, Params ++ [RD]),    
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
             ,resp_error_msg = "Request processing timed out"
             ,resp_error_code = 504
          }
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
	    [{"status", S}, {"message", whistle_util:to_binary(E)}, {"data", {struct, D}}];
	_ ->
	    [{"status", S}, {"message", whistle_util:to_binary(E)}, {"error", C}, {"data", {struct, D}}]
    end.
