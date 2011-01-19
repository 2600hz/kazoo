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
-module(v1_resource).

-export([init/1]).
-export([to_html/2, to_json/2, to_xml/2, to_text/2]).
-export([from_json/2, from_xml/2, from_form/2]).
-export([encodings_provided/2, finish_request/2, is_authorized/2, allowed_methods/2]).
-export([known_content_type/2, content_types_provided/2, content_types_accepted/2, resource_exists/2]).
-export([expires/2, generate_etag/2]).
-export([process_post/2, delete_resource/2]).

-import(logger, [format_log/3]).

-include("crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(NAME, <<"v1_resource">>).

-define(XML_FORMAT, "<field name=\"~s\">~s</field>").

-define(HTML_FORMAT, "<ul><li><span class=\"field\">~s</span> <span class=\"value\">~s</span></li></ul>").

-define(TEXT_FORMAT, "~s ( ~s )~n").

%%%===================================================================
%%% WebMachine API
%%%===================================================================
init(Opts) ->
    {Context, _} = crossbar_bindings:fold(<<"v1.0_resource.init">>, {#cb_context{}, Opts}),
    {ok, Context}.
    %% {{wmtrace, "/tmp"}, #cb_context}.

allowed_methods(RD, #cb_context{allowed_methods=Methods}=Context) ->
    Verb = whistle_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD)))),
    Tokens = lists:map(fun whistle_util:to_binary/1, wrq:path_tokens(RD)),
    Loaded = lists:map(fun whistle_util:to_binary/1, erlang:loaded()),
    case parse_url(Tokens, Loaded, []) of
        [{Mod, Params}|_] = Nouns ->
            Responses = crossbar_bindings:map(<<"v1_resource.allowed_methods.", Mod/binary>>, Params),
            Methods1 = allow_methods(Responses, Methods),
            {Methods1, RD, Context#cb_context{req_nouns=Nouns, req_verb=Verb}};
        [] ->
            {Methods, RD, Context#cb_context{req_verb=Verb}}
    end.

is_authorized(RD, Context) ->
    S0 = crossbar_session:start_session(RD),
    Event = <<"v1_resource.start_session">>,
    S = crossbar_bindings:fold(Event, S0),
    {true, RD, Context#cb_context{session=S}}.

known_content_type(RD, Context) ->
   %% Event = list_to_binary([CbM, <<".known_content_type">>]),
   %% crossbar_bindings:map(Event, {RD, Context}),
    try	
        Json = case wrq:req_body(RD) of
		   <<>> ->
		       [];
		   ReqBody ->
		       {struct, Json1} = mochijson2:decode(ReqBody),
		       Json1
	       end,
        Data = proplists:get_value(<<"data">>, Json, []),
        Auth = get_auth_token(RD, Json),
	{true, RD, Context#cb_context{req_json=Json, req_data=Data, auth_token=Auth}}
    catch
        Exception:Reason ->
            format_log(info, "API_1_0_R: Unknown content type (~p:~p)~n", [Exception, Reason]),
	    Context1 = Context#cb_context{
			  resp_status = error
			 ,resp_error_msg = <<"Invalid or malformed content">>
			 ,resp_error_code = 400
			},
	    RD1 = wrq:set_resp_body(create_body(Context1), RD),
            {{halt, 400}, RD1, Context1}
    end.

resource_exists(RD, #cb_context{req_nouns=[{<<"404">>,[]}|[]]}=Context) ->
    {false, RD, Context};
resource_exists(RD, Context) ->
    case does_resource_exist(RD, Context) of
            true ->
                case is_authentic(RD, Context) andalso is_permitted(RD, Context) of
                    true ->
                        is_valid(RD, Context);
                    false ->
                        {{halt, 401}, RD, Context}
                end;
            false ->
                {false, RD, Context}
    end.

content_types_provided(RD, #cb_context{content_types_provided=CTP}=Context) ->
    CTP1 = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], CTP),
    {CTP1, RD, Context}.

content_types_accepted(RD, #cb_context{content_types_accepted=CTA}=Context) ->
    CTA1 = lists:foldr(fun({Fun, L}, Acc) ->
				   lists:foldr(fun(EncType, Acc1) -> [ {EncType, Fun} | Acc1 ] end, Acc, L)
			   end, [], CTA),
    {CTA1, RD, Context}.

generate_etag(RD, Context) ->
    { mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context }.

encodings_provided(RD, Context) ->
    { [ {"identity", fun(X) -> X end}
       ,{"gzip", fun(X) -> zlib:gzip(X) end}]
      ,RD, Context}.

expires(RD, #cb_context{req_nouns=[{Mod, _}|_]}=Context) ->
    Event = <<"v1_resource.expires.", Mod/binary>>,
    {Expiration, RD1} = crossbar_bindings:fold(Event, {{{1999,1,1},{0,0,0}}, RD}),
    {Expiration, RD1, Context}.

process_post(RD, Context) ->
    Event = <<"v1_resource.process_post">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {true, RD, Context}.

delete_resource(RD, Context) ->
    Event = <<"v1_resource.delete_resource">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {false, RD, Context}.

finish_request(RD, #cb_context{session=S}=Context) ->
    Event = <<"v1_resource.finish_request">>,
    crossbar_bindings:map(Event, {RD, Context}),
    RD1 = crossbar_session:finish_session(S, RD),
    {true, RD1, Context}.

%%%===================================================================
%%% Content Acceptors
%%%===================================================================
from_json(RD, Context) ->
    Event = <<"v1_resource.from_json">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {true, RD, Context}.

from_xml(RD, Context) ->
    Event = <<"v1_resource.from_xml">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {true, RD, Context}.

from_form(RD, Context) ->
    Event = <<"v1_resource.from_form">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {true, RD, Context}.

%%%===================================================================
%%% Content Providers
%%%===================================================================
to_html(RD, Context) ->
    Event = <<"v1_resource.to_html">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {"", RD, Context}.

to_json(RD, Context) ->
  %%  Event = <<"v1_resource.to_json">>,
  %%  crossbar_bindings:map(Event, {RD, Context}),
  %%  Context1 = do_request(RD, Context),
    io:format("TO JSON~n", []),
    {create_body(Context), RD, Context}.

to_xml(RD, Context) ->
    Event = <<"v1_resource.to_xml">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {"", RD, Context}.

to_text(RD, Context) ->
    Event = <<"v1_resource.to_text">>,
    crossbar_bindings:map(Event, {RD, Context}),
    {"", RD, Context}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will loop over the Tokens in a request and seperate return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------
-spec(parse_url/3 :: (Tokens :: list(), Loaded :: list(), Events :: list()) -> proplist()).
parse_url([], _Loaded, Events) ->
        Events;
parse_url([Mod|T], Loaded, Events) ->
    case lists:member(Mod, Loaded) of
        false ->
            parse_url([], Loaded, []);
        true ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not lists:member(Elem, Loaded) end, T),
            parse_url(List2, Loaded, [{Mod, Params} | Events])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will find the intersection of the allowed methods
%% among event respsonses.  The responses can only veto the list of
%% methods, they can not add.
%% @end
%%--------------------------------------------------------------------
-spec(allow_methods/2  :: (Reponses :: proplist(), Avaliable :: proplist()) -> #cb_context{}).
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
%% This method will look for the authorization token, first checking the
%% request headers, if not found there it will look either in the HTTP
%% query paramerts (for GET and DELETE) or HTTP content (for POST and PUT)
%% @end
%%--------------------------------------------------------------------
-spec(get_auth_token/2 :: (RD :: #wm_reqdata{}, JSON :: proplist()) -> binary()).
get_auth_token(RD, JSON) ->
    case wrq:get_req_header("X-Auth-Token", RD) of
        undefined ->
            case wrq:method(RD) of
                'GET' ->
                    whistle_util:to_binary(proplists:get_value("auth-token", wrq:req_qs(RD), ""));
                'POST' ->
                    whistle_util:to_binary(proplists:get_value("auth-token", JSON, ""));
                'PUT' ->
                    whistle_util:to_binary(proplists:get_value("auth-token", JSON, ""));
                'DELETE' ->
                    whistle_util:to_binary(proplists:get_value("auth-token", wrq:req_qs(RD), ""))
            end;
        AuthToken ->
            whistle_util:to_binary(AuthToken)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will use event bindings to determine if the target noun
%% (the final module in the chain) accepts this verb parameter pair.
%% @end
%%--------------------------------------------------------------------
-spec(does_resource_exist/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> boolean()).
does_resource_exist(_RD, #cb_context{req_nouns=Nouns}) ->
    lists:foldl(fun({Mod, Params}, Acc) ->
                                Event = <<"v1_resource.resource_exists.", Mod/binary>>,
                                Responses = crossbar_bindings:map(Event, Params),
                                crossbar_bindings:all(Responses) and Acc
                end, true, Nouns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will use event bindings to determine if the client has
%% provided a valid authentication token
%% @end
%%--------------------------------------------------------------------
-spec(is_authentic/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> boolean()).
is_authentic(_RD, #cb_context{req_nouns=Nouns, auth_token=AuthToken})->
    Event = <<"v1_resource.authenticate">>,
    Responses = crossbar_bindings:map(Event, {AuthToken, Nouns}),
    crossbar_bindings:any(Responses).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will use event bindings to determine if the client is
%% authorized for this request
%% @end
%%--------------------------------------------------------------------
-spec(is_permitted/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> boolean()).
is_permitted(RD, #cb_context{req_nouns=Nouns, auth_token=AuthToken})->
    Event = <<"v1_resource.authorize">>,
    Responses = crossbar_bindings:map(Event, {AuthToken, wrq:method(RD), Nouns}),
    crossbar_bindings:any(Responses).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will use validate to determine if the request is valid,
%% returning 400 if invalid
%% @end
%%--------------------------------------------------------------------
-spec(is_valid/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple({true | tuple(halt, 400), #wm_reqdata{}, #cb_context{}})).
is_valid(RD, Context) ->
    {Valid, RD1, Context1} = validate(RD, Context),
    case Valid of
        true ->
            {true, RD1, Context1};
        false ->
            RD2 = wrq:append_to_response_body(create_body(Context1), RD1),
            {{halt, 400}, wrq:remove_resp_header("Content-Encoding", RD2), Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Child function of is_valid, gives each noun a chance to determine if
%% it is valid and returns the status, and any errors
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple({true | false, #wm_reqdata{}, #cb_context{}})).
validate(RD, #cb_context{req_nouns=Nouns}=Context) ->
    lists:foldl(fun({Mod, Params}, {_, RD1, Context1}) ->
                                Event = <<"v1_resource.validate.", Mod/binary>>,
                                {_, RD2, Context2} = crossbar_bindings:fold(Event, {Params, RD1, Context1}),
                                Valid = Context2#cb_context.resp_status == success,
                                {Valid, RD2, Context2}
                end, {true, RD, Context}, Nouns).
















do_request(RD, Context, Nouns) ->
    {RD1, Context1} = pre_execute(RD, Context, Nouns),
    {RD2, Context2} = execute(RD1, Context1, Nouns),
    post_execute(RD2, Context2, Nouns).

pre_execute(RD, Context, Nouns) ->
    Method = whistle_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD)))),
    lists:foldr(fun ({Mod, Params}, {RD1, Context1}) ->
                                Event = <<"v1_resource.pre_execute.", Method/binary, ".", Mod/binary>>,
                                Payload = [RD1, Context1] ++ Params,
                                [RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
                                {RD2, Context2}
                end, {RD, Context}, Nouns).

execute(RD, Context, Nouns) ->
    Method = whistle_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD)))),
    lists:foldr(fun ({Mod, Params}, {RD1, Context1}) ->
                                Event = <<"v1_resource.execute.", Method/binary, ".", Mod/binary>>,
                                Payload = [RD1, Context1] ++ Params,
                                [RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
                                {RD2, Context2}
                end, {RD, Context}, Nouns).

post_execute(RD, Context, Nouns) ->
    Method = whistle_util:to_binary(string:to_lower(atom_to_list(wrq:method(RD)))),
    lists:foldr(fun ({Mod, Params}, {RD1, Context1}) ->
                                Event = <<"v1_resource.post_execute.", Method/binary, ".", Mod/binary>>,
                                Payload = [RD1, Context1] ++ Params,
                                [RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
                                {RD2, Context2}
                end, {RD, Context}, Nouns).

























%%--------------------------------------------------------------------
%% @private
%% @doc
%% This method will create the content body from the resp_* fields in the 
%% context record.  Currently the type parameter is used to choose the
%% the format but in the future this may be drawn from the req headers
%% @end
%%--------------------------------------------------------------------
-spec(create_body/2 :: (Context :: #cb_context{}, Type :: atom) -> string()).
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
-spec(create_resp_prop/1 :: (Context :: #cb_context{}) -> proplist()).
create_resp_prop(#cb_context{resp_data=D, resp_status=S, resp_error_msg=E, resp_error_code=C}) ->
    case {S, C} of
	{success, _} ->	     
	    [{"status", S}, {"data", {struct, D}}];
	{_, undefined} ->
	    [{"status", S}, {"message", whistle_util:to_binary(E)}, {"data", {struct, D}}];
	_ ->
	    [{"status", S}, {"message", whistle_util:to_binary(E)}, {"error", C}, {"data", {struct, D}}]
    end.