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
-export([to_json/2, to_xml/2]).
-export([from_json/2, from_xml/2, from_form/2]).
-export([encodings_provided/2, finish_request/2, is_authorized/2, allowed_methods/2]).
-export([malformed_request/2, content_types_provided/2, content_types_accepted/2, resource_exists/2]).
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
    case parse_path_tokens(Tokens, Loaded, []) of
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

malformed_request(RD, Context) ->
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
	{false, RD, Context#cb_context{req_json=Json, req_data=Data, auth_token=Auth}}
    catch
        _Exception:_Reason ->
	    Context1 = Context#cb_context{
			  resp_status = error
			 ,resp_error_msg = <<"Invalid or malformed content">>
			 ,resp_error_code = 400
			},
            Content = create_resp_content(RD, Context1),
	    RD1 = wrq:set_resp_body(Content, RD),
            {true, RD1, Context1}
    end.

resource_exists(RD, #cb_context{req_nouns=[{<<"404">>,_}|_]}=Context) ->
    {false, RD, Context};
resource_exists(RD, Context) ->
    case does_resource_exist(RD, Context) of
	true ->
	    case is_authentic(RD, Context) andalso is_permitted(RD, Context) of
		true ->
                    {RD1, Context1} = validate(RD, Context),
                    case succeeded(Context1) of
                        true ->
                            execute_request(RD1, Context1);
                        false ->
                            Content = create_resp_content(RD, Context1),
                            RD2 = wrq:append_to_response_body(Content, RD1),
                            {{halt, 400}, wrq:remove_resp_header("Content-Encoding", RD2), Context1}
                    end;
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

generate_etag(RD, #cb_context{resp_etag=ETag}=Context) ->
    Event = <<"v1_resource.etag">>,
    {ETag1, _, _} = crossbar_bindings:fold(Event, {ETag, RD, Context}),
    case ETag1 of
        automatic ->
            {mochihex:to_hex(crypto:md5(wrq:resp_body(RD))), RD, Context };
        undefined ->
            {undefined, RD, Context};
        Tag when is_list(Tag) ->
            {Tag, RD, Context}
    end.

encodings_provided(RD, Context) ->
    { [ {"identity", fun(X) -> X end} ]
      %%,{"gzip", fun(X) -> zlib:gzip(X) end}]
      ,RD, Context}.

expires(RD, #cb_context{resp_expires=Expires}=Context) ->
    Event = <<"v1_resource.expires">>,
    crossbar_bindings:fold(Event, {Expires, RD, Context}).

process_post(RD, Context) ->
    Event = <<"v1_resource.process_post">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_push_response(RD, Context).

delete_resource(RD, Context) ->
    Event = <<"v1_resource.delete_resource">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_push_response(RD, Context).

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
    create_push_response(RD, Context).

from_xml(RD, Context) ->
    Event = <<"v1_resource.from_xml">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_push_response(RD, Context).

from_form(RD, Context) ->
    Event = <<"v1_resource.from_form">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_push_response(RD, Context).

%%%===================================================================
%%% Content Providers
%%%===================================================================
to_json(RD, Context) ->
    Event = <<"v1_resource.to_json">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_pull_response(RD, Context).

to_xml(RD, Context) ->
    Event = <<"v1_resource.to_xml">>,
    crossbar_bindings:map(Event, {RD, Context}),
    create_pull_response(RD, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will loop over the Tokens in the request path and return
%% a proplist with keys being the module and values a list of parameters
%% supplied to that module.  If the token order is improper a empty list
%% is returned.
%% @end
%%--------------------------------------------------------------------
-spec(parse_path_tokens/3 :: (Tokens :: list(), Loaded :: list(), Events :: list()) -> proplist()).
parse_path_tokens([], _Loaded, Events) ->
    Events;
parse_path_tokens([Mod|T], Loaded, Events) ->
    case lists:member(Mod, Loaded) of
        false ->
            parse_path_tokens([], Loaded, []);
        true ->
            {Params, List2} = lists:splitwith(fun(Elem) -> not lists:member(Elem, Loaded) end, T),
            parse_path_tokens(List2, Loaded, [{Mod, Params} | Events])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will find the intersection of the allowed methods
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
%% This function will look for the authorization token, first checking the
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
%% This function will use event bindings to determine if the target noun
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
%% This function will use event bindings to determine if the client has
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
%% This function will use event bindings to determine if the client is
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
%% This function gives each noun a chance to determine if
%% it is valid and returns the status, and any errors
%% @end
%%--------------------------------------------------------------------
-spec(validate/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(#wm_reqdata{}, #cb_context{})).
validate(RD, #cb_context{req_nouns=Nouns}=Context) ->
    lists:foldl(fun({Mod, Params}, {RD1, Context1}) ->
			Event = <<"v1_resource.validate.", Mod/binary>>,
                        Payload = [RD1, Context1] ++ Params,
			[RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
			{RD2, Context2}
                end, {RD, Context}, Nouns).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will execute the request
%% @end
%%--------------------------------------------------------------------
-spec(execute_request/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(true|tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
execute_request(RD, #cb_context{req_nouns=Nouns, req_verb=Verb}=Context) ->
    {RD1, Context1} = lists:foldr(fun ({Mod, Params}, {RD1, Context1}) ->
			Event = <<"v1_resource.execute.", Verb/binary, ".", Mod/binary>>,
			Payload = [RD1, Context1] ++ Params,
			[RD2, Context2 | _] = crossbar_bindings:fold(Event, Payload),
			{RD2, Context2}
                end, {RD, Context}, Nouns),
    case succeeded(Context1) of
        false ->
            {{halt, 500}, RD1, Context1};
        true ->
            {true, RD1, Context1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create the content for the response body
%% @end
%%--------------------------------------------------------------------
-spec(create_resp_content/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> iolist()).
create_resp_content(RD, Context) ->
    Prop = create_resp_envelope(Context),
    case get_resp_type(RD) of
	html ->
	    Html = format_json(?HTML_FORMAT, Prop),
	    io_lib:format("<html><body>~s</body></html>", [Html]);
	plain ->
	    format_json(?TEXT_FORMAT, Prop);
	xml ->
	    Xml = format_json(?XML_FORMAT, Prop),
	    io_lib:format("<xml>~s</xml>", [Xml]);
        json ->
            mochijson2:encode({struct, Prop})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pushing data (like PUT)
%% @end
%%--------------------------------------------------------------------
-spec(create_push_response/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(boolean(), #wm_reqdata{}, #cb_context{})).
create_push_response(RD, Context) ->
    Content = create_resp_content(RD, Context),
    {succeeded(Context), wrq:set_resp_body(Content, RD), Context}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create response expected for a request that
%% is pulling data (like GET)
%% @end
%%--------------------------------------------------------------------
-spec(create_pull_response/2 :: (RD :: #wm_reqdata{}, Context :: #cb_context{}) -> tuple(iolist() | tuple(halt, 500), #wm_reqdata{}, #cb_context{})).
create_pull_response(RD, Context) ->
    Content = create_resp_content(RD, Context),
    case succeeded(Context) of
        false ->
            {{halt, 500}, wrq:set_resp_body(Content, RD), Context};
        true ->
            {Content, RD, Context}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the response is of type success
%% @end
%%--------------------------------------------------------------------
-spec(succeeded/1 :: (Context :: #cb_context{}) -> boolean()).
succeeded(#cb_context{resp_status=Status}) ->
    Status =:= success.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function extracts the reponse fields and pits them in a proplist
%% @end
%%--------------------------------------------------------------------
-spec(create_resp_envelope/1 :: (Context :: #cb_context{}) -> proplist()).
create_resp_envelope(#cb_context{auth_token=A, resp_data=D, resp_status=S, resp_error_msg=E, resp_error_code=C}) ->
    case {S, C} of
	{success, _} ->
	    [
                 {"auth-token", A}
                ,{"status", S}
                ,{"data", {struct, D}}
            ];
	{_, undefined} ->
	    [
                 {"auth-token", A}
                ,{"status", S}
                ,{"message", whistle_util:to_binary(E)}
                ,{"data", {struct, D}}
            ];
	_ ->
	    [
                 {"auth-token", A}
                ,{"status", S}
                ,{"message", whistle_util:to_binary(E)}
                ,{"error", C}
                ,{"data", {struct, D}}
            ]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine the appropriate content format to return
%% based on the request.... TODO: RETURN XML SOMETIMES
%% @end
%%--------------------------------------------------------------------
-spec(get_resp_type/1 :: (RD :: #wm_reqdata{}) -> json).
get_resp_type(_RD) ->
    json.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function applies the format string to json objects.  This is
%% used to parse JSON into xml, plain text, and html.  The expected
%% JSON structure is that of mochiweb.  TODO: CLEAN ME UP
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