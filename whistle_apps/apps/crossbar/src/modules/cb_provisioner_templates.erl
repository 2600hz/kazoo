%%%-------------------------------------------------------------------
%%% @author Jon Blanton  <jon@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Provision template module
%%%
%%% Handle client requests for provisioner template documents
%%%
%%% @end
%%% Created : 20 Dec 2011 by Jon Blanton <jon@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_provisioner_templates).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(CB_LIST, <<"provisioner_templates/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok, ok, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.provisioner_templates">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.provisioner_templates">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.provisioner_templates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = validate(Params, Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.provisioner_templates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.provisioner_templates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.provisioner_templates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info(timeout, State) ->
    bind_to_crossbar(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec bind_to_crossbar/0 :: () ->  no_return().
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.provisioner_templates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.provisioner_templates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.provisioner_templates">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.provisioner_templates">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/1 :: (path_tokens()) -> {boolean(), []}.
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    load_provisioner_template_summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create_provisioner_template(Context);
validate([DocId], #cb_context{req_verb = <<"get">>}=Context) ->
    load_provisioner_template(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"post">>}=Context) ->
    update_provisioner_template(DocId, Context);
validate([DocId], #cb_context{req_verb = <<"delete">>}=Context) ->
    load_provisioner_template(DocId, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load list of provision templates, each summarized.  Or a specific
%% provision template summary.
%% @end
%%--------------------------------------------------------------------
-spec load_provisioner_template_summary/1 :: (#cb_context{}) -> #cb_context{}.
load_provisioner_template_summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new provision template document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_provisioner_template/1 :: (#cb_context{}) -> #cb_context{}.
create_provisioner_template(Context) ->
    case get_provision_defaults(Context) of
        #cb_context{doc=Doc, resp_status=success}=Context1 ->
            Pvts = [fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"provisioner_template">>, J) end
                    ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                   ],

            Context1#cb_context{
                doc = lists:foldr(fun(F, J) -> F(J) end, Doc, Pvts)
            };
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a provision template document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_provisioner_template/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_provisioner_template(DocId, Context) ->
    cond_remove_image(crossbar_doc:load(DocId, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing provision template document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_provisioner_template/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_provisioner_template(DocId, #cb_context{req_data=JObj}=Context) ->
    #cb_context{doc=Doc} = crossbar_doc:load(DocId, Context),

    JObj1 = case wh_json:get_value(<<"image">>, JObj) of
                undefined ->
                    wh_json:set_value(<<"image">>, wh_json:get_value(<<"image">>, Doc, wh_json:new()), JObj);
                _ ->
                    JObj
            end,

    cond_remove_image(crossbar_doc:load_merge(DocId, JObj1, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This doesn't belong here, needs to be in an external library. Make request to
%% get provisioning defaults
%% @end
%%--------------------------------------------------------------------
-spec get_provision_defaults/1 :: (#cb_context{}) -> #cb_context{}.
get_provision_defaults(#cb_context{req_data=JObj}=Context) ->
    Url = [whapps_config:get_string(<<"crossbar.provisioner_templates">>, <<"provisioner_template_url">>)
           ,"?request=data"
           ,"&brand=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"brand">>], JObj))
           ,"&model=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"model">>], JObj))
           ,"&product=", mochiweb_util:quote_plus(wh_json:get_string_value([<<"properties">>, <<"product">>], JObj))
          ],

    UrlString = lists:flatten(Url),

    Headers = [{"Host", whapps_config:get_string(<<"crossbar.provisioner_templates">>, <<"provisioner_template_host">>)}
               ,{"Referer", whapps_config:get_string(<<"crossbar.provisioner_templates">>, <<"provisioner_template_referer">>)}
               ,{"User-Agent", wh_util:to_list(erlang:node())}
              ],

    Body = [],
    HTTPOptions = [],

    ?LOG("Attempting to pull provisioning configs from ~s", [UrlString]),

    case ibrowse:send_req(UrlString, Headers, get, Body, HTTPOptions) of
        {ok, "200", _, Response} ->
            ?LOG("Great success! Acquired provisioning template.", []),
            JResp = wh_json:decode(Response),
            Context#cb_context{
                doc = wh_json:set_value(<<"template">>, JResp, JObj)
                ,resp_status = success
            };
        _ ->
            ?LOG("Error! Could not acquiring provisioning template.", []),
            crossbar:response(error, "Error retrieving content from external site", 500, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Conditionally removes image key from response
%% @end
%%--------------------------------------------------------------------
-spec cond_remove_image/1 :: (#cb_context{}) -> #cb_context{}.
cond_remove_image(#cb_context{resp_data=JResp, resp_status='success', query_json=Query}=Context) ->
    case wh_json:is_true(<<"withoutImage">>, Query, false) of
        true ->
            Context#cb_context{
                resp_data = wh_json:delete_key(<<"image">>, JResp)
            };
        false ->
            Context
    end;
cond_remove_image(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (json_object(), json_objects()) -> json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
