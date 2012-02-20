%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Provision template module
%%%
%%% Handle client requests for provisioner template documents
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_provisioner_templates).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/1
         ,post/2
         ,delete/2
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(CB_LIST, <<"provisioner_templates/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.provisioner_templates">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.provisioner_templates">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.provisioner_templates">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.provisioner_templates">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.provisioner_templates">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.provisioner_templates">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_provisioner_template_summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_provisioner_template(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_provisioner_template(DocId, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_provisioner_template(DocId, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_provisioner_template(DocId, Context).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, _) ->
    crossbar_doc:save(Context).

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(Context) ->
    crossbar_doc:save(Context).

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
            ?LOG("Great success! Acquired provisioning template."),
            JResp = wh_json:decode(Response),
            Context#cb_context{
                doc = wh_json:set_value(<<"template">>, JResp, JObj)
                ,resp_status = success
            };
        _ ->
            ?LOG("Error! Could not acquiring provisioning template."),
            crossbar_util:response(error, <<"Error retrieving content from external site">>, 500, Context)
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
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
