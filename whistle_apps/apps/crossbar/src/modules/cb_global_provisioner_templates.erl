%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Provision template module
%%%
%%% Handle client requests for provisioner template documents
%%%
%%% Note regarding storing the template as an attachment:
%%% Since the template is a 300k json object it is more efficent to store it as
%%% an attachment, funky I know but necessary. Also since we already require
%%% two API calls for editing a template we will maintain backward compatiblity by
%%% not requiring an additional API call for the template and merge/unmerge it
%%% from requests.
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_global_provisioner_templates).

-export([init/0
         ,content_types_provided/3, content_types_accepted/3
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,device_updated/1
         ,put/1
         ,post/2
         ,delete/2
         ,put/3
         ,post/3
         ,delete/3
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".provisioner_templates">>).
-define(CB_LIST, <<"provisioner_templates/crossbar_listing">>).
-define(IMAGE_REQ, <<"image">>).
-define(TEMPLATE_ATTCH, <<"template">>).
-define(MIME_TYPES, [{<<"image">>, <<"*">>}
                     ,{<<"application">>, <<"base64">>}
                     ,{<<"application">>, <<"x-base64">>}
                     ,{<<"application">>, <<"octet-stream">>}
                    ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.global_provisioner_templates">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.global_provisioner_templates">>, ?MODULE, content_types_accepted),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.global_provisioner_templates">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.global_provisioner_templates">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.global_provisioner_templates">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.global_provisioner_templates">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.global_provisioner_templates">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.global_provisioner_templates">>, ?MODULE, delete),
    _ = crossbar_bindings:bind(<<"v1_resource.finish_request.put.devices">>, ?MODULE, device_updated),
    crossbar_bindings:bind(<<"v1_resource.finish_request.post.devices">>, ?MODULE, device_updated).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types provided by this module
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/3 :: (#cb_context{}, path_token(), path_token()) -> crossbar_content_handlers().
content_types_provided(#cb_context{req_verb = <<"get">>}=Context, DocId, ?IMAGE_REQ) ->
    case couch_mgr:open_doc(?WH_PROVISIONER_DB, DocId) of
        {error, _} -> Context;
        {ok, JObj} ->
            ContentType = wh_json:get_value([<<"_attachments">>, ?IMAGE_REQ, <<"content_type">>]
                                            ,JObj
                                            ,<<"application/octet-stream">>),
            [Type, SubType] = binary:split(ContentType, <<"/">>),
            lager:debug("found attachement of content type: ~s/~s~n", [Type, SubType]),
            Context#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
    end;
content_types_provided(Context, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Add content types accepted by this module
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = <<"put">>}=Context, _, ?IMAGE_REQ) ->
    Context#cb_context{content_types_accepted=[{from_binary, ?MIME_TYPES}]};
content_types_accepted(#cb_context{req_verb = <<"post">>}=Context, _, ?IMAGE_REQ) ->
    Context#cb_context{content_types_accepted=[{from_binary, ?MIME_TYPES}]};
content_types_accepted(Context, _, _) ->
    Context.

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
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(_) ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(_, ?IMAGE_REQ) ->
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
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(_) ->
    true.
resource_exists(_, ?IMAGE_REQ) ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% When a device is successfully updated (put or post) then attempt
%% to provision it
%% @end
%%--------------------------------------------------------------------
-spec device_updated/1 :: (#cb_context{}) -> ok.
device_updated(#cb_context{resp_status=success}=Context) ->
    case get_template(Context) of
        {error, _} -> ok;
        {ok, JObj} ->
            provisioner_util:send_provisioning_template(JObj, Context)
    end,
    ok;
device_updated(_) ->
    ok.

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
    load_provisioner_template_summary(Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_provisioner_template(Context#cb_context{db_name=?WH_PROVISIONER_DB}).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId) ->
    load_provisioner_template(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"post">>}=Context, DocId) ->
    update_provisioner_template(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId) ->
    load_provisioner_template(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB}).

validate(#cb_context{req_verb = <<"get">>}=Context, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"put">>}=Context, DocId, ?IMAGE_REQ) ->
    upload_template_image(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"post">>}=Context, DocId, ?IMAGE_REQ) ->
    upload_template_image(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB});
validate(#cb_context{req_verb = <<"delete">>}=Context, DocId, ?IMAGE_REQ) ->
    load_template_image(DocId, Context#cb_context{db_name=?WH_PROVISIONER_DB}).

-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{doc=JObj}=Context, DocId) ->
    %% see note at top of file
    Template = wh_json:get_value(<<"template">>, JObj),
    Doc = wh_json:delete_key(<<"template">>, JObj),
    case crossbar_doc:save(Context#cb_context{doc=Doc}) of
        #cb_context{resp_status=success, resp_data=SavedResp}=Context1 ->
            Opts = [{headers, [{content_type, "application/json"}]}],
            case crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, wh_json:encode(Template), Context, Opts) of
                #cb_context{resp_data=success} ->
                    Context1#cb_context{resp_data=wh_json:set_value(<<"template">>, Template, SavedResp)};
                Else -> Else
            end;
        Else -> Else
    end.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{doc=JObj}=Context) ->
    %% see note at top of file
    Template = wh_json:get_value(<<"template">>, JObj),
    Doc = wh_json:delete_key(<<"template">>, JObj),
    case crossbar_doc:save(Context#cb_context{doc=Doc}) of
        #cb_context{resp_status=success, doc=SavedDoc, resp_data=SavedResp}=Context1 ->
            DocId = wh_json:get_value(<<"_id">>, SavedDoc),
            Opts = [{headers, [{content_type, "application/json"}]}],
            case crossbar_doc:save_attachment(DocId, ?TEMPLATE_ATTCH, wh_json:encode(Template), Context, Opts) of
                #cb_context{resp_status=success} ->
                    Context1#cb_context{resp_data=wh_json:set_value(<<"template">>, Template, SavedResp)};
                Else -> Else
            end;
        Else -> Else
    end.

-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, _) ->
    crossbar_doc:delete(Context).

-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(#cb_context{req_files=[{_, JObj}]}=Context, DocId, ?IMAGE_REQ) ->
    Contents = wh_json:get_value(<<"contents">>, JObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{headers, [{content_type, wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec put/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
put(#cb_context{req_files=[{_, JObj}]}=Context, DocId, ?IMAGE_REQ) ->
    Contents = wh_json:get_value(<<"contents">>, JObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], JObj, <<"application/octet-stream">>),
    Opts = [{headers, [{content_type, wh_util:to_list(CT)}]}],
    crossbar_doc:save_attachment(DocId, ?IMAGE_REQ, Contents, Context, Opts).

-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(Context, DocId, ?IMAGE_REQ) ->
    crossbar_doc:delete_attachment(DocId, ?IMAGE_REQ, Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec load_template_image/2 :: (path_token(), #cb_context{}) -> #cb_context{}.
load_template_image(DocId, Context) ->
    crossbar_doc:load_attachment(DocId, ?IMAGE_REQ, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec upload_template_image/2 :: (path_token(), #cb_context{}) -> #cb_context{}.
upload_template_image(_, #cb_context{req_files=[]}=Context) ->
    crossbar_util:response_invalid_data(<<"please provide an image file">>, Context);
upload_template_image(_, #cb_context{req_files=[{_, _}]}=Context) ->
    crossbar_util:response([], Context);
upload_template_image(_, #cb_context{req_files=[_|_]}=Context) ->
    crossbar_util:response_invalid_data(<<"please provide a single image file">>, Context).

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
create_provisioner_template(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"provisioner_templates">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Updates = [fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"provisioner_template">>, J) end
                       ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                       ,fun(J) -> wh_json:set_value(<<"pvt_provider">>, <<"provisioner.net">>, J) end
                       ,fun(J) -> wh_json:set_value(<<"pvt_provisioner_type">>, <<"global">>, J) end
                      ],
            provisioner_util:get_provision_defaults(Context#cb_context{doc=lists:foldr(fun(F, J) -> F(J) end, JObj, Updates)})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a provision template document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_provisioner_template/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_provisioner_template(DocId, Context) ->
    %% see note at top of file
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, resp_data=RespJObj}=Context1 ->
            case crossbar_doc:load_attachment(DocId, ?TEMPLATE_ATTCH, Context) of
                #cb_context{resp_status=success, resp_data=TemplateJObj} ->
                    Template = wh_json:decode(TemplateJObj),
                    Context1#cb_context{resp_data=wh_json:set_value(<<"template">>, Template, RespJObj)};
                Else -> Else
            end;
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing provision template document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_provisioner_template/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update_provisioner_template(DocId, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"provisioner_templates">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            crossbar_doc:load_merge(DocId, JObj, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If the device specifies a global template id then return that
%% template
%% @end
%%--------------------------------------------------------------------
-spec get_template/1 :: (#cb_context{}) -> {ok, wh_json:json_object()} |
                                           {error, term()}.
get_template(#cb_context{doc=Device}) ->
    DocId = wh_json:get_value([<<"provision">>, <<"id">>], Device),
    case is_binary(DocId) andalso couch_mgr:fetch_attachment(?WH_PROVISIONER_DB, DocId, ?TEMPLATE_ATTCH) of
        false ->
            lager:debug("unknown template id ~s", [DocId]),
            {error, not_found};
        {error, _R}=E ->
            lager:debug("could not fetch template doc ~s: ~p", [DocId, _R]),
            E;
        {ok, Attachment} ->
            {ok, wh_json:decode(Attachment)}
    end.
