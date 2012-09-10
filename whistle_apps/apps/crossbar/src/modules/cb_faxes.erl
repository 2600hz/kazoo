%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% Listing of all expected v1 callbacks
%%%
%%% @end
%%% @contributors:
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cb_faxes).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2, allowed_methods/3
         ,resource_exists/0, resource_exists/1, resource_exists/2, resource_exists/3
         ,content_types_provided/4
         ,validate/1, validate/2, validate/3, validate/4
         ,get/1, get/2, get/3, get/4
         ,put/1, put/2
         ,post/1, post/2
         ,delete/3
        ]).

-include("include/crossbar.hrl").

-define(ATTACHMENT, <<"attachment">>).

-define(PVT_TYPE, <<"fax">>).
-define(PVT_FUNS, [fun add_pvt_type/2
                   ,fun add_pvt_status/2
                   ,fun add_pvt_account_db/2
                   ,fun add_pvt_account_id/2
                   ,fun reset_attempts/2
                  ]).
-define(CB_LIST, <<"media/listing_private_media">>).
-define(FAX_FILE_TYPE, <<"tiff">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.faxes">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.faxes">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.faxes">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.faxes">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.faxes">>, ?MODULE, get),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.faxes">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.faxes">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.faxes">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods() | [].
-spec allowed_methods/1 :: (path_token()) -> http_methods() | [].
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods() | [].
-spec allowed_methods/3 :: (path_token(), path_token(), path_token()) -> http_methods() | [].

allowed_methods() ->
    ['PUT'].

allowed_methods(<<"incoming">>) ->
    ['GET'];
allowed_methods(<<"outgoing">>) ->
    ['GET'].

allowed_methods(<<"incoming">>, _Id) ->
    ['GET'];
allowed_methods(<<"outgoing">>, _Id) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(<<"incoming">>, _Id, ?ATTACHMENT) ->
    ['GET'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /faxes => []
%%    /faxes/foo => [<<"foo">>]
%%    /faxes/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
-spec resource_exists/3 :: (path_token(), path_token(), path_token()) -> 'true'.

resource_exists() -> true.
resource_exists(<<"incoming">>) -> true;
resource_exists(<<"outgoing">>) -> true.
resource_exists(<<"incoming">>, _Id) -> true;
resource_exists(<<"outgoing">>, _Id) -> true.
resource_exists(<<"incoming">>, _Id, ?ATTACHMENT) -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.
content_types_provided(#cb_context{req_verb = <<"get">>}=Context, <<"incoming">>, FaxId, ?ATTACHMENT) ->
    case load_fax_meta(FaxId, Context) of
        #cb_context{resp_status=success, doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj)) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
            end
    end;
content_types_provided(Context, _, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /faxes mights load a list of fax objects
%% /faxes/123 might load the fax object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec validate/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.

validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create(Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = <<"get">>}=Context, <<"outgoing">>) ->
    outgoing_summary(Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = <<"get">>}=Context, <<"incoming">>) ->
    incoming_summary(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, <<"incoming">>, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"get">>}=Context, <<"outgoing">>, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = <<"post">>}=Context, <<"outgoing">>, Id) ->
    update(Id, Context#cb_context{db_name=?WH_FAXES});
validate(#cb_context{req_verb = <<"delete">>}=Context, <<"outgoing">>, Id) ->
    read(Id, Context#cb_context{db_name=?WH_FAXES}).

validate(#cb_context{req_verb = <<"get">>}=Context, <<"inbound">>, Id, ?ATTACHMENT) ->
    load_fax_binary(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is a GET, execute necessary code to fulfill the GET
%% request. Generally, this will involve stripping pvt fields and loading
%% the resource into the resp_data, resp_headers, etc...
%% @end
%%--------------------------------------------------------------------
-spec get/1 :: (#cb_context{}) -> #cb_context{}.
-spec get/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec get/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
-spec get/4 :: (#cb_context{}, path_token(), path_token(), path_token()) -> #cb_context{}.

get(#cb_context{}=Context) ->
    Context.
get(#cb_context{}=Context, _) ->
    Context.
get(#cb_context{}=Context, _, _) ->
    Context.
get(#cb_context{}=Context, _, _, _) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (#cb_context{}) -> #cb_context{}.
-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
put(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
put(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/1 :: (#cb_context{}) -> #cb_context{}.
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(#cb_context{}=Context) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, <<"outgoing">>, _Id) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"faxes">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a fax document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_fax_meta/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_fax_meta(FaxId, Context) ->
    crossbar_doc:load(FaxId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"faxes">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec incoming_summary/1 :: (#cb_context{}) -> #cb_context{}.
incoming_summary(#cb_context{}=Context) ->
    crossbar_doc:load_view(?CB_LIST
                           ,[{startkey, [?FAX_FILE_TYPE]}
                             ,{endkey, [?FAX_FILE_TYPE, wh_json:new()]}
                             ,include_docs
                            ]
                           ,Context
                           ,fun normalize_incoming_view_results/2
                          ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a fax doc
%% @end
%%--------------------------------------------------------------------
-spec load_fax_binary/2 :: (path_token(), #cb_context{}) -> #cb_context{}.
load_fax_binary(FaxId, #cb_context{resp_headers=RespHeaders}=Context) ->
    case load_fax_meta(FaxId, Context) of
        #cb_context{resp_status=success, doc=JObj} ->
            FaxMeta = wh_json:get_value([<<"_attachments">>], JObj),
            case wh_json:get_keys(FaxMeta) of
                [] -> crossbar_util:response_bad_identifier(FaxId, Context);
                [Attachment|_] ->
                    Context1 = crossbar_doc:load_attachment(JObj, Attachment, Context),
                    Context1#cb_context{resp_headers = [{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                        ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], FaxMeta)}
                                                        ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], FaxMeta)}
                                                        | RespHeaders
                                                       ]
                                       ,resp_etag=undefined}
            end;
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec outgoing_summary/1 :: (#cb_context{}) -> #cb_context{}.
outgoing_summary(#cb_context{}=Context) ->
    crossbar_doc:load_view(<<"faxes/crossbar_listing">>
                           ,[include_docs]
                           ,Context
                           ,fun normalize_view_results/2
                          ).

-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_incoming_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_incoming_view_results(JObj, Acc) ->
    [wh_json:public_fields(wh_json:get_value(<<"doc">>, JObj))|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

-spec add_pvt_status/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_status(JObj, _) ->
    wh_json:set_value(<<"pvt_job_status">>, <<"pending">>, JObj).

-spec add_pvt_account_db/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_account_db(JObj, #cb_context{account_id=AccountId}) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        undefined ->
            DBName = wh_util:format_account_id(AccountId, encoded),
            wh_json:set_value(<<"pvt_account_db">>, DBName, JObj);
        _Else -> JObj
    end.

-spec add_pvt_account_id/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_account_id(JObj, #cb_context{account_id=AccountId}) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        undefined ->
            wh_json:set_value(<<"pvt_account_id">>, wh_util:format_account_id(AccountId, raw), JObj);
        _Else -> JObj
    end.

-spec reset_attempts/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
reset_attempts(JObj, _) ->
    wh_json:set_value(<<"attempts">>, 0, JObj).
