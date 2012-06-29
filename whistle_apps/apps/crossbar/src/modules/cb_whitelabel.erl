%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Account module
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton <jon@2600hz.com>
%%%-------------------------------------------------------------------
-module(cb_whitelabel).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,authorize/1, authenticate/1
         ,validate/1, validate/2, validate/3
         ,content_types_provided/2, content_types_provided/3
         ,content_types_accepted/2
         ,get/2, get/3
         ,put/1
         ,post/1, post/2
         ,delete/1
        ]).

-include_lib("crossbar/include/crossbar.hrl").

-define(WHITELABEL_ID, <<"whitelabel">>).
-define(LOGO_REQ, <<"logo">>).

-define(WHITELABEL_MIME_TYPES, [{<<"image">>, <<"jpg">>}
                           ,{<<"image">>, <<"jpeg">>}
                           ,{<<"image">>, <<"png">>}
                           ,{<<"image">>, <<"gif">>}
                           ,{<<"application">>, <<"base64">>}
                           ,{<<"application">>, <<"x-base64">>}
                          ]).

-define(PVT_TYPE, <<"whitelabel">>).
-define(PVT_FUNS, [fun add_pvt_type/2, fun set_id/2]).

-define(AGG_VIEW_WHITELABEL_DOMAIN, <<"accounts/list_by_whitelabel_domain">>).

%-define(CB_LIST, <<"media/crossbar_listing">>).
%-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".media">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.authenticate">>, ?MODULE, authenticate),
    _ = crossbar_bindings:bind(<<"v1_resource.authorize">>, ?MODULE, authorize),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_provided.whitelabel">>, ?MODULE, content_types_provided),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.whitelabel">>, ?MODULE, content_types_accepted),
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.whitelabel">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.whitelabel">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.whitelabel">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.get.whitelabel">>, ?MODULE, get),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.whitelabel">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.whitelabel">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.whitelabel">>, ?MODULE, delete).

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
    ['GET', 'PUT', 'POST', 'DELETE'].
allowed_methods(?LOGO_REQ) ->
    ['GET', 'POST'];
allowed_methods(_) ->
    ['GET'].
allowed_methods(_, ?LOGO_REQ) ->
    ['GET'].


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
resource_exists() -> true.
resource_exists(?LOGO_REQ) -> true;
resource_exists(_) -> true.
resource_exists(_, ?LOGO_REQ) -> true. 

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize/1 :: (#cb_context{}) -> boolean().
authorize(#cb_context{req_nouns=[{<<"whitelabel">>, [_]}], req_verb= <<"get">>}) ->
    true;
authorize(#cb_context{req_nouns=[{<<"whitelabel">>, [_ | [?LOGO_REQ]]}], req_verb= <<"get">>}) ->
    true;
authorize(_) ->
    false.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate/1 :: (#cb_context{}) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"whitelabel">>, [_]}], req_verb= <<"get">>}) ->
    true;
authenticate(#cb_context{req_nouns=[{<<"whitelabel">>, [_ | [?LOGO_REQ]]}], req_verb= <<"get">>}) ->
    true;
authenticate(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
content_types_provided(#cb_context{req_verb = <<"get">>}=Context, ?LOGO_REQ) ->
    case load_whitelabel_meta(?WHITELABEL_ID, Context) of
        #cb_context{resp_status=success, doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj)) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
            end
    end;
content_types_provided(Context, _) ->
    Context.

-spec content_types_provided/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
content_types_provided(#cb_context{req_verb = <<"get">>}=Context, Domain, ?LOGO_REQ) ->
    case find_whitelabel_meta(Domain, Context) of
        #cb_context{resp_status=success, doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj)) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{to_binary, [{Type, SubType}]}]}
            end
    end.

-spec content_types_accepted/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
content_types_accepted(#cb_context{req_verb = <<"post">>}=Context, ?LOGO_REQ) ->
    CTA = [{from_binary, ?WHITELABEL_MIME_TYPES}],
    Context#cb_context{content_types_accepted=CTA};
content_types_accepted(Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    load_whitelabel_meta(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create_whitelabel_meta(Context);
validate(#cb_context{req_verb = <<"post">>}=Context) ->
    update_whitelabel_meta(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = <<"delete">>, req_data=_Data}=Context) ->
    load_whitelabel_meta(?WHITELABEL_ID, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?LOGO_REQ) ->
    load_whitelabel_binary(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = <<"post">>, req_files=[]}=Context, ?LOGO_REQ) ->
    E = wh_json:set_value([<<"content_size">>, <<"minLength">>], <<"No file uploaded">>, wh_json:new()),
    crossbar_util:response_invalid_data(E, Context);
validate(#cb_context{req_verb = <<"post">>, req_files=[{_Filename, FileObj}]}=Context, ?LOGO_REQ) ->
    case load_whitelabel_meta(?WHITELABEL_ID, Context) of
        #cb_context{resp_status=success, doc=Data}=Context1 ->
            Updaters = [fun(J) ->
                                CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj, <<"application/octet-stream">>),
                                wh_json:set_value(<<"content_type">>, CT, J)
                        end
                        ,fun(J) ->
                                 Size = wh_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                                                 ,FileObj
                                                                  ,byte_size(wh_json:get_value(<<"contents">>, FileObj, <<>>))),
                                 wh_json:set_value(<<"content_length">>, Size, J)
                         end
                       ],
            case wh_json_validator:is_valid(lists:foldr(fun(F, J) -> F(J) end, Data, Updaters), <<"whitelabel">>) of
                {fail, Errors} ->
                    crossbar_util:response_invalid_data(Errors, Context);
                {pass, JObj} ->
                    Context1#cb_context{doc=JObj}
            end;
        Else -> Else
    end;
validate(#cb_context{req_verb = <<"post">>}=Context, ?LOGO_REQ) ->
    lager:debug("Multiple files in request to save attachment"),
    E = wh_json:set_value([<<"content_size">>, <<"maxLength">>], <<"Uploading multiple files is not supported">>, wh_json:new()),
    crossbar_util:response_invalid_data(E, Context);
validate(#cb_context{req_verb = <<"get">>, account_id=undefined}=Context, Domain) ->
    find_whitelabel_meta(Domain, Context).

validate(#cb_context{req_verb = <<"get">>, account_id=undefined}=Context, Domain, ?LOGO_REQ) ->
    find_whitelabel_binary(Domain, Context).


-spec get/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
get(Context, ?LOGO_REQ) ->
    Context#cb_context{resp_headers = [{<<"Content-Type">>
                                            ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                       ,{<<"Content-Length">>
                                             ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                       | Context#cb_context.resp_headers]}.


-spec get/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
get(Context, _, ?LOGO_REQ) ->
    Context#cb_context{resp_headers = [{<<"Content-Type">>
                                            ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                       ,{<<"Content-Length">>
                                             ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                       | Context#cb_context.resp_headers]}.

-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{doc=JObj, account_id=AccountId}=Context) ->
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success}=Context1 ->
            case crossbar_doc:load(AccountId, Context) of
                #cb_context{resp_status=success, doc=AccountDoc}=AccountContext ->
                    Domain = wh_json:get_ne_value(<<"domain">>, JObj),
                    AccountDoc1 = wh_json:set_value(<<"pvt_whitelabel_domain">>, Domain, AccountDoc),
                    _ = cb_accounts:post(AccountContext#cb_context{doc=AccountDoc1}, AccountId),
                    Context1;
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

-spec post/1 :: (#cb_context{}) -> #cb_context{}.
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.

post(#cb_context{doc=JObj, account_id=AccountId}=Context) ->
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success}=Context1 ->
            case crossbar_doc:load(AccountId, Context) of
                #cb_context{resp_status=success, doc=AccountDoc}=AccountContext ->
                    Domain = wh_json:get_ne_value(<<"domain">>, JObj),
                    AccountDoc1 = wh_json:set_value(<<"pvt_whitelabel_domain">>, Domain, AccountDoc),
                    _ = cb_accounts:post(AccountContext#cb_context{doc=AccountDoc1}, AccountId),
                    Context1;
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

post(Context, ?LOGO_REQ) ->
    update_whitelabel_binary(?WHITELABEL_ID, Context).

-spec delete/1 :: (#cb_context{}) -> #cb_context{}.
delete(#cb_context{account_id=AccountId}=Context) ->
    case crossbar_doc:delete(Context, permanent) of 
        #cb_context{resp_status=success}=Context1 ->
            case crossbar_doc:load(AccountId, Context) of
                #cb_context{resp_status=success, doc=AccountDoc}=AccountContext ->
                    AccountDoc1 = wh_json:delete_key(<<"pvt_whitelabel_domain">>, AccountDoc),
                    _ = cb_accounts:post(AccountContext#cb_context{doc=AccountDoc1}, AccountId),
                    Context1;
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a whitelabel document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_meta/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_whitelabel_meta(WhitelabelId, Context) ->
    crossbar_doc:load(WhitelabelId, Context).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a whitelabel document based on a domain
%% @end
%%--------------------------------------------------------------------
-spec find_whitelabel_meta/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
find_whitelabel_meta(Domain, Context) ->
    Domain1 = wh_util:to_lower_binary(Domain),
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, [{<<"key">>, Domain1}]) of
        {ok, []} ->
            crossbar_util:response(error, <<"domain not found">>, 404, Context);
        {ok, [_ | [_]]} ->
            crossbar_util:response(error, <<"multiple domains found">>, 409, Context);
        {ok, [JObj]} ->
            Db = wh_json:get_ne_value([<<"value">>, <<"account_db">>], JObj),
            Id = wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj),
            load_whitelabel_meta(?WHITELABEL_ID, Context#cb_context{db_name=Db, account_id=Id})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new whitelabel_meta document with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create_whitelabel_meta/1 :: (#cb_context{}) -> #cb_context{}.
create_whitelabel_meta(#cb_context{req_data=Data, account_id=AccountId}=Context) ->
    case wh_json_validator:is_valid(Data, <<"whitelabel">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Domain = wh_json:get_ne_value(<<"domain">>, JObj),
            case is_domain_unique(AccountId, Domain) of
                true ->
                    {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                                     {F(J, C), C}
                                             end
                                             ,{JObj, Context}
                                             ,?PVT_FUNS),
                    Context#cb_context{doc=JObj1, resp_status=success};
                false ->
                    E = wh_json:set_value([<<"domain">>, <<"unique">>], <<"Whitelabel domain is not unique for this system">>, wh_json:new()),
                    crossbar_util:response_invalid_data(E, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing whitelabel document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update_whitelabel_meta/2 :: (binary(), #cb_context{}) -> #cb_context{}.
update_whitelabel_meta(WhitelabelId, #cb_context{req_data=Data, account_id=AccountId}=Context) ->
    case wh_json_validator:is_valid(Data, <<"whitelabel">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            Domain = wh_json:get_ne_value(<<"domain">>, JObj),
            case is_domain_unique(AccountId, Domain) of
                true ->
                    crossbar_doc:load_merge(WhitelabelId, add_pvt_type(JObj, Context), Context);
                false ->
                    E = wh_json:set_value([<<"domain">>, <<"unique">>], <<"Whitelabel domain is not unique for this system">>, wh_json:new()),
                    crossbar_util:response_invalid_data(E, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_binary/2 :: (path_token(), #cb_context{}) -> #cb_context{}.
load_whitelabel_binary(WhitelabelId, #cb_context{resp_headers=RespHeaders}=Context) ->
    case load_whitelabel_meta(WhitelabelId, Context) of
        #cb_context{resp_status=success, doc=JObj} ->
            WhitelabelMeta = wh_json:get_value([<<"_attachments">>], JObj),
            case wh_json:get_keys(WhitelabelMeta) of
                [] -> crossbar_util:response_bad_identifier(WhitelabelId, Context);
                [Attachment|_] ->
                    Context1 = crossbar_doc:load_attachment(JObj, Attachment, Context),
                    Context1#cb_context{resp_headers = [{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                        ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], WhitelabelMeta)}
                                                        ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], WhitelabelMeta)}
                                                        | RespHeaders
                                                       ]
                                       ,resp_etag=undefined}
            end;
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc (based on a domain)
%% @end
%%--------------------------------------------------------------------
-spec find_whitelabel_binary/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
find_whitelabel_binary(Domain, Context) ->
    Domain1 = wh_util:to_lower_binary(Domain),
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, [{<<"key">>, Domain1}]) of
        {ok, []} ->
            crossbar_util:response(error, <<"domain not found">>, 404, Context);
        {ok, [_ | [_]]} ->
            crossbar_util:response(error, <<"multiple domains found">>, 409, Context);
        {ok, [JObj]} ->
            Db = wh_json:get_ne_value([<<"value">>, <<"account_db">>], JObj),
            Id = wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj),
            load_whitelabel_binary(?WHITELABEL_ID, Context#cb_context{db_name=Db, account_id=Id})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------
-spec update_whitelabel_binary/2 :: (path_token(), #cb_context{}) -> #cb_context{}.
update_whitelabel_binary(WhitelabelId, #cb_context{doc=JObj, req_files=[{Filename, FileObj}], db_name=Db}=Context) ->
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{headers, [{content_type, wh_util:to_list(CT)}]}],
    OldAttachments = wh_json:get_value(<<"_attachments">>, JObj, wh_json:new()),
    Id = wh_json:get_value(<<"_id">>, JObj),
    _ = [couch_mgr:delete_attachment(Db, Id, Attachment)
         || Attachment <- wh_json:get_keys(OldAttachments)
        ],
    crossbar_doc:save_attachment(WhitelabelId, attachment_name(Filename, CT), Contents, Context, Opts).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%--------------------------------------------------------------------
-spec attachment_name/2 :: (ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              true -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              false -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               false -> A;
                               true ->
                                   <<A/binary, ".", (content_type_to_extension(CT))/binary>>
                           end
                   end
                 ],
    lists:foldr(fun(F, A) -> F(A) end, Filename, Generators).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert known whitelabel types to extensions
%% @end
%%--------------------------------------------------------------------
-spec content_type_to_extension/1 :: (ne_binary()) -> ne_binary().
content_type_to_extension(<<"image/jpg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/jpeg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/png">>) -> <<"png">>;
content_type_to_extension(<<"image/gif">>) -> <<"gif">>.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Misc. functions required by this module
%% @end
%%--------------------------------------------------------------------
-spec set_id/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
set_id(JObj, _) ->
    wh_json:set_value(<<"_id">>, ?WHITELABEL_ID, JObj).

-spec is_domain_unique/2 :: (ne_binary(), ne_binary()) -> boolean().
is_domain_unique(AccountId, Domain) ->
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, [{<<"key">>, Domain}]) of
        {ok, [JObj|[]]} ->
            case wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj) of
                AccountId ->
                    true;
                _ ->
                    false
            end;
        {ok, [_|_]} ->
            false;
        {ok, []} ->
            true
    end.

