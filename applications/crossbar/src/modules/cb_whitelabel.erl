%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
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

-include("../crossbar.hrl").

-define(WHITELABEL_ID, <<"whitelabel">>).
-define(LOGO_REQ, <<"logo">>).

-define(WHITELABEL_MIME_TYPES, [{<<"image">>, <<"jpg">>}
                                ,{<<"image">>, <<"jpeg">>}
                                ,{<<"image">>, <<"png">>}
                                ,{<<"image">>, <<"gif">>}
                                ,{<<"application">>, <<"base64">>}
                                ,{<<"application">>, <<"x-base64">>}
                               ]).

-define(AGG_VIEW_WHITELABEL_DOMAIN, <<"accounts/list_by_whitelabel_domain">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.whitelabel">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.whitelabel">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.whitelabel">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.whitelabel">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.whitelabel">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.get.whitelabel">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.whitelabel">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.whitelabel">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.whitelabel">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(?LOGO_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_) ->
    [?HTTP_GET].
allowed_methods(_, ?LOGO_REQ) ->
    [?HTTP_GET].


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(?LOGO_REQ) -> 'true';
resource_exists(_) -> 'true'.
resource_exists(_, ?LOGO_REQ) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(#cb_context{req_nouns=[{<<"whitelabel">>, [_]}]
                      ,req_verb= ?HTTP_GET
                     }) ->
    'true';
authorize(#cb_context{req_nouns=[{<<"whitelabel">>, [_ | [?LOGO_REQ]]}]
                      ,req_verb= ?HTTP_GET
                     }) ->
    'true';
authorize(_) ->
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(#cb_context{req_nouns=[{<<"whitelabel">>, [_]}]
                         ,req_verb= ?HTTP_GET
                        }) ->
    'true';
authenticate(#cb_context{req_nouns=[{<<"whitelabel">>, [_ | [?LOGO_REQ]]}]
                         ,req_verb= ?HTTP_GET
                        }) ->
    'true';
authenticate(_) ->
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, ?LOGO_REQ) ->
    case find_whitelabel(?WHITELABEL_ID, Context, 'meta') of
        #cb_context{resp_status='success', doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj, wh_json:new())) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end
    end;
content_types_provided(Context, _) ->
    Context.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
                                    cb_context:context().
content_types_provided(#cb_context{req_verb = ?HTTP_GET}=Context, Domain, ?LOGO_REQ) ->
    case find_whitelabel(Domain, Context, 'meta') of
        #cb_context{resp_status='success', doc=JObj} ->
            case wh_json:get_keys(wh_json:get_value([<<"_attachments">>], JObj)) of
                [] -> Context;
                [Attachment|_] ->
                    CT = wh_json:get_value([<<"_attachments">>, Attachment, <<"content_type">>], JObj),
                    [Type, SubType] = binary:split(CT, <<"/">>),
                    Context#cb_context{content_types_provided=[{'to_binary', [{Type, SubType}]}]}
            end
    end.

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(#cb_context{req_verb = ?HTTP_POST}=Context, ?LOGO_REQ) ->
    CTA = [{'from_binary', ?WHITELABEL_MIME_TYPES}],
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
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    load_whitelabel_meta(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = ?HTTP_PUT}=Context) ->
    validate_request(undefined, Context);
validate(#cb_context{req_verb = ?HTTP_POST}=Context) ->
    validate_request(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = ?HTTP_DELETE
                     ,req_data=_Data
                    }=Context) ->
    load_whitelabel_meta(?WHITELABEL_ID, Context).

validate(#cb_context{req_verb = ?HTTP_GET}=Context, ?LOGO_REQ) ->
    load_whitelabel_binary(?WHITELABEL_ID, Context);
validate(#cb_context{req_verb = ?HTTP_POST
                     ,req_files=[]
                    }=Context, ?LOGO_REQ) ->
    Message = <<"please provide an image file">>,
    cb_context:add_validation_error(<<"file">>, <<"required">>, Message, Context);
validate(#cb_context{req_verb = ?HTTP_POST
                     ,req_files=[{_Filename, FileObj}]
                    }=Context, ?LOGO_REQ) ->
    case load_whitelabel_meta(?WHITELABEL_ID, Context) of
        #cb_context{resp_status='success', doc=JObj}=C ->
            CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj, <<"application/octet-stream">>),
            Size = wh_json:get_integer_value([<<"headers">>, <<"content_length">>]
                                             ,FileObj
                                             ,byte_size(wh_json:get_value(<<"contents">>, FileObj, <<>>))),
            Props = [{<<"content_type">>, CT}
                     ,{<<"content_length">>, Size}
                    ],
            validate_request(?WHITELABEL_ID, C#cb_context{req_data=wh_json:set_values(Props, JObj)});
        Else -> Else
    end;
validate(#cb_context{req_verb = ?HTTP_POST}=Context, ?LOGO_REQ) ->
    Message = <<"please provide a single image file">>,
    cb_context:add_validation_error(<<"file">>, <<"maxItems">>, Message, Context);
validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id='undefined'
                    }=Context, Domain) ->
    find_whitelabel(Domain, Context, 'meta').

validate(#cb_context{req_verb = ?HTTP_GET
                     ,account_id='undefined'
                    }=Context, Domain, ?LOGO_REQ) ->
    find_whitelabel(Domain, Context, binary).


-spec get(cb_context:context(), path_token()) -> cb_context:context().
get(Context, ?LOGO_REQ) ->
    Context#cb_context{resp_headers=[{<<"Content-Type">>
                                          ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                     ,{<<"Content-Length">>
                                           ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                     | Context#cb_context.resp_headers
                                    ]}.


-spec get(cb_context:context(), path_token(), path_token()) -> cb_context:context().
get(Context, _, ?LOGO_REQ) ->
    Context#cb_context{resp_headers=[{<<"Content-Type">>
                                          ,wh_json:get_value(<<"content-type">>, Context#cb_context.doc, <<"application/octet-stream">>)}
                                     ,{<<"Content-Length">>
                                           ,wh_util:to_binary(binary:referenced_byte_size(Context#cb_context.resp_data))}
                                     | Context#cb_context.resp_headers
                                    ]}.

-spec put(cb_context:context()) -> cb_context:context().
put(#cb_context{}=Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().

post(#cb_context{}=Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

post(Context, ?LOGO_REQ) ->
    update_whitelabel_binary(?WHITELABEL_ID, Context).

-spec delete(cb_context:context()) -> cb_context:context().
delete(#cb_context{}=Context) ->
    maybe_cleanup_account_definition(crossbar_doc:delete(Context, 'permanent')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc (based on a domain)
%% @end
%%--------------------------------------------------------------------
-spec find_whitelabel(ne_binary(), cb_context:context(), 'meta' | 'binary') ->
                             cb_context:context().
find_whitelabel(Domain, Context, LookingFor) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Domain)}],
    case crossbar_doc:load_view(?AGG_VIEW_WHITELABEL_DOMAIN, ViewOptions, Context#cb_context{db_name=?WH_ACCOUNTS_DB}) of
        #cb_context{resp_status='success', doc=[JObj]}=C ->
            Db = wh_json:get_ne_value([<<"value">>, <<"account_db">>], JObj),
            Id = wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj),
            case LookingFor of
                'meta' -> load_whitelabel_meta(?WHITELABEL_ID, C#cb_context{db_name=Db, account_id=Id});
                'binary' -> load_whitelabel_binary(?WHITELABEL_ID, C#cb_context{db_name=Db, account_id=Id})
            end;
        #cb_context{resp_status='success'} ->
            cb_context:add_system_error('bad_identifier', [{'details', Domain}], Context);
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a whitelabel document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_meta(ne_binary(), cb_context:context()) -> cb_context:context().
load_whitelabel_meta(WhitelabelId, Context) ->
    crossbar_doc:load(WhitelabelId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(WhitelabelId, Context) ->
    validate_unique_domain(WhitelabelId, Context).

validate_unique_domain(WhitelabelId, #cb_context{req_data=JObj
                                                 ,account_id=AccountId
                                                }=Context) ->
    Domain = wh_json:get_ne_value(<<"domain">>, JObj),
    case is_domain_unique(AccountId, Domain) of
        'true' -> check_whitelabel_schema(WhitelabelId, Context);
        'false' ->
            C = cb_context:add_validation_error(<<"domain">>
                                                ,<<"unique">>
                                                ,<<"Whitelable domain is already in use">>
                                                ,Context),
            check_whitelabel_schema(WhitelabelId, C)
    end.

check_whitelabel_schema(WhitelabelId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(WhitelabelId, C) end,
    cb_context:validate_request_data(<<"whitelabel">>, Context, OnSuccess).

on_successful_validation('undefined', #cb_context{doc=Doc}=Context) ->
    Props = [{<<"pvt_type">>, <<"whitelabel">>}
             ,{<<"_id">>, ?WHITELABEL_ID}
            ],
    Context#cb_context{doc=wh_json:set_values(Props, Doc)};
on_successful_validation(WhitelabelId, Context) ->
    crossbar_doc:load_merge(WhitelabelId, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------
-spec load_whitelabel_binary(path_token(), cb_context:context()) -> cb_context:context().
load_whitelabel_binary(WhitelabelId, #cb_context{resp_headers=RespHeaders}=Context) ->
    case load_whitelabel_meta(WhitelabelId, Context) of
        #cb_context{resp_status='success', doc=JObj} ->
            WhitelabelMeta = wh_json:get_value([<<"_attachments">>], JObj, wh_json:new()),
            case wh_json:get_keys(WhitelabelMeta) of
                [] -> crossbar_util:response_bad_identifier(WhitelabelId, Context);
                [Attachment|_] ->
                    Context1 = crossbar_doc:load_attachment(JObj, Attachment, Context),
                    Context1#cb_context{resp_headers=[{<<"Content-Disposition">>, <<"attachment; filename=", Attachment/binary>>}
                                                        ,{<<"Content-Type">>, wh_json:get_value([Attachment, <<"content_type">>], WhitelabelMeta)}
                                                        ,{<<"Content-Length">>, wh_json:get_value([Attachment, <<"length">>], WhitelabelMeta)}
                                                        | RespHeaders
                                                       ]
                                        ,resp_etag='undefined'}
            end;
        Context1 -> Context1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update the binary attachment of a whitelabel doc
%% @end
%%--------------------------------------------------------------------
-spec update_whitelabel_binary(path_token(), cb_context:context()) -> cb_context:context().
update_whitelabel_binary(WhitelabelId, #cb_context{doc=JObj
                                                   ,req_files=[{Filename, FileObj}]
                                                   ,db_name=Db
                                                  }=Context) ->
    Contents = wh_json:get_value(<<"contents">>, FileObj),
    CT = wh_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    lager:debug("file content type: ~s", [CT]),
    Opts = [{'headers', [{'content_type', wh_util:to_list(CT)}]}],
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
-spec attachment_name(ne_binary(), ne_binary()) -> ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun(A) ->
                          case wh_util:is_empty(A) of
                              'true' -> wh_util:to_hex_binary(crypto:rand_bytes(16));
                              'false' -> A
                          end
                  end
                  ,fun(A) ->
                           case wh_util:is_empty(filename:extension(A)) of
                               'false' -> A;
                               'true' ->
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
-spec content_type_to_extension(ne_binary()) -> ne_binary().
content_type_to_extension(<<"image/jpg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/jpeg">>) -> <<"jpg">>;
content_type_to_extension(<<"image/png">>) -> <<"png">>;
content_type_to_extension(<<"image/gif">>) -> <<"gif">>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_domain_unique(ne_binary(), ne_binary()) -> boolean().
is_domain_unique(AccountId, Domain) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Domain)}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} ->
            wh_json:get_ne_value([<<"value">>, <<"account_id">>], JObj) =:= AccountId;
        {'ok', _} -> 'false';
        {'error', _R} ->
            lager:debug("unable to get whitelable domain view: ~p", [_R]),
            'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_account_definition(cb_context:context()) -> cb_context:context().
maybe_update_account_definition(#cb_context{resp_status='success'
                                            ,account_id=AccountId
                                            ,doc=JObj
                                           }=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status='success', doc=AccountDoc}=AccountContext ->
            Domain = wh_json:get_ne_value(<<"domain">>, JObj),
            AccountDoc1 = wh_json:set_value(<<"pvt_whitelabel_domain">>, Domain, AccountDoc),
            _ = cb_accounts:post(AccountContext#cb_context{doc=AccountDoc1}, AccountId),
            Context;
        Else ->
            Else
    end;
maybe_update_account_definition(Context) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_cleanup_account_definition(cb_context:context()) -> cb_context:context().
maybe_cleanup_account_definition(#cb_context{resp_status='success'
                                             ,account_id=AccountId
                                            }=Context) ->
    case crossbar_doc:load(AccountId, Context) of
        #cb_context{resp_status='success', doc=AccountDoc}=AccountContext ->
            AccountDoc1 = wh_json:delete_key(<<"pvt_whitelabel_domain">>, AccountDoc),
            _ = cb_accounts:post(AccountContext#cb_context{doc=AccountDoc1}, AccountId),
            Context;
        Else ->
            Else
    end;
maybe_cleanup_account_definition(Context) -> Context.
