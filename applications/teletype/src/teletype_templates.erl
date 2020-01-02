%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_templates).

-include("teletype.hrl").

-export([init/1, init/2
        ,renderer_name/2
        ,render/2, render/3, render/4
        ,preview/3
        ,fetch_notification/2
        ,write_templates_to_disk/2
        ,templates_source/2
        ]).
-export([doc_id/1]).
-export([params/1]).

-ifdef(TEST).
-export([master_content_types/1]).
-endif.

-type macro() :: {kz_term:ne_binary(), kz_term:ne_binary() | number() | macros()}.
-type macros() :: [macro()].

-define(TEMPLATE_FAILURE_KEY(TemplateId, AccountId)
       ,{?MODULE, TemplateId, AccountId}
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(module()) -> ok.
init(Module)
  when is_atom(Module) ->
    init(Module:id(), params(Module)).

-spec params(module() | kz_term:ne_binary()) -> kz_term:proplist().
params(TemplateId=?NE_BINARY) ->
    Module = kz_term:to_atom(<<(?APP_NAME)/binary, "_", TemplateId/binary>>, true),
    params(Module);
params(Module)
  when is_atom(Module) ->
    TemplateId = Module:id(),
    [{macros, Module:macros()}
    ,{subject, Module:subject()}
    ,{category, Module:category()}
    ,{friendly_name, Module:friendly_name()}
    ,{to, Module:to()}
    ,{from, Module:from()}
    ,{cc, Module:cc()}
    ,{bcc, Module:bcc()}
    ,{reply_to, Module:reply_to()}
    ,{html, TemplateId}
    ,{text, TemplateId}
    ].

-spec init(kz_term:ne_binary(), init_params()) -> 'ok'.
init(TemplateId, Params) ->
    UpdatedParams = props:set_values([{'html', TemplateId}
                                     ,{'text', TemplateId}
                                     ]
                                    ,Params
                                    ),
    DocId = doc_id(TemplateId),
    lager:debug("init template ~s", [DocId]),
    case kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, DocId) of
        {'ok', TemplateJObj} ->
            maybe_update(TemplateJObj, UpdatedParams);
        {'error', 'not_found'} ->
            create(DocId, UpdatedParams);
        {'error', _E} ->
            lager:warning("failed to find template ~s", [DocId])
    end,
    compile_master_renderers(TemplateId).

-spec compile_master_renderers(kz_term:ne_binary()) -> 'ok'.
compile_master_renderers(TemplateId) ->
    _ = [build_renderer(TemplateId, ContentType, Template)
         || {ContentType, Template} <- fetch_master_attachments(TemplateId)
        ],
    %% ?LOG_DEBUG("built master renderer modules for ~s", [TemplateId]).
    lager:debug("built master renderer modules for ~s", [TemplateId]).

-spec build_renderer(kz_term:ne_binary(), kz_term:ne_binary(), binary()) -> 'ok'.
build_renderer(TemplateId, ContentType, Template) ->
    ModuleName = renderer_name(TemplateId, ContentType),
    case kz_template:compile(Template, ModuleName,[{'auto_escape', 'false'}]) of
        {'ok', _} -> 'ok';
        {'error', _E} ->
            ?LOG_DEBUG("failed to render '~s': ~p", [TemplateId, _E]),
            throw({'error', 'failed_template', ModuleName})
    end.

-type template_attachment() :: {kz_term:ne_binary(), binary()}.
-type template_attachments() :: [template_attachment()].

-spec fetch_master_attachments(kz_term:ne_binary()) -> template_attachments().
fetch_master_attachments(TemplateId) ->
    fetch_attachments(TemplateId, ?KZ_CONFIG_DB).

-spec fetch_attachments(kz_term:ne_binary(), kz_term:ne_binary()) -> template_attachments().
fetch_attachments(TemplateId, Account) ->
    AccountDb = case ?KZ_CONFIG_DB =:= Account of
                    true -> ?KZ_CONFIG_DB;
                    false -> kzs_util:format_account_db(Account)
                end,
    DocId = doc_id(TemplateId),
    case fetch_notification(TemplateId, AccountDb) of
        {'ok', TemplateDoc} ->
            kz_json:foldl(fun(AttachmentName, AttachmentProps, Acc) ->
                                  fetch_attachment(AttachmentName, AttachmentProps, Acc, AccountDb, DocId)
                          end
                         ,[]
                         ,kz_doc:attachments(TemplateDoc)
                         );
        {'error', _E} ->
            lager:error("failed to open ~s:~s: ~p", [AccountDb, DocId, _E]),
            []
    end.

-spec fetch_attachment(kz_term:ne_binary(), kz_json:object(), template_attachments(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          template_attachments().
fetch_attachment(AttachmentName, AttachmentProps, Acc, AccountDb, DocId) ->
    ContentType = kz_json:get_value(<<"content_type">>, AttachmentProps),
    case kz_datamgr:fetch_attachment(AccountDb, DocId, AttachmentName) of
        {'ok', AttachmentBinary} ->
            [{ContentType, AttachmentBinary} | Acc];
        {'error', _E} ->
            lager:error("failed to fetch attachment ~s:~s:~s: ~p"
                       ,[AccountDb, DocId, AttachmentName, _E]
                       ),
            Acc
    end.

-spec preview(kz_term:ne_binary(), macros(), kz_json:object()) -> kz_term:proplist().
preview(TemplateId, Macros, DataJObj) ->
    render_farm(TemplateId
               ,Macros
               ,load_preview_templates(DataJObj)
               ).

-spec render(kz_term:ne_binary(), macros()) -> kz_term:proplist().
render(TemplateId, Macros) ->
    render(TemplateId, Macros, kz_json:new()).

-spec render(kz_term:ne_binary(), macros(), kz_json:object()) -> kz_term:proplist().
render(TemplateId, Macros, DataJObj) ->
    put('template_id', TemplateId),
    JMacros = kz_json:from_list_recursive(Macros),
    put('macros', JMacros),
    render(TemplateId, Macros, DataJObj, teletype_util:is_preview(DataJObj)).

-spec render(kz_term:ne_binary(), macros(), kz_json:object(), boolean()) -> kz_term:proplist().
render(TemplateId, Macros, DataJObj, 'false') ->
    case templates_source(TemplateId, DataJObj) of
        'undefined' ->
            lager:warning("no source db for template ~s: ~p", [TemplateId, DataJObj]),
            [];
        ?KZ_CONFIG_DB ->
            lager:debug("rendering system template for ~s", [TemplateId]),
            render_masters(TemplateId, Macros);
        AccountDb ->
            put('template_account_id', kzs_util:format_account_id(AccountDb)),
            lager:debug("rendering template ~s from account ~s", [TemplateId, AccountDb]),
            render_accounts(TemplateId, AccountDb, Macros)
    end;
render(TemplateId, Macros, DataJObj, 'true') ->
    preview(TemplateId, Macros, DataJObj).

-spec templates_source(kz_term:ne_binary(), kz_term:api_binary() | kz_json:object()) -> kz_term:api_binary() | 'not_found'.
templates_source(_TemplateId, 'undefined') ->
    lager:warning("no account id for template ~s, no template to process", [_TemplateId]),
    'undefined';
templates_source(TemplateId, ?KZ_CONFIG_DB) ->
    case fetch_notification(TemplateId, ?KZ_CONFIG_DB) of
        {'ok', Template} ->
            templates_source_has_attachments(TemplateId, ?KZ_CONFIG_DB, ?KZ_CONFIG_DB, Template);
        {'error', 'not_found'} -> 'not_found';
        {'error', _E} -> 'undefined'
    end;
templates_source(TemplateId, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    %% ?LOG_DEBUG("trying to fetch template ~s for ~s", [TemplateId, AccountId]),
    lager:debug("trying to fetch template '~s' for '~s' account", [TemplateId, AccountId]),
    ResellerId = kz_services_reseller:get_id(AccountId),
    templates_source(TemplateId, AccountId, ResellerId);
templates_source(TemplateId, DataJObj) ->
    case kapi_notifications:account_id(DataJObj) of
        'undefined' -> ?KZ_CONFIG_DB;
        AccountId -> templates_source(TemplateId, AccountId)
    end.

-spec templates_source(kz_term:ne_binary(), kz_term:api_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
templates_source(_TemplateId, 'undefined', _ResellerId) ->
    lager:warning("failed to find parent account for template ~s", [_TemplateId]),
    'undefined';
templates_source(TemplateId, AccountId, AccountId) ->
    case fetch_notification(TemplateId, AccountId) of
        {'ok', Template} ->
            templates_source_has_attachments(TemplateId, AccountId, AccountId, Template);
        {'error', 'not_found'} -> 'not_found';
        {'error', _E} -> 'undefined'
    end;
templates_source(TemplateId, AccountId, ResellerId) ->
    case fetch_notification(TemplateId, AccountId) of
        {'error', 'not_found'} ->
            parent_templates_source(TemplateId, AccountId, ResellerId);
        {'ok', Template} ->
            templates_source_has_attachments(TemplateId, AccountId, ResellerId, Template);
        {'error', _E} -> 'undefined'
    end.

-spec templates_source_has_attachments(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          kz_term:api_binary().
templates_source_has_attachments(_TemplateId, ?KZ_CONFIG_DB, ?KZ_CONFIG_DB, Template) ->
    case kz_doc:attachments(Template) of
        'undefined' -> 'not_found';
        _ -> ?KZ_CONFIG_DB
    end;
templates_source_has_attachments(TemplateId, AccountId, ResellerId, Template) ->
    case kz_doc:attachments(Template) of
        'undefined' -> parent_templates_source(TemplateId, AccountId, ResellerId);
        _ -> kz_doc:account_id(Template)
    end.

-spec parent_templates_source(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          kz_term:api_binary().
parent_templates_source(_TemplateId, AccountId, AccountId) -> ?KZ_CONFIG_DB;
parent_templates_source(TemplateId, AccountId, ResellerId) ->
    lager:debug("failed to find template ~s in account ~s", [TemplateId, AccountId]),
    ParentId = kzd_accounts:get_parent_account_id(AccountId),
    templates_source(TemplateId, ParentId, ResellerId).

-spec fetch_notification(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
fetch_notification(TemplateId, ?KZ_CONFIG_DB) ->
    fetch_notification(TemplateId, ?KZ_CONFIG_DB, 'undefined');
fetch_notification(TemplateId, Account) ->
    fetch_notification(TemplateId, Account, kz_services_reseller:get_id(Account)).

fetch_notification(TemplateId, 'undefined', _ResellerId) ->
    kz_datamgr:open_cache_doc(?KZ_CONFIG_DB, doc_id(TemplateId), [{'cache_failures', ['not_found']}]);
fetch_notification(TemplateId, Db=?KZ_CONFIG_DB, _ResellerId) ->
    kz_datamgr:open_cache_doc(Db, doc_id(TemplateId), [{'cache_failures', ['not_found']}]);
fetch_notification(TemplateId, AccountId, AccountId) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    DocId = doc_id(TemplateId),

    case kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E} -> fetch_notification(TemplateId, 'undefined', AccountId)
    end;
fetch_notification(TemplateId, AccountId, ResellerId) ->
    AccountDb = kzs_util:format_account_db(AccountId),
    DocId = doc_id(TemplateId),
    case kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E} ->
            fetch_notification(TemplateId, kzd_accounts:get_parent_account_id(AccountId), ResellerId)
    end.

-spec render_masters(kz_term:ne_binary(), macros()) -> kz_term:proplist().
render_masters(TemplateId, Macros) ->
    [{ContentType, render_master(TemplateId, ContentType, Macros)}
     || ContentType <- master_content_types(TemplateId)
    ].

master_content_types(TemplateId) ->
    case fetch_notification(TemplateId, ?KZ_CONFIG_DB) of
        {'ok', NotificationJObj} ->
            [kz_json:get_ne_binary_value(<<"content_type">>, AttachmentJObj)
             || {_,AttachmentJObj} <- kz_json:to_proplist(kz_doc:attachments(NotificationJObj))
            ];
        {'error', _E} ->
            lager:warning("failed to find master notification ~s", [TemplateId]),
            []
    end.

-spec render_master(kz_term:ne_binary(), kz_term:ne_binary(), macros()) -> kz_term:ne_binary().
render_master(?NE_BINARY=TemplateId, ?NE_BINARY=ContentType, Macros) ->
    ModuleName = renderer_name(TemplateId, ContentType),
    case kz_template:render(ModuleName, Macros) of
        {'ok', IOList} ->
            iolist_to_binary(IOList);
        {'error', _R} ->
            lager:debug("failed to render '~s': ~p", [TemplateId, _R]),
            throw({'error', 'template_error', <<"failed to render template ", TemplateId/binary>>})
    end.

-spec render_accounts(kz_term:ne_binary(), kz_term:ne_binary(), macros()) -> kz_term:proplist().
render_accounts(TemplateId, AccountDb, Macros) ->
    render_farm(TemplateId, Macros, fetch_attachments(TemplateId, AccountDb)).

-spec render_farm(kz_term:ne_binary(), macros(), kz_term:proplist()) -> kz_term:proplist().
render_farm(TemplateId, Macros, Templates) ->
    [{ContentType, teletype_util:render(TemplateId, Template, Macros)}
     || {ContentType, Template} <- Templates
    ].

-spec renderer_name(kz_term:ne_binary(), kz_term:ne_binary()) -> atom().
renderer_name(TemplateId, ContentType) ->
    kz_term:to_atom(<<(TemplateId)/binary, ".", ContentType/binary>>, 'true').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec doc_id(kz_term:ne_binary()) -> kz_term:ne_binary().
doc_id(Id) ->
    kz_notification:db_id(Id).

-spec load_preview_templates(kz_json:object()) -> kz_term:proplist().
load_preview_templates(DataJObj) ->
    props:filter_undefined(
      [{?TEXT_HTML, maybe_decode_html(kz_json:get_value(<<"html">>, DataJObj))}
      ,{?TEXT_PLAIN, kz_json:get_value(<<"text">>, DataJObj)}
      ]).

-spec maybe_decode_html(kz_term:api_binary()) -> kz_term:api_binary().
maybe_decode_html('undefined') -> 'undefined';
maybe_decode_html(HTML) ->
    try base64:decode(HTML)
    catch
        _E:'badarg' -> HTML;
        _E:_R ->
            lager:debug("failed to decode HTML ~s: ~p", [_E, _R]),
            'undefined'
    end.


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), init_params()) -> 'ok'.
create(DocId, Params) ->
    lager:debug("attempting to create template ~s", [DocId]),
    TemplateJObj =
        kz_doc:update_pvt_parameters(kz_json:from_list([{<<"_id">>, DocId}])
                                    ,?KZ_CONFIG_DB
                                    ,[{'account_db', ?KZ_CONFIG_DB}
                                     ,{'account_id', ?KZ_CONFIG_DB}
                                     ,{'type', kz_notification:pvt_type()}
                                     ]
                                    ),
    {'ok', UpdatedTemplateJObj} = save(TemplateJObj),
    lager:debug("created base template ~s(~s)", [DocId, kz_doc:revision(UpdatedTemplateJObj)]),

    case update(UpdatedTemplateJObj, Params) of
        {'ok', _OK} -> lager:debug("template ~s(~s) created", [DocId, kz_doc:revision(_OK)]);
        {'error', _E} -> lager:debug("failed template update: ~p", [_E])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update(kz_json:object(), init_params()) -> 'ok'.
maybe_update(TemplateJObj, Params) ->
    case kz_doc:is_soft_deleted(TemplateJObj)
        orelse has_manual_modifications(TemplateJObj)
    of
        'true' -> lager:warning("template is currently soft-deleted or has manual changes, not updating!");
        'false' ->
            case update(TemplateJObj, Params) of
                'ok' -> 'ok';
                {'ok', _OK} -> lager:debug("template updated successfully");
                {'error', _E} -> lager:debug("failed to update template: ~p", [_E])
            end
    end.

-spec has_manual_modifications(kz_json:object()) -> boolean().
has_manual_modifications(TemplateJObj) ->
    kz_doc:document_hash(TemplateJObj) =/= kz_doc:calculate_document_hash(TemplateJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_json:object(), init_params()) ->
          'ok' | {'ok', kz_json:object()} |
          {'error', any()}.
update(TemplateJObj, Params) ->
    case update_from_params(TemplateJObj, Params) of
        {'false', _} -> lager:debug("no updates to template");
        {'true', UpdatedTemplateJObj} ->
            ?LOG_DEBUG("template has updates to save"),
            save(UpdatedTemplateJObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save(kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
save(TemplateJObj) ->
    SaveJObj = kz_doc:update_pvt_parameters(TemplateJObj, ?KZ_CONFIG_DB, [{'type', kz_notification:pvt_type()}]),
    case kz_datamgr:save_doc(?KZ_CONFIG_DB, SaveJObj) of
        {'ok', _JObj}=OK ->
            lager:debug("saved updated template ~s(~s) to ~s"
                       ,[kz_doc:id(_JObj), kz_doc:revision(_JObj), ?KZ_CONFIG_DB]
                       ),
            OK;
        {'error', _E}=E ->
            lager:debug("failed to save template to ~s: ~p", [?KZ_CONFIG_DB, _E]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type update_acc() :: {boolean(), kz_json:object()}.

-spec update_from_params(kz_json:object(), init_params()) ->
          update_acc().
update_from_params(TemplateJObj, Params) ->
    lists:foldl(fun update_from_param/2
               ,{'false', TemplateJObj}
               ,[{'pvt_type', kz_notification:pvt_type()} | Params]
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_from_param(init_param() | {'pvt_type', kz_term:ne_binary()}, update_acc()) -> update_acc().
update_from_param({'macros', Macros}, Acc) ->
    update_macros(Macros, Acc);
update_from_param({'text', Basename}, Acc) ->
    update_text_attachment(Basename, Acc);
update_from_param({'html', Basename}, Acc) ->
    update_html_attachment(Basename, Acc);
update_from_param({'subject', Subject}, Acc) ->
    update_subject(Subject, Acc);
update_from_param({'category', Category}, Acc) ->
    update_category(Category, Acc);
update_from_param({'friendly_name', Name}, Acc) ->
    update_name(Name, Acc);
update_from_param({'to', To}, Acc) ->
    update_to(To, Acc);
update_from_param({'cc', CC}, Acc) ->
    update_cc(CC, Acc);
update_from_param({'bcc', BCC}, Acc) ->
    update_bcc(BCC, Acc);
update_from_param({'from', From}, Acc) ->
    update_from(From, Acc);
update_from_param({'reply_to', ReplyTo}, Acc) ->
    update_reply_to(ReplyTo, Acc);
update_from_param({'pvt_type', PvtType}, Acc) ->
    update_field(PvtType
                ,Acc
                ,fun kz_doc:type/1
                ,fun kz_doc:set_type/2
                ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_category(kz_term:ne_binary(), update_acc()) ->
          update_acc().
update_category(Category, Acc) ->
    update_field(Category
                ,Acc
                ,fun kz_notification:category/1
                ,fun kz_notification:set_category/2
                ).

-spec update_name(kz_term:ne_binary(), update_acc()) ->
          update_acc().
update_name(Name, Acc) ->
    update_field(Name
                ,Acc
                ,fun kz_notification:name/1
                ,fun kz_notification:set_name/2
                ).

-spec update_from(kz_term:ne_binary(), update_acc()) ->
          update_acc().
update_from(From, Acc) ->
    update_field(From
                ,Acc
                ,fun kz_notification:from/1
                ,fun kz_notification:set_from/2
                ).

-spec update_reply_to(kz_term:ne_binary(), update_acc()) ->
          update_acc().
update_reply_to(ReplyTo, Acc) ->
    update_field(ReplyTo
                ,Acc
                ,fun kz_notification:reply_to/1
                ,fun kz_notification:set_reply_to/2
                ).

-spec update_to(kz_json:object(), update_acc()) ->
          update_acc().
update_to(To, Acc) ->
    update_field(To
                ,Acc
                ,fun kz_notification:to/1
                ,fun kz_notification:set_to/2
                ).

-spec update_cc(kz_json:object(), update_acc()) ->
          update_acc().
update_cc(CC, Acc) ->
    update_field(CC
                ,Acc
                ,fun kz_notification:cc/1
                ,fun kz_notification:set_cc/2
                ).

-spec update_bcc(kz_json:object(), update_acc()) ->
          update_acc().
update_bcc(Bcc, Acc) ->
    update_field(Bcc
                ,Acc
                ,fun kz_notification:bcc/1
                ,fun kz_notification:set_bcc/2
                ).

-spec update_field(kz_term:api_object() | kz_term:ne_binary(), update_acc(), fun(), fun()) ->
          update_acc().
update_field('undefined', Acc, _GetFun, _SetFun) -> Acc;
update_field(Value, {_IsUpdated, TemplateJObj}=Acc, GetFun, SetFun) ->
    case GetFun(TemplateJObj) of
        'undefined' ->
            lager:debug("updating field to ~p: ~p on ~s"
                       ,[Value, GetFun, kz_doc:revision(TemplateJObj)]
                       ),
            {'true', SetFun(TemplateJObj, Value)};
        Value -> Acc;
        _V -> {'true', SetFun(TemplateJObj, Value)}
    end.

-spec update_subject(kz_term:ne_binary(), update_acc()) ->
          update_acc().
update_subject(Subject, Acc) ->
    update_field(Subject
                ,Acc
                ,fun kz_notification:subject/1
                ,fun kz_notification:set_subject/2
                ).

-spec update_html_attachment(binary(), update_acc()) -> update_acc().
update_html_attachment(Basename, Acc) ->
    case read_template_from_disk(Basename, 'html') of
        {'ok', HTML} ->
            update_attachment(HTML, Acc, ?TEXT_HTML);
        {'error', _E} ->
            lager:info("failed to find template '~s.html': ~p", [Basename, _E]),
            Acc
    end.

-spec update_text_attachment(binary(), update_acc()) -> update_acc().
update_text_attachment(Basename, Acc) ->
    case read_template_from_disk(Basename, 'text') of
        {'ok', Text} ->
            update_attachment(Text, Acc, ?TEXT_PLAIN);
        {'error', _E} ->
            lager:info("failed to find template '~s.text': ~p", [Basename, _E]),
            Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec update_attachment(binary(), update_acc(), kz_term:ne_binary()) ->
          update_acc().
update_attachment(Contents, {_IsUpdated, TemplateJObj}=Acc, ContentType) ->
    AttachmentName = attachment_name(ContentType),
    Id = kz_doc:id(TemplateJObj),
    case does_attachment_exist(Id, AttachmentName) of
        'true' -> Acc;
        'false' ->
            update_attachment(Contents, Acc, ContentType, Id, AttachmentName)
    end.

-spec update_attachment(binary(), update_acc(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          update_acc().
update_attachment(Contents, {IsUpdated, TemplateJObj}=Acc, ContentType, Id, AName) ->
    lager:debug("attachment ~s doesn't exist for ~s", [AName, Id]),
    case save_attachment(Id, AName, ContentType, Contents) of
        {'ok', AttachmentJObj} ->
            ?LOG_DEBUG("saved attachment: ~p", [AttachmentJObj]),
            {'ok', UpdatedJObj} = kz_datamgr:open_doc(?KZ_CONFIG_DB, Id),
            Merged = kz_json:merge_jobjs(UpdatedJObj, TemplateJObj),
            {IsUpdated, Merged};
        {'error', _E} ->
            ?LOG_DEBUG("failed to save attachment ~s: ~p", [AName, _E]),
            Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_macros(kz_json:object(), update_acc()) ->
          update_acc().
update_macros(Macros, Acc) ->
    kz_json:foldl(fun update_macro/3, Acc, Macros).

-spec update_macro(kz_json:key(), kz_json:json_term(), update_acc()) ->
          update_acc().
update_macro(MacroKey, MacroValue, {_IsUpdated, TemplateJObj}=Acc) ->
    case kz_notification:macro(TemplateJObj, MacroKey) of
        'undefined' ->
            lager:debug("adding macro ~s to template", [MacroKey]),
            {'true', kz_notification:set_macro(TemplateJObj, MacroKey, MacroValue)};
        _Value -> Acc
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attachment_name(kz_term:ne_binary()) -> kz_term:ne_binary().
attachment_name(ContentType) ->
    kz_binary:clean(<<"template.", ContentType/binary>>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec does_attachment_exist(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
does_attachment_exist(DocId, AName) ->
    case kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId) of
        {'ok', JObj} ->
            kz_doc:attachment(JObj, cow_qs:urldecode(AName)) =/= 'undefined';
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec save_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
save_attachment(DocId, AName, ContentType, Contents) ->
    Options = [{'content_type', kz_term:to_list(ContentType)}],
    case kz_datamgr:put_attachment(?KZ_CONFIG_DB, DocId, AName, Contents, Options) of
        {'ok', _UpdatedJObj}=OK ->
            lager:debug("added attachment ~s to ~s", [AName, DocId]),
            OK;
        {'error', 'conflict'}=E ->
            case does_attachment_exist(DocId, AName) of
                'true' ->
                    lager:debug("added attachment ~s to ~s", [AName, DocId]),
                    kz_datamgr:open_doc(?KZ_CONFIG_DB, DocId);
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    E
            end;
        {'error', _E}=E ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            E
    end.

-spec write_templates_to_disk(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
write_templates_to_disk(TemplateId, Params) ->
    lists:foreach(fun(Template) ->
                          write_template_to_disk(TemplateId, Template)
                  end
                 ,props:filter_undefined(
                    [{'text', props:get_value('text', Params)}
                    ,{'html', props:get_value('html', Params)}
                    ]
                   )
                 ).

-spec read_template_from_disk(kz_term:ne_binary(), 'html' | 'text') ->
          {'ok', binary()} |
          {'error', file:posix() | 'badarg' | 'terminated' | 'system_limit'}.
read_template_from_disk(TemplateId, Type) ->
    file:read_file(
      template_filename(TemplateId, Type)).

-spec write_template_to_disk(kz_term:ne_binary(), {'html' | 'text', binary()}) -> 'ok'.
write_template_to_disk(TemplateId, {Type, Template}) ->
    File = template_filename(TemplateId, Type),
    'ok' = file:write_file(File, Template).

-spec template_filename(kz_term:ne_binary(), 'html' | 'text') -> file:filename_all().
template_filename(TemplateId, Type) ->
    Basename = iolist_to_binary([TemplateId, ".", kz_term:to_list(Type)]),
    filename:join([code:priv_dir(?APP), "templates", Basename]).
