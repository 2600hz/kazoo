%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_templates).

-include("teletype.hrl").

-define(TEMPLATE_FAILURE_KEY(TemplateId, AccountId)
        ,{?MODULE, TemplateId, AccountId}
       ).

-export([init/2
         ,renderer_name/2
         ,render/2, render/3, render/4
         ,preview/3
         ,fetch_notification/2
        ]).
-export([doc_id/1]).

-type macro() :: {ne_binary(), ne_binary() | number() | macros()}.
-type macros() :: [macro()].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(ne_binary(), init_params()) -> 'ok'.
init(TemplateId, Params) ->
    DocId = doc_id(TemplateId),
    lager:debug("init template ~s", [DocId]),
    case kz_datamgr:open_cache_doc(?WH_CONFIG_DB, DocId) of
        {'ok', TemplateJObj} ->
            maybe_update(TemplateJObj, Params);
        {'error', 'not_found'} ->
            create(DocId, Params);
        {'error', _E} ->
            lager:warning("failed to find template ~s", [DocId])
    end,
    compile_master_renderers(TemplateId).

-spec compile_master_renderers(ne_binary()) -> 'ok'.
compile_master_renderers(TemplateId) ->
    _ = [build_renderer(TemplateId, ContentType, Template) ||
            {ContentType, Template} <- fetch_master_attachments(TemplateId)
        ],
    lager:debug("built master renderers for ~s", [TemplateId]).

-spec build_renderer(ne_binary(), ne_binary(), binary()) -> 'ok'.
build_renderer(TemplateId, ContentType, Template) ->
    ModuleName = renderer_name(TemplateId, ContentType),
    case erlydtl:compile_template(Template
                                 ,ModuleName
                                 ,[{'out_dir', 'false'}
                                  ,'return'
                                  ,'report'
                                   ,{'auto_escape', 'false'}
                                  ]
                                 )
    of
        {'ok', Name} ->
            lager:debug("built ~s renderer for ~s", [TemplateId, Name]);
        {'ok', Name, []} ->
            lager:debug("built ~s renderer for ~s", [TemplateId, Name]);
        {'ok', Name, Warnings} ->
            lager:debug("compiling template ~s for renderer ~s produced warnings: ~p", [TemplateId, Name, Warnings]);
        {'error', Errors, Warnings} ->
            lager:debug("failed to compile template ~s", [TemplateId]),
            teletype_renderer:log_errors(Errors, Template),
            teletype_renderer:log_warnings(Warnings, Template),
            throw({'error', 'failed_template', ModuleName})
    end.

-type template_attachment() :: {ne_binary(), binary()}.
-type template_attachments() :: [template_attachment()].

-spec fetch_master_attachments(ne_binary()) -> template_attachments().
fetch_master_attachments(TemplateId) ->
    fetch_attachments(TemplateId, ?WH_CONFIG_DB).

-spec fetch_attachments(ne_binary(), ne_binary()) -> template_attachments().
fetch_attachments(TemplateId, Account) ->
    AccountDb = case Account of
                    ?WH_CONFIG_DB -> ?WH_CONFIG_DB;
                    Account -> wh_util:format_account_db(Account)
                end,
    DocId = doc_id(TemplateId),
    case fetch_notification(TemplateId, AccountDb) of
        {'ok', TemplateDoc} ->
            wh_json:foldl(fun(AttachmentName, AttachmentProps, Acc) ->
                                  fetch_attachment(AttachmentName, AttachmentProps, Acc, AccountDb, DocId)
                          end
                          ,[]
                          ,wh_doc:attachments(TemplateDoc)
                         );
        {'error', _E} ->
            lager:error("failed to open ~s:~s: ~p", [AccountDb, DocId, _E]),
            []
    end.

-spec fetch_attachment(ne_binary(), wh_json:object(), template_attachments(), ne_binary(), ne_binary()) ->
                              template_attachments().
fetch_attachment(AttachmentName, AttachmentProps, Acc, AccountDb, DocId) ->
    ContentType = wh_json:get_value(<<"content_type">>, AttachmentProps),
    case kz_datamgr:fetch_attachment(AccountDb, DocId, AttachmentName) of
        {'ok', AttachmentBinary} ->
            [{ContentType, AttachmentBinary} | Acc];
        {'error', _E} ->
            lager:error("failed to fetch attachemt ~s:~s:~s: ~p"
                       ,[AccountDb, DocId, AttachmentName, _E]
                       ),
            Acc
    end.

-spec preview(ne_binary(), macros(), wh_json:object()) -> wh_proplist().
preview(TemplateId, Macros, DataJObj) ->
    render_farm(TemplateId
               ,Macros
               ,load_preview_templates(DataJObj)
               ).

-spec render(ne_binary(), macros()) -> wh_proplist().
-spec render(ne_binary(), macros(), wh_json:object()) -> wh_proplist().
-spec render(ne_binary(), macros(), wh_json:object(), boolean()) -> wh_proplist().
render(TemplateId, Macros) ->
    render(TemplateId, Macros, wh_json:new()).

render(TemplateId, Macros, DataJObj) ->
    render(TemplateId, Macros, DataJObj, teletype_util:is_preview(DataJObj)).

render(TemplateId, Macros, DataJObj, 'false') ->
    case templates_source(TemplateId, DataJObj) of
        'undefined' ->
            lager:warning("no source db for template ~s: ~p", [TemplateId, DataJObj]),
            [];
        ?WH_CONFIG_DB ->
            lager:debug("rendering system template for ~s", [TemplateId]),
            render_masters(TemplateId, Macros);
        AccountDb ->
            lager:debug("rendering template ~s from account ~s", [TemplateId, AccountDb]),
            render_accounts(TemplateId, AccountDb, Macros)
    end;
render(TemplateId, Macros, DataJObj, 'true') ->
    preview(TemplateId, Macros, DataJObj).

-spec templates_source(ne_binary(), api_binary() | wh_json:object()) -> api_binary().
-spec templates_source(ne_binary(), api_binary(), ne_binary()) -> api_binary().
templates_source(_TemplateId, 'undefined') ->
    lager:warning("no account id for template ~s, no template to process", [_TemplateId]),
    'undefined';
templates_source(_TemplateId, ?WH_CONFIG_DB) ->
    ?WH_CONFIG_DB;
templates_source(TemplateId, <<_/binary>> = AccountId) ->
    lager:debug("trying to fetch template ~s for ~s", [TemplateId, AccountId]),
    ResellerId = wh_services:find_reseller_id(AccountId),
    templates_source(TemplateId, AccountId, ResellerId);
templates_source(TemplateId, DataJObj) ->
    AccountId = teletype_util:find_account_id(DataJObj),
    templates_source(TemplateId, AccountId).

templates_source(_TemplateId, 'undefined', _ResellerId) ->
    lager:warning("failed to find parent account for template ~s", [_TemplateId]),
    'undefined';
templates_source(TemplateId, AccountId, AccountId) ->
    case fetch_notification(TemplateId, AccountId) of
        {'ok', _Template} -> AccountId;
        {'error', 'not_found'} -> ?WH_CONFIG_DB;
        {'error', _E} -> 'undefined'
    end;
templates_source(TemplateId, AccountId, ResellerId) ->
    case fetch_notification(TemplateId, AccountId) of
        {'error', 'not_found'} ->
            lager:debug("failed to find template ~s in account ~s", [TemplateId, AccountId]),
            ParentId = teletype_util:get_parent_account_id(AccountId),
            templates_source(TemplateId, ParentId, ResellerId);
        {'ok', Template} ->
            wh_doc:account_id(Template);
        {'error', _E} -> 'undefined'
    end.

-spec fetch_notification(ne_binary(), ne_binary()) ->
                                {'ok', wh_json:object()} |
                                {'error', any()}.
fetch_notification(TemplateId, ?WH_CONFIG_DB) ->
    fetch_notification(TemplateId, ?WH_CONFIG_DB, 'undefined');
fetch_notification(TemplateId, Account) ->
    fetch_notification(TemplateId, Account, wh_services:find_reseller_id(Account)).

fetch_notification(TemplateId, 'undefined', _ResellerId) ->
    kz_datamgr:open_cache_doc(?WH_CONFIG_DB, doc_id(TemplateId), [{'cache_failures', ['not_found']}]);
fetch_notification(TemplateId, ?WH_CONFIG_DB, _ResellerId) ->
    kz_datamgr:open_cache_doc(?WH_CONFIG_DB, doc_id(TemplateId), [{'cache_failures', ['not_found']}]);
fetch_notification(TemplateId, AccountId, AccountId) ->
    AccountDb = wh_util:format_account_db(AccountId),
    DocId = doc_id(TemplateId),

    case kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E} -> fetch_notification(TemplateId, 'undefined', AccountId)
    end;
fetch_notification(TemplateId, AccountId, ResellerId) ->
    AccountDb = wh_util:format_account_db(AccountId),
    DocId = doc_id(TemplateId),
    case kz_datamgr:open_cache_doc(AccountDb, DocId, [{'cache_failures', ['not_found']}]) of
        {'ok', _JObj}=OK -> OK;
        {'error', _E} ->
            fetch_notification(TemplateId, teletype_util:get_parent_account_id(AccountId), ResellerId)
    end.

-spec render_masters(ne_binary(), macros()) -> wh_proplist().
render_masters(TemplateId, Macros) ->
      [{ContentType, render_master(TemplateId, ContentType, Macros)}
       || ContentType <- master_content_types(TemplateId)
      ].

master_content_types(TemplateId) ->
    case fetch_notification(TemplateId, ?WH_CONFIG_DB) of
        {'ok', NotificationJObj} ->
            wh_json:foldl(fun master_content_type/3
                          ,[]
                          ,wh_doc:attachments(NotificationJObj)
                         );
        {'error', _E} ->
            lager:warning("failed to find master notification ~s", [TemplateId]),
            []
    end.

-spec master_content_type(ne_binary(), wh_json:object(), ne_binaries()) -> ne_binaries().
master_content_type(_AttachmentName, AttachmentProps, Acc) ->
    [wh_json:get_value(<<"content_type">>, AttachmentProps) | Acc].

-spec render_master(ne_binary(), ne_binary(), macros()) -> ne_binary().
render_master(<<_/binary>> = TemplateId, <<_/binary>> = ContentType, Macros) ->
    ModuleName = teletype_templates:renderer_name(TemplateId, ContentType),
    try ModuleName:render(Macros) of
        {'ok', IOList} ->
            lager:debug("rendered ~s template successfully: ~s", [ContentType, IOList]),
            iolist_to_binary(IOList);
        {'error', _E} ->
            lager:debug("failed to render ~s template: ~p", [ContentType, _E]),
            throw({'error', 'template_error'})
    catch
        'error':'undef' ->
            ST = erlang:get_stacktrace(),
            lager:debug("something in the template ~s is undefined", [ModuleName]),
            wh_util:log_stacktrace(ST),
            throw({'error', 'template_error'});
        _E:R ->
            ST = erlang:get_stacktrace(),
            lager:debug("crashed rendering template ~s: ~s: ~p", [ModuleName, _E, R]),
            wh_util:log_stacktrace(ST),
            throw({'error', 'template_error'})
    end.

-spec render_accounts(ne_binary(), ne_binary(), macros()) -> wh_proplist().
render_accounts(TemplateId, AccountDb, Macros) ->
    render_farm(TemplateId, Macros, fetch_attachments(TemplateId, AccountDb)).

-spec render_farm(ne_binary(), macros(), wh_proplist()) -> wh_proplist().
render_farm(TemplateId, Macros, Templates) ->
    [{ContentType, teletype_util:render(TemplateId, Template, Macros)}
     || {ContentType, Template} <- Templates
    ].

-spec renderer_name(ne_binary(), ne_binary()) -> atom().
renderer_name(TemplateId, ContentType) ->
    wh_util:to_atom(<<(TemplateId)/binary, ".", ContentType/binary>>, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec doc_id(ne_binary()) -> ne_binary().
doc_id(Id) ->
    kz_notification:db_id(Id).

-spec load_preview_templates(wh_json:object()) -> wh_proplist().
load_preview_templates(DataJObj) ->
    props:filter_undefined(
      [{?TEXT_HTML, maybe_decode_html(wh_json:get_value(<<"html">>, DataJObj))}
       ,{?TEXT_PLAIN, wh_json:get_value(<<"text">>, DataJObj)}
      ]).

-spec maybe_decode_html(api_binary()) -> api_binary().
maybe_decode_html('undefined') -> 'undefined';
maybe_decode_html(HTML) ->
    try base64:decode(HTML) of
        Decoded -> Decoded
    catch
        _E:'badarg' -> HTML;
        _E:_R ->
            lager:debug("failed to decode HTML ~s: ~p", [_E, _R]),
            'undefined'
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), init_params()) ->
                    {'ok', wh_json:object()} |
                    kz_datamgr:data_error().
create(DocId, Params) ->
    lager:debug("attempting to create template ~s", [DocId]),
    TemplateJObj =
        wh_doc:update_pvt_parameters(
          wh_json:from_list([{<<"_id">>, DocId}])
          ,?WH_CONFIG_DB
          ,[{'account_db', ?WH_CONFIG_DB}
            ,{'account_id', ?WH_CONFIG_DB}
            ,{'type', kz_notification:pvt_type()}
           ]
         ),
    {'ok', UpdatedTemplateJObj} = save(TemplateJObj),
    lager:debug("created base template ~s(~s)", [DocId, wh_doc:revision(UpdatedTemplateJObj)]),

    case update(UpdatedTemplateJObj, Params) of
        {'ok', _OK} -> lager:debug("template ~s(~s) created", [DocId, wh_doc:revision(_OK)]);
        {'error', _E} -> lager:debug("failed template update: ~p", [_E])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_update(wh_json:object(), init_params()) -> 'ok'.
maybe_update(TemplateJObj, Params) ->
    case wh_doc:is_soft_deleted(TemplateJObj) of
        'true' -> lager:warning("template is currently soft-deleted");
        'false' ->
            case update(TemplateJObj, Params) of
                'ok' -> 'ok';
                {'ok', _OK} -> lager:debug("template updated successfully");
                {'error', _E} -> lager:debug("failed to update template: ~p", [_E])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update(wh_json:object(), init_params()) ->
                    'ok' | {'ok', wh_json:object()} |
                    {'error', any()}.
update(TemplateJObj, Params) ->
    case update_from_params(TemplateJObj, Params) of
        {'false', _} -> lager:debug("no updates to template");
        {'true', UpdatedTemplateJObj} ->
            lager:debug("template has updates to save"),
            save(UpdatedTemplateJObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(wh_json:object()) ->
                  {'ok', wh_json:object()} |
                  {'error', any()}.
save(TemplateJObj) ->
    SaveJObj = wh_doc:update_pvt_parameters(TemplateJObj, ?WH_CONFIG_DB),
    case kz_datamgr:save_doc(?WH_CONFIG_DB, SaveJObj) of
        {'ok', _JObj}=OK ->
            lager:debug("saved updated template ~s(~s) to ~s"
                        ,[wh_doc:id(_JObj), wh_doc:revision(_JObj), ?WH_CONFIG_DB]
                       ),
            OK;
        {'error', _E}=E ->
            lager:debug("failed to save template to ~s: ~p", [?WH_CONFIG_DB, _E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type update_acc() :: {boolean(), wh_json:object()}.

-spec update_from_params(wh_json:object(), init_params()) ->
                                update_acc().
update_from_params(TemplateJObj, Params) ->
    lists:foldl(
      fun update_from_param/2
      ,{'false', TemplateJObj}
      ,Params
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_from_param(init_param(), update_acc()) -> update_acc().
update_from_param({'macros', Macros}, Acc) ->
    update_macros(Macros, Acc);
update_from_param({'text', Text}, Acc) ->
    update_text_attachment(Text, Acc);
update_from_param({'html', HTML}, Acc) ->
    update_html_attachment(HTML, Acc);
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
    update_reply_to(ReplyTo, Acc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_category(ne_binary(), update_acc()) ->
                             update_acc().
update_category(Category, Acc) ->
    update_field(Category
                 ,Acc
                 ,fun kz_notification:category/1
                 ,fun kz_notification:set_category/2
                ).

-spec update_name(ne_binary(), update_acc()) ->
                         update_acc().
update_name(Name, Acc) ->
    update_field(Name
                 ,Acc
                 ,fun kz_notification:name/1
                 ,fun kz_notification:set_name/2
                ).

-spec update_from(ne_binary(), update_acc()) ->
                         update_acc().
update_from(From, Acc) ->
    update_field(From
                 ,Acc
                 ,fun kz_notification:from/1
                 ,fun kz_notification:set_from/2
                ).

-spec update_reply_to(ne_binary(), update_acc()) ->
                             update_acc().
update_reply_to(ReplyTo, Acc) ->
    update_field(ReplyTo
                 ,Acc
                 ,fun kz_notification:reply_to/1
                 ,fun kz_notification:set_reply_to/2
                ).

-spec update_to(wh_json:object(), update_acc()) ->
                       update_acc().
update_to(To, Acc) ->
    update_field(To
                 ,Acc
                 ,fun kz_notification:to/1
                 ,fun kz_notification:set_to/2
                ).

-spec update_cc(wh_json:object(), update_acc()) ->
                       update_acc().
update_cc(CC, Acc) ->
    update_field(CC
                 ,Acc
                 ,fun kz_notification:cc/1
                 ,fun kz_notification:set_cc/2
                ).

-spec update_bcc(wh_json:object(), update_acc()) ->
                        update_acc().
update_bcc(Bcc, Acc) ->
    update_field(Bcc
                 ,Acc
                 ,fun kz_notification:bcc/1
                 ,fun kz_notification:set_bcc/2
                ).

-spec update_field(api_object() | ne_binary(), update_acc(), fun(), fun()) ->
                          update_acc().
update_field('undefined', Acc, _GetFun, _SetFun) -> Acc;
update_field(Value, {_IsUpdated, TemplateJObj}=Acc, GetFun, SetFun) ->
    case GetFun(TemplateJObj) of
        'undefined' ->
            lager:debug("updating field to ~p: ~p on ~s"
                        ,[Value, GetFun, wh_doc:revision(TemplateJObj)]
                       ),
            {'true', SetFun(TemplateJObj, Value)};
        _V -> Acc
    end.

-spec update_subject(ne_binary(), update_acc()) ->
                            update_acc().
update_subject(Subject, Acc) ->
    update_field(Subject
                 ,Acc
                 ,fun kz_notification:subject/1
                 ,fun kz_notification:set_subject/2
                ).

-spec update_html_attachment(binary(), update_acc()) -> update_acc().
update_html_attachment(HTML, Acc) ->
    update_attachment(HTML, Acc, ?TEXT_HTML).

-spec update_text_attachment(binary(), update_acc()) -> update_acc().
update_text_attachment(Text, Acc) ->
    update_attachment(Text, Acc, ?TEXT_PLAIN).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_attachment(binary(), update_acc(), ne_binary()) ->
                               update_acc().
-spec update_attachment(binary(), update_acc(), ne_binary(), ne_binary(), ne_binary()) ->
                               update_acc().
update_attachment(Contents, {_IsUpdated, TemplateJObj}=Acc, ContentType) ->
    AttachmentName = attachment_name(ContentType),
    Id = wh_doc:id(TemplateJObj),
    case does_attachment_exist(Id, AttachmentName) of
        'true' -> Acc;
        'false' ->
            update_attachment(Contents, Acc, ContentType, Id, AttachmentName)
    end.

update_attachment(Contents, {IsUpdated, TemplateJObj}=Acc, ContentType, Id, AName) ->
    lager:debug("attachment ~s doesn't exist for ~s", [AName, Id]),
    case save_attachment(Id, AName, ContentType, Contents) of
        {'ok', AttachmentJObj} ->
            lager:debug("saved attachment: ~p", [AttachmentJObj]),
            {'ok', UpdatedJObj} = kz_datamgr:open_doc(?WH_CONFIG_DB, Id),
            Merged = wh_json:merge_jobjs(UpdatedJObj, TemplateJObj),
            {IsUpdated, Merged};
        {'error', _E} ->
            lager:debug("failed to save attachment ~s: ~p", [AName, _E]),
            Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_macros(wh_json:object(), update_acc()) ->
                           update_acc().
update_macros(Macros, Acc) ->
    wh_json:foldl(fun update_macro/3, Acc, Macros).

-spec update_macro(wh_json:key(), wh_json:json_term(), update_acc()) ->
                          update_acc().
update_macro(MacroKey, MacroValue, {_IsUpdated, TemplateJObj}=Acc) ->
    case kz_notification:macro(TemplateJObj, MacroKey) of
        'undefined' ->
            lager:debug("adding macro ~s to template", [MacroKey]),
            {'true', kz_notification:set_macro(TemplateJObj, MacroKey, MacroValue)};
        _Value -> Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec attachment_name(ne_binary()) -> ne_binary().
attachment_name(ContentType) ->
    wh_util:clean_binary(<<"template.", (cow_qs:urlencode(ContentType))/binary>>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec does_attachment_exist(ne_binary(), ne_binary()) -> boolean().
does_attachment_exist(DocId, AName) ->
    case kz_datamgr:open_doc(?WH_CONFIG_DB, DocId) of
        {'ok', JObj} ->
            wh_doc:attachment(JObj, cow_qs:urldecode(AName)) =/= 'undefined';
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), binary()) ->
                             {'ok', wh_json:object()} |
                             {'error', any()}.
save_attachment(DocId, AName, ContentType, Contents) ->
    case
        kz_datamgr:put_attachment(
          ?WH_CONFIG_DB
          ,DocId
          ,AName
          ,Contents
          ,[{'content_type', wh_util:to_list(ContentType)}]
         )
    of
        {'ok', _UpdatedJObj}=OK ->
            lager:debug("added attachment ~s to ~s", [AName, DocId]),
            OK;
        {'error', 'conflict'}=E ->
            case does_attachment_exist(DocId, AName) of
                'true' ->
                    lager:debug("added attachment ~s to ~s", [AName, DocId]),
                    kz_datamgr:open_doc(?WH_CONFIG_DB, DocId);
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    E
            end;
        {'error', _E}=E ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            E
    end.
