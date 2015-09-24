%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
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

-export([init/2]).
-export([doc_id/1]).
-export([fetch/1, fetch/2]).
-export([fetch_meta/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(ne_binary(), init_params()) -> 'ok'.
init(TemplateId, Params) ->
    DocId = doc_id(TemplateId),
    lager:debug("init template ~s", [DocId]),
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, DocId) of
        {'ok', TemplateJObj} ->
            maybe_update(TemplateJObj, Params);
        {'error', 'not_found'} ->
            create(DocId, Params);
        {'error', _E} ->
            lager:warning("failed to find template ~s", [DocId])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec doc_id(ne_binary()) -> ne_binary().
doc_id(Id) ->
    kz_notification:db_id(Id).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> wh_proplist().
-spec fetch(ne_binary(), api_object() | ne_binary()) -> wh_proplist().
-spec fetch(ne_binary(), ne_binary(), ne_binary()) -> wh_proplist().
fetch(TemplateId) ->
    fetch_master(TemplateId).

fetch(_TemplateId, 'undefined') ->
    lager:debug("no account id for ~s, no template available", [_TemplateId]),
    [];
fetch(TemplateId, AccountId) when is_binary(AccountId) ->
    lager:debug("trying to fetch template ~s for ~s", [TemplateId, AccountId]),
    ResellerId = wh_services:find_reseller_id(AccountId),
    fetch(TemplateId, AccountId, ResellerId);
fetch(TemplateId, DataJObj) ->
    case load_templates(DataJObj) of
        [] ->
            fetch(TemplateId, teletype_util:find_account_id(DataJObj));
        Templates ->
            lager:debug("preview API has templates defined"),
            Templates
    end.

-spec load_templates(wh_json:object()) -> wh_proplist().
load_templates(DataJObj) ->
    props:filter_undefined(
      [{?TEXT_HTML, maybe_decode_html(wh_json:get_value(<<"html">>, DataJObj))}
       ,{?TEXT_PLAIN, wh_json:get_value(<<"text">>, DataJObj)}
      ]).

fetch(TemplateId, AccountId, AccountId) ->
    case fetch_failure_cache(TemplateId, AccountId) of
        'error' -> fetch_master(TemplateId);
        'ok' -> fetch_from_db(TemplateId, AccountId, AccountId)
    end;
fetch(TemplateId, AccountId, ResellerId) ->
    case fetch_failure_cache(TemplateId, AccountId) of
        'error' -> fetch_parent(TemplateId, AccountId, ResellerId);
        'ok' -> fetch_from_db(TemplateId, AccountId, ResellerId)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_meta(ne_binary(), api_binary()) ->
                        {'ok', wh_json:object()} |
                        couch_mgr:couchbeam_error().
-spec fetch_meta(ne_binary(), api_binary(), ne_binary()) ->
                        {'ok', wh_json:object()} |
                        couch_mgr:couchbeam_error().
fetch_meta(TemplateId, AccountId) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    fetch_meta(TemplateId, AccountId, ResellerId).

fetch_meta(TemplateId, 'undefined', _ResellerId) ->
    fetch_master_meta(TemplateId);
fetch_meta(TemplateId, AccountId, AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, doc_id(TemplateId)) of
        {'ok', _TemplateJObj}=OK -> OK;
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s ~p", [TemplateId, AccountId, _E]),
            fetch_master_meta(TemplateId)
    end;
fetch_meta(TemplateId, AccountId, ResellerId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, doc_id(TemplateId)) of
        {'ok', _TemplateJObj}=OK -> OK;
        {'error', 'not_found'} ->
            fetch_meta(TemplateId, get_parent_account_id(AccountId), ResellerId);
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s ~p", [TemplateId, AccountId, _E]),
            fetch_master_meta(TemplateId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_master_meta(ne_binary()) ->
                               {'ok', wh_json:object()} |
                               couch_mgr:couchbeam_error().
fetch_master_meta(TemplateId) ->
    lager:debug("fetching master meta for ~s", [TemplateId]),
    couch_mgr:open_cache_doc(?WH_CONFIG_DB, doc_id(TemplateId)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type fetch_return() :: {ne_binary(), binary()} | 'undefined'.

-spec fetch_attachment(ne_binary(), ne_binary(), wh_json:object() | fetch_return()) ->
                              wh_proplist() | fetch_return().
fetch_attachment(TemplateId, AccountDb, {AName, Properties}) ->
    case couch_mgr:fetch_attachment(AccountDb, TemplateId, AName) of
        {'ok', Contents} ->
            lager:debug("found template attachment ~s.~s", [TemplateId, AName]),
            {wh_json:get_value(<<"content_type">>, Properties), Contents};
        {'error', _E} ->
            lager:debug("failed to load attachment ~s from ~s(~s): ~p", [AName, TemplateId, AccountDb, _E]),
            'undefined'
    end;
fetch_attachment(TemplateId, AccountDb, Attachments)->
    props:filter_undefined(
      [fetch_attachment(TemplateId, AccountDb, Attachment)
       || Attachment <- wh_json:to_proplist(Attachments)
      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_master(ne_binary()) -> wh_proplist().
fetch_master(TemplateId) ->
    DocId = doc_id(TemplateId),
    lager:debug("fetching master template ~s", [DocId]),
    case couch_mgr:open_cache_doc(?WH_CONFIG_DB, DocId) of
        {'ok', TemplateJObj} ->
            fetch_attachment(
              DocId
              ,?WH_CONFIG_DB
              ,wh_doc:attachments(TemplateJObj, wh_json:new())
             );
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s", [TemplateId, ?WH_CONFIG_DB]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_parent(ne_binary(), ne_binary(), ne_binary()) -> wh_proplist().
fetch_parent(TemplateId, AccountId, AccountId) ->
    lager:debug("trying to fetch parent template ~s for ~s but account is reseller", [TemplateId, AccountId]),
    fetch_master(TemplateId);
fetch_parent(TemplateId, AccountId, ResellerId) ->
    lager:debug("trying to fetch parent template ~s for ~s", [TemplateId, AccountId]),
    case get_parent_account_id(AccountId) of
        'undefined' ->
            lager:debug("parent account_id for ~s is undefined", [AccountId]),
            [];
        ParentAccountId ->
            fetch(TemplateId, ParentAccountId, ResellerId)
    end.

-spec get_parent_account_id(ne_binary()) -> api_binary().
get_parent_account_id(AccountId) ->
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} -> kz_account:parent_account_id(JObj);
        {'error', _E} ->
            lager:error("failed to account ~s", [AccountId]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_from_db(ne_binary(), ne_binary(), ne_binary()) -> wh_proplist().
fetch_from_db(TemplateId, AccountId, ResellerId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    DocId = doc_id(TemplateId),
    lager:debug("fetching template ~s in ~s", [DocId, AccountId]),
    case couch_mgr:open_cache_doc(AccountDb, DocId) of
        {'ok', TemplateJObj} ->
            fetch_attachment(
              DocId
              ,AccountDb
              ,wh_doc:attachments(TemplateJObj, wh_json:new())
             );
        {'error', 'not_found'} ->
            lager:debug("failed to fetch template ~s from ~s not_found", [TemplateId, AccountId]),
            _ = cache_failure(TemplateId, AccountId),
            fetch_parent(TemplateId, AccountId, ResellerId);
        {'error', _E} ->
            lager:debug("failed to fetch template ~s from ~s ~p", [TemplateId, AccountId, _E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch_failure_cache(ne_binary(), ne_binary()) -> 'ok' | 'error'.
fetch_failure_cache(TemplateId, AccountId) ->
    lager:debug("looking if ~s recenlty failed to be fetched in ~s", [TemplateId, AccountId]),
    case wh_cache:fetch_local(?CACHE_NAME, ?TEMPLATE_FAILURE_KEY(TemplateId, AccountId)) of
        {'ok', 'failed'} -> 'error';
        {'error', 'not_found'} -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cache_failure(ne_binary(), ne_binary()) -> 'ok'.
cache_failure(TemplateId, AccountId) ->
    lager:debug("caching failure to find ~s in ~s", [TemplateId, AccountId]),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    CacheProps = [{'origin', {'db', AccountDb, doc_id(TemplateId)}}],
    wh_cache:store_local(
      ?CACHE_NAME
      ,?TEMPLATE_FAILURE_KEY(TemplateId, AccountId)
      ,'failed'
      ,CacheProps
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), init_params()) ->
                    {'ok', wh_json:object()} |
                    couch_mgr:couchbeam_error().
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
    case couch_mgr:save_doc(?WH_CONFIG_DB, SaveJObj) of
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
            {'ok', UpdatedJObj} = couch_mgr:open_doc(?WH_CONFIG_DB, Id),
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
    case couch_mgr:open_doc(?WH_CONFIG_DB, DocId) of
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
        couch_mgr:put_attachment(
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
                    couch_mgr:open_doc(?WH_CONFIG_DB, DocId);
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    E
            end;
        {'error', _E}=E ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
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
