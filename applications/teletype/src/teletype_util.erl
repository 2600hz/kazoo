%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_util).

-export([template_doc_id/1
         ,init_template/4
        ]).

-include("teletype.hrl").

-spec template_doc_id(ne_binary()) -> ne_binary().
template_doc_id(<<"notification.", _/binary>> = ID) -> ID;
template_doc_id(<<_/binary>> = ID) -> <<"notification.", ID/binary>>.

-spec init_template(ne_binary(), wh_json:object(), binary(), binary()) -> 'ok'.
init_template(Id, Macros, Text, HTML) ->
    DocId = template_doc_id(Id),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    Attachments = [{<<"text/plain">>, Text}
                   ,{<<"text/html">>, HTML}
                  ],

    lager:debug("looking for ~s", [DocId]),
    case couch_mgr:open_doc(MasterAccountDb, DocId) of
        {'ok', TemplateJObj} -> update_template(TemplateJObj, Macros, Attachments);
        {'error', 'not_found'} -> create_template(MasterAccountDb, DocId, Macros, Attachments);
        {'error', _E} -> lager:warning("failed to find template ~s", [DocId])
    end.

-spec create_template(ne_binary(), ne_binary(), wh_json:object(), wh_proplist()) ->
                             'ok' |
                             couch_mgr:couchbeam_error().
create_template(MasterAccountDb, DocId, Macros, Attachments) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),

    lager:debug("attempting to create template ~s", [DocId]),
    Doc = wh_doc:update_pvt_parameters(
            wh_json:from_list(
              [{<<"_id">>, DocId}
               ,{<<"macros">>, Macros}
              ])
            ,MasterAccountDb
            ,[{'account_db', MasterAccountDb}
              ,{'account_id', MasterAccountId}
              ,{'type', ?PVT_TYPE}
             ]),
    case couch_mgr:save_doc(MasterAccountDb, Doc) of
        {'ok', _UpdatedDoc} ->
            lager:debug("created template ~s", [DocId]),
            create_template_attachments(MasterAccountDb
                                        ,DocId
                                        ,Attachments
                                       );
        {'error', _E}=E ->
            lager:debug("failed to create template ~s: ~p", [DocId, _E]),
            E
    end.

create_template_attachments(_MasterAccountDb, _DocId, []) -> 'ok';
create_template_attachments(MasterAccountDb, DocId, [{_ContentType, <<>>} | As]) ->
    create_template_attachments(MasterAccountDb, DocId, As);
create_template_attachments(MasterAccountDb, DocId, [{ContentType, Contents} | As]) ->
    AName = template_attachment_name(ContentType),

    case couch_mgr:put_attachment(MasterAccountDb
                                  ,DocId
                                  ,AName
                                  ,Contents
                                  ,[{'content_type', wh_util:to_list(ContentType)}]
                                 )
    of
        {'ok', _Doc} ->
            lager:debug("saved attachment ~s for ~s", [AName, DocId]),
            create_template_attachments(MasterAccountDb, DocId, As);
        {'error', 'conflict'} ->
            case does_attachment_exist(MasterAccountDb, DocId, AName) of
                'true' ->
                    lager:debug("template attachment ~s exists", [AName]),
                    create_template_attachments(MasterAccountDb, DocId, As);
                'false' ->
                    lager:debug("uploading ~s appears to have failed for ~s", [AName, DocId]),
                    create_template_attachments(MasterAccountDb, DocId, As)
            end;
        {'error', _E} ->
            lager:debug("uploading ~s appears to have failed for ~s: ~p", [AName, DocId, _E]),
            create_template_attachments(MasterAccountDb, DocId, As)
    end.

-spec template_attachment_name(ne_binary()) -> ne_binary().
template_attachment_name(ContentType) ->
    wh_util:clean_binary(<<"template.", (cow_qs:urlencode(ContentType))/binary>>).

-spec does_attachment_exist(ne_binary(), ne_binary(), ne_binary()) -> boolean().
does_attachment_exist(MasterAccountDb, DocId, AName) ->
    case couch_mgr:open_doc(MasterAccountDb, DocId) of
        {'ok', JObj} ->
            does_attachment_exist(JObj, AName);
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.

-spec does_attachment_exist(wh_json:object(), ne_binary()) -> boolean().
does_attachment_exist(JObj, AName) ->
    wh_json:get_value([<<"_attachments">>, cow_qs:urldecode(AName)], JObj) =/= 'undefined'.

-spec update_template(wh_json:object(), wh_json:object(), wh_proplist()) ->
                                   'ok' | couch_mgr:couchbeam_error().
update_template(TemplateJObj, MacroJObj, Attachments) ->
    {HasUpdates, JObj} =
        wh_json:foldl(fun maybe_update_template_fold/3
                      ,{'false', TemplateJObj}
                      ,MacroJObj
                     ),
    maybe_update_template_with_changes(JObj, HasUpdates, Attachments).

-spec maybe_update_template_with_changes(wh_json:object(), boolean(), wh_proplist()) ->
                                                'ok' | couch_mgr:couchbeam_error().
maybe_update_template_with_changes(JObj, 'false', Attachments) ->
    lager:debug("no updates to the macros found"),
    maybe_update_attachments(JObj, Attachments);
maybe_update_template_with_changes(JObj, 'true', Attachments) ->
    lager:debug("updating to ~p", [JObj]),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    case couch_mgr:save_doc(MasterAccountDb, JObj) of
        {'ok', UpdatedJObj} ->
            lager:debug("updated template with latest macros"),
            maybe_update_attachments(UpdatedJObj, Attachments);
        {'error', _E}=E -> E
    end.

-spec maybe_update_template_fold(wh_json:key(), wh_json:json_term(), {boolean(), wh_json:object()}) ->
                                        {boolean(), wh_json:object()}.
maybe_update_template_fold(MacroKey, MacroValue, {_HU, JObj}=Acc) ->
    case wh_json:get_value([<<"macros">>, MacroKey], JObj) of
        'undefined' ->
            lager:debug("adding macro ~s to template", [MacroKey]),
            {'true', wh_json:set_value([<<"macros">>, MacroKey], MacroValue, JObj)};
        _Value -> Acc
    end.

-spec maybe_update_attachments(wh_json:object(), wh_proplist()) -> 'ok'.
maybe_update_attachments(_JObj, []) ->
    lager:debug("finished checking attachments");
maybe_update_attachments(JObj, [{_ContentType, <<>>} | As]) ->
    maybe_update_attachments(JObj, As);
maybe_update_attachments(JObj, [{ContentType, Contents}|As]) ->
    AName = template_attachment_name(ContentType),

    case does_attachment_exist(JObj, AName) of
        'true' ->
            maybe_update_attachments(JObj, As);
        'false' ->
            DocId = wh_json:get_first_defined([<<"id">>,<<"_id">>], JObj),
            case save_attachment(DocId, AName, ContentType, Contents) of
                'true' ->
                    lager:debug("saved attachment ~s", [AName]);
                'false' ->
                    lager:debug("failed to save attachment ~s", [AName])
            end,
            maybe_update_attachments(JObj, As)
    end.

-spec save_attachment(ne_binary(), ne_binary(), ne_binary(), binary()) -> boolean().
save_attachment(DocId, AName, ContentType, Contents) ->
    lager:debug("attachment ~s missing", [AName]),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    case couch_mgr:put_attachment(MasterAccountDb
                                  ,DocId
                                  ,AName
                                  ,Contents
                                  ,[{'content_type', wh_util:to_list(ContentType)}]
                                 )
    of
        {'ok', _UpdatedJObj} ->
            lager:debug("added attachment ~s to ~s", [AName, DocId]),
            'true';
        {'error', 'conflict'} ->
            case does_attachment_exist(MasterAccountDb, DocId, AName) of
                'true' ->
                    lager:debug("added attachment ~s to ~s", [AName, DocId]),
                    'true';
                'false' ->
                    lager:debug("failed to add attachment ~s to ~s", [AName, DocId]),
                    'false'
            end;
        {'error', _E} ->
            lager:debug("failed to add attachment ~s to ~s: ~p", [AName, DocId, _E]),
            'false'
    end.
