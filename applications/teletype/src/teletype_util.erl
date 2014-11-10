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

    case couch_mgr:open_cache_doc(MasterAccountDb, DocId) of
        {'ok', TemplateJObj} -> update_template(TemplateJObj, Macros);
        {'error', 'not_found'} -> create_template(MasterAccountDb, DocId, Macros, Text, HTML);
        {'error', _E} -> lager:warning("failed to find template ~s", [DocId])
    end.

-spec create_template(ne_binary(), ne_binary(), wh_json:object(), binary(), binary()) ->
                             'ok' |
                             couch_mgr:couchbeam_error().
create_template(MasterAccountDb, DocId, Macros, Text, HTML) ->
    {'ok', MasterAccountId} = whapps_util:get_master_account_id(),

    Doc = wh_doc:update_pvt_parameters(
            [{<<"_id">>, DocId}
             ,{<<"macros">>, Macros}
            ]
            ,MasterAccountDb
            ,[{'account_db', MasterAccountDb}
              ,{'account_id', MasterAccountId}
              ,{'type', ?PVT_TYPE}
             ]),
    case couch_mgr:save_doc(MasterAccountDb, Doc) of
        {'ok', Doc} -> create_template_attachments(MasterAccountId
                                                   ,DocId
                                                   ,[{<<"text/plain">>, Text}
                                                     ,{<<"text/html">>, HTML}
                                                    ]
                                                  );
        {'error', _E}=E -> E
    end.

create_template_attachments(_MasterAccountDb, _DocId, []) -> 'ok';
create_template_attachments(MasterAccountDb, DocId, [{_ContentType, <<>>} | As]) ->
    create_template_attachments(MasterAccountDb, DocId, As);
create_template_attachments(MasterAccountDb, DocId, [{ContentType, Contents} | As]) ->
    AName = wh_util:clean_binary(<<"template.", (cow_qs:urlencode(ContentType))/binary>>),
    case couch_mgr:put_attachment(MasterAccountDb, DocId, AName, Contents, [{'content_type', ContentType}]) of
        {'ok', _Doc} ->
            lager:debug("saved attachment ~s for ~s", [AName, DocId]),
            create_template_attachments(MasterAccountDb, DocId, As);
        {'error', 'conflict'} ->
            case does_attachment_exist(MasterAccountDb, DocId, AName) of
                'true' -> create_template_attachments(MasterAccountDb, DocId, As);
                'false' ->
                    lager:debug("uploading ~s appears to have failed for ~s", [AName, DocId]),
                    create_template_attachments(MasterAccountDb, DocId, As)
            end;
        {'error', _E} ->
            lager:debug("uploading ~s appears to have failed for ~s: ~p", [AName, DocId, _E]),
            create_template_attachments(MasterAccountDb, DocId, As)
    end.

-spec does_attachment_exist(ne_binary(), ne_binary(), ne_binary()) -> boolean().
does_attachment_exist(MasterAccountDb, DocId, AName) ->
    case couch_mgr:oepn_doc(MasterAccountDb, DocId) of
        {'ok', JObj} ->
            wh_json:get_value([<<"_attachments">>, AName], JObj) =/= 'undefined';
        {'error', _E} ->
            lager:debug("failed to open ~s to check for ~s: ~p", [DocId, AName, _E]),
            'false'
    end.

-spec update_template(wh_json:object(), wh_json:object()) ->
                                   'ok' | couch_mgr:couchbeam_error().
update_template(TemplateJObj, MacroJObj) ->
    {HasUpdates, JObj} =
        wh_json:foldl(fun maybe_update_template_fold/3
                      ,{'false', TemplateJObj}
                      ,MacroJObj
                     ),
    maybe_update_template_with_changes(JObj, HasUpdates).

-spec maybe_update_template_with_changes(wh_json:object(), boolean()) ->
                                                'ok' | couch_mgr:couchbeam_error().
maybe_update_template_with_changes(_JObj, 'false') ->
    lager:debug("no updates to the macros found");
maybe_update_template_with_changes(JObj, 'true') ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    case couch_mgr:save_doc(MasterAccountDb, JObj) of
        {'ok', _JObj} -> 'ok';
        {'error', _E}=E -> E
    end.

-spec maybe_update_template_fold(wh_json:key(), wh_json:json_term(), {boolean(), wh_json:object()}) ->
                                        {boolean(), wh_json:object()}.
maybe_update_template_fold(MacroKey, MacroValue, {_HU, JObj}=Acc) ->
    case wh_json:get_value(MacroKey, JObj) of
        'undefined' ->
            lager:debug("adding macro ~s to template", [MacroKey]),
            {'true', wh_json:set_value(MacroKey, MacroValue, JObj)};
        _Value -> Acc
    end.
