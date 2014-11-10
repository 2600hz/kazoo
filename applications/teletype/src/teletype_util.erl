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
         ,init_template/2
         ,create_template/2
         ,update_template/2
        ]).

-include("teletype.hrl").

-spec template_doc_id(ne_binary()) -> ne_binary().
template_doc_id(<<"notification.", _/binary>> = ID) -> ID;
template_doc_id(<<_/binary>> = ID) -> <<"notification.", ID/binary>>.

init_template(Id, Macros) ->
    DocId = template_doc_id(Id),
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),

    case couch_mgr:open_cache_doc(MasterAccountDb, DocId) of
        {'ok', TemplateJObj} -> update_template(TemplateJObj, Macros);
        {'error', 'not_found'} -> create_template(DocId, Macros);
        {'error', _E} -> lager:warning("failed to find template ~s", [DocId])
    end.


-spec create_template(ne_binary(), wh_json:object()) ->
                             'ok' |
                             couch_mgr:couchbeam_error().
create_template(DocId, Macros) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
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
        {'ok', _Doc} -> 'ok';
        {'error', _E}=E -> E
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
