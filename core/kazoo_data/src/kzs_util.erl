%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_util).


-export([maybe_add_rev/3
         ,maybe_add_pvt_type/3
        ]).

-include("kz_data.hrl").

%%------------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_rev(db(), ne_binary(), wh_proplist()) -> wh_proplist().
maybe_add_rev(#db{name=Name, app=App, server=Server}, DocId, Options) ->
    case props:get_value('rev', Options) =:= 'undefined'
        andalso kzs_doc:lookup_doc_rev({App, Server}, Name, DocId)
    of
        <<_/binary>> = Rev ->
            lager:debug("adding rev ~s to options", [Rev]),
            [{'rev', Rev} | Options];
        'false' ->
            lager:debug("rev is in options list: ~p", [Options]),
            Options;
        {'error', 'not_found'} ->
            lager:debug("failed to find rev of ~s in ~p, not_found in db", [DocId, Name]),
            Options;
        {'error', 'empty_doc_id'} ->
            lager:debug("failed to find doc id ~p", [DocId]),
            Options;
        _Else ->
            lager:debug("unknown rev format for ~p: ~p", [DocId, _Else]),
            Options
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_pvt_type(db(), ne_binary(), wh_json:object()) -> wh_json:object().
maybe_add_pvt_type(Db, DocId, JObj) ->
    case wh_doc:type(JObj) =:= 'undefined'
        andalso kzs_doc:open_doc(Db, DocId)
    of
        {'error', R} ->
            lager:error("failed to open doc ~p in ~p : ~p", [DocId, Db, R]),
            JObj;
        {'ok', Doc} ->
            wh_json:set_values(kzs_publish:publish_fields(Doc), JObj);
        _Else ->
            JObj
    end.

