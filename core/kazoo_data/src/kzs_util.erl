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
         ,db_classification/1
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

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec db_classification(text()) -> db_classifications().
db_classification(Db) when not is_binary(Db) ->
    db_classification(wh_util:to_binary(Db));
db_classification(<<"ts">>) -> 'depreciated';
db_classification(<<"crossbar_schemas">>) -> 'deprecated';
db_classification(<<"registrations">>) -> 'deprecated';
db_classification(<<"crossbar%2Fsessions">>) -> 'deprecated';
db_classification(<<"sms">>) -> 'deprecated';
db_classification(<<"signups">>) -> 'system'; %% Soon to be deprecated
db_classification(?WH_PROVISIONER_DB) -> 'system'; %% Soon to be deprecated
db_classification(?WH_ACCOUNTS_DB) -> 'aggregate';
db_classification(?KZ_TOKEN_DB) -> 'aggregate';
db_classification(?WH_SIP_DB) -> 'aggregate';
db_classification(?WH_FAXES_DB) -> 'aggregate';
db_classification(?KZ_ACDC_DB) -> 'aggregate';
db_classification(?WH_SERVICES_DB) -> 'aggregate';
db_classification(?KZ_PORT_REQUESTS_DB) -> 'aggregate';
db_classification(?KZ_WEBHOOKS_DB) -> 'aggregate';
db_classification(<<"numbers/", _Prefix:5/binary>>) -> 'numbers';
db_classification(<<"numbers%2F", _Prefix:5/binary>>) -> 'numbers';
db_classification(<<"numbers%2f", _Prefix:5/binary>>) -> 'numbers';
db_classification(?MATCH_MODB_SUFFIX_UNENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';% these only need to match
db_classification(?MATCH_MODB_SUFFIX_ENCODED(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   "account..." then the
db_classification(?MATCH_MODB_SUFFIX_encoded(_A,_B,_Rest,_Year,_Month)) -> 'modb';%   right size.
db_classification(?MATCH_ACCOUNT_UNENCODED(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_encoded(_AccountId)) -> 'account';
db_classification(?MATCH_ACCOUNT_ENCODED(_AccountId)) -> 'account';
db_classification(?WH_RATES_DB) -> 'system';
db_classification(?WH_OFFNET_DB) -> 'system';
db_classification(?WH_ANONYMOUS_CDR_DB) -> 'system';
db_classification(?WH_DEDICATED_IP_DB) -> 'system';
db_classification(?WH_CONFIG_DB) -> 'system';
db_classification(?WH_MEDIA_DB) -> 'system';
db_classification(?WH_SCHEMA_DB) -> 'system';
db_classification(?KZ_OAUTH_DB) -> 'system';
db_classification(_Database) ->
    lager:debug("unknown type for database ~s", [_Database]),
    'undefined'.
