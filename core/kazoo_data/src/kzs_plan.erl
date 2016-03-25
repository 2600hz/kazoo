%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data plan
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_plan).

-include("kz_data.hrl").

-export([plan/0, plan/1, plan/2]).

-define(IS_JSON_GUARD(Obj), is_tuple(Obj) andalso is_list(element(1, Obj))).

plan() ->
    system_dataplan().

plan(DbName) ->
    get_dataplan(DbName).

-spec plan(ne_binary(), atom() | ne_binary() | wh_proplist() | wh_json:object()) -> map().
plan(DbName, DocType) when is_binary(DocType) ->
    get_dataplan(DbName, DocType);
plan(DbName, Props) when is_list(Props) ->
    plan(DbName, props:get_value('doc_type', Props));
plan(DbName, Doc) when ?IS_JSON_GUARD(Doc) ->
    plan(DbName, wh_doc:type(Doc));
plan(DbName, 'undefined')  ->
    plan(DbName);
plan(DbName, DocType)
  when is_atom(DocType) ->
    plan(DbName, wh_util:to_binary(DocType)).

-spec server_tag() -> term().
server_tag() ->
    case get('$kz_dataserver_tag') of
        'undefined' -> 'local';
        Tag -> Tag
    end.

%% -spec server_tag(term()) -> 'ok'.
%% server_tag(Tag) ->
%%     put('$kz_dataserver_tag', Tag).

get_dataplan(DBName) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName);
        'account' -> account_dataplan(DBName);
        _Else -> system_dataplan()
    end.

get_dataplan(DBName, DocType) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType);
        'account' -> account_dataplan(DBName, DocType);
        _Else -> system_dataplan()
    end.


system_dataplan() ->
    SysTag = 'local',
    #{tag => SysTag, server => kz_dataconnections:get_server(SysTag)}.

account_dataplan(AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb),
    lager:debug("ACCOUNT ID IS ~p", [AccountId]),
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.

account_dataplan(AccountDb, <<"voicemail">>) ->
    AccountId = wh_util:format_account_id(AccountDb),
    lager:debug("VOICEMAIL ACCOUNT ID IS ~p", [AccountId]),
    #{tag => 'local'
     ,att_proxy => 'true'
     ,att_handler => {kz_att_gdrive, #{}}
     ,server => kz_dataconnections:get_server(server_tag())
     };
account_dataplan(AccountDb, _DocType) ->
    AccountId = wh_util:format_account_id(AccountDb),
    lager:debug("ACCOUNT ID IS ~p : ~p", [AccountId, _DocType]),
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.

account_modb_dataplan(AccountMODB) ->
    AccountId = wh_util:format_account_id(AccountMODB),
    lager:debug("ACCOUNT ID IS ~p", [AccountId]),
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.

account_modb_dataplan(AccountMODB, _DocType) ->
    AccountId = wh_util:format_account_id(AccountMODB),
    lager:debug("ACCOUNT ID IS ~p : ~p", [AccountId, _DocType]),
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.
