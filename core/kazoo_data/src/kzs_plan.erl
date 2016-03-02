%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data plan
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_dataplan).

-include("kz_data.hrl").

-export([plan/2]).

-define(IS_JSON_GUARD(Obj), is_tuple(Obj) andalso is_list(element(1, Obj))).

-spec plan(ne_binary(), ne_binary() | wh_proplist() | wh_json:object()) -> map().
plan(DbName, DocType) when is_binary(DocType) ->
    get_dataplan(DbName, DocType);
plan(DbName, Props) when is_list(Props) ->
    plan(DbName, props:get_value('doc_type', Props));
plan(DbName, Doc) when ?IS_JSON_GUARD(Doc) ->
    plan(DbName, wh_doc:type(Doc)).

-spec server_tag() -> term().
server_tag() ->
    case get('$kz_dataserver_tag') of
        'undefined' -> 'local';
        Tag -> Tag
    end.

%% -spec server_tag(term()) -> 'ok'.
%% server_tag(Tag) ->
%%     put('$kz_dataserver_tag', Tag).

get_dataplan(DBName, DocType) ->
    case kzs_util:db_classification(DBName) of
        'modb' -> account_modb_dataplan(DBName, DocType);
        'account' -> account_dataplan(DBName, DocType);
        _Else -> system_dataplan()
    end.


system_dataplan() ->
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.

account_dataplan(_DBName, _DocType) ->
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.

account_modb_dataplan(_DBName, _DocType) ->
    #{tag => 'local', server => kz_dataconnections:get_server(server_tag())}.
