%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_fixturedb_view).

%% View-related
-export([design_info/3
        ,all_design_docs/3
        ,get_results/4
        ,get_results_count/4
        ,all_docs/3
        ]).

-include("kz_fixturedb.hrl").

%%%=============================================================================
%%% View-related
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec design_info(server_map(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc_resp().
design_info(_Server, _DbName, Design) ->
    {'ok', kz_json:from_list([{<<"name">>, Design}])}.

-spec all_design_docs(server_map(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
all_design_docs(Server, DbName, _Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    Path = kz_fixturedb_util:docs_dir(Db),
    case filelib:wildcard(Path ++ "/_design*") of
        [] ->
            case kz_fixturedb_db:db_exists(Server, DbName) of
                'true' -> {'ok', []};
                'false' -> {'error', 'not_found'}
            end;
        DesignDocs ->
            {'ok', [kz_http_util:urldecode(kz_term:to_binary(filename:basename(D))) || D <- DesignDocs]}
    end.

-spec get_results(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> get_results().
get_results(Server, DbName, Design, Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),

    get_results(Server, DbName, Design, Options, Db).

-spec get_results(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options(), map()) -> get_results().
get_results(Server, DbName, Design, Options, Db) ->
    process_view_results(Server, DbName, Design, Options, Db, kz_fixturedb_util:open_view(Db, Design, Options)).

-spec process_view_results(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options(), map(), docs_resp()) -> get_results().
process_view_results(Server, DbName, _Design, Options, _Db, {'ok', Result}) ->
    {'ok', prepare_view_result(Server, DbName, Result, Options)};
process_view_results(_Server, DbName, Design, Options, Db, {'error', _}) ->
    fixturedb_view_is_missing(DbName, Design, Options, Db).

-spec get_results_count(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) ->
          {'ok', non_neg_integer()} | no_return().
get_results_count(Server, DbName, Design, Options) ->
    {'ok', JObjs} = get_results(Server, DbName, Design, Options),
    {'ok', erlang:length(JObjs)}.

-spec all_docs(server_map(), kz_term:ne_binary(), kz_data:options()) -> get_results().
all_docs(Server, DbName, Options) ->
    Ids = props:get_first_defined(['key', 'keys'], Options),
    all_docs(Server, DbName, Options, Ids).

-spec all_docs(server_map(), kz_term:ne_binary(), kz_data:options(), kz_term:api_ne_binary() | kz_term:ne_binaries()) -> get_results().
all_docs(Server, DbName, Options, 'undefined') ->
    case search_db_path(Server, DbName, "*") of
        {'ok', DocIds} ->
            {'ok'
            ,[prepare_all_docs_result(Server, DbName, Options, DocId)
              || DocId <- sort_and_limit(DocIds, Options)
             ]
            };
        {'error', _} ->
            fixturedb_is_missing(Server, DbName)
    end;
all_docs(Server, DbName, Options, <<DocId/binary>>) ->
    {'ok', prepare_all_docs_result(Server, DbName, Options, DocId)};
all_docs(Server, DbName, Options, DocIds) ->
    {'ok'
    ,[prepare_all_docs_result(Server, DbName, Options, DocId)
      || DocId <- sort_and_limit(DocIds, Options)
     ]
    }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_view_result(server_map(), kz_term:ne_binary(), kz_json:objects(), kz_data:options()) -> kz_json:objects().
prepare_view_result(Server, DbName, Result, Options) ->
    case props:get_value('include_docs', Options, 'false')
        andalso kz_term:is_false(props:get_first_defined(['reduce', 'group', 'group_level'], Options, 'false'))
    of
        'false' -> sort_and_limit(Result, Options);
        'true' ->
            [kz_json:set_value(<<"doc">>, JObjOrUndefined, V)
             || V <- sort_and_limit(Result, Options),
                JObjOrUndefined <- [include_doc_or_undefined(Server, DbName, kz_doc:id(V), Options)]
            ]
    end.

-spec include_doc_or_undefined(server_map(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_data:options()) -> kz_term:api_object().
include_doc_or_undefined(Server, DbName, DocId, Options) ->
    case kz_fixturedb_doc:open_doc(Server, DbName, DocId, Options) of
        {'ok', JObj} -> JObj;
        {'error', _} -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec prepare_all_docs_result(server_map(), kz_term:ne_binary(), kz_data:options(), kz_term:ne_binary()) -> kz_json:object().
prepare_all_docs_result(Server, DbName, Options, DocId) ->
    IncludeDocs = kz_term:is_not_empty(props:get_value('include_docs', Options, 'undefined')),
    case kz_fixturedb_doc:open_doc(Server, DbName, DocId, Options) of
        {'ok', JObj} when IncludeDocs ->
            kz_json:from_list(
              [{<<"key">>, DocId}
              ,{<<"id">>, DocId}
              ,{<<"value">>, kz_json:from_list([{<<"rev">>, kz_doc:revision(JObj)}])}
              ,{<<"doc">>, JObj}
              ]
             );
        {'ok', JObj} ->
            kz_json:from_list(
              [{<<"key">>, DocId}
              ,{<<"id">>, DocId}
              ,{<<"value">>, kz_json:from_list([{<<"rev">>, kz_doc:revision(JObj)}])}
              ]
             );
        {'error', _} ->
            kz_json:from_list(
              [{<<"key">>, DocId}
              ,{<<"error">>, <<"not_found">>}
              ]
             )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sort_and_limit(kz_json:objects() | kz_term:ne_binaries(), kz_data:options()) -> kz_json:objects() | kz_term:ne_binaries().
sort_and_limit(Result, Options) ->
    Limit = props:get_integer_value('limit', Options),
    case props:get_value('descending', Options, 'false') of
        'false' -> limit_result(Result, Limit);
        'true' -> limit_result(lists:reverse(Result), Limit)
    end.

-spec limit_result(kz_json:objects() | kz_term:ne_binaries(), kz_term:api_integer()) ->
          kz_json:objects() | kz_term:ne_binaries().
limit_result(Result, Limit) when is_integer(Limit),
                                 Limit > 0 ->
    try lists:split(Limit, Result) of
        {L1, _} -> L1
    catch
        'error':'badarg' -> Result
    end;
limit_result(Result, _) ->
    Result.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fixturedb_view_is_missing(kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options(), map()) -> no_return().
fixturedb_view_is_missing(_DbName, Design, Options, Db) ->
    ?BIG_WARN("Fixture for view is missing.~n"
              "Please create the CouchDB JSON view result exactly as what you expect for the test at the path below:~n"
              "~p~n"
              "~n~nAfter creating the file follow these instructions to add the query to views index~n"
              "to keep track of files:~n"
              "1) run 'make fixture_shell' to get an Erlang shell in kazoo source path~n"
              "2) run below erlang command:~n"
              "   kz_fixturedb_util:add_view_to_index(~100p, ~100p, ~1000p).~n"
             ,[kz_fixturedb_util:view_path(Db, Design, Options)
              ,_DbName, Design, Options
              ]
             ),
    throw('fixturedb_view_is_missing').

-spec fixturedb_is_missing(server_map(), kz_term:ne_binary()) -> no_return().
fixturedb_is_missing(Server, DbName) ->
    ?BIG_WARN("Fixture database directory for '~s' is missing.~n"
              "Please create the database directory in path below and populate its '/docs' and '/views' directories as necessary:~n"
              "~s~n"
             ,[DbName, kz_fixturedb_server:db_url(Server, DbName)]
             ),
    throw('fixturedb_is_missing').
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec search_db_path(server_map(), kz_term:ne_binary(), string()) ->
          {'ok', kz_term:ne_binaries()} |
          {'error', 'not_found'}.
search_db_path(Server, DbName, Wildcard) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    Path = kz_fixturedb_util:docs_dir(Db),
    case filelib:wildcard(filename:join(Path, Wildcard)) of
        [] ->
            case kz_fixturedb_db:db_exists(Server, DbName) of
                'true' -> {'ok', []};
                'false' -> {'error', 'not_found'}
            end;
        Result ->
            {'ok', [filename:basename(File, ".json") || File <- Result]}
    end.
