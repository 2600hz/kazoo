%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
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

-spec get_results(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
get_results(Server, DbName, Design, Options) ->
    Db = kz_fixturedb_server:get_db(Server, DbName),
    case kz_fixturedb_util:open_view(Db, kz_term:to_binary(Design), Options) of
        {'ok', Result} -> {'ok', prepare_view_result(Server, DbName, Result, Options)};
        {'error', _} ->
            ?SUP_LOG_ERROR("view file ~s does not not exists, view_options ~p"
                          ,[kz_fixturedb_util:view_path(Db, Design, Options), Options]
                          ),
            {'error', 'invalid_view_name'}
    end.

-spec get_results_count(server_map(), kz_term:ne_binary(), kz_term:ne_binary(), kz_data:options()) -> {ok, non_neg_integer()} | fixture_error().
get_results_count(Server, DbName, Design, Options) ->
    case get_results(Server, DbName, Design, Options) of
        {'ok', JObjs} -> {'ok', erlang:length(JObjs)};
        {'error', _}=Error -> Error
    end.

-spec all_docs(server_map(), kz_term:ne_binary(), kz_data:options()) -> docs_resp().
all_docs(Server, DbName, Options) ->
    get_results(Server, DbName, <<"all_docs">>, Options).

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

-spec sort_and_limit(kz_json:objects(), kz_data:options()) -> kz_json:objects().
sort_and_limit(Result, Options) ->
    Limit = props:get_integer_value('limit', Options),
    case props:get_value('descending', Options, 'false') of
        'false' -> limit_result(Result, Limit);
        'true' -> limit_result(lists:reverse(Result), Limit)
    end.

-spec limit_result(kz_json:objects(), kz_term:api_integer()) -> kz_json:objects().
limit_result(Result, Limit) when is_integer(Limit),
                                 Limit > 0 ->
    try lists:split(Limit, Result) of
        {L1, _} -> L1
    catch
        'error':'badarg' -> Result
    end;
limit_result(Result, _) ->
    Result.
