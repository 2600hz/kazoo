%%%-------------------------------------------------------------------
%%% @copyright (c) 2010-2013, 2600Hz
%%% @doc
%%% Utility module for V3 Kazoo Migration
%%% @end
%%% @contributors
%%%   Ben Wann
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(csv_util).

-include("cdr.hrl").

%% API
-export([json_objs_to_csv/1,
         json_objs_to_csv/2
         ,test_convert/1
        ]).

-define(INCLUDE_HEADERS, 'true').

%%%===================================================================
%%% API
%%%===================================================================
%% TODO change name...
-spec json_objs_to_csv(wh_json:objects()) -> iolist().
json_objs_to_csv(JObjs) ->
    json_objs_to_csv(JObjs, ?INCLUDE_HEADERS).

-spec json_objs_to_csv(wh_json:objects(), boolean()) -> iolist().
json_objs_to_csv([], _) -> [];
json_objs_to_csv(JObjs, _) ->
    wh_json:encode(JObjs).

test_convert(AccountDb) ->
    ViewOptions = ['include_docs'],
    case couch_mgr:get_results(AccountDb, <<"cdrs/crossbar_listing">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'error', _E} ->
            lager:error("failed view ~s: ~p", [AccountDb, _E]), [];
        {'ok', JObjs} ->
            CdrDocs = lists:foldr(fun(JObj, Acc) ->
                                          Doc = wh_json:get_value([<<"doc">>], JObj),
                                          CdrDoc = wh_json:delete_key(<<"custom_channel_vars">>, Doc),
                                          [CdrDoc | Acc]
                                  end, [], JObjs),
            CsvData = json_objs_to_csv(CdrDocs),
            maybe_save_csv(<<"test.csv">>, CsvData)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec maybe_save_csv(ne_binary(), iolist()) -> 'ok'.
maybe_save_csv(FileName, CsvData) ->
    TestPath = list_to_binary([code:priv_dir('cdr')
                               ,"/test_data/"
                              ]),
    case filelib:ensure_dir(TestPath) of
        'ok' ->
            FilePath = list_to_binary([TestPath, FileName]),
            case file:write_file(FilePath, CsvData) of
                {'error', _E} -> lager:error("Error writing file: ~p", [_E]);
                'ok' -> 'ok'
            end;
        {'error', _E} -> lager:error("Error creating directory: ~p", [_E])
    end.
