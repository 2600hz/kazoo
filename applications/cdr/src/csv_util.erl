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
-export([json_objs_to_csv/1
         ,test_convert/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================


-spec json_objs_to_csv(wh_json:objects()) -> iolist().
json_objs_to_csv([]) -> [];
json_objs_to_csv([J|JObjs]) ->
    [csv_header(J), [json_to_csv(JObj) || JObj <- [J | JObjs]]].

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

-spec csv_header(wh_json:object()) -> iolist().
csv_header(JObj) ->
    csv_ize(wh_json:get_keys(JObj)).

-spec json_to_csv(wh_json:object()) -> iolist().
json_to_csv(JObj) ->
    {Vs, _} = wh_json:get_values(correct_jobj(JObj)),
    csv_ize(Vs).

-spec csv_ize(wh_json:keys()) -> iolist().
csv_ize([F|Rest]) ->
    [<<"\"">>, wh_util:to_binary(F), <<"\"">>
     ,[[<<",\"">>, wh_util:to_binary(V), <<"\"">>] || V <- Rest]
     ,<<"\n">>
    ].

-spec correct_jobj(wh_json:object()) -> wh_json:object().
correct_jobj(JObj) ->
    Prop = wh_json:to_proplist(JObj),
    L = lists:map(fun(X) -> correct_proplist(X) end, Prop),
    wh_json:from_list(L).

correct_proplist({K}) -> {K, <<"">>};
correct_proplist(T) -> T.
