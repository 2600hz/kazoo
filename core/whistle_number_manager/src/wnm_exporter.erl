%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_exporter).

-export([to_csv/0]).

-include("wnm.hrl").

-define(CSV_FILE, <<"/tmp/numbers.csv">>).
-define(PAGE_SIZE, whapps_config:get_integer(?WNM_CONFIG_CAT
                                            ,[<<"csv_exporter">>, <<"batch_size">>]
                                            ,500
                                            )
       ).
-define(SLEEP_TIME, whapps_config:get_integer(?WNM_CONFIG_CAT
                                              ,[<<"csv_exporter">>, <<"wait_between_batches_ms">>]
                                              ,5 * ?MILLISECONDS_IN_SECOND
                                             )
       ).

-spec to_csv() -> 'ok'.
to_csv() ->
    maybe_remove_csv(),
    Fields = number_doc_fields(),
    write_csv_header(Fields),
    NumberDbs = wnm_util:get_all_number_dbs(),
    to_csv(Fields, NumberDbs).

-spec to_csv(ne_binaries(), ne_binaries()) -> 'ok'.
to_csv(_Fields, []) ->
    io:format("finished creating CSV in ~s~n", [?CSV_FILE]);
to_csv(Fields, [NumberDb|NumberDbs]) ->
    number_db_to_csv(Fields, NumberDb),
    to_csv(Fields, NumberDbs).

-spec number_db_to_csv(ne_binaries(), ne_binary()) -> 'ok'.
-spec number_db_to_csv(ne_binaries(), ne_binary(), ne_binary()) -> 'ok'.
number_db_to_csv(Fields, NumberDb) ->
    number_db_to_csv(Fields, NumberDb, <<"+">>).

number_db_to_csv(Fields, NumberDb, StartKey) ->
    batch_process(Fields
                  ,NumberDb
                  ,kz_datamgr:all_docs(NumberDb
                                      ,[{'startkey', StartKey}
                                        ,{'endkey', <<"+\uffff">>}
                                        ,{'limit', ?PAGE_SIZE + 1}
                                        ,'include_docs'
                                       ])
                 ).


-spec batch_process(ne_binaries(), wh_json:objects()) -> 'ok'.
-spec batch_process(ne_binaries(), ne_binary(), {'ok', wh_json:objects()}) -> 'ok'.
batch_process(Fields, NumberDb, {'ok', Numbers}) ->
    timer:sleep(?SLEEP_TIME),

    Db = cow_qs:urldecode(NumberDb),
    try lists:split(?PAGE_SIZE, Numbers) of
        {ViewResults, []} ->
            io:format("processing last batch for ~s~n", [Db]),
            batch_process(Fields, ViewResults);
        {ViewResults, [NextResult]} ->
            io:format("processing batch for ~s~n", [Db]),
            batch_process(Fields, ViewResults),
            number_db_to_csv(Fields, NumberDb, wh_json:get_value(<<"key">>, NextResult))
    catch
        'error':'badarg' ->
            io:format("processing last batch for ~s~n", [Db]),
            batch_process(Fields, Numbers)
    end.

batch_process(Fields, ViewResults) ->
    _ = [process_number_doc(Fields, wh_json:get_value(<<"doc">>, Result))
         || Result <- ViewResults
        ],
    'ok'.

-spec process_number_doc(ne_binaries(), wh_json:object()) -> 'ok'.
process_number_doc(Fields, NumberDoc) ->
    CSVRow = number_doc_to_csv_row(Fields, NumberDoc),
    append_to_csv(CSVRow),
    'ok'.

-spec number_doc_to_csv_row(ne_binaries(), wh_json:object()) -> iodata().
number_doc_to_csv_row(Fields, NumberDoc) ->
    CSVFields =
        lists:foldr(fun(Field, Acc) ->
                            number_key_to_csv_field(Field, Acc, NumberDoc)
                    end
                    ,[]
                    ,Fields
                   ),
    wrap_fields(CSVFields).

-spec wrap_fields(ne_binaries()) -> iodata().
wrap_fields([First | CSVFields]) ->
    [wrap_field(First)
     ,[[<<",">>, wrap_field(V)] || V <- CSVFields]
    ].

-spec wrap_field(ne_binary()) -> iodata().
wrap_field(V) ->
    [<<"\"">>, V, <<"\"">>].

-spec number_key_to_csv_field(wh_json:key(), binaries(), wh_json:object()) ->
                                     binaries().
number_key_to_csv_field(Key, Acc, NumberDoc) ->
    case wh_json:get_value(Key, NumberDoc) of
        'undefined' -> [<<>> | Acc];
        V -> [maybe_modify(Key, V) | Acc]
    end.

-spec maybe_modify(wh_json:key(), wh_json:json_term()) -> ne_binary().
maybe_modify(<<"pvt_modified">>, Timestamp) ->
    wh_util:iso8601(Timestamp);
maybe_modify(<<"pvt_created">>, Timestamp) ->
    wh_util:iso8601(Timestamp);
maybe_modify(<<"pvt_features">>, Vs) ->
    wh_util:join_binary(Vs, <<", ">>);
maybe_modify(<<"pvt_reserve_history">>, Vs) ->
    wh_util:join_binary(Vs, <<", ">>);
maybe_modify(_Key, V) -> wh_util:to_binary(V).

-spec number_doc_fields() -> wh_json:keys().
number_doc_fields() ->
    {'ok', Schema} = wh_json_schema:load(<<"phone_numbers">>),
    PublicKeys = wh_json:get_keys(<<"properties">>, Schema),
    number_doc_fields(Schema, PublicKeys).

-spec number_doc_fields(wh_json:object(), wh_json:keys()) -> wh_json:keys().
-spec number_doc_fields(wh_json:object(), wh_json:keys(), ne_binaries()) -> wh_json:keys().
number_doc_fields(Schema, PublicKeys) ->
    number_doc_fields(Schema, PublicKeys, default_number_doc_fields()).

number_doc_fields(_Schema, [], Fields) ->
    lists:reverse(Fields);
number_doc_fields(Schema, [Property|Properties], Fields) ->
    number_doc_fields(Schema, Properties, property_fields(Schema, Property, Fields)).

-spec property_fields(wh_json:object(), ne_binary(), wh_json:keys()) -> wh_json:keys().
property_fields(Schema, Property, Fields) ->
    PropertyKeys = wh_json:get_keys([<<"properties">>, Property, <<"properties">>]
                                    ,Schema
                                   ),
    lists:foldl(fun(Key, Acc) ->
                        property_key_paths(Key, Acc, Schema, Property)
                end
                ,Fields
                ,PropertyKeys
               ).

-spec property_key_paths(wh_json:key(), wh_json:keys(), wh_json:object(), wh_json:key()) -> wh_json:keys().
property_key_paths(Key, Acc, Schema, Property) ->
    KeyPath = [<<"properties">>, Property, <<"properties">>, Key],
    case wh_json:get_value(KeyPath ++ [<<"type">>], Schema) of
        <<"object">> ->
            SubKeys = wh_json:get_keys(KeyPath ++ [<<"properties">>], Schema),
            lists:foldl(fun(SubKey, Acc1) ->
                                [[Property, Key, SubKey] | Acc1]
                        end
                        ,Acc
                        ,SubKeys
                       );
        _Type -> [[Property, Key] | Acc]
    end.

-spec default_number_doc_fields() -> wh_json:keys().
default_number_doc_fields() ->
    [<<"pvt_type">>
     ,<<"pvt_features">>
     ,<<"pvt_reserve_history">>
     ,<<"pvt_module_name">>
     ,<<"pvt_db_name">>
     ,<<"pvt_modified">>
     ,<<"pvt_created">>
     ,<<"pvt_resource_db">>
     ,<<"pvt_number_state">>
     ,<<"pvt_authorizing_account">>
     ,<<"pvt_assigned_to">>
     ,<<"_id">>
    ].

-spec write_csv_header(ne_binaries()) -> 'ok'.
write_csv_header(Fields) ->
    Headers = lists:foldr(fun field_to_csv/2, [], Fields),
    append_to_csv(wrap_fields(Headers)).

-spec append_to_csv(iodata()) -> 'ok'.
append_to_csv(Data) ->
    case file:write_file(?CSV_FILE, [Data, <<"\n">>], ['append']) of
        'ok' -> 'ok';
        {'error', E} ->
            io:format("failed to write data to ~s: ~p~n", [?CSV_FILE, E]),
            exit(E)
    end.

-spec field_to_csv(wh_json:key(), ne_binaries()) -> ne_binaries().
field_to_csv([_|_]=Field, Acc) ->
    [wh_util:join_binary(Field, <<"_">>) | Acc];
field_to_csv(Field, Acc) ->
    [Field | Acc].

-spec maybe_remove_csv() -> 'ok'.
maybe_remove_csv() ->
    case file:delete(?CSV_FILE) of
        'ok' -> io:format("removed old version of ~s~n", [?CSV_FILE]);
        {'error', 'enoent'} -> 'ok';
        {'error', E} ->
            io:format("failed to cleanup ~s: ~s~n", [?CSV_FILE, E]),
            exit(E)
    end.
