%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Module for converting JSON Schema to OpenAPI Specification 2, 3.
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_oas_schema).

-export([convert/2
        ,to_swagger_definitions/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type oas_schema_ret() :: {'ok', kz_json:object(), kz_term:ne_binaries()} |
                          {'error', kz_term:ne_binaries(), kz_term:ne_binaries()}.

%%------------------------------------------------------------------------------
%% @doc Convert all JSON Schema files in `SchemasPath' to OpenAPI Specification
%% `OasVersion' compatible Schema.
%%
%% `OasVersion' can be `<<"swagger2">>' or `<<"oas3">>'.
%%
%% Prints warning or errors if any.
%% @throws {'error', 'failed_to_convert_oas'}
%% @end
%%------------------------------------------------------------------------------
-spec to_swagger_definitions(kz_term:ne_binary(), file:filename_all()) -> kz_json:object().
to_swagger_definitions(OasVersion, SchemasPath) ->
    {OasSchemas, Report, HasError} =
        filelib:fold_files(kz_term:to_list(SchemasPath)
                          ,"\\.json\$"
                          ,'false'
                          ,fun(FileName, {Definitions, Report, HasError}) ->
                                   process_schema(FileName, Definitions, OasVersion, Report, HasError)
                           end
                          ,{kz_json:new(), #{}, 'false'}
                          ),
    print_report(Report),
    case HasError of
        'false' -> OasSchemas;
        'true' -> throw({'error', 'failed_to_convert_oas'})
    end.

-spec print_report(map()) -> 'ok'.
print_report(Report) ->
    _ = maps:map(fun(F, R) ->
                         print_report(F, maps:get(warn, R, []), maps:get(err, R, [])) end
                , Report
                ),
    'ok'.

-spec print_report(string(), kz_term:ne_binaries(), kz_term:ne_binaries()) -> 'ok'.
print_report(File, Warn, Err) ->
    print_messages(<<"Warnings">>, File, Warn),
    print_messages(<<"Errors">>, File, Err).

-spec print_messages(kz_term:ne_binary(), string(), kz_term:ne_binaries()) -> 'ok'.
print_messages(<<"Warnings">> = Title, File, Msgs) ->
    Ignore = [<<"path '._id': unsupported keyword.">>
             ,<<"path '.$schema': unsupported keyword.">>
             ],
    FilterFun = fun(Msg) -> not lists:any(fun(Elem) -> Elem =:= Msg end, Ignore) end,
    Head = iolist_to_binary(io_lib:format("~s in file: ~s", [Title, File])),
    print_messages(Head, lists:filter(FilterFun, Msgs));
print_messages(Title, File, Msgs) ->
    Head = iolist_to_binary(io_lib:format("~s in file: ~s", [Title, File])),
    print_messages(Head, Msgs).

-spec print_messages(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
print_messages(_, []) -> 'ok';
print_messages(Head, Msgs) ->
    print_messages([Head | Msgs]),
    io:format(user, "~n", []).

-spec print_messages(kz_term:ne_binaries()) -> 'ok'.
print_messages([]) -> 'ok';
print_messages([Msg | Msgs]) ->
    io:format(user, "~s~n", [Msg]),
    print_messages(Msgs).

-spec process_schema(file:filename_all(), kz_json:object(), kz_term:ne_binary(), map(), boolean()) ->
                            {kz_json:object(), map(), boolean()}.
process_schema(Filename, Definitions, OasVersion, Warn, Err) ->
    process_schema(Filename, Definitions, OasVersion, filename:basename(Filename, ".json"), Warn, Err).

-spec process_schema(file:filename_all(), kz_json:object(), kz_term:ne_binary(), string(), map(), boolean()) ->
                            {kz_json:object(), map(), boolean()}.
process_schema(_Filename, Definitions, <<"oas3">>, "kapi."++_ = Name, Report, HasError) ->
    {kz_json:delete_key(kz_term:to_binary(Name), Definitions), Report, HasError};
process_schema(Filename, Definitions, OasVersion, Name, Report, HasError) ->
    case convert(Filename, OasVersion) of
        {'ok', OasSchema, Warn} ->
            {kz_json:set_value(kz_term:to_binary(Name), OasSchema, Definitions)
            ,maps:put(Filename, #{warn => Warn}, Report)
            ,HasError
            };
        {'error', Warn, Err} ->
            {Definitions, maps:put(Filename, #{warn => Warn, err => Err}, Report), 'true'}
    end.

%%------------------------------------------------------------------------------
%% @doc Convert a JSON Schema `File' to OpenAPI Specification `OasVersion'
%% compatible Schema.
%%
%% `OasVersion' can be `<<"swagger2">>' or `<<"oas3">>'.
%% @end
%%------------------------------------------------------------------------------
-spec convert(file:filename_all(), kz_term:ne_binary()) -> oas_schema_ret().
convert(File, OasVersion) ->
    {'ok', Bin} = file:read_file(File),
    KVs = kz_json:to_proplist(kz_json:flatten(kz_json:decode(Bin))),
    case to_oas_schema(KVs, OasVersion) of
        {'ok', OasSchema, Warn} ->
            {'ok', kz_json:expand(kz_json:from_list(OasSchema)), Warn};
        {'error', Warn, Err} ->
            {'error', Warn, Err}
    end.

-spec to_oas_schema(kz_json:flat_proplist(), kz_term:ne_binary()) ->
                           {'ok', kz_json:flat_proplist(), kz_term:ne_binaries()} |
                           {'error', kz_term:ne_binaries(), kz_term:ne_binaries()}.
to_oas_schema(KVs, <<"swagger2">>) ->
    {'ok'
    ,[case lists:last(Path) =:= <<"$ref">> of
          'false' -> KV;
          'true' -> {Path, maybe_fix_ref(V, <<"swagger2">>)}
      end
      || {Path, V}=KV <- KVs,
         is_supported_by_swagger2(Path)
     ]
    ,[]
    };
to_oas_schema(KVs, <<"oas3">>) ->
    case to_oas3_schema(KVs, [], KVs, [], []) of
        {'ok', NewKVs, Warn} ->
            {'ok', NewKVs, format_path_msg(Warn)};
        {'error', Warn, Err} ->
            {'error', format_path_msg(Warn), format_path_msg(Err)}
    end.

-spec format_path_msg(kz_term:proplist()) -> kz_term:ne_binaries().
format_path_msg(PathMsgs) ->
    lists:reverse([iolist_to_binary(io_lib:format("path '~s': ~s", [Path, Msg])) || {Path, Msg} <- PathMsgs]).

-spec is_supported_by_swagger2(kz_term:ne_binaries()) -> boolean().
is_supported_by_swagger2(Path) ->
    %% keep swagger2 behaviour as before
    [<<"_id">>] =/= Path
        andalso [<<"$schema">>] =/= Path
        andalso [<<"additionalProperties">>] =/= Path
        andalso not lists:member(<<"patternProperties">>, Path)
        andalso not is_kazoo_prefixed(Path).

-spec is_kazoo_prefixed(kz_term:ne_binaries()) -> boolean().
is_kazoo_prefixed([]) -> 'false';
is_kazoo_prefixed([<<"kazoo-", _/binary>>|_]) -> 'true';
is_kazoo_prefixed([<<"support_level">>|_]) -> 'true';
is_kazoo_prefixed([_Field|Path]) -> is_kazoo_prefixed(Path).

-define(IS_OAS_PROPERTIES(P), (P =:= <<"properties">>
                                   orelse P =:= <<"additionalProperties">>
                                   orelse P =:= <<"x-patternProperties">>
                              )
       ).

-type oas3_schema_ret() :: {'ok', kz_term:proplist(), kz_term:proplist()} |
                           {'error', kz_term:proplist(), kz_term:proplist()}.

-spec to_oas3_schema(kz_json:flat_proplist(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> oas3_schema_ret().
to_oas3_schema([{Path, Val} | PVs], KVs, OrigKVs, Warn, Err) ->
    case to_oas3_schema(Path, Path, [], Val, KVs, OrigKVs, Warn, Err) of
        {'ok', NewKVs, NewWarn} -> to_oas3_schema(PVs, NewKVs, OrigKVs, NewWarn, Err);
        {'error', NewWarn, NewErr} -> to_oas3_schema(PVs, [{Path, Val} | KVs], OrigKVs, NewWarn, NewErr)
    end;
to_oas3_schema([], KVs, _, Warn, Err) ->
    return_error_on_error(lists:reverse(KVs), Warn, Err).

is_in_oas_properties([_, Properties | _])
  when ?IS_OAS_PROPERTIES(Properties) ->
    'true';
is_in_oas_properties([<<"items">>, _, Properties | _])
  when ?IS_OAS_PROPERTIES(Properties) ->
    'true';
is_in_oas_properties([]) ->
    'true';
is_in_oas_properties(_) ->
    'false'.

-spec to_oas3_schema(OriginalPath::kz_term:ne_binaries(), Path::kz_term:ne_binaries(), VisitedPath::kz_term:ne_binaries()
                    ,Value::kz_term:proplist_value(), KVAcc::kz_term:proplist(), OrigKVs::kz_term:proplist()
                    ,Warnings::kz_term:proplist(), Errors::kz_term:proplist()) -> oas3_schema_ret().
%% remove unsupported keywords
to_oas3_schema(OrigP, [P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when (P =:= <<"_id">>
            andalso Ps =:= []
       )
       orelse (P =:= <<"$schema">>
                   andalso Ps =:= []
              )
       orelse P =:= <<"$id">>
       orelse P =:= <<"additionalItems">>
       orelse P =:= <<"const">>
       orelse P =:= <<"contains">>
       orelse P =:= <<"dependencies">>
       orelse P =:= <<"id">>
       orelse P =:= <<"propertyNames">>
       ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            ret_unsupported_key(KVs, P, ReverseP, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;

%% let's rename this instead of removing
to_oas3_schema(OrigP, [<<"patternProperties">> = P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;

%% add prefix to kazoo specific fields
to_oas3_schema(OrigP, [<<"kazoo-", _/binary>> = P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;
to_oas3_schema(OrigP, [<<"support_level">> = P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            to_oas3_schema(OrigP, Ps, [<<"x-", P/binary>> | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;

%% since `kz_json:flatten/1' is not flattening a list if JObjs, we have to flatten them here.
to_oas3_schema(OrigP, [P], ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when P =:= <<"allOf">>
       orelse P =:= <<"anyOf">>
       orelse P =:= <<"oneOf">> ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            {NewVal, DeepWarn, DeepErr} = oas3_deep_flatten([P | ReverseP], Val, Warn, Err),
            to_oas3_schema(OrigP, [], [P | ReverseP], NewVal, KVs, OrigKVs, DeepWarn, DeepErr);
        'false' ->
            to_oas3_schema(OrigP, [], [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;
to_oas3_schema(OrigP, [P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err)
  when P =:= <<"allOf">>
       orelse P =:= <<"anyOf">>
       orelse P =:= <<"oneOf">> ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;

%% other checks
to_oas3_schema(OrigP, [<<"$ref">> = P], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, [], [P | ReverseP], maybe_fix_ref(Val, <<"oas3">>), KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"additionalProperties">> = P], ReverseP, Val, KVs, OrigKVs, Warn, Err) when is_boolean(Val) ->
    %% wth is this?
    to_oas3_schema(OrigP, [], [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
to_oas3_schema(OrigP, [<<"additionalProperties">> = P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    %% wth is this?
    case is_in_oas_properties(ReverseP) of
        'true' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;
to_oas3_schema(OrigP, [<<"type">> = P | Ps] = PPs, ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case is_in_oas_properties(ReverseP) of
        'true' ->
            to_oas3_type(OrigP, PPs, ReverseP, Val, KVs, OrigKVs, Warn, Err);
        'false' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err)
    end;

%% accept everything else as-is, we don't care about them
to_oas3_schema(OrigP, [P|Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);

%% return the result
to_oas3_schema(OrigP, [], [], Val, KVs, _, Warn, Err) ->
    return_error_on_error([{OrigP, Val} | KVs], Warn, Err);
to_oas3_schema(_, [], ReverseP, Val, KVs, _, Warn, Err) ->
    return_error_on_error([{lists:reverse(ReverseP), Val} | KVs], Warn, Err).


oas3_deep_flatten(ReverseP, [H|_]=Val, Warn, Err) ->
    case kz_json:is_json_object(H) of
        'true' ->
            {_, DeepValFlat, DeepWarn, DeepErr} =
                lists:foldl(fun(J, Acc) -> oas3_deep_flatten(J, ReverseP, Acc) end
                           ,{0, [], Warn, Err}
                           ,Val
                           ),
            {lists:reverse(DeepValFlat), DeepWarn, DeepErr};
        'false' ->
            Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
            {Val, Warn, [{join_oas3_path_reverse(ReverseP), Msg} | Err]}
    end;
oas3_deep_flatten(ReverseP, Val, Warn, Err) ->
    Msg = <<"the subschemas must be a valid list of OpenAPI schemas.">>,
    {Val, Warn, [{join_oas3_path_reverse(ReverseP), Msg} | Err]}.

oas3_deep_flatten(JObj, ReverseP,  {Index, Acc, Warn, Err}) ->
    KVs = kz_json:to_proplist(kz_json:flatten(JObj)),
    ArrayPath = join_oas3_path_reverse([<<"[", (kz_term:to_binary(Index))/binary, "]">> | ReverseP]),
    case to_oas3_schema(KVs, [], KVs, [], []) of
        {'ok', DeepKVs, DeepWarn} ->
            JObj1 = kz_json:expand(kz_json:from_list(DeepKVs)),
            {Index+1, [JObj1 | Acc], [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepWarn] ++ Warn, Err};
        {'error', DeepWarn, DeepErr} ->
            NewWarn = [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepWarn] ++ Warn,
            NewErr = [{<<ArrayPath/binary, Path/binary>>, Msg} || {Path, Msg} <- DeepErr] ++ Err,
            {Index+1, [JObj | Acc], NewWarn, NewErr}
    end.

-spec join_oas3_path_reverse(kz_term:ne_binaries()) -> kz_term:ne_binary().
join_oas3_path_reverse(Path) ->
    kz_binary:join([<<>> | lists:reverse(Path)], <<$.>>).

-spec return_error_on_error(kz_term:proplist(), kz_term:proplist(), kz_term:proplist()) -> oas3_schema_ret().
return_error_on_error(KVs, Warn, []) ->
    {'ok', KVs, Warn};
return_error_on_error(_, Warn, Err) ->
    {'error', Warn, Err}.

-spec ret_unsupported_key(kz_term:proplist(), kz_term:ne_binary(), kz_term:ne_binaries(), kz_term:ne_binaries(), kz_term:ne_binaries()) -> oas3_schema_ret().
ret_unsupported_key(KVs, Key, ReverseP, Warn, Err) ->
    Msg = <<"unsupported keyword.">>,
    return_error_on_error(KVs, [{join_oas3_path_reverse([Key | ReverseP]), Msg} | Warn], Err).

to_oas3_type(OrigP, [P | Ps], ReverseP, Val, KVs, OrigKVs, Warn, Err) ->
    case oas3_type_type(ReverseP, Val, OrigKVs) of
        'array' ->
            Msg = <<"the value must be a single type and not an array of types.">>,
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
        'binary' ->
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Val, KVs, OrigKVs, Warn, Err);
        'missing_items' ->
            Msg = <<"'items' must be present if type is array.">>,
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]};
        'null' ->
            Msg = <<"converting type 'null' to '\"nullable\": true'.">>,
            to_oas3_schema(OrigP, Ps, [<<"nullable">> | ReverseP], 'true', KVs, OrigKVs, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Warn], Err);
        'null_in_array' ->
            Msg = <<"type has 'null', adding '\"nullable\": true' instead.">>,
            Nullable = {lists:reverse([<<"nullable">> | ReverseP]), 'true'},
            [Type] = [T || T <- Val,
                           T =/= 'null',
                           T =/= <<"null">>
                     ],
            to_oas3_schema(OrigP, Ps, [P | ReverseP], Type, [Nullable | KVs ], OrigKVs, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Warn], Err);
        'undefined' ->
            Msg = iolist_to_binary(io_lib:format("invalid type '~p'.", [Val])),
            {'error', Warn, [{join_oas3_path_reverse([P | ReverseP]), Msg} | Err]}
    end.

-spec oas3_type_type(kz_term:ne_binaries(), kz_term:ne_binary() | kz_term:ne_binaries(), kz_term:proplist()) ->
                            'array' |
                            'binary' |
                            'missing_items' |
                            'null' |
                            'null_in_array' |
                            'undefined'.
oas3_type_type(Path, Value, KVs) when is_list(Value) ->
    HasArray = lists:member(<<"array">>, Value),
    case lists:any(fun(Null) -> lists:member(Null, Value) end, ['null', <<"null">>])
        andalso length(Value) =:= 2
    of
        'true' when HasArray ->
            case oas3_type_type(Path, <<"array">>, KVs) of
                'binary' -> null_in_array;
                'missing_items' -> 'missing_items'
            end;
        'true' -> 'null_in_array';
        'false' -> 'array'
    end;
oas3_type_type(Path, <<"array">>, KVs) ->
    RenameFun = fun(<<"x-patternProperties">>, Acc) -> [<<"patternProperties">> | Acc];
                   (Other, Acc) -> [Other | Acc]
                end,
    PartialItemsPath = lists:foldl(RenameFun, [], [<<"items">> | Path]),
    Fun = fun(Elem) -> lists:prefix(PartialItemsPath, Elem) end,
    case lists:any(Fun, [P || {P, _} <- KVs]) of
        'true' -> 'binary';
        'false' -> 'missing_items'
    end;
oas3_type_type(_Path, 'null', _KVs) ->
    'null';
oas3_type_type(_Path, <<"boolean">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"integer">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"null">>, _KVs) ->
    'null';
oas3_type_type(_Path, <<"number">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"object">>, _KVs) ->
    'binary';
oas3_type_type(_Path, <<"string">>, _KVs) ->
    'binary';
oas3_type_type(_Path, _Value, _KVs) ->
    'undefined'.

-spec maybe_fix_ref(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_fix_ref(<<"#",_/binary>>=Ref, _) -> Ref;
maybe_fix_ref(RelativePath=?NE_BINARY, <<"swagger2">>) ->
    <<"#/definitions/", RelativePath/binary>>;
maybe_fix_ref(RelativePath=?NE_BINARY, <<"oas3">>) ->
    <<"#/", RelativePath/binary>>.
