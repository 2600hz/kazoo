%%%=============================================================================
%% Copyright 2013 Klarna AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Schema definitions cache handling.
%%
%% All the schema definitions are stored in an ETS table for quick access during
%% validation. This module provides an interface for: 1) updating of schema
%% definitions in runtime; 2) getting of a schema definition by a key. When
%% an update is ordered, the update function checks a schema definition file
%% timestamp and compares it to a timestamp for the same schema in the `cache',
%% so, it will never update a schema in the database if the definition file
%% was not updated.
%% @end
%%%=============================================================================

-module(jesse_database).

%% API
-export([ add/3
        , delete/1
        , read/1
        , update/4
        ]).

-export_type([ update_result/0
             ]).

-type update_result() :: ok | [fail()].

-type fail()          :: {file:filename(), file:date_time(), reason()}.
-type reason()        :: term().

-define(JESSE_ETS, jesse_ets).

-include_lib("kernel/include/file.hrl").

%%% API
%% @doc Adds a schema definition `Schema' to in-memory storage associated with
%% a key `Key'. It will overwrite an existing schema with the same key if
%% there is any.
-spec add( Schema        :: jesse:json_term()
         , ValidationFun :: fun((any()) -> boolean())
         , MakeKeyFun    :: fun((jesse:json_term()) -> any())
         ) -> update_result().
add(Schema, ValidationFun, MakeKeyFun) ->
  store_schema([{"", "", Schema}], ValidationFun, MakeKeyFun).

%% @doc Deletes a schema definition from in-memory storage associated with
%% the key `Key'.
-spec delete(Key :: any()) -> ok.
delete(Key) ->
  Table = table_name(),
  ets:delete(Table, Key),
  ok.

%% @doc Loads schema definitions from filesystem to in-memory storage.
%% The function loads all the files from directory `Path', then each schema
%% entry will be checked for a validity by function `ValidationFun', and
%% will be stored in in-memory storage with a key returned by `MakeKeyFun'
%% function.
%%
%% In addition to a schema definition, a timestamp of the schema file will be
%% stored, so, during the next update timestamps will be compared to avoid
%% unnecessary updates.
%%
%% Schema definitions are stored in the format which json parsing function
%% `ParseFun' returns.
%%
%% NOTE: it's impossible to automatically update schema definitions added by
%%       add_schema/2, the only way to update them is to use add_schema/2
%%       again with the new definition.
-spec update( Path          :: string()
            , ParseFun      :: fun((binary()) -> jesse:json_term())
            , ValidationFun :: fun((any()) -> boolean())
            , MakeKeyFun    :: fun((jesse:json_term()) -> any())
            ) -> update_result().
update(Path, ParseFun, ValidationFun, MakeKeyFun) ->
  Schemas = load_schema(Path, get_updated_files(Path), ParseFun),
  store_schema(Schemas, ValidationFun, MakeKeyFun).

%% @doc Reads a schema definition with the same key as `Key' from the internal
%% storage. If there is no such key in the storage, an exception will be thrown.
-spec read(Key :: any()) -> jesse:json_term() | no_return().
read(Key) ->
  case ets:lookup(table_name(), Key) of
    [{Key, _SecondaryKey, _TimeStamp, Term}] ->
      Term;
    _ ->
      throw({database_error, Key, schema_not_found})
  end.

%%% Internal functions
%% @doc Stores schema definitions `Schemas' in in-memory storage.
%% Uses `ValidationFun' to validate each schema definition before it is stored.
%% Each schema definition is stored with a key returned by `MakeKeyFun' applied
%% to the schema entry. Returns `ok' in case if all the schemas passed
%% the validation and were stored, otherwise a list of invalid entries
%% is returned.
%% @private
store_schema(Schemas, ValidationFun, MakeKeyFun) ->
  Table    = create_table(table_name()),
  StoreFun = fun({InFile, TimeStamp, Value} = Object, Acc) ->
                 case ValidationFun(Value) of
                   true ->
                     NewObject = { MakeKeyFun(Value)
                                 , InFile
                                 , TimeStamp
                                 , Value
                                 },
                     ets:insert(Table, NewObject),
                     Acc;
                   false ->
                     [Object | Acc]
                 end
             end,
  store_result(lists:foldl(StoreFun, [], Schemas)).

%% @private
store_result([])    -> ok;
store_result(Fails) -> Fails.

%% @doc Creates ETS table for internal cache if it does not exist yet,
%% otherwise the name of the table is returned.
%% @private
create_table(TableName) ->
  case table_exists(TableName) of
    false -> ets:new(TableName, [set, public, named_table]);
    true -> TableName
  end.

%% @doc Checks if ETS table with name `TableName' exists.
%% @private
table_exists(TableName) ->
  case ets:info(TableName) of
    undefined -> false;
    _TableInfo -> true
  end.

%% @doc Returns a list of schema definitions files in `InDir' which need to be
%% updated in the cache.
%% @private
get_updated_files(InDir) ->
  case { get_file_list(InDir)
       , table_exists(table_name())
       } of
    {[] = Files, _TableExists} ->
      Files;
    {Files, false} ->
      Files;
    {Files, _TableExists} ->
      Filter = fun(InFile) ->
                   is_outdated( get_full_path(InDir, InFile)
                              , InFile
                              )
               end,
      lists:filter(Filter, Files)
  end.

%% @doc Loads schema definitions from a list of files `Files' located in
%% directory `InDir', and parses each of entry by the given parse
%% function `ParseFun'. Silently ignores subdirectories.
%% @private
load_schema(InDir, Files, ParseFun) ->
  LoadFun = fun(InFile, Acc) ->
                InFilePath      = get_full_path(InDir, InFile),
                case file:read_file(InFilePath) of
                  {ok, SchemaBin} ->
                    {ok, FileInfo}  = file:read_file_info(InFilePath),
                    TimeStamp       = FileInfo#file_info.mtime,
                    Schema          = try_parse(ParseFun, SchemaBin),
                    [{InFile, TimeStamp, Schema} | Acc];
                  {error, eisdir} ->
                    Acc
                end
            end,
  lists:foldl(LoadFun, [], Files).

%% @doc Wraps up calls to a third party json parser.
%% @private
try_parse(ParseFun, SchemaBin) ->
  try
    ParseFun(SchemaBin)
  catch
    _:Error ->
      {parse_error, Error}
  end.

%% @private
get_file_list(InDir) ->
  {ok, Files} = file:list_dir(InDir),
  Files.

%% @private
get_full_path(Dir, File) ->
  filename:join([Dir, File]).

%% @doc Checks if a cache entry for a schema definition from file `InFile'
%% is outdated. Returns `true' if the cache entry needs to be updated, or if
%% the entry does not exist in the cache, otherwise `false' is returned.
%% @private
is_outdated(InFile, SecondaryKey) ->
  case ets:match_object(table_name(), {'_', SecondaryKey, '_', '_'}) of
    [] ->
      true;
    [{_Key, SecondaryKey, TimeStamp, _Value}] ->
      {ok, #file_info{mtime = MtimeIn}} = file:read_file_info(InFile),
      MtimeIn > TimeStamp
  end.

%% @doc Returns a name of ETS table which is used for in-memory cache.
%% Could be rewritten to use a configuration parameter instead of a hardcoded
%% value.
%% @private
table_name() -> ?JESSE_ETS.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
