%%%=============================================================================
%% Copyright 2014 Klarna AB
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
%% @doc JESSE (JSon Schema Erlang)
%%
%% This is an interface module which provides an access to the main
%% functionality of jesse, such as 1) updating of the schema definitions cache;
%% 2) validation json data against a schema.
%% @end
%%%=============================================================================

-module(jesse).

%% API
-export([ add_schema/2
        , add_schema/3
        , del_schema/1
        , load_schemas/2
        , load_schemas/4
        , validate/2
        , validate/3
        , validate_with_schema/2
        , validate_with_schema/3
        ]).

-export_type([ json_term/0
             ]).

-type json_term() :: term().

%%% API
%% @doc Adds a schema definition `Schema' to in-memory storage associated with
%% a key `Key'. It will overwrite an existing schema with the same key if
%% there is any.
-spec add_schema(Key :: any(), Schema :: json_term()) ->
                    ok | jesse_error:error().
add_schema(Key, Schema) ->
  ValidationFun = fun jesse_lib:is_json_object/1,
  MakeKeyFun    = fun(_) -> Key end,
  jesse_database:add(Schema, ValidationFun, MakeKeyFun).

%% @doc Equivalent to `add_schema/2', but `Schema' is a binary string, and
%% the third agument is a parse function to convert the binary string to
%% a supported internal representation of json.
-spec add_schema( Key       :: any()
                , Schema    :: binary()
                , Options   :: [{Key :: atom(), Data :: any()}]
                ) -> ok | jesse_error:error().
add_schema(Key, Schema, Options) ->
  try
    ParserFun    = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    add_schema(Key, ParsedSchema)
  catch
    throw:Error -> {error, Error}
  end.


%% @doc Deletes a schema definition from in-memory storage associated with
%% the key `Key'.
-spec del_schema(Key :: any()) -> ok.
del_schema(Key) ->
  jesse_database:delete(Key).

%% @doc Loads schema definitions from filesystem to in-memory storage.
%%
%% Equivalent to `load_schemas(Path, ParserFun, ValidationFun, MakeKeyFun)'
%% where `ValidationFun' is `fun jesse_json:is_json_object/1' and
%% `MakeKeyFun' is `fun jesse_lib:get_schema_id/1'. In this case
%% the key will be the value of `id' attribute from the given schemas.
-spec load_schemas( Path      :: string()
                  , ParserFun :: fun((binary()) -> json_term())
                  ) -> jesse_database:update_result().
load_schemas(Path, ParserFun) ->
  load_schemas( Path
              , ParserFun
              , fun jesse_lib:is_json_object/1
              , fun jesse_lib:get_schema_id/1
              ).

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
%% `ParserFun' returns.
%%
%% NOTE: it's impossible to automatically update schema definitions added by
%%       add_schema/2, the only way to update them is to use add_schema/2
%%       again with the new definition.
-spec load_schemas( Path          :: string()
                  , ParserFun     :: fun((binary()) -> json_term())
                  , ValidationFun :: fun((any()) -> boolean())
                  , MakeKeyFun    :: fun((json_term()) -> any())
                  ) -> jesse_database:update_result().
load_schemas(Path, ParserFun, ValidationFun, MakeKeyFun) ->
  jesse_database:update(Path, ParserFun, ValidationFun, MakeKeyFun).

%% @doc Equivalent to {@link validate/3} where `Options' is an empty list.
-spec validate( Schema :: any()
              , Data   :: json_term() | binary()
              ) -> {ok, json_term()}
                 | jesse_error:error().
validate(Schema, Data) ->
  validate(Schema, Data, []).

%% @doc Validates json `Data' against a schema with the same key as `Schema'
%% in the internal storage, using `Options'. If the given json is valid,
%% then it is returned to the caller, otherwise an error with an appropriate
%% error reason is returned. If the `parser_fun' option is provided, then
%% `Data' is considered to be a binary string, so `parser_fun' is used
%% to convert the binary string to a supported internal representation of json.
%% If `parser_fun' is not provided, then `Data' is considered to already be a
%% supported internal representation of json.
-spec validate( Schema   :: any()
              , Data     :: json_term() | binary()
              , Options  :: [{Key :: atom(), Data :: any()}]
              ) -> {ok, json_term()}
                 | jesse_error:error().
validate(Schema, Data, Options) ->
  try
    ParserFun  = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedData = try_parse(data, ParserFun, Data),
    JsonSchema = jesse_database:read(Schema),
    jesse_schema_validator:validate(JsonSchema, ParsedData, Options)
  catch
    throw:Error -> {error, Error}
  end.

%% @doc Equivalent to {@link validate_with_schema/3} where `Options'
%% is an empty list.
-spec validate_with_schema( Schema :: json_term() | binary()
                          , Data   :: json_term() | binary()
                          ) -> {ok, json_term()}
                             | jesse_error:error().
validate_with_schema(Schema, Data) ->
  validate_with_schema(Schema, Data, []).

%% @doc Validates json `Data' agains the given schema `Schema', using `Options'.
%% If the given json is valid, then it is returned to the caller, otherwise
%% an error with an appropriate error reason is returned. If the `parser_fun'
%% option is provided, then both `Schema' and `Data' are considered to be a
%% binary string, so `parser_fun' is used to convert both binary strings to a
%% supported internal representation of json.
%% If `parser_fun' is not provided, then both `Schema' and `Data' are considered
%% to already be a supported internal representation of json.
-spec validate_with_schema( Schema   :: json_term() | binary()
                          , Data     :: json_term() | binary()
                          , Options  :: [{Key :: atom(), Data :: any()}]
                          ) -> {ok, json_term()}
                             | jesse_error:error().
validate_with_schema(Schema, Data, Options) ->
  try
    ParserFun    = proplists:get_value(parser_fun, Options, fun(X) -> X end),
    ParsedSchema = try_parse(schema, ParserFun, Schema),
    ParsedData   = try_parse(data, ParserFun, Data),
    jesse_schema_validator:validate(ParsedSchema, ParsedData, Options)
  catch
    throw:Error -> {error, Error}
  end.

%%% Internal functions
%% @doc Wraps up calls to a third party json parser.
%% @private
try_parse(Type, ParserFun, JsonBin) ->
  try
    ParserFun(JsonBin)
  catch
    _:Error ->
      case Type of
        data   -> throw({data_error, {parse_error, Error}});
        schema -> throw({schema_error, {parse_error, Error}})
      end
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
