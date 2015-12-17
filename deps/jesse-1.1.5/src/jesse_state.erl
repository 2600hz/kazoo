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
%% @doc Json schema validation module.
%%
%% This module is the core of jesse, it implements the validation functionality
%% according to the standard.
%% @end
%%%=============================================================================

-module(jesse_state).

%% API
-export([ add_to_path/2
        , get_allowed_errors/1
        , get_current_path/1
        , get_current_schema/1
        , get_original_schema/1
        , get_default_schema_ver/1
        , get_error_handler/1
        , get_error_list/1
        , new/2
        , remove_last_from_path/1
        , set_allowed_errors/2
        , set_current_schema/2
        , set_error_list/2
        , find_schema/2
        ]).

-export_type([ state/0
             ]).

-include("jesse_schema_validator.hrl").

-define(SCHEMA_READER, fun jesse_database:read/1).

%% Internal datastructures
-record( state
       , { original_schema    :: jesse:json_term()
         , current_schema     :: jesse:json_term()
         , current_path       :: [binary()] %% current path in reversed order
         , allowed_errors     :: non_neg_integer() | 'infinity'
         , error_list         :: list()
         , error_handler      :: jesse_error:error_handler()
         , default_schema_ver :: binary()
         , schema_loader_fun  :: fun((binary()) -> {'ok', jesse:json_term()} | jesse:json_term() | ?not_found)
         }
       ).

-opaque state() :: #state{}.

%%% API
%% @doc Adds `Property' to the `current_path' in `State'.
-spec add_to_path(State :: state(), Property :: binary()) -> state().
add_to_path(State, Property) ->
  CurrentPath = State#state.current_path,
  State#state{current_path = [Property | CurrentPath]}.

%% @doc Getter for `allowed_errors'.
-spec get_allowed_errors(State :: state()) -> non_neg_integer().
get_allowed_errors(#state{allowed_errors = AllowedErrors}) ->
  AllowedErrors.

%% @doc Getter for `current_path'.
-spec get_current_path(State :: state()) -> [binary()].
get_current_path(#state{current_path = CurrentPath}) ->
  CurrentPath.

%% @doc Getter for `original_schema'.
-spec get_original_schema(State :: state()) -> jesse:json_term().
get_original_schema(#state{original_schema = OriginalSchema}) ->
  OriginalSchema.

%% @doc Getter for `current_schema'.
-spec get_current_schema(State :: state()) -> jesse:json_term().
get_current_schema(#state{current_schema = CurrentSchema}) ->
  CurrentSchema.

%% @doc Getter for `default_schema_ver'.
-spec get_default_schema_ver(State :: state()) -> binary().
get_default_schema_ver(#state{default_schema_ver = SchemaVer}) ->
  SchemaVer.

%% @doc Getter for `error_handler'.
-spec get_error_handler(State :: state()) -> jesse_error:error_handler().
get_error_handler(#state{error_handler = ErrorHandler}) ->
  ErrorHandler.

%% @doc Getter for `error_list'.
-spec get_error_list(State :: state()) -> list().
get_error_list(#state{error_list = ErrorList}) ->
  ErrorList.

%% @doc Returns newly created state.
-spec new( JsonSchema :: jesse:json_term()
         , Options    :: [{Key :: atom(), Data :: any()}]
         ) -> state().
new(JsonSchema, Options) ->
  DefaultHandler   = fun jesse_error:default_error_handler/3,
  ErrorHandler     = proplists:get_value( error_handler
                                        , Options
                                        , DefaultHandler
                                        ),
  AllowedErrors    = proplists:get_value( allowed_errors
                                        , Options
                                        , 0
                                        ),
  DefaultSchemaVer = proplists:get_value( default_schema_ver
                                        , Options
                                        , ?default_schema_ver
                                        ),

  SchemaLoaderFun = proplists:get_value( schema_loader_fun
                                       , Options
                                       , ?SCHEMA_READER
                                       ),

  #state{ current_schema     = JsonSchema
        , current_path       = []
        , original_schema    = JsonSchema
        , allowed_errors     = AllowedErrors
        , error_list         = []
        , error_handler      = ErrorHandler
        , default_schema_ver = DefaultSchemaVer
        , schema_loader_fun  = SchemaLoaderFun
        }.

%% @doc Removes the last element from `current_path' in `State'.
-spec remove_last_from_path(State :: state()) -> state().
remove_last_from_path(State = #state{current_path = [_Property | Path]}) ->
  State#state{current_path = Path}.

%% @doc Getter for `allowed_errors'.
-spec set_allowed_errors( State :: state()
                        , AllowedErrors :: non_neg_integer()
                        ) -> state().
set_allowed_errors(#state{} = State, AllowedErrors) ->
  State#state{allowed_errors = AllowedErrors}.

%% @doc Setter for `current_schema'.
-spec set_current_schema( State     :: state()
                        , NewSchema :: jesse:json_term()
                        ) -> state().
set_current_schema(State, NewSchema) ->
  State#state{current_schema = NewSchema}.

%% @doc Setter for `error_list'.
-spec set_error_list(State :: state(), ErrorList :: list()) -> state().
set_error_list(State, ErrorList) ->
  State#state{error_list = ErrorList}.

-spec find_schema(state(), binary()) ->
                     jesse:json_term() | ?not_found.
find_schema(#state{schema_loader_fun=F}, SchemaKey) ->
  try F(SchemaKey) of
      {'ok', Schema} -> Schema;
      Schema ->
      case jesse_lib:is_json_object(Schema) of
        'true' -> Schema;
        'false' -> ?not_found
      end
  catch
    _E:_R -> ?not_found
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
