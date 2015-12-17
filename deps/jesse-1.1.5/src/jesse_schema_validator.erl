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

-module(jesse_schema_validator).

%% API
-export([ validate/3
        , validate_with_state/3
        ]).

%% Includes
-include("jesse_schema_validator.hrl").

%%% API
%% @doc Validates json `Data' against `JsonSchema' with `Options'.
%% If the given json is valid, then it is returned to the caller as is,
%% otherwise an exception will be thrown.
-spec validate( JsonSchema :: jesse:json_term()
              , Data       :: jesse:json_term()
              , Options    :: [{Key :: atom(), Data :: any()}]
              ) -> {ok, jesse:json_term()}
                 | no_return().
validate(JsonSchema, Value, Options) ->
  State    = jesse_state:new(JsonSchema, Options),
  NewState = validate_with_state(JsonSchema, Value, State),
  {result(NewState), Value}.

%% @doc Validates json `Data' against `JsonSchema' with `State'.
%% If the given json is valid, then the latest state is returned to the caller,
%% otherwise an exception will be thrown.
-spec validate_with_state( JsonSchema :: jesse:json_term()
                         , Data       :: jesse:json_term()
                         , State      :: jesse_state:state()
                         ) -> jesse_state:state()
                            | no_return().
validate_with_state(JsonSchema, Value, State) ->
  SchemaVer = get_schema_ver(JsonSchema, State),
  select_and_run_validator(SchemaVer, JsonSchema, Value, State).

%%% Internal functions
%% @doc Returns "$schema" property from `JsonSchema' if it is present,
%% otherwise the default schema version from `State' is returned.
%% @private
get_schema_ver(JsonSchema, State) ->
  case jesse_json_path:value(?_SCHEMA, JsonSchema, ?not_found) of
    ?not_found -> jesse_state:get_default_schema_ver(State);
    SchemaVer  -> SchemaVer
  end.

%% @doc Returns a result depending on `State'.
%% @private
result(State) ->
  ErrorList = jesse_state:get_error_list(State),
  case ErrorList of
    [] -> ok;
    _  -> throw(ErrorList)
  end.

%% @doc Runs appropriate validator depending on schema version
%% it is called with.
%% @private
select_and_run_validator(?json_schema_draft3, JsonSchema, Value, State) ->
  jesse_validator_draft3:check_value( Value
                                    , jesse_json_path:unwrap_value(JsonSchema)
                                    , State
                                    );
select_and_run_validator(SchemaURI, _JsonSchema, _Value, State) ->
  jesse_error:handle_schema_invalid({?schema_unsupported, SchemaURI}, State).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
