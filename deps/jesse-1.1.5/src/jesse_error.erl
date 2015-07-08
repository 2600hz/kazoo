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

-module(jesse_error).

-export([ default_error_handler/3
        , handle_data_invalid/3
        , handle_schema_invalid/2
        ]).

-export_type([ error/0
             , error_reason/0
             , error_reasons/0
               ,error_handler/0
             ]).

-type error() :: {error, error_reasons()}.

-type error_reason() :: { 'schema_invalid'
                        , Schema :: jesse:json_term()
                        , Error  :: error_type()
                        }
                      | { 'data_invalid'
                        , Schema :: jesse:json_term()
                        , Error  :: error_type()
                        , Data   :: jesse:json_term()
                        , Path   :: [binary()]
                        }.
-type error_reasons() :: [error_reason(),...] | [].

-type error_type() :: {'missing_id_field', binary()}
                    | {'missing_required_property', binary()}
                    | {'missing_dependency', binary()}
                    | 'no_match'
                    | 'no_extra_properties_allowed'
                    | 'no_extra_items_allowed'
                    | 'not_enought_items'
                    | 'not_allowed'
                    | {'not_unique', jesse:json_term()}
                    | 'not_in_range'
                    | 'not_divisible'
                    | 'wrong_type'
                    | {'wrong_type_items', jesse:json_term()}
                    | {'wrong_type_dependency', jesse:json_term()}
                    | 'wrong_size'
                    | 'wrong_length'
                    | 'wrong_format'
                    | {'schema_unsupported', binary()}.

%% Includes
-include("jesse_schema_validator.hrl").

-type error_handler() :: fun((error_reason()
                              ,error_reasons()
                              ,non_neg_integer()
                             ) ->
                                error_reasons()
                                  ).

%% @doc Implements the default error handler.
%% If the length of `ErrorList' exceeds `AllowedErrors' then the function
%% throws an exeption, otherwise adds a new element to the list and returs it.
-spec default_error_handler( Error         :: error_reason()
                           , ErrorList     :: error_reasons()
                           , AllowedErrors :: non_neg_integer()
                           ) -> error_reasons() | no_return().
default_error_handler(Error, ErrorList, AllowedErrors) ->
  case AllowedErrors > length(ErrorList) orelse AllowedErrors =:= 'infinity' of
    true  -> [Error | ErrorList];
    false -> throw([Error | ErrorList])
  end.

%% @doc Generates a new data error and returns the updated state.
-spec handle_data_invalid( Info  :: error_type()
                         , Value :: jesse:json_term()
                         , State :: jesse_state:state()
                         ) -> jesse_state:state().
handle_data_invalid(Info, Value, State) ->
  Error = { ?data_invalid
          , jesse_state:get_current_schema(State)
          , Info
          , Value
          , lists:reverse(jesse_state:get_current_path(State))
          },
  handle_error(Error, State).

%% @doc Generates a new schema error and returns the updated state.
-spec handle_schema_invalid( Info  :: error_type()
                           , State :: jesse_state:state()
                           ) -> jesse_state:state().
handle_schema_invalid(Info, State) ->
  Error = { ?schema_invalid
          , jesse_state:get_current_schema(State)
          , Info
          },
  handle_error(Error, State).

%%% Internal functions
%% @private
handle_error(Error, State) ->
  ErrorHandler  = jesse_state:get_error_handler(State),
  ErrorList     = jesse_state:get_error_list(State),
  AllowedErrors = jesse_state:get_allowed_errors(State),
  NewErrorList  = ErrorHandler(Error, ErrorList, AllowedErrors),
  jesse_state:set_error_list(State, NewErrorList).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
