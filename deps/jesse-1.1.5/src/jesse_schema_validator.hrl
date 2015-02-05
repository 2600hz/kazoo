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

-ifndef(JESSE_SCHEMA_VALIDATOR_HRL).

%% Constant definitions for Json schema keywords
-define(_SCHEMA,              <<"$schema">>).
-define(TYPE,                 <<"type">>).
-define(PROPERTIES,           <<"properties">>).
-define(PATTERNPROPERTIES,    <<"patternProperties">>).
-define(ADDITIONALPROPERTIES, <<"additionalProperties">>).
-define(MINPROPERTIES,        <<"minProperties">>).
-define(ITEMS,                <<"items">>).
-define(ADDITIONALITEMS,      <<"additionalItems">>).
-define(REQUIRED,             <<"required">>).
-define(DEPENDENCIES,         <<"dependencies">>).
-define(MINIMUM,              <<"minimum">>).
-define(MAXIMUM,              <<"maximum">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).
-define(MINITEMS,             <<"minItems">>).
-define(MAXITEMS,             <<"maxItems">>).
-define(UNIQUEITEMS,          <<"uniqueItems">>).
-define(PATTERN,              <<"pattern">>).
-define(MINLENGTH,            <<"minLength">>).
-define(MAXLENGTH,            <<"maxLength">>).
-define(ENUM,                 <<"enum">>).
-define(FORMAT,               <<"format">>).               % NOT IMPLEMENTED YET
-define(DIVISIBLEBY,          <<"divisibleBy">>).
-define(DISALLOW,             <<"disallow">>).
-define(EXTENDS,              <<"extends">>).
-define(ID,                   <<"id">>).
-define(_REF,                 <<"$ref">>).                 % NOT IMPLEMENTED YET

%% Constant definitions for Json types
-define(ANY,                  <<"any">>).
-define(ARRAY,                <<"array">>).
-define(BOOLEAN,              <<"boolean">>).
-define(INTEGER,              <<"integer">>).
-define(NULL,                 <<"null">>).
-define(NUMBER,               <<"number">>).
-define(OBJECT,               <<"object">>).
-define(STRING,               <<"string">>).

%% Supported $schema attributes
-define(default_schema_ver, <<"http://json-schema.org/draft-03/schema#">>).
-define(json_schema_draft3, <<"http://json-schema.org/draft-03/schema#">>).

%%
-define(data_invalid,                'data_invalid').
-define(schema_invalid,              'schema_invalid').

-define(missing_id_field,            'missing_id_field').
-define(missing_required_property,   'missing_required_property').
-define(missing_dependency,          'missing_dependency').
-define(no_match,                    'no_match').
-define(no_extra_properties_allowed, 'no_extra_properties_allowed').
-define(no_extra_items_allowed,      'no_extra_items_allowed').
-define(not_enought_items,           'not_enought_items').
-define(not_allowed,                 'not_allowed').
-define(not_unique,                  'not_unique').
-define(not_in_range,                'not_in_range').
-define(not_minimum,                 'not_minimum').
-define(not_maximum,                 'not_maximum').
-define(not_in_enum,                 'not_in_enum').
-define(not_divisible,               'not_divisible').
-define(wrong_type,                  'wrong_type').
-define(wrong_type_items,            'wrong_type_items').
-define(wrong_type_dependency,       'wrong_type_dependency').
-define(wrong_size,                  'wrong_size').
-define(wrong_min_items,             'wrong_min_items').
-define(wrong_max_items,             'wrong_max_items').
-define(wrong_min_properties,        'wrong_min_properties').
-define(wrong_length,                'wrong_length').
-define(wrong_min_length,            'wrong_min_length').
-define(wrong_max_length,            'wrong_max_length').
-define(wrong_format,                'wrong_format').
-define(schema_unsupported,          'schema_unsupported').

%%
-define(not_found, not_found).

-define(JESSE_SCHEMA_VALIDATOR_HRL, 'true').
-endif.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
