-ifndef(KAPI_DEFINITION_HRL).

-type event_name_fun() :: fun((kz_term:api_terms()) -> kz_term:ne_binary()).
-type name() :: kz_term:ne_binary() | event_name_fun().
-type friendly_name() :: kz_term:api_ne_binary().
-type binding() :: kz_term:api_ne_binary() | fun((...) -> kz_term:ne_binary()).
-type build_fun() :: fun((kz_term:api_terms()) -> kz_api:api_formatter_return()).
-type validate_fun() :: fun((kz_term:api_terms()) -> boolean()).
-type publish_fun() :: fun((...) -> 'ok').

-record(kapi_definition, {name :: name() | 'undefined'
                         ,friendly_name :: friendly_name()
                         ,description :: kz_term:api_ne_binary()
                         ,category :: kz_term:api_ne_binary()
                         ,build_fun ::  build_fun() | 'undefined'
                         ,validate_fun :: validate_fun() | 'undefined'
                         ,publish_fun ::  publish_fun() | 'undefined'
                         ,binding = 'undefined' :: binding()
                         ,restrict_to = 'undefined' :: kz_term:api_atom()
                         ,required_headers = [] :: kz_api:api_headers()
                         ,optional_headers = [] :: kz_api:api_headers()
                         ,values = [] :: kz_api:api_valid_values()
                         ,types = [] :: kz_api:api_types()
                         }).

-define(KAPI_DEFINITION_HRL, 'true').
-endif.
