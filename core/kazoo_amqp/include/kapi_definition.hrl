-ifndef(KAPI_DEFINITION_HRL).
-type binding() :: kz_term:api_ne_binary() | fun((...) -> kz_term:ne_binary()).

-record(kapi_definition, {name :: kz_term:api_ne_binary()
                         ,friendly_name :: kz_term:api_ne_binary()
                         ,description :: kz_term:api_ne_binary()
                         ,category = 'undefined' :: kz_term:api_ne_binary()
                         ,build_fun :: fun((kz_term:api_terms()) -> kz_api:api_formatter_return()) | 'undefined'
                         ,validate_fun :: fun((kz_term:api_terms()) -> boolean()) | 'undefined'
                         ,publish_fun :: fun((...) -> 'ok') | 'undefined'
                         ,binding = 'undefined' :: binding()
                         ,restrict_to = 'undefined' :: kz_term:api_atom()
                         ,required_headers = [] :: kz_api:api_headers()
                         ,optional_headers = [] :: kz_api:api_headers()
                         ,values = [] :: kz_api:api_valid_values()
                         ,types = [] :: kz_api:api_types()
                         }).

-define(KAPI_DEFINITION_HRL, 'true').
-endif.
