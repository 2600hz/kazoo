-ifndef(CROSSBAR_TYPES_INCLUDED).

-type crossbar_status() :: success | error | fatal.
-type crossbar_module_result() :: tuple(crossbar_status(), proplist())
				  | tuple(crossbar_status(), proplist(), string())
				  | tuple(crossbar_status(), proplist(), string(), integer()).

-type crossbar_content_handler() :: tuple(atom(), list(string())).

-type http_method() :: 'POST' | 'GET' | 'PUT' | 'DELETE'.
-type http_methods() :: [http_method()].

-type validator() :: required | not_empty | is_type | is_format | numeric_min | numeric_max | numeric_between | width | width.
-type validator_rule() :: tuple(validator(), list() | []).
-type validator_rules() :: list(validator_rule()).

-type couch_doc_path() :: list(binary()).
-type couch_schema() :: list(tuple(couch_doc_path(), validator_rules())).

-type proplist_bool() :: list(tuple(boolean(), term())) | [].

-define(CROSSBAR_TYPES_INCLUDED, true).
-endif.
