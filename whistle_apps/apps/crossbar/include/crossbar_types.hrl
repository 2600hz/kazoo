-ifndef(CROSSBAR_TYPES_INCLUDED).

-type crossbar_status() :: 'success' | 'error' | 'fatal'.
-type crossbar_module_result() :: {crossbar_status(), proplist()}
				  | {crossbar_status(), proplist(), string()}
				  | {crossbar_status(), proplist(), string(), integer()}.

-type crossbar_content_handler() :: {atom(), [string(),...]}.

-type http_method() :: 'POST' | 'GET' | 'PUT' | 'DELETE'.
-type http_methods() :: [http_method()].

-type validator() :: 'required' | 'not_empty' | 'is_type' | 'is_format' | 'numeric_min' | 'numeric_max' | 'numeric_between' | 'width'.
-type validator_rule() :: {validator(), list() | []}.
-type validator_rules() :: [validator_rule(),...] | [].

-type couch_doc_path() :: list(binary()).
-type couch_schema() :: list({couch_doc_path(), validator_rules()}).

-type proplist_bool() :: [{boolean(), term()},...] | [].

-define(CROSSBAR_TYPES_INCLUDED, 'true').
-endif.
