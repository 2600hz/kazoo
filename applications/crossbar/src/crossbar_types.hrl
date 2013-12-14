-ifndef(CROSSBAR_TYPES_INCLUDED).

-type crossbar_status() :: 'success' | 'error' | 'fatal'.
-type crossbar_module_result() :: {crossbar_status(), wh_proplist()}
                                  | {crossbar_status(), wh_proplist(), string()}
                                  | {crossbar_status(), wh_proplist(), string(), integer()}.

-type path_token() :: ne_binary().
-type path_tokens() :: [path_token(),...] | [].

-type resp_data() :: wh_json:object() | wh_json:objects() | api_binary() | wh_json:json_term().

 %% {file_name, {"contents":<<bin>>, "headers":{"content-type":"", "content-length":1}}}
-type req_file() :: {ne_binary(), wh_json:object()}.
-type req_files() :: [req_file(),...] | [].

-type req_nouns() :: [{ne_binary(), wh_json:json_strings()},...] | [].

%% {handler_fun, {type, sub_type}} => {to_json, [{<<"application">>, <<"json">>}]}

%% {Type, SubType, Options}
-type content_type() :: {ne_binary(), ne_binary(), wh_proplist()} | ne_binary().

-type crossbar_content_handler() :: {atom(), [{ne_binary(), ne_binary()} | ne_binary(),...]}.
-type crossbar_content_handlers() :: [crossbar_content_handler(),...] | [].

-type http_method() :: ne_binary(). %% HTTP Verbs in UPPERCASE
-type http_methods() :: ne_binaries().

-type validator() :: 'required' | 'not_empty' | 'is_type'
                   | 'is_format' | 'numeric_min'
                   | 'numeric_max' | 'numeric_between'
                   | 'width'.
-type validator_rule() :: {validator(), list() | []}.
-type validator_rules() :: [validator_rule(),...] | [].

-type couch_doc_path() :: ne_binaries().
-type couch_schema() :: list({couch_doc_path(), validator_rules()}).

-define(HTTP_GET, <<"GET">>).
-define(HTTP_POST, <<"POST">>).
-define(HTTP_PUT, <<"PUT">>).
-define(HTTP_DELETE, <<"DELETE">>).
-define(HTTP_OPTIONS, <<"OPTIONS">>).
-define(HTTP_HEAD, <<"HEAD">>).
-define(HTTP_PATCH, <<"PATCH">>).

-define(CSV_CONTENT_TYPES, [{<<"application">>, <<"octet-stream">>}
                            ,{<<"text">>, <<"csv">>}
                            ,{<<"text">>, <<"comma-separated-values">>}
                           ]).
-define(JSON_CONTENT_TYPES, [{<<"application">>, <<"json">>}
                             ,{<<"application">>, <<"x-json">>}
                            ]).

-define(JSONP_CONTENT_TYPE, <<"application/javascript">>).

-define(CROSSBAR_TYPES_INCLUDED, 'true').
-endif.
