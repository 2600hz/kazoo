-ifndef(CROSSBAR_TYPES_INCLUDED).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_web/include/kazoo_web.hrl").

-define(CATCH_ALL, <<"_">>).
-define(HARD_DELETE, 'false').
-define(SOFT_DELETE, 'true').

-type successful_status() :: 'success' | 'accepted'.
-type error_status() :: 'error' | 'fatal' | 'stop'.
-type crossbar_status() :: successful_status() | error_status().
-type crossbar_module_result() :: {crossbar_status(), kz_term:proplist()} |
                                  {crossbar_status(), kz_term:proplist(), string()} |
                                  {crossbar_status(), kz_term:proplist(), string(), integer()}.

-type path_token() :: kz_term:ne_binary().
-type path_tokens() :: [path_token()].

-type resp_data() :: kz_json:object() | kz_json:objects() |
                     kz_json:json_term() | kz_json:json_proplist() |
                     kz_term:api_binary() | kz_term:ne_binaries() |
                     map().

-type req_file() :: {kz_term:ne_binary(), kz_json:object()}.
%% `{file_name, {"contents":<<bin>>, "headers":{"content-type":"", "content-length":1}}}'
-type req_files() :: [req_file()].
-type req_json() :: kz_json:object() | {'malformed', binary()}.

-type req_noun() :: {kz_term:ne_binary(), kz_term:ne_binaries()}.
-type req_nouns() :: [req_noun()].

-type content_type() :: {kz_term:ne_binary(), kz_term:ne_binary(), '*' | kz_term:proplist()} | kz_term:ne_binary().
%% `{Type, SubType, Options}'

-type media_value() :: {content_type(), non_neg_integer(), list()}.
-type media_values() :: [media_value()].

-define(MEDIA_VALUE(Type, SubType, Weight, Options, Extensions)
       ,{{Type, SubType, Options}, Weight, Extensions}
       ).
-define(MEDIA_VALUE(Type, SubType, Weight), ?MEDIA_VALUE(Type, SubType, Weight, [], [])).
-define(MEDIA_VALUE(Type, SubType), ?MEDIA_VALUE(Type, SubType, 1000, [], [])).

-type crossbar_content_handler() :: {atom(), kz_term:proplist()}.
%% `{handler_fun, {type, sub_type}} => {to_json, [{<<"application">>, <<"json">>}]}'
-type crossbar_content_handlers() :: [crossbar_content_handler()].

-type http_method() :: kz_term:ne_binary(). %% HTTP Verbs in UPPERCASE
-type http_methods() :: kz_term:ne_binaries().
-type req_verb() :: http_method().

-type validator() :: 'required' | 'not_empty' | 'is_type'
                   | 'is_format' | 'numeric_min'
                   | 'numeric_max' | 'numeric_between'
                   | 'width'.
-type validator_rule() :: {validator(), list()}.
-type validator_rules() :: [validator_rule()].

-type validation_error() :: jesse_error:error_reason().
-type validation_errors() :: [validation_error()].

-type couch_doc_path() :: kz_term:ne_binaries().
-type couch_schema() :: [{couch_doc_path(), validator_rules()}].

-type cb_cowboy_payload() :: {cowboy_req:req(), cb_context:context()}.

-define(CSV_CONTENT_TYPES, [{<<"application">>, <<"octet-stream">>}
                           ,{<<"text">>, <<"csv">>}
                           ,{<<"text">>, <<"comma-separated-values">>}
                           ]).
-define(JSON_CONTENT_TYPES, [{<<"application">>, <<"json">>}
                            ,{<<"application">>, <<"x-json">>}
                            ]).

-define(MULTIPART_CONTENT_TYPES, [{<<"application">>, <<"x-www-form-urlencoded">>}
                                 ,{<<"multipart">>, <<"form-data">>}
                                 ,{<<"multipart">>, <<"mixed">>}
                                 ]).

-define(IMAGE_CONTENT_TYPES, [{<<"image">>, <<"jpg">>}
                             ,{<<"image">>, <<"jpeg">>}
                             ,{<<"image">>, <<"png">>}
                             ,{<<"image">>, <<"gif">>}
                             ]).

-define(AUDIO_CONTENT_TYPES, [{<<"audio">>, <<"x-wav">>}
                             ,{<<"audio">>, <<"wav">>}
                             ,{<<"audio">>, <<"mpeg">>}
                             ,{<<"audio">>, <<"mp3">>}
                             ,{<<"audio">>, <<"ogg">>}
                             ]).

-define(VIDEO_CONTENT_TYPES, [{<<"video">>, <<"x-flv">>}
                             ,{<<"video">>, <<"h264">>}
                             ,{<<"video">>, <<"mpeg">>}
                             ,{<<"video">>, <<"quicktime">>}
                             ,{<<"video">>, <<"mp4">>}
                             ,{<<"video">>, <<"webm">>}
                             ]).

-define(BASE64_CONTENT_TYPES, [{<<"application">>, <<"base64">>}
                              ,{<<"application">>, <<"x-base64">>}
                              ]).

-define(PDF_CONTENT_TYPES, [{<<"application">>, <<"pdf">>}
                           ,{<<"application">>, <<"x-pdf">>}
                           ]).

-define(JSONP_CONTENT_TYPE, <<"application/javascript">>).

-define(CROSSBAR_TYPES_INCLUDED, 'true').
-endif.
