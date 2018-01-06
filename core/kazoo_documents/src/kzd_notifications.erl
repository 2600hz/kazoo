-module(kzd_notifications).

-export([new/0]).
-export([bcc/1, bcc/2, set_bcc/2]).
-export([category/1, category/2, set_category/2]).
-export([cc/1, cc/2, set_cc/2]).
-export([enabled/1, enabled/2, set_enabled/2]).
-export([friendly_name/1, friendly_name/2, set_friendly_name/2]).
-export([from/1, from/2, set_from/2]).
-export([macros/1, macros/2, set_macros/2]).
-export([reply_to/1, reply_to/2, set_reply_to/2]).
-export([subject/1, subject/2, set_subject/2]).
-export([template_charset/1, template_charset/2, set_template_charset/2]).
-export([to/1, to/2, set_to/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec bcc(doc()) -> api_object().
-spec bcc(doc(), Default) -> kz_json:object() | Default.
bcc(Doc) ->
    bcc(Doc, 'undefined').
bcc(Doc, Default) ->
    kz_json:get_json_value(<<"bcc">>, Doc, Default).

-spec set_bcc(doc(), kz_json:object()) -> doc().
set_bcc(Doc, Bcc) ->
    kz_json:set_value(<<"bcc">>, Bcc, Doc).

-spec category(doc()) -> api_ne_binary().
-spec category(doc(), Default) -> ne_binary() | Default.
category(Doc) ->
    category(Doc, 'undefined').
category(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"category">>, Doc, Default).

-spec set_category(doc(), ne_binary()) -> doc().
set_category(Doc, Category) ->
    kz_json:set_value(<<"category">>, Category, Doc).

-spec cc(doc()) -> api_object().
-spec cc(doc(), Default) -> kz_json:object() | Default.
cc(Doc) ->
    cc(Doc, 'undefined').
cc(Doc, Default) ->
    kz_json:get_json_value(<<"cc">>, Doc, Default).

-spec set_cc(doc(), kz_json:object()) -> doc().
set_cc(Doc, Cc) ->
    kz_json:set_value(<<"cc">>, Cc, Doc).

-spec enabled(doc()) -> boolean().
-spec enabled(doc(), Default) -> boolean() | Default.
enabled(Doc) ->
    enabled(Doc, true).
enabled(Doc, Default) ->
    kz_json:get_boolean_value(<<"enabled">>, Doc, Default).

-spec set_enabled(doc(), boolean()) -> doc().
set_enabled(Doc, Enabled) ->
    kz_json:set_value(<<"enabled">>, Enabled, Doc).

-spec friendly_name(doc()) -> api_ne_binary().
-spec friendly_name(doc(), Default) -> ne_binary() | Default.
friendly_name(Doc) ->
    friendly_name(Doc, 'undefined').
friendly_name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"friendly_name">>, Doc, Default).

-spec set_friendly_name(doc(), ne_binary()) -> doc().
set_friendly_name(Doc, FriendlyName) ->
    kz_json:set_value(<<"friendly_name">>, FriendlyName, Doc).

-spec from(doc()) -> api_binary().
-spec from(doc(), Default) -> binary() | Default.
from(Doc) ->
    from(Doc, 'undefined').
from(Doc, Default) ->
    kz_json:get_binary_value(<<"from">>, Doc, Default).

-spec set_from(doc(), binary()) -> doc().
set_from(Doc, From) ->
    kz_json:set_value(<<"from">>, From, Doc).

-spec macros(doc()) -> kz_json:object().
-spec macros(doc(), Default) -> kz_json:object() | Default.
macros(Doc) ->
    macros(Doc, kz_json:new()).
macros(Doc, Default) ->
    kz_json:get_json_value(<<"macros">>, Doc, Default).

-spec set_macros(doc(), kz_json:object()) -> doc().
set_macros(Doc, Macros) ->
    kz_json:set_value(<<"macros">>, Macros, Doc).

-spec reply_to(doc()) -> api_binary().
-spec reply_to(doc(), Default) -> binary() | Default.
reply_to(Doc) ->
    reply_to(Doc, 'undefined').
reply_to(Doc, Default) ->
    kz_json:get_binary_value(<<"reply_to">>, Doc, Default).

-spec set_reply_to(doc(), binary()) -> doc().
set_reply_to(Doc, ReplyTo) ->
    kz_json:set_value(<<"reply_to">>, ReplyTo, Doc).

-spec subject(doc()) -> api_ne_binary().
-spec subject(doc(), Default) -> ne_binary() | Default.
subject(Doc) ->
    subject(Doc, 'undefined').
subject(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"subject">>, Doc, Default).

-spec set_subject(doc(), ne_binary()) -> doc().
set_subject(Doc, Subject) ->
    kz_json:set_value(<<"subject">>, Subject, Doc).

-spec template_charset(doc()) -> ne_binary().
-spec template_charset(doc(), Default) -> ne_binary() | Default.
template_charset(Doc) ->
    template_charset(Doc, <<"utf-8">>).
template_charset(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"template_charset">>, Doc, Default).

-spec set_template_charset(doc(), ne_binary()) -> doc().
set_template_charset(Doc, TemplateCharset) ->
    kz_json:set_value(<<"template_charset">>, TemplateCharset, Doc).

-spec to(doc()) -> api_object().
-spec to(doc(), Default) -> kz_json:object() | Default.
to(Doc) ->
    to(Doc, 'undefined').
to(Doc, Default) ->
    kz_json:get_json_value(<<"to">>, Doc, Default).

-spec set_to(doc(), kz_json:object()) -> doc().
set_to(Doc, To) ->
    kz_json:set_value(<<"to">>, To, Doc).
