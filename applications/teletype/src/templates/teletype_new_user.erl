%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_new_user).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/0, from/0, cc/0, bcc/0, reply_to/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> kz_term:ne_binary().
id() -> <<"new_user">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
       | ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Your new VoIP services user profile has been created">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"user">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"New User">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL).

-spec from() -> kz_term:api_ne_binary().
from() -> teletype_util:default_from_address().

-spec cc() -> kz_json:object().
cc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc() -> kz_json:object().
bcc() -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to() -> kz_term:api_ne_binary().
reply_to() -> teletype_util:default_reply_to().

-spec init() -> 'ok'.
init() ->
    kz_log:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:new_user_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    {ReqData, Macros} = macros(DataJObj, 'true'),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, ReqData),

    AccountId = kapi_notifications:account_id(ReqData),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [ReqData, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(ReqData, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    macros(DataJObj, 'false').

-spec macros(kz_json:object(), boolean()) -> {kz_json:object(), kz_term:proplist()} | kz_term:proplist().
macros(DataJObj, 'true') ->
    ReqData = get_user_doc(DataJObj),
    {ReqData, create_macros(ReqData)};
macros(DataJObj, 'false') ->
    ReqData = get_user_doc(DataJObj),
    create_macros(ReqData).

-spec create_macros(kz_json:object()) -> kz_term:proplist().
create_macros(DataJObj) ->
    UserDoc = kz_json:get_value(<<"user">>, DataJObj, kz_json:new()),
    UserParams = case kz_json:get_value(<<"password">>, UserDoc) of
                     'undefined' -> teletype_util:user_params(UserDoc);
                     Password -> [{<<"password">>, Password}|teletype_util:user_params(UserDoc)]
                 end,
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, UserParams}
    ].

-spec get_user_doc(kz_json:object()) -> kz_json:object().
get_user_doc(DataJObj) ->
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = teletype_util:open_doc(<<"user">>, UserId, DataJObj),
    Password = kz_json:get_value(<<"password">>, DataJObj),

    Values = [{<<"user">>, kz_json:set_value(<<"password">>, Password, UserJObj)}
             ,{<<"to">>, [kz_json:get_ne_value(<<"email">>, UserJObj)]}
             ],
    ReqData = kz_json:set_values(Values, DataJObj),

    case teletype_util:is_preview(DataJObj) of
        'false' -> ReqData;
        'true' -> kz_json:merge_jobjs(DataJObj, ReqData)
    end.
