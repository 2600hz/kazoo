%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_new_user).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ,to/1, from/1, cc/1, bcc/1, reply_to/1
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> ne_binary().
id() -> <<"new_user">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"user.password">>, <<"password">>, <<"Password">>, <<"Password">>)
       | ?USER_MACROS
       ++ ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() -> <<"Your new VoIP services user profile has been created">>.

-spec category() -> ne_binary().
category() -> <<"user">>.

-spec friendly_name() -> ne_binary().
friendly_name() -> <<"New User">>.

-spec to(ne_binary()) -> kz_json:object().
to(_) -> ?CONFIGURED_EMAILS(?EMAIL_ORIGINAL).

-spec from(ne_binary()) -> api_ne_binary().
from(ModConfigCat) -> teletype_util:default_from_address(ModConfigCat).

-spec cc(ne_binary()) -> kz_json:object().
cc(_) -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec bcc(ne_binary()) -> kz_json:object().
bcc(_) -> ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, []).

-spec reply_to(ne_binary()) -> api_ne_binary().
reply_to(ModConfigCat) -> teletype_util:default_reply_to(ModConfigCat).

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:new_user_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> 'ok'.
handle_req(JObj, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:send_update(JObj, <<"failed">>, <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),

    %% Gather data for template
    DataJObj = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> do_handle_req(DataJObj)
    end.

-spec do_handle_req(kz_json:object()) -> 'ok'.
do_handle_req(DataJObj) ->
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),
    {'ok', UserJObj} = teletype_util:open_doc(<<"user">>, UserId, DataJObj),
    Password = kz_json:get_value(<<"password">>, DataJObj),

    Values = [{<<"user">>, kz_json:set_value(<<"password">>, Password, UserJObj)}
             ,{<<"to">>, [kz_json:get_ne_value(<<"email">>, UserJObj)]}
             ],
    ReqData = kz_json:set_values(Values, DataJObj),

    case teletype_util:is_preview(DataJObj) of
        'false' -> process_req(ReqData);
        'true' -> process_req(kz_json:merge_jobjs(DataJObj, ReqData))
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject()),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, teletype_util:mod_config_cat(id())),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:send_update(DataJObj, <<"completed">>);
        {'error', Reason} -> teletype_util:send_update(DataJObj, <<"failed">>, Reason)
    end.

-spec macros(kz_json:object()) -> kz_proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"user">>, teletype_util:public_proplist(<<"user">>, DataJObj)}
    ].
