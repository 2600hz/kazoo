%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_low_balance).
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
id() -> <<"low_balance">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"current_balance">>, <<"current_balance">>, <<"Current Balance">>, <<"Account's Current Credit Balance">>)
      ,?MACRO_VALUE(<<"threshold">>, <<"threshold">>, <<"Threshold">>, <<"Account's Low Credit Balance Threshold">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> kz_term:ne_binary().
subject() -> <<"Account '{{account.name}}' is running out of credit">>.

-spec category() -> kz_term:ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() -> <<"Low Balance">>.

-spec to() -> kz_json:object().
to() -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

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
    handle_req(JObj, kapi_notifications:low_balance_v(JObj)).

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
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj]),
    Subject = teletype_util:render_subject(Subject0, Macros),
    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    [{<<"system">>, teletype_util:system_params()}
    ,{<<"account">>, teletype_util:account_params(DataJObj)}
    ,{<<"current_balance">>, get_current_balance(DataJObj)}
    ,{<<"threshold">>, get_balance_threshold(DataJObj)}
     | build_macro_data(DataJObj)
    ].

-spec get_current_balance(kz_json:object()) -> kz_term:ne_binary().
get_current_balance(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case current_account_dollars(AccountId) of
        {'ok', Dollars} -> kz_currency:pretty_print_dollars(Dollars);
        {'error', _R} -> <<"not known at the moment">>
    end.

-spec get_balance_threshold(kz_json:object()) -> kz_term:ne_binary().
get_balance_threshold(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    kz_currency:pretty_print_dollars(kzd_accounts:low_balance_threshold(AccountId)).

-spec build_macro_data(kz_json:object()) -> kz_term:proplist().
build_macro_data(DataJObj) ->
    kz_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                 ,[]
                 ,macros()
                 ).

-spec maybe_add_macro_key(kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) ->
                                 kz_term:proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, DataJObj) ->
    maybe_add_user_data(UserKey, Acc, DataJObj);
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    Acc.

-spec maybe_add_user_data(kz_term:ne_binary(), kz_term:proplist(), kz_json:object()) ->
                                 kz_term:proplist().
maybe_add_user_data(Key, Acc, DataJObj) ->
    User = get_user(DataJObj),
    case kz_json:get_value(Key, User) of
        'undefined' ->
            Acc;
        V ->
            UserMacros = props:get_value(<<"user">>, Acc, []),
            props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec get_user(kz_json:object()) -> kz_json:object().
get_user(DataJObj) ->
    AccountId = kz_json:get_ne_binary_value(<<"account_id">>, DataJObj),
    UserId = kz_json:get_ne_binary_value(<<"user_id">>, DataJObj),
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            ?LOG_DEBUG("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            kz_json:new()
    end.

-ifdef(TEST).
current_account_dollars(_) -> {'ok', 3.6592}.
-else.
current_account_dollars(AccountId) ->
    kz_currency:available_dollars(AccountId).
-endif.
