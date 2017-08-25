%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(teletype_low_balance).
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
id() -> <<"low_balance">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"current_balance">>, <<"current_balance">>, <<"Current Balance">>, <<"Account's Current Credit Balance">>)
      ,?MACRO_VALUE(<<"threshold">>, <<"threshold">>, <<"Threshold">>, <<"Account's Low Credit Balance Threshold">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() -> <<"Account '{{account.name}}' is running out of credit">>.

-spec category() -> ne_binary().
category() -> <<"account">>.

-spec friendly_name() -> ne_binary().
friendly_name() -> <<"Low Balance">>.

-spec to(ne_binary()) -> kz_json:object().
to(_) -> ?CONFIGURED_EMAILS(?EMAIL_ADMINS).

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
    handle_req(JObj, kapi_notifications:low_balance_v(JObj)).

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
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> 'ok'.
process_req(DataJObj) ->
    Macros = macros(DataJObj),

    %% Load templates
    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    AccountId = kapi_notifications:account_id(DataJObj),
    {'ok', TemplateMetaJObj} = teletype_templates:fetch_notification(id(), AccountId),
    Subject0 = kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj]),
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
    ,{<<"current_balance">>, get_current_balance(DataJObj)}
    ,{<<"threshold">>, get_balance_threshold(DataJObj)}
     | build_macro_data(DataJObj)
    ].

-spec get_current_balance(kz_json:object()) -> ne_binary().
get_current_balance(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case current_account_dollars(AccountId) of
        {'ok', Dollars} -> wht_util:pretty_print_dollars(Dollars);
        {'error', _R} -> <<"not known at the moment">>
    end.

-spec get_balance_threshold(kz_json:object()) -> ne_binary().
get_balance_threshold(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    wht_util:pretty_print_dollars(low_balance_threshold(AccountId)).

-spec build_macro_data(kz_json:object()) -> kz_proplist().
build_macro_data(DataJObj) ->
    kz_json:foldl(fun(MacroKey, _V, Acc) ->
                          maybe_add_macro_key(MacroKey, Acc, DataJObj)
                  end
                 ,[]
                 ,macros()
                 ).

-spec maybe_add_macro_key(kz_json:path(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_macro_key(<<"user.", UserKey/binary>>, Acc, DataJObj) ->
    maybe_add_user_data(UserKey, Acc, DataJObj);
maybe_add_macro_key(_Key, Acc, _DataJObj) ->
    ?LOG_DEBUG("unprocessed macro key ~s: ~p", [_Key, _DataJObj]),
    Acc.

-spec maybe_add_user_data(kz_json:path(), kz_proplist(), kz_json:object()) -> kz_proplist().
maybe_add_user_data(Key, Acc, DataJObj) ->
    User = get_user(DataJObj),
    case kz_json:get_value(Key, User) of
        'undefined' ->
            ?LOG_DEBUG("unprocessed user macro key ~s: ~p", [Key, User]),
            Acc;
        V ->
            UserMacros = props:get_value(<<"user">>, Acc, []),
            props:set_value(<<"user">>, [{Key, V} | UserMacros], Acc)
    end.

-spec get_user(kz_json:object()) -> kz_json:object().
get_user(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    UserId = kz_json:get_value(<<"user_id">>, DataJObj),
    case fetch_user(AccountId, UserId) of
        {'ok', UserJObj} -> UserJObj;
        {'error', _E} ->
            ?LOG_DEBUG("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            kz_json:new()
    end.

-ifdef(TEST).
current_account_dollars(?AN_ACCOUNT_ID) -> {ok, 38.6592}.

low_balance_threshold(?AN_ACCOUNT_ID) ->
    {ok,AccountJObj} = kz_json:fixture(?APP, "an_account.json"),
    kz_account:low_balance_threshold(AccountJObj).

fetch_user(?AN_ACCOUNT_ID, ?AN_ACCOUNT_USER_ID) ->
    kz_json:fixture(?APP, "an_account_user.json").
-else.
current_account_dollars(AccountId) ->
    wht_util:current_account_dollars(AccountId).

low_balance_threshold(AccountId) ->
    kz_account:low_balance_threshold(AccountId).

fetch_user(AccountId, UserId) ->
    kzd_user:fetch(AccountId, UserId).
-endif.
