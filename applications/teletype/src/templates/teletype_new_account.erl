%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2017, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_new_account).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ]).
-export([handle_req/1]).

-include("teletype.hrl").

-spec id() -> ne_binary().
id() ->
    <<"new_account">>.

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"admin.first_name">>, <<"first_name">>, <<"First Name">>, <<"Admin user first name">>)
      ,?MACRO_VALUE(<<"admin.last_name">>, <<"last_name">>, <<"Last Name">>, <<"Admin user last name">>)
      ,?MACRO_VALUE(<<"admin.email">>, <<"email">>, <<"email">>, <<"Admin user email">>)
      ,?MACRO_VALUE(<<"admin.timezone">>, <<"timezone">>, <<"timezone">>, <<"Admin user timezone">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec subject() -> ne_binary().
subject() ->
    <<"Your new VoIP services account '{{account.name}}' has been created">>.

-spec category() -> ne_binary().
category() ->
    <<"account">>.

-spec friendly_name() -> ne_binary().
friendly_name() ->
    <<"New Account">>.

-spec init() -> 'ok'.
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(?MODULE),
    teletype_bindings:bind(id(), ?MODULE, 'handle_req').

-spec handle_req(kz_json:object()) -> 'ok'.
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:new_account_v(JObj)).

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

    AccountId = teletype_util:find_account_id(DataJObj),
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
    ,{<<"admin">>, admin_user_properties(DataJObj)}
    ].

-spec admin_user_properties(kz_json:object()) -> kz_proplist().
admin_user_properties(DataJObj) ->
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),
    case account_fetch(AccountId) of
        {'ok', JObj} -> account_admin_user_properties(JObj);
        {'error', _} -> []
    end.

-spec account_admin_user_properties(kz_json:object()) -> kz_proplist().
account_admin_user_properties(AccountJObj) ->
    AccountDb = kz_doc:account_db(AccountJObj),
    case list_users(AccountDb) of
        {'error', _E} ->
            ?LOG_DEBUG("failed to get user listing from ~s: ~p", [AccountDb, _E]),
            [];
        {'ok', Users} ->
            find_admin(Users)
    end.

-spec find_admin(kz_json:objects()) -> kz_proplist().
find_admin([]) ->
    ?LOG_DEBUG("account has no admin users"),
    [];
find_admin([User|Users]) ->
    UserDoc = kz_json:get_value(<<"doc">>, User),
    case kzd_user:is_account_admin(UserDoc) of
        'true' -> teletype_util:user_params(UserDoc);
        'false' -> find_admin(Users)
    end.

-ifdef(TEST).
account_fetch(?AN_ACCOUNT_ID) ->
    {ok, teletype_util:fixture("an_account.json")}.

list_users(?AN_ACCOUNT_DB) ->
    UserJObj = kzd_user:set_priv_level(<<"admin">>, teletype_util:fixture("an_account_user.json")),
    {ok, [kz_json:from_list([{<<"doc">>, UserJObj}])]}.
-else.
account_fetch(AccountId) ->
    kz_account:fetch(AccountId).

list_users(AccountDb) ->
    kz_datamgr:get_results(AccountDb, <<"users/crossbar_listing">>, ['include_docs']).
-endif.
