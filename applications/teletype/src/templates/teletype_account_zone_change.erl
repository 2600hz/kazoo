%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% @author Mark Magnusson
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_account_zone_change).
-behaviour(teletype_gen_email_template).

-export([id/0
        ,init/0
        ,macros/0, macros/1
        ,subject/0
        ,category/0
        ,friendly_name/0
        ]).
-export([handle_req/1
        ]).

-include("teletype.hrl").

-ifdef(TEST).
-export([build_zones_data/1
        ]).
-endif.

-define(TEMPLATE_TO, ?CONFIGURED_EMAILS(?EMAIL_ADMINS)).
-define(TEMPLATE_FROM, teletype_util:default_from_address()).
-define(TEMPLATE_CC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_BCC, ?CONFIGURED_EMAILS(?EMAIL_SPECIFIED, [])).
-define(TEMPLATE_REPLY_TO, teletype_util:default_reply_to()).

-spec id() -> kz_term:ne_binary().
id() ->
    <<"account_zone_change">>.

-spec init() -> template_response().
init() ->
    kz_util:put_callid(?MODULE),
    teletype_templates:init(id(), [{'macros', macros()}
                                  ,{'subject', subject()}
                                  ,{'category', category()}
                                  ,{'friendly_name', friendly_name()}
                                  ,{'to', ?TEMPLATE_TO}
                                  ,{'from', ?TEMPLATE_FROM}
                                  ,{'cc', ?TEMPLATE_CC}
                                  ,{'bcc', ?TEMPLATE_BCC}
                                  ,{'reply_to', ?TEMPLATE_REPLY_TO}
                                  ]),
    teletype_bindings:bind(<<"account_zone_change">>, ?MODULE, 'handle_req').

-spec macros() -> kz_json:object().
macros() ->
    kz_json:from_list(
      [?MACRO_VALUE(<<"zones">>, <<"zones">>, <<"Zones">>, <<"List of account's zones">>)
       | ?COMMON_TEMPLATE_MACROS
      ]).

-spec macros(kz_json:object()) -> kz_term:proplist().
macros(DataJObj) ->
    zones_data(DataJObj).

-spec subject() -> kz_term:ne_binary().
subject() ->
    <<"Account '{{account.name}}' zone have changed">>.

-spec category() -> kz_term:ne_binary().
category() ->
    <<"account">>.

-spec friendly_name() -> kz_term:ne_binary().
friendly_name() ->
    <<"Account Zone Change">>.

-spec handle_req(kz_json:object()) -> template_response().
handle_req(JObj) ->
    handle_req(JObj, kapi_notifications:account_zone_change_v(JObj)).

-spec handle_req(kz_json:object(), boolean()) -> template_response().
handle_req(_, 'false') ->
    lager:debug("invalid data for ~s", [id()]),
    teletype_util:notification_failed(id(), <<"validation_failed">>);
handle_req(JObj, 'true') ->
    lager:debug("valid data for ~s, processing...", [id()]),
    DataJObj  = kz_json:normalize(JObj),
    AccountId = kz_json:get_value(<<"account_id">>, DataJObj),

    case teletype_util:is_notice_enabled(AccountId, JObj, id()) of
        'false' -> teletype_util:notification_disabled(DataJObj, id());
        'true' -> process_req(DataJObj)
    end.

-spec process_req(kz_json:object()) -> template_response().
process_req(DataJObj) ->
    Macros = [{<<"system">>, teletype_util:system_params()}
             ,{<<"account">>, teletype_util:account_params(DataJObj)}
              | build_zones_data(DataJObj)
             ],

    RenderedTemplates = teletype_templates:render(id(), Macros, DataJObj),

    {'ok', TemplateMetaJObj} =
        teletype_templates:fetch_notification(id()
                                             ,kapi_notifications:account_id(DataJObj)
                                             ),

    Subject =
        teletype_util:render_subject(kz_json:find(<<"subject">>, [DataJObj, TemplateMetaJObj], subject())
                                    ,Macros
                                    ),

    Emails = teletype_util:find_addresses(DataJObj, TemplateMetaJObj, id()),

    case teletype_util:send_email(Emails, Subject, RenderedTemplates) of
        'ok' -> teletype_util:notification_completed(id());
        {'error', Reason} -> teletype_util:notification_failed(id(), Reason)
    end.

-spec build_zones_data(kz_json:object()) -> kz_term:proplist().
build_zones_data(DataJObj) ->
    [{<<"zones">>, zones_data(DataJObj)}
    ].

-spec zones_data(kz_json:object()) -> kz_term:proplist().
zones_data(DataJObj) ->
    case teletype_util:is_preview(DataJObj) of
        'false' ->
            kz_json:recursive_to_proplist(kz_json:get_json_value(<<"zones">>, DataJObj, []));
        'true' ->
            [{<<"home">>, <<"Zone">>}]
    end.
