%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(teletype_template_skel).

-export([init/0
         ,handle/2
        ]).

-include("../teletype.hrl").

-define(TEMPLATE_ID, <<"skel">>).
-define(TEMPLATE_MACROS, wh_json:from_list([{<<"user.first_name">>
                                             ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                                 ,{<<"friendly_name">>, <<"First Name">>}
                                                                 ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                                ])
                                            }
                                            ,{<<"user.last_name">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"first_name">>}
                                                                  ,{<<"friendly_name">>, <<"First Name">>}
                                                                  ,{<<"description">>, <<"First name of the owner of the voicemail box">>}
                                                                 ])
                                             }
                                            ,{<<"user.email">>
                                              ,wh_json:from_list([{<<"i18n_label">>, <<"email">>}
                                                                  ,{<<"friendly_name">>, <<"Email">>}
                                                                  ,{<<"description">>, <<"Email of the user">>}
                                                                 ])
                                             }
                                           ])).
-define(TEMPLATE_TEXT, <<"Hi {{user.first_name}} {{user.last_name}}.\n\nThis is the skeleton template\n">>).
-define(TEMPLATE_HTML, <<"<p>Hi {{user.first_name}} {{user.last_name}}.</p><p>This is the skeleton template</p>\n">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML).

-spec handle(wh_json:object(), wh_proplist()) -> 'ok'.
handle(JObj, _Props) ->
    'true' = wapi_notifications:skel_v(JObj),
    wh_util:put_callid(JObj),

    %% Gather data for template
    DataJObj = wh_api:remove_defaults(JObj),
    Macros = build_template_data(DataJObj),

    %% Load templates
    Templates = teletype_util:fetch_templates(?TEMPLATE_ID, wh_json:get_value(<<"Account-ID">>, DataJObj)),

    %% Populate templates
    _RenderedTemplates = [{ContentType, render(Template, Macros)} || {ContentType, Template} <- Templates],

    lager:debug("rendered: ~p", [_RenderedTemplates]),

    %% Send email
    'ok'.

-spec render(binary(), wh_proplist()) -> iodata() | 'undefined'.
render(Template, Macros) ->
    case teletype_render_farm_sup:render(?TEMPLATE_ID, Template, Macros) of
        {'ok', IOData} -> IOData;
        {'error', _E} ->
            lager:debug("failed to render template: ~p '~s'", [_E, Template]),
            'undefined'
    end.

-spec build_template_data(wh_json:object()) -> wh_proplist().
build_template_data(DataJObj) ->
    [{<<"user">>, build_user_data(DataJObj)}
     | wh_json:to_proplist(wh_json:normalize(DataJObj))
    ].

-spec build_user_data(wh_json:object()) -> wh_proplist().
-spec build_user_data(wh_json:object(), wh_json:object()) -> wh_proplist().
build_user_data(DataJObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, DataJObj),
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    UserId = wh_json:get_value(<<"User-ID">>, DataJObj),

    case couch_mgr:open_cache_doc(AccountDb, UserId) of
        {'ok', UserJObj} ->
            build_user_data(DataJObj, UserJObj);
        {'error', _E} ->
            lager:debug("failed to find user ~s in ~s: ~p", [UserId, AccountId, _E]),
            throw({'error', 'no_user_data'})
    end.

build_user_data(_DataJObj, UserJObj) ->
    UserKeys = [<<"first_name">>, <<"last_name">>, <<"email">>],
    [{Key, wh_json:get_value(Key, UserJObj)} || Key <- UserKeys].
