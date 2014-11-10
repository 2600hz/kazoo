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

-include("teletype.hrl").

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
                                           ])).
-define(TEMPLATE_TEXT, <<"Hi {{user.first_name}} {{user.last_name}}.\n\nThis is the skeleton template\n">>).
-define(TEMPLATE_HTML, <<"<p>Hi {{user.first_name}} {{user.last_name}}.</p><p>This is the skeleton template</p>\n">>).

-spec init() -> 'ok'.
init() ->
    wh_util:put_callid(?MODULE),
    teletype_util:init_template(?TEMPLATE_ID, ?TEMPLATE_MACROS, ?TEMPLATE_TEXT, ?TEMPLATE_HTML),

-spec handle(wh_json:object(), wh_proplist()) -> 'ok'.
handle(JObj, _Props) ->
    %% Gather data for template
    %% Load templates
    %% Populate templates
    %% Send email
    'ok'.
