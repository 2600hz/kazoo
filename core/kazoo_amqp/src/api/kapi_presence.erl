%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_presence).

-export([api_definitions/0, api_definition/1]).

-export([search_req/1
        ,search_req_v/1
        ,publish_search_req/1
        ]).
-export([search_partial_resp/1
        ,search_partial_resp_v/1
        ,publish_search_partial_resp/2
        ,publish_search_partial_resp/3
        ]).
-export([search_resp/1
        ,search_resp_v/1
        ,publish_search_resp/2
        ]).
-export([subscribe/1
        ,subscribe_v/1
        ,publish_subscribe/1
        ,publish_subscribe/2
        ]).
-export([dialog/1
        ,dialog_v/1
        ,publish_dialog/1
        ,publish_dialog/2
        ]).
-export([update/1
        ,update_v/1
        ,publish_update/1
        ,publish_update/2
        ]).
-export([probe/1
        ,probe_v/1
        ,publish_probe/1
        ,publish_probe/2
        ]).
-export([mwi_update/1
        ,mwi_update_v/1
        ,publish_mwi_update/1
        ,publish_mwi_update/2
        ]).
-export([mwi_unsolicited_update/1
        ,mwi_unsolicited_update_v/1
        ,publish_unsolicited_mwi_update/1
        ,publish_unsolicited_mwi_update/2
        ]).
-export([mwi_query/1
        ,mwi_query_v/1
        ,publish_mwi_query/1
        ,publish_mwi_query/2
        ]).
-export([sync/1
        ,sync_v/1
        ,publish_sync/1
        ,publish_sync/2
        ]).
-export([register_overwrite/1
        ,register_overwrite_v/1
        ,publish_register_overwrite/1
        ,publish_register_overwrite/2
        ]).
-export([flush/1
        ,flush_v/1
        ,publish_flush/1
        ,publish_flush/2
        ]).
-export([reset/1
        ,reset_v/1
        ,publish_reset/1
        ]).

-export([subscribe_routing_key/1]).
-export([presence_states/0]).

-export([bind_q/2
        ,unbind_q/2
        ]).

-export([declare_exchanges/0]).

-include_lib("kazoo_amqp/src/kz_amqp_util.hrl").

-define(DIALOG_STATES, [<<"confirmed">>
                       ,<<"early">>
                       ,<<"terminated">>
                       ]).

-ifdef(TEST).
-export([search_req_routing_key/1
        ,dialog_routing_key/2
        ,update_routing_key/2
        ,probe_routing_key/1
        ,mwi_update_routing_key/2
        ,mwi_unsolicited_update_routing_key/1
        ,mwi_query_routing_key/1
        ,register_overwrite_routing_key/1
        ,reset_routing_key/2
        ,get_value/2
        ]).
-endif.

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [search_req_definition()
    ,search_partial_resp_definition()
    ,search_resp_definition()
    ,subscribe_definition()
    ,dialog_definition()
    ,update_definition()
    ,probe_definition()
    ,mwi_update_definition()
    ,mwi_unsolicited_update_definition()
    ,mwi_query_definition()
    ,sync_definition()
    ,register_overwrite_definition()
    ,flush_definition()
    ,reset_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"search_req">>) ->
    search_req_definition();
api_definition(<<"search_partial_resp">>) ->
    search_partial_resp_definition();
api_definition(<<"search_resp">>) ->
    search_resp_definition();
api_definition(<<"subscribe">>) ->
    subscribe_definition();
api_definition(<<"dialog">>) ->
    dialog_definition();
api_definition(<<"update">>) ->
    update_definition();
api_definition(<<"probe">>) ->
    probe_definition();
api_definition(<<"mwi_update">>) ->
    mwi_update_definition();
api_definition(<<"mwi_unsolicited_update">>) ->
    mwi_unsolicited_update_definition();
api_definition(<<"mwi_query">>) ->
    mwi_query_definition();
api_definition(<<"sync">>) ->
    sync_definition();
api_definition(<<"register_overwrite">>) ->
    register_overwrite_definition();
api_definition(<<"flush">>) ->
    flush_definition();
api_definition(<<"reset">>) ->
    reset_definition().

-spec search_req_definition() -> kapi_definition:api().
search_req_definition() ->
    EventName = <<"search_req">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Search Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Search request for active subscriptions">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun search_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun search_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_search_req/1}
              ,{fun kapi_definition:set_binding/2, fun search_req_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Realm">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Event-Package">>
                                                            ,<<"Search-Type">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec search_partial_resp_definition() -> kapi_definition:api().
search_partial_resp_definition() ->
    EventName = <<"search_partial_resp">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Search Partial Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Search partial response for active subscriptions">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun search_partial_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun search_partial_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_search_partial_resp/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Subscriptions">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec search_resp_definition() -> kapi_definition:api().
search_resp_definition() ->
    EventName = <<"search_resp">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Search response for active subscriptions">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun search_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun search_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_search_resp/2}
              ,{fun kapi_definition:set_required_headers/2, []}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Subscriptions">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec subscribe_definition() -> kapi_definition:api().
subscribe_definition() ->
    EventName = <<"subscription">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Subscription">>}
              ,{fun kapi_definition:set_description/2, <<"Presence subscription from Kamailio">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun subscribe/1}
              ,{fun kapi_definition:set_validate_fun/2, fun subscribe_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_subscribe/1}
              ,{fun kapi_definition:set_binding/2, fun subscribe_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"User">>
                                                            ,<<"Expires">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Contact">>
                                                            ,<<"Event-Package">>
                                                            ,<<"From">>
                                                            ,<<"From-Tag">>
                                                            ,<<"Queue">>
                                                            ,<<"To-Tag">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Expires">>, fun(V) -> is_integer(kz_term:to_integer(V)) end}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec dialog_definition() -> kapi_definition:api().
dialog_definition() ->
    EventName = <<"dialog_update">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Dialog Update">>}
              ,{fun kapi_definition:set_description/2, <<"Dialog state updates">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun dialog/1}
              ,{fun kapi_definition:set_validate_fun/2, fun dialog_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_dialog/1}
              ,{fun kapi_definition:set_binding/2, fun dialog_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"From">>
                                                            ,<<"To">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-Cookie">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Direction">>
                                                            ,<<"Event-Package">>
                                                            ,<<"Expires">>
                                                            ,<<"Flush-Level">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-Tag">>
                                                            ,<<"From-URI">>
                                                            ,<<"From-User">>
                                                            ,<<"Presence-ID">>
                                                            ,<<"Presentity">>
                                                            ,<<"Presentity-Realm">>
                                                            ,<<"Presentity-User">>
                                                            ,<<"State">>
                                                            ,<<"Switch-URI">>
                                                            ,<<"Target-Call-ID">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-Tag">>
                                                            ,<<"To-URI">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"State">>, ?DIALOG_STATES}
                | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec update_definition() -> kapi_definition:api().
update_definition() ->
    EventName = <<"update">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Update">>}
              ,{fun kapi_definition:set_description/2, <<"Presence state updates">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun update/1}
              ,{fun kapi_definition:set_validate_fun/2, fun update_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_update/1}
              ,{fun kapi_definition:set_binding/2, fun update_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Presence-ID">>
                                                            ,<<"State">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-Direction">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Event-Package">>
                                                            ,<<"From">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-Tag">>
                                                            ,<<"From-User">>
                                                            ,<<"Presence-ID">>
                                                            ,<<"Switch-URI">>
                                                            ,<<"Target-Call-ID">>
                                                            ,<<"To">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-Tag">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"State">>, presence_states()}
                | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec probe_definition() -> kapi_definition:api().
probe_definition() ->
    EventName = <<"probe">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Presence Probe">>}
              ,{fun kapi_definition:set_description/2, <<"Presence Probe">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun probe/1}
              ,{fun kapi_definition:set_validate_fun/2, fun probe_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_probe/1}
              ,{fun kapi_definition:set_binding/2, fun probe_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Event-Package">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Expires">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec mwi_update_definition() -> kapi_definition:api().
mwi_update_definition() ->
    EventName = <<"mwi_update">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"MWI Update">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Update the Message Waiting Indicator on a device">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun mwi_update/1}
              ,{fun kapi_definition:set_validate_fun/2, fun mwi_update_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_mwi_update/1}
              ,{fun kapi_definition:set_binding/2, fun mwi_update_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Messages-New">>
                                                            ,<<"Messages-Saved">>
                                                            ,<<"To">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Expires">>
                                                            ,<<"Extended-Presence-ID">>
                                                            ,<<"From">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"Message-Account">>
                                                            ,<<"Messages-Urgent">>
                                                            ,<<"Messages-Urgent-Saved">>
                                                            ,<<"Messages-Waiting">>
                                                            ,<<"Presence-ID">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2
               ,[{<<"Messages-New">>, fun is_integer/1}
                ,{<<"Messages-Saved">>, fun is_integer/1}
                ,{<<"Messages-Urgent">>, fun is_integer/1}
                ,{<<"Messages-Urgent-Saved">>, fun is_integer/1}
                ]
               }
              ],
    kapi_definition:setters(Setters).

-spec mwi_unsolicited_update_definition() -> kapi_definition:api().
mwi_unsolicited_update_definition() ->
    EventName = <<"mwi_unsolicited_update">>,
    Category = <<"presence">>,
    MWIUpdate = mwi_update_definition(),
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"MWI Unsolicited Update">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Update the Message Waiting Indicator on a device">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun mwi_unsolicited_update/1}
              ,{fun kapi_definition:set_validate_fun/2, fun mwi_unsolicited_update_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_unsolicited_mwi_update/1}
              ,{fun kapi_definition:set_binding/2, fun mwi_unsolicited_update_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2
               ,kapi_definition:required_headers(MWIUpdate)
               }
              ,{fun kapi_definition:set_optional_headers/2
               ,kapi_definition:optional_headers(MWIUpdate)
               }
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, kapi_definition:types(MWIUpdate)}
              ],
    kapi_definition:setters(Setters).

-spec mwi_query_definition() -> kapi_definition:api().
mwi_query_definition() ->
    EventName = <<"mwi_query">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"MWI Query">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Query the Message Waiting Indicator on a device">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun mwi_query/1}
              ,{fun kapi_definition:set_validate_fun/2, fun mwi_query_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_mwi_query/1}
              ,{fun kapi_definition:set_binding/2, fun mwi_query_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec sync_definition() -> kapi_definition:api().
sync_definition() ->
    EventName = <<"sync">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Sync presence">>}
              ,{fun kapi_definition:set_description/2, <<"Sync presence">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sync/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sync_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_sync/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Action">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Event-Package">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,[{<<"Action">>, [<<"End">>, <<"Request">>, <<"Start">>]}
                | kapi_definition:event_type_headers(Category, EventName)
                ]
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec register_overwrite_definition() -> kapi_definition:api().
register_overwrite_definition() ->
    EventName = <<"register_overwrite">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Register Overwrite">>}
              ,{fun kapi_definition:set_description/2, <<"Unregister is a keyword">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun register_overwrite/1}
              ,{fun kapi_definition:set_validate_fun/2, fun register_overwrite_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_register_overwrite/1}
              ,{fun kapi_definition:set_binding/2, fun register_overwrite_routing_key/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Contact">>
                                                            ,<<"Previous-Contact">>
                                                            ,<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec flush_definition() -> kapi_definition:api().
flush_definition() ->
    EventName = <<"flush">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Flush presence">>}
              ,{fun kapi_definition:set_description/2, <<"Flush presence dialog cache">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun flush/1}
              ,{fun kapi_definition:set_validate_fun/2, fun flush_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_flush/1}
              ,{fun kapi_definition:set_required_headers/2, [<<"Type">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Event-Package">>
                                                            ,<<"User">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec reset_definition() -> kapi_definition:api().
reset_definition() ->
    EventName = <<"reset">>,
    Category = <<"presence">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Reset presence">>}
              ,{fun kapi_definition:set_description/2, <<"Reset presence dialog cache entry">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun reset/1}
              ,{fun kapi_definition:set_validate_fun/2, fun reset_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_reset/1}
              ,{fun kapi_definition:set_binding/2, fun reset_routing_key/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Realm">>
                                                            ,<<"Username">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Event-Package">>
                                                            ]}
              ,{fun kapi_definition:set_values/2
               ,kapi_definition:event_type_headers(Category, EventName)
               }
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Search request for active subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec search_req(kz_term:api_terms()) -> kz_api:api_formatter_return().
search_req(Req) ->
    kapi_definition:build_message(Req, search_req_definition()).

-spec search_req_v(kz_term:api_terms()) -> boolean().
search_req_v(Req) ->
    kapi_definition:validate(Req, search_req_definition()).

-spec publish_search_req(kz_term:api_terms()) -> 'ok'.
publish_search_req(JObj) ->
    publish_search_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_req(kz_term:api_terms(), binary()) -> 'ok'.
publish_search_req(Req, ContentType) ->
    Definition = search_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:presence_publish((kapi_definition:binding(Definition))(get_value(<<"Realm">>, Req))
                                 ,Payload
                                 ,ContentType
                                 ).

-spec search_req_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
search_req_routing_key(Realm) ->
    list_to_binary([<<"presence.search_req.">>, kz_amqp_util:encode(Realm)]).

%%------------------------------------------------------------------------------
%% @doc Search partial response for active subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec search_partial_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
search_partial_resp(Req) ->
    kapi_definition:build_message(Req, search_partial_resp_definition()).

-spec search_partial_resp_v(kz_term:api_terms()) -> boolean().
search_partial_resp_v(Req) ->
    kapi_definition:validate(Req, search_partial_resp_definition()).

-spec publish_search_partial_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_search_partial_resp(Queue, JObj) ->
    publish_search_partial_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_partial_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_search_partial_resp(Queue, Resp, ContentType) ->
    Definition = search_partial_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Search response for active subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec search_resp(kz_term:api_terms()) -> kz_api:api_formatter_return().
search_resp(Req) ->
    kapi_definition:build_message(Req, search_resp_definition()).

-spec search_resp_v(kz_term:api_terms()) -> boolean().
search_resp_v(Req) ->
    kapi_definition:validate(Req, search_resp_definition()).

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_search_resp(Queue, JObj) ->
    publish_search_resp(Queue, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_search_resp(kz_term:ne_binary(), kz_term:api_terms(), binary()) -> 'ok'.
publish_search_resp(Queue, Resp, ContentType) ->
    Definition = search_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Resp
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:targeted_publish(Queue, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Subscribing for updates - Presence subscription from Kamailio.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec subscribe(kz_term:api_terms()) -> kz_api:api_formatter_return().
subscribe(Req) ->
    kapi_definition:build_message(Req, subscribe_definition()).

-spec subscribe_v(kz_term:api_terms()) -> boolean().
subscribe_v(Req) ->
    kapi_definition:validate(Req, subscribe_definition()).

-spec publish_subscribe(kz_term:api_terms()) -> 'ok'.
publish_subscribe(JObj) ->
    publish_subscribe(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_subscribe(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_subscribe(Req, ContentType) ->
    Definition = subscribe_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:presence_publish((kapi_definition:binding(Definition))(get_value(<<"User">>, Req))
                                 ,Payload
                                 ,ContentType
                                 ).

-spec subscribe_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
subscribe_routing_key(User) ->
    <<"subscriptions.", (kz_amqp_util:encode(realm_from_presence_id(User)))/binary>>.

%%------------------------------------------------------------------------------
%% @doc Dialog state updates
%% @end
%%------------------------------------------------------------------------------
-spec dialog(kz_term:api_terms()) -> kz_api:api_formatter_return().
dialog(Req) ->
    kapi_definition:build_message(Req, dialog_definition()).

-spec dialog_v(kz_term:api_terms()) -> boolean().
dialog_v(Req) ->
    kapi_definition:validate(Req, dialog_definition()).

-spec publish_dialog(kz_term:api_terms()) -> 'ok'.
publish_dialog(JObj) ->
    publish_dialog(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_dialog(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_dialog(Req, ContentType) ->
    Definition = dialog_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Call-ID">>, Req)
                                                      ,get_value(<<"Presence-ID">>, Req)
                                                      ),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType).

-spec dialog_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
dialog_routing_key(CallId, PresenceID) ->
    list_to_binary([<<"dialog.">>
                   ,kz_amqp_util:encode(realm_from_presence_id(PresenceID))
                   ,"."
                   ,kz_amqp_util:encode(CallId)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update(kz_term:api_terms()) -> kz_api:api_formatter_return().
update(Req) ->
    kapi_definition:build_message(Req, update_definition()).

-spec update_v(kz_term:api_terms()) -> boolean().
update_v(Req) ->
    kapi_definition:validate(Req, update_definition()).

-spec publish_update(kz_term:api_terms()) -> 'ok'.
publish_update(JObj) ->
    publish_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update(Req, ContentType) ->
    Definition = update_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Call-ID">>, Req)
                                                      ,get_value(<<"Presence-ID">>, Req)
                                                      ),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType).

-spec update_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
update_routing_key(CallId, PresenceID) ->
    list_to_binary([<<"update.">>
                   ,kz_amqp_util:encode(realm_from_presence_id(PresenceID))
                   ,"."
                   ,kz_amqp_util:encode(CallId)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec probe(kz_term:api_terms()) -> kz_api:api_formatter_return().
probe(Req) ->
    kapi_definition:build_message(Req, probe_definition()).

-spec probe_v(kz_term:api_terms()) -> boolean().
probe_v(Req) ->
    kapi_definition:validate(Req, probe_definition()).

-spec publish_probe(kz_term:api_terms()) -> 'ok'.
publish_probe(JObj) ->
    publish_probe(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_probe(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_probe(Req, ContentType) ->
    Definition = probe_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Event-Package">>, Req)),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType).

-spec probe_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
probe_routing_key(SubscriptionType) ->
    <<"probes.", SubscriptionType/binary>>.

%%------------------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mwi_extended_update(kz_term:proplist()) -> kz_term:proplist().
mwi_extended_update(Prop) ->
    MessagesNew = props:get_integer_value(<<"Messages-New">>, Prop, 0),
    MessagesWaiting = case MessagesNew of 0 -> <<"no">>; _ -> <<"yes">> end,
    To = props:get_value(<<"To">>, Prop),
    [ToUsername, ToRealm] = binary:split(To, <<"@">>),
    CallId = ?FAKE_CALLID(To),
    props:delete_keys([<<"Call-ID">>, <<"To">>], Prop)
        ++ [{<<"From">>, <<"sip:", To/binary>>}
           ,{<<"From-User">>, ToUsername}
           ,{<<"From-Realm">>, ToRealm}
           ,{<<"To">>, <<"sip:", To/binary>>}
           ,{<<"To-User">>, ToUsername}
           ,{<<"To-Realm">>, ToRealm}
           ,{<<"Message-Account">>, <<"sip:", To/binary>>}
           ,{<<"Messages-Waiting">>, MessagesWaiting}
           ,{<<"Messages-New">>, MessagesNew}
           ,{<<"Messages-Saved">>, 0}
           ,{<<"Messages-Urgent">>, 0}
           ,{<<"Messages-Urgent-Saved">>, 0}
           ,{<<"Presence-ID">>, To}
           ,{<<"Call-ID">>, CallId}
           ].

-spec mwi_update(kz_term:api_terms()) -> kz_api:api_formatter_return().
mwi_update(Req) ->
    kapi_definition:build_message(mwi_extended_update(Req), mwi_update_definition()).

-spec mwi_update_v(kz_term:api_terms()) -> boolean().
mwi_update_v(Req) ->
    kapi_definition:validate(Req, mwi_update_definition()).

-spec publish_mwi_update(kz_term:api_terms()) -> 'ok'.
publish_mwi_update(JObj) ->
    publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_mwi_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_mwi_update(Req, ContentType) ->
    Definition = mwi_update_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    [User, Realm] = binary:split(get_value(<<"To">>, Req), <<"@">>),
    kz_amqp_util:presence_publish((kapi_definition:binding(Definition))(User, Realm)
                                 ,Payload
                                 ,ContentType
                                 ).

-spec mwi_update_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
mwi_update_routing_key(User, Realm) ->
    list_to_binary([<<"mwi_updates.">>
                   ,kz_amqp_util:encode(Realm)
                   ,"."
                   ,kz_amqp_util:encode(User)
                   ]).

%%------------------------------------------------------------------------------
%% @doc MWI Unsolicited Update - Update the Message Waiting Indicator on a device.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mwi_unsolicited_update(kz_term:api_terms()) -> kz_api:api_formatter_return().
mwi_unsolicited_update(Req) ->
    kapi_definition:build_message(Req, mwi_unsolicited_update_definition()).

-spec mwi_unsolicited_update_v(kz_term:api_terms()) -> boolean().
mwi_unsolicited_update_v(Req) ->
    kapi_definition:validate(Req, mwi_unsolicited_update_definition()).

-spec publish_unsolicited_mwi_update(kz_term:api_terms()) -> 'ok'.
publish_unsolicited_mwi_update(JObj) ->
    publish_unsolicited_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_unsolicited_mwi_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_unsolicited_mwi_update(Req, ContentType) ->
    Definition = mwi_unsolicited_update_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:presence_publish((kapi_definition:binding(Definition))(get_value(<<"To">>, Req))
                                 ,Payload
                                 ,ContentType
                                 ).

-spec mwi_unsolicited_update_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
mwi_unsolicited_update_routing_key(To) ->
    <<"mwi_unsolicited_updates.", (kz_amqp_util:encode(realm_from_presence_id(To)))/binary>>.

%%------------------------------------------------------------------------------
%% @doc MWI - Query the Message Waiting Indicator on a device.
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec mwi_query(kz_term:api_terms()) -> kz_api:api_formatter_return().
mwi_query(Req) ->
    kapi_definition:build_message(Req, mwi_query_definition()).

-spec mwi_query_v(kz_term:api_terms()) -> boolean().
mwi_query_v(Req) ->
    kapi_definition:validate(Req, mwi_query_definition()).

-spec publish_mwi_query(kz_term:api_terms()) -> 'ok'.
publish_mwi_query(JObj) ->
    publish_mwi_query(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_mwi_query(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_mwi_query(Req, ContentType) ->
    Definition = mwi_query_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Realm">>, Req)),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType).

-spec mwi_query_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
mwi_query_routing_key(Realm)  ->
    <<"mwi_queries.", (kz_amqp_util:encode(Realm))/binary>>.

%%------------------------------------------------------------------------------
%% @doc Sync Presence.
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:api_terms()) -> kz_api:api_formatter_return().
sync(Req) ->
    kapi_definition:build_message(Req, sync_definition()).

-spec sync_v(kz_term:api_terms()) -> boolean().
sync_v(Req) ->
    kapi_definition:validate(Req, sync_definition()).

-spec publish_sync(kz_term:api_terms()) -> 'ok'.
publish_sync(JObj) ->
    publish_sync(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sync(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sync(Req, ContentType) ->
    Definition = sync_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:presence_publish(<<"sync">>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Register_Overwrite (unregister is a key word).
%% Takes proplist, creates JSON string or error.
%% @end
%%------------------------------------------------------------------------------
-spec register_overwrite(kz_term:api_terms()) -> kz_api:api_formatter_return().
register_overwrite(Req) ->
    kapi_definition:build_message(Req, register_overwrite_definition()).

-spec register_overwrite_v(kz_term:api_terms()) -> boolean().
register_overwrite_v(Req) ->
    kapi_definition:validate(Req, register_overwrite_definition()).

-spec publish_register_overwrite(kz_term:api_terms()) -> 'ok'.
publish_register_overwrite(JObj) ->
    publish_register_overwrite(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_register_overwrite(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register_overwrite(Req, ContentType) when is_list(Req) ->
    Definition = register_overwrite_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Realm">>, Req)),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType);
publish_register_overwrite(JObj, ContentType) ->
    publish_register_overwrite(kz_json:to_proplist(JObj), ContentType).

-spec register_overwrite_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
register_overwrite_routing_key(Realm) ->
    <<"register_overwrites.", (kz_amqp_util:encode(Realm))/binary>>.

%%------------------------------------------------------------------------------
%% @doc Flush presence dialog cache.
%% @end
%%------------------------------------------------------------------------------
-spec flush(kz_term:api_terms()) -> kz_api:api_formatter_return().
flush(Req) ->
    kapi_definition:build_message(Req, flush_definition()).

-spec flush_v(kz_term:api_terms()) -> boolean().
flush_v(Req) ->
    kapi_definition:validate(Req, flush_definition()).

-spec publish_flush(kz_term:api_terms()) -> 'ok'.
publish_flush(JObj) ->
    publish_flush(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_flush(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_flush(Req, ContentType) ->
    Definition = flush_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    kz_amqp_util:presence_publish(<<"flush">>, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Reset presence dialog cache entry.
%% @end
%%------------------------------------------------------------------------------
-spec reset(kz_term:api_terms()) -> kz_api:api_formatter_return().
reset(Req) ->
    kapi_definition:build_message(Req, reset_definition()).

-spec reset_v(kz_term:api_terms()) -> boolean().
reset_v(Req) ->
    kapi_definition:validate(Req, reset_definition()).

-spec publish_reset(kz_term:api_terms()) -> 'ok'.
publish_reset(JObj) ->
    publish_reset(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_reset(kz_term:api_terms(), binary()) -> 'ok'.
publish_reset(Req, ContentType) ->
    Definition = reset_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req
                                                ,kapi_definition:values(Definition)
                                                ,kapi_definition:build_fun(Definition)
                                                ),
    RoutingKey = (kapi_definition:binding(Definition))(get_value(<<"Realm">>, Req)
                                                      ,get_value(<<"Username">>, Req)
                                                      ),
    kz_amqp_util:presence_publish(RoutingKey, Payload, ContentType).

-spec reset_routing_key(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
reset_routing_key(Realm, Username) ->
    list_to_binary([<<"presence.reset.">>
                   ,kz_amqp_util:encode(Realm)
                   ,"."
                   ,kz_amqp_util:encode(Username)
                   ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    bind_q(Queue, RestrictTo, Props).

-spec bind_q(kz_term:ne_binary(), kz_term:api_binaries(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, 'undefined', _) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"#">>);
bind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(search_req_definition()))(Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['sync'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"sync">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(subscribe_definition()))(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['update'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(update_definition()))(CallId, PresenceId),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['dialog'|Restrict], Props) ->
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    CallId = props:get_value('call', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(dialog_definition()))(CallId, PresenceId),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(probe_definition()))(ProbeType),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_update_definition()))(User, Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_unsolicited_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_unsolicited_update_definition()))(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_query_definition()))(User),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(register_overwrite_definition()))(Realm),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['flush'|Restrict], Props) ->
    kz_amqp_util:bind_q_to_presence(Queue, <<"flush">>),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(reset_definition()))(Realm, Username),
    kz_amqp_util:bind_q_to_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
bind_q(Queue, [_|Restrict], Props) ->
    bind_q(Queue, Restrict, Props);
bind_q(_, [], _) -> 'ok'.

-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    RestrictTo = props:get_value('restrict_to', Props),
    unbind_q(Queue, RestrictTo, Props).

-spec unbind_q(kz_term:ne_binary(), kz_term:api_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, 'undefined', _) ->
    kz_amqp_util:unbind_q_from_presence(Queue, <<"#">>);
unbind_q(Queue, ['search_req'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(search_req_definition()))(Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['sync'|Restrict], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, <<"sync">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['subscribe'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(subscribe_definition()))(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['update'|Restrict], Props) ->
    CallId = props:get_value('call', Props, <<"*">>),
    PresenceId = props:get_value('presence-id', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(update_definition()))(CallId, PresenceId),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['dialog'|Restrict], Props) ->
    CallId = props:get_value('call', Props, <<"*">>),
    PresenceId = props:get_value('presence-id', Props, <<"*@*">>),
    RoutingKey = (kapi_definition:binding(dialog_definition()))(CallId, PresenceId),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['probe'|Restrict], Props) ->
    ProbeType = props:get_value('probe_type', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(probe_definition()))(ProbeType),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_update_definition()))(User, Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_unsolicited_update'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_unsolicited_update_definition()))(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    bind_q(Queue, Restrict, Props);
unbind_q(Queue, ['mwi_query'|Restrict], Props) ->
    User = props:get_value('user', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(mwi_query_definition()))(User),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['register_overwrite'|Restrict], Props) ->
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(register_overwrite_definition()))(Realm),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['flush'|Restrict], Props) ->
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, <<"flush">>),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, ['reset'|Restrict], Props) ->
    Username = props:get_value('username', Props, <<"*">>),
    Realm = props:get_value('realm', Props, <<"*">>),
    RoutingKey = (kapi_definition:binding(reset_definition()))(Realm, Username),
    'ok' = kz_amqp_util:unbind_q_from_presence(Queue, RoutingKey),
    unbind_q(Queue, Restrict, Props);
unbind_q(Queue, [_|Restrict], Props) ->
    unbind_q(Queue, Restrict, Props);
unbind_q(_, [], _) -> 'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec presence_states() -> kz_term:ne_binaries().
presence_states() ->
    [<<"offline">>
    ,<<"online">>
    ,<<"trying">>
    | ?DIALOG_STATES
    ].

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:presence_exchange().

%%------------------------------------------------------------------------------
%% @doc Helpers
%% @end
%%------------------------------------------------------------------------------
-spec realm_from_presence_id(kz_term:ne_binary()) -> kz_term:ne_binary().
realm_from_presence_id(PresenceID) ->
    case binary:split(PresenceID, <<"@">>) of
        [_To, Realm] -> Realm;
        [Realm] -> Realm
    end.

-spec get_value(kz_term:ne_binary(), kz_term:api_terms()) -> kz_term:ne_binary().
get_value(Key, Req) when is_list(Req) ->
    props:get_value(Key, Req);
get_value(Key, Req) ->
    kz_json:get_value(Key, Req).
