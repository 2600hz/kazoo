%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_bookkeepers).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([api_definitions/0, api_definition/1]).

-export([collect_recurring_req/1, collect_recurring_req_v/1
        ,publish_collect_recurring_req/1, publish_collect_recurring_req/2
        ]).

-export([collect_recurring_resp/1, collect_recurring_resp_v/1
        ,publish_collect_recurring_resp/2, publish_collect_recurring_resp/3
        ]).

-export([sale_req/1, sale_req_v/1
        ,publish_sale_req/1, publish_sale_req/2
        ]).
-export([sale_resp/1, sale_resp_v/1
        ,publish_sale_resp/2, publish_sale_resp/3
        ]).
-export([refund_req/1, refund_req_v/1
        ,publish_refund_req/1, publish_refund_req/2
        ]).
-export([refund_resp/1, refund_resp_v/1
        ,publish_refund_resp/2, publish_refund_resp/3
        ]).
-export([update_req/1, update_req_v/1
        ,publish_update_req/1, publish_update_req/2
        ]).
-export([update_resp/1, update_resp_v/1
        ,publish_update_resp/2, publish_update_resp/3
        ]).
-export([standing_req/1, standing_req_v/1
        ,publish_standing_req/1, publish_standing_req/2
        ]).
-export([standing_resp/1, standing_resp_v/1
        ,publish_standing_resp/2, publish_standing_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(API_CATEGORY, <<"bookkeepers">>).
-define(BINDING_STRING(Category, Name)
       ,<<(?API_CATEGORY)/binary, ".", (Category)/binary, ".", (Name)/binary>>
       ).

%%%=============================================================================
%%% Internal Bookkeeper Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collect_recurring_req_definition() -> kapi_definition:api().
collect_recurring_req_definition() ->
    EventName = <<"collect_recurring_req">>,
    Category = <<"collect_recurring">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Collect Recurring Charges Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Will trigger the appropriate bookkeeper to collect recurring charges">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun collect_recurring_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun collect_recurring_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun collect_recurring_req/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'collect_recurring'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Bookkeeper-ID">>
                                                            ,<<"Bookkeeper-Type">>
                                                            ,<<"Due-Timestamp">>
                                                            ,<<"Vendor-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Audit-Log">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec collect_recurring_resp_definition() -> kapi_definition:api().
collect_recurring_resp_definition() ->
    EventName = <<"collect_recurring_resp">>,
    Category = <<"collect_recurring">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Collect Recurring Charges Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Result of request to collect recurring charges via the bookkeeper">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun collect_recurring_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun collect_recurring_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_collect_recurring_resp/2}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"response">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'collect_recurring'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Bookkeeper-ID">>
                                                            ,<<"Bookkeeper-Type">>
                                                            ,<<"Status">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Details">>
                                                            ,<<"Message">>
                                                            ,<<"Reason">>
                                                            ,<<"Transaction-ID">>
                                                            ,<<"Transaction-DB">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sale_req_definition() -> kapi_definition:api().
sale_req_definition() ->
    EventName = <<"sale_req">>,
    Category = <<"sale">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Sale Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Will trigger the appropriate bookkeeper to issue a sale">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sale_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sale_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_sale_req/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'sale'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Bookkeeper-Type">>
                                                            ,<<"Vendor-ID">>
                                                            ,<<"Account-ID">>
                                                            ,<<"Transaction-ID">>
                                                            ,<<"Transaction-DB">>
                                                            ,<<"Amount">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec sale_resp_definition() -> kapi_definition:api().
sale_resp_definition() ->
    EventName = <<"sale_resp">>,
    Category = <<"sale">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Sale Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Result of the attempt to issue a sale via the bookkeeper">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun sale_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun sale_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_sale_resp/2}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"response">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'sale'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>
                                                            ,<<"Transaction-ID">>
                                                            ,<<"Transaction-DB">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Message">>
                                                            ,<<"Reason">>
                                                            ,<<"Details">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refund_req_definition() -> kapi_definition:api().
refund_req_definition() ->
    EventName = <<"refund_req">>,
    Category = <<"refund">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Refund Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Will trigger the appropriate bookkeeper to issue a refund">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun refund_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun refund_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_refund_req/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'refund'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Bookkeeper-Type">>
                                                            ,<<"Vendor-ID">>
                                                            ,<<"Account-ID">>
                                                            ,<<"Transaction-ID">>
                                                            ,<<"Transaction-DB">>
                                                            ,<<"Amount">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec refund_resp_definition() -> kapi_definition:api().
refund_resp_definition() ->
    EventName = <<"refund_resp">>,
    Category = <<"refund">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Refund Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"Result of the attempt to refund via the bookkeeper">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun refund_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun refund_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_refund_resp/2}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"response">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'refund'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>
                                                            ,<<"Transaction-ID">>
                                                            ,<<"Transaction-DB">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Message">>
                                                            ,<<"Reason">>
                                                            ,<<"Details">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_req_definition() -> kapi_definition:api().
update_req_definition() ->
    EventName = <<"update_req">>,
    Category = <<"update">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Subscription Update Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"A request to a bookkeeper to update or create a subscription">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun update_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun update_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_update_req/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'update'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Bookkeeper-ID">>
                                                            ,<<"Bookkeeper-Type">>
                                                            ,<<"Invoice">>
                                                            ,<<"Vendor-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Dry-Run">>
                                                            ,<<"Audit-Log">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec update_resp_definition() -> kapi_definition:api().
update_resp_definition() ->
    EventName = <<"update_resp">>,
    Category = <<"update">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Subscription Update Response">>}
              ,{fun kapi_definition:set_description/2
               ,<<"The result of a subscription update request">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun update_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun update_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_update_resp/2}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"response">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'update'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Message">>
                                                            ,<<"Reason">>
                                                            ,<<"Details">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec standing_req_definition() -> kapi_definition:api().
standing_req_definition() ->
    EventName = <<"standing_req">>,
    Category = <<"standing">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Currnet Status (Standing) Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"A request to a bookkeeper to get the current standing of an account">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun standing_req/1}
              ,{fun kapi_definition:set_validate_fun/2, fun standing_req_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_standing_req/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'standing'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Bookkeeper-ID">>
                                                            ,<<"Bookkeeper-Type">>
                                                            ,<<"Vendor-ID">>
                                                            ,<<"Items">>
                                                            ,<<"Estimated-Withdrawal">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, []}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

-spec standing_resp_definition() -> kapi_definition:api().
standing_resp_definition() ->
    EventName = <<"standing_resp">>,
    Category = <<"standing">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Current Status (Standing) Response">>}
              ,{fun kapi_definition:set_description/2, <<"The result of a standing request">>}
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun standing_resp/1}
              ,{fun kapi_definition:set_validate_fun/2, fun standing_resp_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_standing_resp/2}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"response">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'standing'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Message">>
                                                            ,<<"Reason">>
                                                            ,<<"Details">>
                                                            ]}
              ,{fun kapi_definition:set_values/2, kapi_definition:event_type_headers(?API_CATEGORY, EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [collect_recurring_req_definition()
    ,collect_recurring_resp_definition()
    ,sale_req_definition()
    ,sale_resp_definition()
    ,refund_req_definition()
    ,refund_resp_definition()
    ,update_req_definition()
    ,update_resp_definition()
    ,standing_req_definition()
    ,standing_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(atom() | kz_term:text() | kz_term:ne_binary()) -> kapi_definition:api().
api_definition(Name) when is_atom(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(Name) when is_list(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"collect_recurring_req">>) ->
    collect_recurring_req_definition();
api_definition(<<"collect_recurring_resp">>) ->
    collect_recurring_resp_definition();
api_definition(<<"sale_req">>) ->
    sale_req_definition();
api_definition(<<"sale_resp">>) ->
    sale_resp_definition();
api_definition(<<"refund_req">>) ->
    refund_req_definition();
api_definition(<<"refund_resp">>) ->
    refund_resp_definition();
api_definition(<<"update_req">>) ->
    update_req_definition();
api_definition(<<"update_resp">>) ->
    update_resp_definition();
api_definition(<<"standing_req">>) ->
    standing_req_definition();
api_definition(<<"standing_resp">>) ->
    standing_resp_definition().

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

-spec bind_to_q(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_bookkeepers(Q, <<"bookkeepers.*.*">>);
bind_to_q(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:bind_q_to_bookkeepers(Q, Binding),
            bind_to_q(Q, T);
        _Else ->
            bind_to_q(Q, T)
    catch
        error:undef ->
            bind_to_q(Q, T)
    end;
bind_to_q(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Unbind from a queue of this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props)).

-spec unbind_q_from(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_bookkeepers(Q, <<"bookkeepers.*.*">>);
unbind_q_from(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:unbind_q_from_bookkeepers(Q, Binding),
            unbind_q_from(Q, T);
        _Else -> unbind_q_from(Q, T)
    catch
        error:undef ->
            unbind_q_from(Q, T)
    end;
unbind_q_from(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:bookkeepers_exchange().

%%%=============================================================================
%%% Helpers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generic function to build API payload.
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), kapi_definition:api()) -> api_formatter_return().
build_message(Prop, Definition) when is_list(Prop) ->
    ReqH = kapi_definition:required_headers(Definition),
    OptH = kapi_definition:optional_headers(Definition),
    Validate = kapi_definition:validate_fun(Definition),
    Binding = kapi_definition:binding(Definition),
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(Binding)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, Definition) when is_list(Prop) ->
    ReqH = kapi_definition:required_headers(Definition),
    Values = kapi_definition:values(Definition),
    Types = kapi_definition:types(Definition),
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%%=============================================================================
%%% Internal Bookkeepers Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Collect Recurring.
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec collect_recurring_req(kz_term:api_terms()) -> api_formatter_return().
collect_recurring_req(Prop) ->
    build_message(Prop, collect_recurring_req_definition()).

-spec collect_recurring_req_v(kz_term:api_terms()) -> boolean().
collect_recurring_req_v(Prop) ->
    validate(Prop, collect_recurring_req_definition()).

-spec publish_collect_recurring_req(kz_term:api_terms()) -> 'ok'.
publish_collect_recurring_req(JObj) ->
    publish_collect_recurring_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_collect_recurring_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_collect_recurring_req(API, ContentType) ->
    Definition = collect_recurring_req_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun collect_recurring_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec collect_recurring_resp(kz_term:api_terms()) -> api_formatter_return().
collect_recurring_resp(Prop) ->
    build_message(Prop, collect_recurring_resp_definition()).

-spec collect_recurring_resp_v(kz_term:api_terms()) -> boolean().
collect_recurring_resp_v(Prop) ->
    validate(Prop, collect_recurring_resp_definition()).

-spec publish_collect_recurring_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_collect_recurring_resp(RespQ, JObj) ->
    publish_collect_recurring_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_collect_recurring_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_collect_recurring_resp(RespQ, API, ContentType) ->
    Values = kapi_definition:values(collect_recurring_resp_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun collect_recurring_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Sale
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec sale_req(kz_term:api_terms()) -> api_formatter_return().
sale_req(Prop) ->
    build_message(Prop, sale_req_definition()).

-spec sale_req_v(kz_term:api_terms()) -> boolean().
sale_req_v(Prop) ->
    validate(Prop, sale_req_definition()).

-spec publish_sale_req(kz_term:api_terms()) -> 'ok'.
publish_sale_req(JObj) ->
    publish_sale_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sale_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sale_req(API, ContentType) ->
    Definition = sale_req_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun sale_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec sale_resp(kz_term:api_terms()) -> api_formatter_return().
sale_resp(Prop) ->
    build_message(Prop, sale_resp_definition()).

-spec sale_resp_v(kz_term:api_terms()) -> boolean().
sale_resp_v(Prop) ->
    validate(Prop, sale_resp_definition()).

-spec publish_sale_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_sale_resp(RespQ, JObj) ->
    publish_sale_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_sale_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_sale_resp(RespQ, API, ContentType) ->
    Values = kapi_definition:values(sale_resp_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun sale_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Refund
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec refund_req(kz_term:api_terms()) -> api_formatter_return().
refund_req(Prop) ->
    build_message(Prop, refund_req_definition()).

-spec refund_req_v(kz_term:api_terms()) -> boolean().
refund_req_v(Prop) ->
    validate(Prop, refund_req_definition()).

-spec publish_refund_req(kz_term:api_terms()) -> 'ok'.
publish_refund_req(JObj) ->
    publish_refund_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_refund_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_refund_req(API, ContentType) ->
    Definition = refund_req_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun refund_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec refund_resp(kz_term:api_terms()) -> api_formatter_return().
refund_resp(Prop) ->
    build_message(Prop, refund_resp_definition()).

-spec refund_resp_v(kz_term:api_terms()) -> boolean().
refund_resp_v(Prop) ->
    validate(Prop, refund_resp_definition()).

-spec publish_refund_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_refund_resp(RespQ, JObj) ->
    publish_refund_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_refund_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_refund_resp(RespQ, API, ContentType) ->
    Values = kapi_definition:values(refund_resp_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun refund_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Subscription Update
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec update_req(kz_term:api_terms()) -> api_formatter_return().
update_req(Prop) ->
    build_message(Prop, update_req_definition()).

-spec update_req_v(kz_term:api_terms()) -> boolean().
update_req_v(Prop) ->
    validate(Prop, update_req_definition()).

-spec publish_update_req(kz_term:api_terms()) -> 'ok'.
publish_update_req(JObj) ->
    publish_update_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update_req(API, ContentType) ->
    Definition = update_req_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun update_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec update_resp(kz_term:api_terms()) -> api_formatter_return().
update_resp(Prop) ->
    build_message(Prop, update_resp_definition()).

-spec update_resp_v(kz_term:api_terms()) -> boolean().
update_resp_v(Prop) ->
    validate(Prop, update_resp_definition()).

-spec publish_update_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_update_resp(RespQ, JObj) ->
    publish_update_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update_resp(RespQ, API, ContentType) ->
    Values = kapi_definition:values(update_resp_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun update_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Current Account Standing Query
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec standing_req(kz_term:api_terms()) -> api_formatter_return().
standing_req(Prop) ->
    build_message(Prop, standing_req_definition()).

-spec standing_req_v(kz_term:api_terms()) -> boolean().
standing_req_v(Prop) ->
    validate(Prop, standing_req_definition()).

-spec publish_standing_req(kz_term:api_terms()) -> 'ok'.
publish_standing_req(JObj) ->
    publish_standing_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_standing_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_standing_req(API, ContentType) ->
    Definition = standing_req_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun standing_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec standing_resp(kz_term:api_terms()) -> api_formatter_return().
standing_resp(Prop) ->
    build_message(Prop, standing_resp_definition()).

-spec standing_resp_v(kz_term:api_terms()) -> boolean().
standing_resp_v(Prop) ->
    validate(Prop, standing_resp_definition()).

-spec publish_standing_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_standing_resp(RespQ, JObj) ->
    publish_standing_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_standing_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_standing_resp(RespQ, API, ContentType) ->
    Values = kapi_definition:values(standing_resp_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun standing_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
