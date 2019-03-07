%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_bookkeepers).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([api_definitions/0, api_definition/1]).

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

-define(BINDING_STRING(Category, Name), <<"bookkeepers.", (Category)/binary, ".", (Name)/binary>>).

%%%=============================================================================
%%% Internal Bookkeeper Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sale_req_definition() -> kapi_definition:api().
sale_req_definition() ->
    #kapi_definition{name = <<"sale_req">>
                    ,friendly_name = <<"Sale Request">>
                    ,description = <<"Will trigger the appropriate bookkeeper to issue a sale">>
                    ,build_fun = fun sale_req/1
                    ,validate_fun = fun sale_req_v/1
                    ,publish_fun = fun publish_sale_req/1
                    ,binding = ?BINDING_STRING(<<"sale">>, <<"request">>)
                    ,restrict_to = 'sale'
                    ,required_headers = [<<"Bookkeeper-Type">>
                                        ,<<"Vendor-ID">>
                                        ,<<"Account-ID">>
                                        ,<<"Transaction-ID">>
                                        ,<<"Transaction-DB">>
                                        ,<<"Amount">>
                                        ]
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"sale_req">>}
                              ]
                    ,types = []
                    }.

-spec sale_resp_definition() -> kapi_definition:api().
sale_resp_definition() ->
    #kapi_definition{name = <<"sale_resp">>
                    ,friendly_name = <<"Sale Response">>
                    ,description = <<"Result of the attempt to issue a sale via the bookkeeper">>
                    ,build_fun = fun sale_resp/1
                    ,validate_fun = fun sale_resp_v/1
                    ,publish_fun = fun publish_sale_resp/2
                    ,binding = ?BINDING_STRING(<<"sale">>, <<"response">>)
                    ,restrict_to = 'sale'
                    ,required_headers = [<<"Status">>
                                        ,<<"Transaction-ID">>
                                        ,<<"Transaction-DB">>
                                        ]
                    ,optional_headers = [<<"Message">>
                                        ,<<"Reason">>
                                        ,<<"Details">>
                                        ]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"sale_resp">>}
                              ]
                    ,types = []
                    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refund_req_definition() -> kapi_definition:api().
refund_req_definition() ->
    #kapi_definition{name = <<"refund_req">>
                    ,friendly_name = <<"Refund Request">>
                    ,description = <<"Will trigger the appropriate bookkeeper to issue a refund">>
                    ,build_fun = fun refund_req/1
                    ,validate_fun = fun refund_req_v/1
                    ,publish_fun = fun publish_refund_req/1
                    ,binding = ?BINDING_STRING(<<"refund">>, <<"request">>)
                    ,restrict_to = 'refund'
                    ,required_headers = [<<"Bookkeeper-Type">>
                                        ,<<"Vendor-ID">>
                                        ,<<"Account-ID">>
                                        ,<<"Transaction-ID">>
                                        ,<<"Transaction-DB">>
                                        ,<<"Amount">>
                                        ]
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"refund_req">>}
                              ]
                    ,types = []
                    }.

-spec refund_resp_definition() -> kapi_definition:api().
refund_resp_definition() ->
    #kapi_definition{name = <<"refund_resp">>
                    ,friendly_name = <<"Refund Response">>
                    ,description = <<"Result of the attempt to refund via the bookkeeper">>
                    ,build_fun = fun refund_resp/1
                    ,validate_fun = fun refund_resp_v/1
                    ,publish_fun = fun publish_refund_resp/2
                    ,binding = ?BINDING_STRING(<<"refund">>, <<"response">>)
                    ,restrict_to = 'refund'
                    ,required_headers = [<<"Status">>
                                        ,<<"Transaction-ID">>
                                        ,<<"Transaction-DB">>
                                        ]
                    ,optional_headers = [<<"Message">>
                                        ,<<"Reason">>
                                        ,<<"Details">>
                                        ]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"refund_resp">>}
                              ]
                    ,types = []
                    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec update_req_definition() -> kapi_definition:api().
update_req_definition() ->
    #kapi_definition{name = <<"update_req">>
                    ,friendly_name = <<"Subscription Update Request">>
                    ,description = <<"A request to a bookkeeper to update or create a subscription">>
                    ,build_fun = fun update_req/1
                    ,validate_fun = fun update_req_v/1
                    ,publish_fun = fun publish_update_req/1
                    ,binding = ?BINDING_STRING(<<"update">>, <<"request">>)
                    ,restrict_to = 'update'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Bookkeeper-ID">>
                                        ,<<"Bookkeeper-Type">>
                                        ,<<"Vendor-ID">>
                                        ,<<"Items">>
                                        ]
                    ,optional_headers = [<<"Dry-Run">>
                                        ,<<"Audit-Log">>
                                        ]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"update_req">>}
                              ]
                    ,types = []
                    }.

-spec update_resp_definition() -> kapi_definition:api().
update_resp_definition() ->
    #kapi_definition{name = <<"update_resp">>
                    ,friendly_name = <<"Subscription Update Response">>
                    ,description = <<"The result of a subscription update request">>
                    ,build_fun = fun update_resp/1
                    ,validate_fun = fun update_resp_v/1
                    ,publish_fun = fun publish_update_resp/2
                    ,binding = ?BINDING_STRING(<<"update">>, <<"response">>)
                    ,restrict_to = 'update'
                    ,required_headers = [<<"Status">>]
                    ,optional_headers = [<<"Message">>
                                        ,<<"Reason">>
                                        ,<<"Details">>
                                        ]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"update_resp">>}
                              ]
                    ,types = []
                    }.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec standing_req_definition() -> kapi_definition:api().
standing_req_definition() ->
    #kapi_definition{name = <<"standing_req">>
                    ,friendly_name = <<"Currnet Status (Standing) Request">>
                    ,description = <<"A request to a bookkeeper to get the current standing of an account">>
                    ,build_fun = fun standing_req/1
                    ,validate_fun = fun standing_req_v/1
                    ,publish_fun = fun publish_standing_req/1
                    ,binding = ?BINDING_STRING(<<"standing">>, <<"request">>)
                    ,restrict_to = 'standing'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Bookkeeper-ID">>
                                        ,<<"Bookkeeper-Type">>
                                        ,<<"Vendor-ID">>
                                        ,<<"Items">>
                                        ,<<"Estimated-Withdrawal">>
                                        ]
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"standing_req">>}
                              ]
                    ,types = []
                    }.

-spec standing_resp_definition() -> kapi_definition:api().
standing_resp_definition() ->
    #kapi_definition{name = <<"standing_resp">>
                    ,friendly_name = <<"Current Status (Standing) Response">>
                    ,description = <<"The result of a standing request">>
                    ,build_fun = fun standing_resp/1
                    ,validate_fun = fun standing_resp_v/1
                    ,publish_fun = fun publish_standing_resp/2
                    ,binding = ?BINDING_STRING(<<"standing">>, <<"response">>)
                    ,restrict_to = 'standing'
                    ,required_headers = [<<"Status">>]
                    ,optional_headers = [<<"Message">>
                                        ,<<"Reason">>
                                        ,<<"Details">>
                                        ]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"standing_resp">>}
                              ]
                    ,types = []
                    }.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [sale_req_definition()
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
build_message(Prop, #kapi_definition{required_headers = ReqH
                                    ,optional_headers = OptH
                                    ,validate_fun = Validate
                                    ,name = _Name
                                    }) when is_list(Prop) ->
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(_Name)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, #kapi_definition{required_headers = ReqH
                               ,values = Values
                               ,types = Types
                               }) when is_list(Prop) ->
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%%=============================================================================
%%% Internal Bookkeepers Functions
%%%=============================================================================


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
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = sale_req_definition(),
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
    #kapi_definition{values = Values} = sale_resp_definition(),
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
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = refund_req_definition(),
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
    #kapi_definition{values = Values} = refund_resp_definition(),
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
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = update_req_definition(),
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
    #kapi_definition{values = Values} = update_resp_definition(),
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
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = standing_req_definition(),
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
    #kapi_definition{values = Values} = standing_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun standing_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).
