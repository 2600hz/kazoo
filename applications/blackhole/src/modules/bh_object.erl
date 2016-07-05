%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(bh_object).

-export([handle_event/2
        ,add_amqp_binding/2, rm_amqp_binding/2
        ]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    kz_util:put_callid(EventJObj),
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    blackhole_data_emitter:emit(
      bh_context:websocket_pid(Context)
                               ,event_name(EventJObj)
                               ,EventJObj
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
-spec add_amqp_binding(ne_binary(), bh_context:context(), ne_binary(), ne_binary()) -> 'ok'.
add_amqp_binding(Binding, Context) ->
    [Action, _, Type|_] = binary:split(Binding, <<".">>, ['global']),
    case
        lists:member(Action, ?DOC_ACTIONS)
        andalso lists:member(Type, ?DOC_TYPES)
    of
        'false' -> lager:debug("unmatched binding ~s", [Binding]);
        'true' ->
            add_amqp_binding(Binding, Context, Action, Type)
    end.

add_amqp_binding(_Binding, Context, Action, Type) ->
    lager:debug("adding amqp binding: ~s", [_Binding]),
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    blackhole_listener:add_binding(
      'conf'
                                  ,[{'restrict_to', ['doc_updates']}
                                   ,{'account_id', AccountId}
                                   ,{'keys', Keys}
                                   ,'federate'
                                   ]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(Binding, Context) ->
    lager:debug("removing amqp binding: ~s", [Binding]),
    [Action, _, Type|_] = binary:split(Binding, <<".">>, ['global']),
    AccountId = bh_context:account_id(Context),
    AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    blackhole_listener:remove_binding(
      'conf'
                                     ,[{'restrict_to', ['doc_updates']}
                                      ,{'account_id', AccountId}
                                      ,{'keys', Keys}
                                      ,'federate'
                                      ]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec event_name(kz_json:object()) -> ne_binary().
event_name(EventJObj) ->
    EventName = kz_api:event_name(EventJObj),
    DocType = kz_json:get_value(<<"Type">>, EventJObj),
    <<EventName/binary, "_", DocType/binary>>.
