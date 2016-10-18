%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(bh_events).

-export([init/0
        ,event/3
        ,validate/2
        ,subscribe/2
        ,unsubscribe/2
        ,close/1
        ,authorize/2
        ]).

-include("blackhole.hrl").

-define(SUBSCRIBE_KEYS, [<<"account_id">>, <<"binding">>]).

-spec init() -> 'ok'.
init() ->
    blackhole_bindings:bind(<<"blackhole.authorize.subscribe">>, ?MODULE, 'authorize'),
    blackhole_bindings:bind(<<"blackhole.validate.subscribe">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.validate.unsubscribe">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.command.subscribe">>, ?MODULE, 'subscribe'),
    blackhole_bindings:bind(<<"blackhole.command.unsubscribe">>, ?MODULE, 'unsubscribe'),
    blackhole_bindings:bind(<<"blackhole.session.close">>, ?MODULE, 'close'),
    'ok'.

-spec authorize(bh_context:context(), kz_json:object()) -> bh_context:context().
authorize(Context, Payload) ->
    AccountId = kz_json:get_value([<<"data">>, <<"account_id">>], Payload),
    Binding = kz_json:get_value([<<"data">>, <<"binding">>], Payload),
    [Key | Keys] = binary:split(Binding, <<".">>, ['global']),
    Event = <<"blackhole.events.authorize.", Key/binary>>,
    Map = #{account_id => AccountId
           ,key => Key
           ,keys => Keys
           },
    Res = blackhole_bindings:fold(Event, [Context, Map]),
    validate_result(Context, Res).

-spec validate(bh_context:context(), kz_json:object()) -> bh_context:context().
validate(Context, Payload) ->
    case kz_json:get_keys(<<"data">>, Payload) of
        [] -> bh_context:add_error(Context, <<"missing required keys in data object">>);
        Keys -> validate_data(Context, Payload, Keys)
    end.

-spec validate_data(bh_context:context(), kz_json:object(), ne_binaries()) -> bh_context:context().
validate_data(Context, Payload, Keys) ->
    case ?SUBSCRIBE_KEYS -- Keys of
        [] -> validate_subscription(Context, Payload);
        Missing ->
            Error = <<"missing required keys : ", (kz_util:join_binary(Missing))/binary>>,
            bh_context:add_error(Context, Error)
    end.

-spec validate_subscription(bh_context:context(), kz_json:object()) -> bh_context:context().
validate_subscription(Context, Payload) ->
    AccountId = kz_json:get_value([<<"data">>, <<"account_id">>], Payload),
    Binding = kz_json:get_value([<<"data">>, <<"binding">>], Payload),
    [Key | Keys] = binary:split(Binding, <<".">>, ['global']),
    Event = <<"blackhole.events.validate.", Key/binary>>,
    Map = #{account_id => AccountId
           ,key => Key
           ,keys => Keys
           },
    Res = blackhole_bindings:map(Event, [Context, Map]),
    validate_result(Context, Res).

-spec validate_result(bh_context:context(), ne_binaries()) -> bh_context:context().
validate_result(Context, []) -> Context;
validate_result(Context, Res) ->
    case blackhole_bindings:failed(Res) of
        [Ctx | _] -> Ctx;
        [] -> case blackhole_bindings:succeeded(Res) of
                  [] -> Context;
                  [Ctx | _] -> Ctx
              end
    end.

-spec subscribe(bh_context:context(), kz_json:object()) -> bh_context:context().
subscribe(Context, Payload) ->
    AccountId = kz_json:get_value([<<"data">>, <<"account_id">>], Payload),
    Binding = kz_json:get_value([<<"data">>, <<"binding">>], Payload),
    [Key | Keys] = binary:split(Binding, <<".">>, ['global']),
    Event = <<"blackhole.events.bindings.", Key/binary>>,
    Map = #{account_id => AccountId
           ,key => Key
           ,keys => Keys
           },
    MapResults = blackhole_bindings:map(Event, [Context, Map]),
    case blackhole_bindings:succeeded(MapResults) of
        [] -> bh_context:add_error(Context, <<"no available subscriptions to requested binding">>);
        Bindings -> add_event_bindings(Context, Bindings)
    end.

-spec unsubscribe(bh_context:context(), kz_json:object()) -> bh_context:context().
unsubscribe(Context, Payload) ->
    AccountId = kz_json:get_value([<<"data">>, <<"account_id">>], Payload),
    Binding = kz_json:get_value([<<"data">>, <<"binding">>], Payload),
    [Key | Keys] = binary:split(Binding, <<".">>, ['global']),
    Event = <<"blackhole.events.bindings.", Key/binary>>,
    Map = #{account_id => AccountId
           ,key => Key
           ,keys => Keys
           },
    MapResults = blackhole_bindings:map(Event, [Context, Map]),
    case blackhole_bindings:succeeded(MapResults) of
        [] -> bh_context:add_error(Context, <<"no available subscriptions to requested binding">>);
        Bindings -> remove_event_bindings(Context, Bindings)
    end.

-spec event(map(), ne_binary(), kz_json:object()) -> 'ok'.
event(Binding, RK, EventJObj) ->
    kz_util:put_callid(EventJObj),
    Name = event_name(EventJObj),
    NormJObj = kz_json:normalize_jobj(EventJObj),
    blackhole_data_emitter:event(Binding, RK, Name, NormJObj).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

add_event_bindings(Context, BindingResults) ->
    case lists:foldl(fun add_event_bindings_fold/2, {Context, []}, BindingResults) of
        {Ctx, []} -> Ctx;
        {Ctx, Subscribed} ->
            Data = kz_json:from_list([{<<"subscribed">>, Subscribed}]),
            bh_context:set_resp_data(Ctx, Data)
    end.

add_event_bindings_fold(#{requested := Requested
                         ,subscribed := Subscribed
                         ,listeners := Listeners
                         }, {Context, Subs}) ->
    SessionBindings = bh_context:bindings(Context),
    Subscribe = Subscribed -- SessionBindings,
    lists:foreach(fun(B) -> bind(Context, Requested, B) end, Subscribe),
    blackhole_listener:add_bindings(Listeners),
    Ctx = bh_context:add_listeners(Context, Listeners),
    {bh_context:set_bindings(Ctx, SessionBindings ++ Subscribe), Subs ++ Subscribe}.

bind(Context, ReqKey, Key) ->
    SessionPid = bh_context:websocket_pid(Context),
    SessionId = bh_context:websocket_session_id(Context),
    Binding =  #{subscribed_key => ReqKey
                ,subscription_key => Key
                ,session_pid => SessionPid
                ,session_id => SessionId
                },
    KSession = base64:encode(SessionId),
    BHKey = <<"blackhole.event.", Key/binary, ".", KSession/binary>>,
    blackhole_bindings:bind(BHKey, ?MODULE, 'event', Binding).

-spec remove_event_bindings(bh_context:context(), [map()]) -> bh_context:context().
remove_event_bindings(Context, BindingResults) ->
    case lists:foldl(fun remove_event_bindings_fold/2, {Context, []}, BindingResults) of
        {Ctx, []} -> Ctx;
        {Ctx, Subscribed} ->
            Data = kz_json:from_list([{<<"unsubscribed">>, Subscribed}]),
            bh_context:set_resp_data(Ctx, Data)
    end.

-spec remove_event_bindings_fold(map(), {bh_context:context(), list()}) ->
                                        {bh_context:context(), list()}.
remove_event_bindings_fold(#{subscribed := Subscribed
                            ,listeners := Listeners
                            }, {Context, Subs}) ->
    SessionBindings = bh_context:bindings(Context),
    lists:foreach(fun(B) -> unbind(Context, B) end, Subscribed),
    blackhole_listener:remove_bindings(Listeners),
    Ctx = bh_context:remove_listeners(Context, Listeners),
    {bh_context:set_bindings(Ctx, SessionBindings -- Subscribed), Subs ++ Subscribed}.

unbind(Context, Key) ->
    KSession = base64:encode(bh_context:websocket_session_id(Context)),
    BHKey = <<"blackhole.event.", Key/binary, ".", KSession/binary>>,
    blackhole_bindings:flush(BHKey).

-spec close(bh_context:context()) -> bh_context:context().
close(Context) ->
    Listeners = bh_context:listeners(Context),
    blackhole_listener:remove_bindings(Listeners),
    Bindings = bh_context:bindings(Context),
    lists:foreach(fun(B) -> unbind(Context, B) end, Bindings),
    Routines = [{fun bh_context:remove_listeners/2, Listeners}
               ,{fun bh_context:remove_bindings/2, Bindings}
               ],
    bh_context:setters(Context, Routines).
