%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_fax).

-export([init/0
        ,validate/2
        ,bindings/2
        ,subscribe/2
        ,unsubscribe/2
        ]).

-include("blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.fax">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.fax">>, ?MODULE, 'bindings').
%%     _ = blackhole_bindings:bind(<<"blackhole.events.subscribe.fax">>, ?MODULE, 'subscribe'),
%%     blackhole_bindings:bind(<<"blackhole.events.unsubscribe.fax">>, ?MODULE, 'unsubscribe').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"status">>, _]
                   }) ->
    Context;
validate(Context, #{keys := [<<"object">>, _]
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for object subscription : ", (kz_util:join_binary(Keys))/binary>>).


-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"status">>, FaxId]
                    }=Map) ->
    Requested = <<"fax.status.", FaxId/binary>>,
    Subscribed = [<<"fax.status.", AccountId/binary, ".", FaxId/binary>>],
    Listeners = [{'amqp', 'fax', fax_status_bind_options(AccountId, FaxId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"object">>, Action]
                    }=Map) ->
    MODB = kazoo_modb:get_modb(AccountId),
    Requested = <<"fax.object.", Action/binary>>,
    Subscribed = [<<Action/binary, ".", MODB/binary, ".fax.*">>],
    Listeners = [{'amqp', 'conf', fax_object_bind_options(MODB, Action)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.


-spec subscribe(bh_context:context(), map()) -> map().
subscribe(_Context, #{account_id := AccountId
                     ,keys := [<<"status">>, FaxId]
                     }=Map) ->
    Requested = <<"fax.status.", FaxId/binary>>,
    Subscribed = [<<"fax.status.", AccountId/binary, ".", FaxId/binary>>],
    Listeners = [{'amqp', 'fax', fax_status_bind_options(AccountId, FaxId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
subscribe(_Context, #{account_id := AccountId
                     ,keys := [<<"object">>, Action]
                     }=Map) ->
    MODB = kazoo_modb:get_modb(AccountId),
    Requested = <<"fax.object.", Action/binary>>,
    Subscribed = [<<Action/binary, ".", MODB/binary, ".fax.*">>],
    Listeners = [{'amqp', 'conf', fax_object_bind_options(MODB, Action)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.


-spec unsubscribe(bh_context:context(), map()) -> map().
unsubscribe(_Context, #{account_id := AccountId
                       ,keys := [<<"status">>, FaxId]
                       }=Map) ->
    Requested = <<"fax.status.", FaxId/binary>>,
    Subscribed = [<<"fax.status.", AccountId/binary, ".", FaxId/binary>>],
    Listeners = [{'amqp', 'fax', fax_status_bind_options(AccountId, FaxId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        };
unsubscribe(_Context, #{account_id := AccountId
                       ,keys := [<<"object">>, Action]
                       }=Map) ->
    MODB = kazoo_modb:get_modb(AccountId),
    Requested = <<"fax.object.", Action/binary>>,
    Subscribed = [<<Action/binary, ".", MODB/binary, ".fax.*">>],
    Listeners = [{'amqp', 'conf', fax_object_bind_options(MODB, Action)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.


-spec fax_status_bind_options(ne_binary(), ne_binary()) -> kz_proplist().
fax_status_bind_options(AccountId, FaxId) ->
    [{'restrict_to', ['status']}
    ,{'account_id', AccountId}
    ,{'fax_id', FaxId}
    ,'federate'
    ].

-spec fax_object_bind_options(ne_binary(), ne_binary()) -> kz_json:object().
fax_object_bind_options(MODB, Action) ->
    [{'keys', [[{'action', Action}, {'db', MODB}, {'doc_type', <<"fax">>}]]}
    ,'federate'
    ].
