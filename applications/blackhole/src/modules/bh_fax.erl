%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Roman Galeev
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_fax).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.fax">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.fax">>, ?MODULE, 'bindings').

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"status">>, _]
                   }) ->
    Context;
validate(Context, #{keys := [<<"object">>, _]
                   }) ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for object subscription : ", (kz_binary:join(Keys))/binary>>).


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

-spec fax_status_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     kz_term:proplist().
fax_status_bind_options(AccountId, FaxId) ->
    [{'restrict_to', ['status']}
    ,{'account_id', AccountId}
    ,{'fax_id', FaxId}
    ,'federate'
    ].

-spec fax_object_bind_options(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                     kz_term:proplist().
fax_object_bind_options(MODB, Action) ->
    [{'keys', [[{'action', Action}
               ,{'db', MODB}
               ,{'doc_type', <<"fax">>}
               ]
              ]}
    ,'federate'
    ].
