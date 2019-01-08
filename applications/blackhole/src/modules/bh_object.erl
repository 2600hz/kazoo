%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @author Roman Galeev
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_object).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include("blackhole.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/doc_types.hrl").

-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.object">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.object">>, ?MODULE, 'bindings').

%% example binding: object.fax.doc_update

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, <<"*">>]
                   }) ->
    Context;
validate(Context, #{keys := [Action, <<"*">>]
                   }) ->
    case lists:member(Action, ?DOC_ACTIONS) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Action/binary, ".* not supported">>)
    end;
validate(Context, #{keys := [<<"*">>, Type]
                   }) ->
    case lists:member(Type, ?DOC_TYPES) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event *.", Type/binary, " not supported">>)
    end;
validate(Context, #{keys := [Action, Type]
                   }) ->
    case lists:member(Action, ?DOC_ACTIONS)
        andalso lists:member(Type, ?DOC_TYPES)
    of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Action/binary, ".", Type/binary, " not supported">>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for object subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [Action, Type]
                    }=Map) ->
    AccountDb = kz_util:format_account_db(AccountId),
    AccountMODB = kazoo_modb:get_modb(AccountId),
    Requested = <<"object.", Action/binary, ".", Type/binary>>,
    Map#{requested => Requested
        ,subscribed => subscribed(Type, Action, AccountDb, AccountMODB)
        ,listeners => listeners(Type, Action, AccountDb, AccountMODB)
        }.

subscribed(<<"*">>, Action, AccountDb, AccountMODB) ->
    [<<Action/binary, ".", AccountDb/binary, ".*.*">>
    ,<<Action/binary, ".", AccountMODB/binary, ".*.*">>
    ];
subscribed(Type, Action, AccountDb, AccountMODB) ->
    case lists:member(Type, ?DOC_MODB_TYPES) of
        'true'  -> [<<Action/binary, ".", AccountMODB/binary, ".", Type/binary, ".*">>];
        'false' -> [<<Action/binary, ".", AccountDb/binary, ".", Type/binary, ".*">>]
    end.

listeners(<<"*">>, Action, AccountDb, AccountMODB) ->
    [{'amqp', 'conf', bind_options(Action, <<"*">>, AccountDb)}
    ,{'amqp', 'conf', bind_options(Action, <<"*">>, AccountMODB)}
    ];
listeners(Type, Action, AccountDb, AccountMODB) ->
    case lists:member(Type, ?DOC_MODB_TYPES) of
        'true'  -> [{'amqp', 'conf', bind_options(Action, Type, AccountMODB)}];
        'false' -> [{'amqp', 'conf', bind_options(Action, Type, AccountDb)}]
    end.

-spec bind_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
bind_options(Action, Type, Db) ->
    [{'action', Action}
    ,{'db', Db}
    ,{'doc_type', Type}
    ,'federate'
    ].
