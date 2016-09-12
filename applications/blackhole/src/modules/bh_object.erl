%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%% Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_object).

-export([init/0
        ,validate/2
        ,bindings/2
        ,subscribe/2
        ,unsubscribe/2
        ]).

-include("blackhole.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").


-spec init() -> any().
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.object">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.object">>, ?MODULE, 'bindings').
%%     _ = blackhole_bindings:bind(<<"blackhole.events.subscribe.object">>, ?MODULE, 'subscribe'),
%%     blackhole_bindings:bind(<<"blackhole.events.unsubscribe.object">>, ?MODULE, 'unsubscribe').

%% example binding: object.fax.doc_update

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [<<"*">>, <<"*">>]
                   }) ->
    Context;
validate(Context, #{keys := [Event, <<"*">>]
                   }) ->
    case lists:member(Event, ?DOC_TYPES) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, " not supported">>)
    end;
validate(Context, #{keys := [<<"*">>, Action]
                   }) ->
    case lists:member(Action, ?DOC_ACTIONS) of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Action/binary, " not supported">>)
    end;
validate(Context, #{keys := [Event, Action]
                   }) ->
    case lists:member(Action, ?DOC_ACTIONS)
        andalso lists:member(Event, ?DOC_TYPES)
    of
        'true' -> Context;
        'false' -> bh_context:add_error(Context, <<"event ", Event/binary, ".", Action/binary, " not supported">>)
    end;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for object subscription : ", (kz_util:join_binary(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [Type, Action]
                    }=Map) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    Requested = <<"object.", Type/binary, ".", Action/binary>>,
    Subscribed = [<<Action/binary, ".", AccountDb/binary, ".", Type/binary, ".*">>],
    Listeners = [{'amqp', 'conf', bind_options(AccountId, Keys)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.


-spec bind_options(ne_binary(), list()) -> kz_json:object().
bind_options(AccountId, Keys) ->
    [{'restrict_to', ['doc_updates']}
    ,{'account_id', AccountId}
    ,{'keys', Keys}
    ,'federate'
    ].

-spec subscribe(bh_context:context(), map()) -> map().
subscribe(_Context, #{account_id := AccountId
                     ,keys := [Type, Action]
                     }=Map) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    Requested = <<"object.", Type/binary, ".", Action/binary>>,
    Subscribed = [<<Action/binary, ".", AccountDb/binary, ".", Type/binary, ".*">>],
    Listeners = [{'amqp', 'conf', bind_options(AccountId, Keys)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec unsubscribe(bh_context:context(), map()) -> map().
unsubscribe(_Context, #{account_id := AccountId
                       ,keys := [Type, Action]
                       }=Map) ->
    AccountDb = kz_util:format_account_db(AccountId),
    Keys = [[{'action', Action}, {'db', AccountDb}, {'doc_type', Type}]],
    Requested = <<"object.", Type/binary, ".", Action/binary>>,
    Subscribed = [<<Action/binary, ".", AccountDb/binary, ".", Type/binary, ".*">>],
    Listeners = [{'amqp', 'conf', bind_options(AccountId, Keys)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.
