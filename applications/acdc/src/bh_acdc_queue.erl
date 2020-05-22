%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, Kage DS Ltd
%%% @doc
%%% @author Alan R Evans
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_acdc_queue).

-export([init/0
        ,validate/2
        ,bindings/2
        ]).

-include_lib("../blackhole/src/blackhole.hrl").

-define(BINDING(),
        [
         <<"acdc_queue.doc_created">>
        ,<<"acdc_queue.doc_edited">>
        ,<<"acdc_queue.doc_deleted">>
        ]).

-spec init() -> any().
init() ->
    init_bindings(),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.acdc_queue">>, ?MODULE, 'validate'),
    blackhole_bindings:bind(<<"blackhole.events.bindings.acdc_queue">>, ?MODULE, 'bindings').

init_bindings() ->
    Bindings = ?BINDING(),
    case kapps_config:set_default(?CONFIG_CAT, [<<"bindings">>, <<"queue">>], Bindings) of
        {'ok', _} -> lager:debug("initialized ACDC queue bindings");
        {'error', _E} -> lager:info("failed to initialize ACDC queue bindings: ~p", [_E])
    end.

-spec validate(bh_context:context(), map()) -> bh_context:context().
validate(Context, #{keys := [Action]
                   }) when Action =:= <<"doc_created">>
                           ; Action =:= <<"doc_edited">>
                           ; Action =:= <<"doc_deleted">> ->
    Context;
validate(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for queues subscription : ", (kz_binary:join(Keys))/binary>>).

-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [Action]
                    }=Map) ->
    Requested = <<"acdc_queue.", Action/binary>>,
    AccountDb = kzs_util:format_account_db(AccountId),
    Subscribed = [<<Action/binary, ".", AccountDb/binary, ".queue.*">>],
    Listeners = [{'amqp', 'conf', bind_options(Action, <<"queue">>, AccountDb)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

-spec bind_options(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:proplist().
bind_options(Action, Type, Db) ->
    [{'action', Action}
    ,{'db', Db}
    ,{'doc_type', Type}
    ,'federate'
    ].
