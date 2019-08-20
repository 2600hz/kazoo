%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_handler).

-export([handle_call_event/2
        ,handle_member_call/3
        ,handle_member_resp/2
        ,handle_member_accepted/2
        ,handle_member_retry/2
        ,handle_config_change/2
        ,handle_presence_probe/2
        ]).

-include("acdc.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-spec handle_call_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_event(JObj, Props) ->
    'true' = kapi_call:event_v(JObj),
    {Cat, Name} = kz_util:get_event_type(JObj),
    handle_call_event(Cat, Name, JObj, Props).

-spec handle_call_event(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_call_event(Category, <<"CHANNEL_DESTROY">> = Name, JObj, Props) ->
    case acdc_queue_fsm:cdr_url(props:get_value('fsm_pid', Props)) of
        'undefined' -> 'ok';
        Url -> acdc_util:send_cdr(Url, JObj)
    end,

    Srv = props:get_value('server', Props),
    CallId = kz_json:get_value(<<"Call-ID">>, JObj),
    acdc_util:unbind_from_call_events(CallId, Srv),
    acdc_queue_fsm:call_event(props:get_value('fsm_pid', Props)
                             ,Category
                             ,Name
                             ,JObj
                             );
handle_call_event(Category, Name, JObj, Props) ->
    acdc_queue_fsm:call_event(props:get_value('fsm_pid', Props)
                             ,Category
                             ,Name
                             ,JObj
                             ).

-spec handle_member_call(kz_json:object(), kz_term:proplist(), gen_listener:basic_deliver()) -> 'ok'.
handle_member_call(JObj, Props, Delivery) ->
    'true' = kapi_acdc_queue:member_call_v(JObj),
    acdc_queue_fsm:member_call(props:get_value('fsm_pid', Props), JObj, Delivery),
    gen_listener:cast(props:get_value('server', Props), {'delivery', Delivery}).

-spec handle_member_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_resp(JObj, Props) ->
    'true' = kapi_acdc_queue:member_connect_resp_v(JObj),
    acdc_queue_fsm:member_connect_resp(props:get_value('fsm_pid', Props), JObj).

-spec handle_member_accepted(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_accepted(JObj, Props) ->
    'true' = kapi_acdc_queue:member_connect_accepted_v(JObj),
    acdc_queue_fsm:member_accepted(props:get_value('fsm_pid', Props), JObj).

-spec handle_member_retry(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_member_retry(JObj, Props) ->
    'true' = kapi_acdc_queue:member_connect_retry_v(JObj),
    acdc_queue_fsm:member_connect_retry(props:get_value('fsm_pid', Props), JObj).

-spec handle_config_change(kz_json:object(), kz_term:proplist()) -> any().
handle_config_change(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    handle_queue_change(kz_json:get_value(<<"Database">>, JObj)
                       ,kz_json:get_value(<<"Account-ID">>, JObj)
                       ,kz_json:get_value(<<"ID">>, JObj)
                       ,kz_json:get_value(<<"Event-Name">>, JObj)
                       ).

-spec handle_queue_change(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_queue_change(_, AccountId, QueueId, ?DOC_CREATED) ->
    lager:debug("maybe starting new queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> acdc_queues_sup:new(AccountId, QueueId);
        P when is_pid(P) -> 'ok'
    end;
handle_queue_change(AccountDb, AccountId, QueueId, ?DOC_EDITED) ->
    lager:debug("maybe updating existing queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> acdc_queues_sup:new(AccountId, QueueId);
        QueueSup when is_pid(QueueSup) ->
            {'ok', JObj} = kz_datamgr:open_doc(AccountDb, QueueId),
            WorkersSup = acdc_queue_sup:workers_sup(QueueSup),
            WorkersSups = acdc_queue_workers_sup:workers(WorkersSup),
            Refresher = fun (Sup) -> acdc_queue_fsm:refresh(acdc_queue_worker_sup:fsm(Sup), JObj) end,
            lists:foreach(Refresher, WorkersSups),
            Mgr = acdc_queue_sup:manager(QueueSup),
            acdc_queue_manager:refresh(Mgr, JObj)
    end;
handle_queue_change(_, AccountId, QueueId, ?DOC_DELETED) ->
    lager:debug("maybe stopping existing queue for ~s: ~s", [AccountId, QueueId]),
    case acdc_queues_sup:find_queue_supervisor(AccountId, QueueId) of
        'undefined' -> lager:debug("no queue(~s) started for account ~s", [QueueId, AccountId]);
        P when is_pid(P) ->
            lager:debug("stopping queue(~s) in account ~s (deleted): ~p", [QueueId, AccountId, P]),
            acdc_queue_sup:stop(P)
    end.

%%------------------------------------------------------------------------------
%% @doc Handle presence probes for queues by filtering out those that cannot be
%% queue probes (do not look like 32 character hex binaries) and those that do
%% not correspond to an existing account ID/queue ID pair
%%
%% @end
%%------------------------------------------------------------------------------
-spec handle_presence_probe(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_presence_probe(JObj, _Props) ->
    'true' = kapi_presence:probe_v(JObj),
    Username = kz_json:get_ne_binary_value(<<"Username">>, JObj),
    case potentially_queue_presence_probe(Username) of
        'true' ->
            Realm = kz_json:get_ne_binary_value(<<"Realm">>, JObj),
            maybe_respond_to_presence_probe(
              acdc_presence_realm_lookup:lookup(Realm)
             ,Username
             );
        'false' -> 'ok'
    end.

-spec potentially_queue_presence_probe(kz_term:api_binary()) -> boolean().
potentially_queue_presence_probe(<<_:32/binary>>) -> 'true';
potentially_queue_presence_probe(_) -> 'false'.

-spec maybe_respond_to_presence_probe(kz_term:ne_binary() | 'not_found', kz_term:ne_binary()) -> 'ok'.
maybe_respond_to_presence_probe('not_found', _) -> 'ok';
maybe_respond_to_presence_probe(AcctId, QueueId) ->
    case acdc_queues_sup:find_queue_supervisor(AcctId, QueueId) of
        'undefined' -> 'ok';
        QueueSup ->
            Manager = acdc_queue_sup:manager(QueueSup),
            update_probe(Manager, AcctId, QueueId)
    end.

-spec update_probe(kz_term:api_pid(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_probe(Manager, AcctId, QueueId) ->
    case acdc_queue_manager:queue_size(Manager) of
        0 ->
            lager:debug("no calls in queue, ignore!"),
            acdc_util:presence_update(AcctId, QueueId, ?PRESENCE_GREEN);
        N ->
            lager:debug("~b calls in queue, redify!", [N]),
            acdc_util:presence_update(AcctId, QueueId, ?PRESENCE_RED_FLASH)
    end.
