%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Subscribe to AMQP events on behalf of user
%%%
%%% Subscribing
%%% 1. Request comes in from an account's user to start/stop subscribing to events
%%%    a. spawn process to save/delete the subscription from the user's event sub doc in Couch
%%% 2. cb_events asks cb_events_sup for the user's event server
%%%    a. if no server exists (and a sub is requested, start the server)
%%% 3. cb_events uses the returned PID to call cb_events_srv:sub/unsub with requested subs
%%%    a. if unsubbing the last subscription, cb_events_srv stops running
%%%
%%% Polling
%%% 1. Same as (1) above
%%% 2. Same as (2), except an error is returned if no server exists
%%% 3. cb_events uses the returned PID to call cb_events_srv:fetch to retrieve all events
%%%
%%% REST API
%%%
%%% /v1/accounts/{AID}/events
%%% - GET => fetches all events and overflow flag: RESP: {"events": [{},...], "overflow":boolean()}
%%% - POST => {"max_events": integer()} sets the max events to store
%%% - DELETE => removes all subscriptions
%%%
%%% /v1/accounts/{AID}/events/available
%%% - GET => returns all known subscriptions: RESP: {"subscriptions": ["authentication", "cdrs",...]}
%%%
%%% /v1/accounts/{AID}/events/subscription
%%% - GET => returns the list of subscriptions currently subscribed
%%% - PUT => {"subscriptions":["sub1", "sub2"]} -> adds the subscription
%%% - DELETE => {"subscriptions":["sub2"]} -> removes the subscription
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_events).

-export([init/0
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,validate/1, validate/2
         ,put/2
         ,post/1
         ,delete/1, delete/2
        ]).

-include("include/crossbar.hrl").

-define(DEFAULT_USER, <<"events_user">>).
-define(EVENT_DOC_ID(User), <<"event_sub_", User/binary>>).
-define(DOC_TYPE, <<"events">>).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    crossbar_module_sup:start_mod(cb_events_sup),

    %% TODO: load existing event servers from the DB

    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.events">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.events">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.events">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.events">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.events">>, ?MODULE, post),
    crossbar_bindings:bind(<<"v1_resource.execute.delete.events">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'POST', 'DELETE'].
allowed_methods(<<"available">>) ->
    ['GET'];
allowed_methods(<<"subscription">>) ->
    ['GET', 'PUT', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() ->
    true.
resource_exists(<<"available">>) ->
    true;
resource_exists(<<"subscription">>) ->
    true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------

-spec validate(#cb_context{}) -> #cb_context{}.
-spec validate(#cb_context{}, path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            load_events(Context, Pid, UserId);
        _Other ->
            cb_context:add_system_error(faulty_request, Context)
    end;
validate(#cb_context{req_verb = <<"post">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            update_srv(Context, Pid, UserId);
        _ ->
            cb_context:add_system_error(faulty_request, Context)
    end;
validate(#cb_context{req_verb = <<"delete">>, account_id=AcctId, auth_doc=AuthDoc}=Context) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            stop_srv(Context, Pid, UserId);
        _ ->
            cb_context:add_system_error(faulty_request, Context)
    end.

validate(#cb_context{req_verb = <<"get">>}=Context, <<"available">>) ->
    load_available_bindings(Context);

validate(#cb_context{req_verb = <<"get">>, account_id=AcctId, auth_doc=AuthDoc}=Context, <<"subscription">>) ->
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            load_current_subscriptions(Context, Pid);
        _Other ->
            cb_context:add_system_error(faulty_request, Context)
    end;

validate(#cb_context{req_verb = <<"put">>, auth_doc=AuthDoc, account_id=AcctId, req_data=ReqData}=Context, <<"subscription">>) ->
    lager:debug("reqd: ~p", [ReqData]),
    Subscriptions = wh_json:get_value(<<"subscriptions">>, ReqData, []),
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            add_subscriptions(Context, Pid, Subscriptions, UserId);
        _ ->
            cb_context:add_system_error(faulty_request, Context)
    end;

validate(#cb_context{req_verb = <<"delete">>, auth_doc=AuthDoc, account_id=AcctId, req_data=ReqData}=Context, <<"subscription">>) ->
    lager:debug("reqd: ~p", [ReqData]),
    Subscriptions = wh_json:get_value(<<"subscriptions">>, ReqData, []),
    UserId = wh_json:get_value(<<"owner_id">>, AuthDoc, ?DEFAULT_USER),
    case cb_events_sup:find_srv(AcctId, UserId) of
        {ok, Pid} when is_pid(Pid) ->
            rm_subscriptions(Context, Pid, Subscriptions, UserId);
        _Other ->
            cb_context:add_system_error(faulty_request, Context)
    end.

-spec post(#cb_context{}) -> #cb_context{}.
post(Context) ->
    crossbar_doc:save(Context).

-spec put(#cb_context{}, path_token()) -> #cb_context{}.
put(Context, _) ->
    crossbar_doc:save(Context).

-spec delete(#cb_context{}) -> #cb_context{}.
-spec delete(#cb_context{}, path_token()) -> #cb_context{}.
delete(Context) ->
    crossbar_doc:save(Context).
delete(Context, _) ->
    crossbar_doc:save(Context).

%%%===================================================================
%%% Internal functions
%%%===================================================================
load_events(Context, Srv, User) ->
    {Events, Overflow} = cb_events_srv:fetch(Srv),
    RespJObj = wh_json:set_value(<<"overflow">>, Overflow, 
                                 wh_json:set_value(<<"events">>, Events, wh_json:new())
                                ),
    save_latest(Context, Srv, User, RespJObj).

update_srv(#cb_context{req_data=ReqJObj}=Context, Srv, User) ->
    MaxEvents = wh_json:get_integer_value(<<"max_events">>, ReqJObj, 100),
    {ok, EventsDropped} = cb_events_srv:set_maxevents(Srv, MaxEvents),
    lager:debug("set max events to ~b: dropped ~b events", [MaxEvents, EventsDropped]),
    save_latest(Context, Srv, User, wh_json:set_value(<<"events_dropped">>, EventsDropped, wh_json:new())).

stop_srv(Context, Srv, User) ->
    ok = cb_events_srv:stop(Srv),
    save_latest(Context, undefined, User).

load_available_bindings(Context) ->
    Mods = [ begin <<"wapi_", Bind/binary>> = Mod, Bind end
             || A <- erlang:loaded(),
                binary:match((Mod=wh_util:to_binary(A)), <<"wapi_">>) =/= nomatch,
                erlang:function_exported(A, bind_q, 2)
           ],
    RespJObj = wh_json:from_list([{<<"available_bindings">>, Mods}]),
    crossbar_util:response(RespJObj, Context).

load_current_subscriptions(Context, Srv) ->
    {ok, Subs} = cb_events_srv:subscriptions(Srv),
    RespJObj = wh_json:set_value(<<"current_subscriptions">>, Subs, wh_json:new()),
    crossbar_util:response(RespJObj, Context).

add_subscriptions(#cb_context{req_data=ReqJObj}=Context, Srv, Subs, User) ->
    lager:debug("Adding ~p", [Subs]),
    RespJObj = wh_json:from_list([{Sub
                                   ,format_sub_result(cb_events_srv:subscribe(Srv, Sub, wh_json:get_value(<<"options">>, ReqJObj, [])))
                         } || Sub <- Subs]),
    Context1 = save_latest(Context, Srv, User),
    crossbar_util:response(RespJObj, Context1).

format_sub_result(ok) ->
    <<"subscribed">>;
format_sub_result({'error', 'unknown'}) ->
    <<"invalid subscription choice">>;
format_sub_result({'error', 'already_present'}) ->
    <<"already subscribed">>.

rm_subscriptions(Context, Srv, Subs, User) ->
    lager:debug("removing: ~p", [Subs]),
    _ = [cb_events_srv:unsubscribe(Srv, Sub) || Sub <- Subs],
    save_latest(Context, Srv, User).

save_latest(Context, Srv, User) ->
    save_latest(Context, Srv, User, wh_json:new()).

save_latest(Context, undefined, User, BaseJObj) ->
    Doc0 = wh_json:from_list([{<<"subscriptions">>, []}
                             ,{<<"max_events">>, 0}
                             ,{<<"_id">>, ?EVENT_DOC_ID(User)}
                             ,{<<"pvt_type">>, ?DOC_TYPE}
                             ]),
    Doc = wh_json:merge_jobjs(Doc0, BaseJObj),

    case crossbar_doc:load_merge(?EVENT_DOC_ID(User), Doc, Context) of
        #cb_context{resp_status=success}=Context1 ->
            Context1;
        _ ->
            Context#cb_context{resp_status=success, doc=Doc}
    end;
save_latest(Context, Srv, User, BaseJObj) ->
    {ok, Subs} = cb_events_srv:subscriptions(Srv),
    Max = cb_events_srv:get_maxevents(Srv),

    Doc0 = wh_json:from_list([{<<"subscriptions">>, Subs}
                             ,{<<"max_events">>, Max}
                             ,{<<"_id">>, ?EVENT_DOC_ID(User)}
                             ,{<<"pvt_type">>, ?DOC_TYPE}
                             ]),
    Doc = wh_json:merge_jobjs(Doc0, BaseJObj),

    case crossbar_doc:load_merge(?EVENT_DOC_ID(User), Doc, Context) of
        #cb_context{resp_status=success}=Context1 ->
            Context1;
        _ ->
            Context#cb_context{resp_status=success, doc=Doc}
    end.
