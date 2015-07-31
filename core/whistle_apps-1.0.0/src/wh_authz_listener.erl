%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
%%% @doc
%%%  The module is authz broadcast message manager.
%%%  It is used to make a number of calls to targets supported authz messages and wait for all responses, then
%%%  then creates a joint response using logical AND.
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(wh_authz_listener).

-behaviour(gen_listener).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
         ,start_link/0
         ,process_authz_broadcast_request/2
         ,process_authz_broadcast_response/2
        ]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").

%% By convention, we put the options here in macros, but not required.
-define(RESPONDERS, [{{?MODULE, 'process_authz_broadcast_request'} ,[{<<"authz">>, <<"authz.broadcast.req">>}]} % TODO: replace by wapi_authz:resp_event_type()-like call
                     ,{{?MODULE, 'process_authz_broadcast_response'} ,[{<<"authz">>, <<"authz.broadcast.resp">>}]}
                    ]).
-define(BINDINGS, [{'aaa', []}
                     ,{'self', []}
                    ]).
-define(QUEUE_NAME, <<"wh_authz_listener">>).
-define(QUEUE_OPTIONS, [{'exclusive', 'false'}]).
-define(CONSUME_OPTIONS, [{'exclusive', 'false'}]).

-define(AUTHZ_ETS, 'authz_ets').

-define(AUTHZ_BROADCAST_TIMEOUT, 5000).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    lager:debug("wh_authz_listener started"),
    gen_listener:start_link({'local', ?SERVER}, ?MODULE, [{'bindings', ?BINDINGS}
                                                          ,{'responders', ?RESPONDERS}
                                                          ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                                          ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                                          ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                                         ], []).

-spec process_authz_broadcast_request(wh_json:object(), wh_proplist()) -> 'ok'.
process_authz_broadcast_request(JObj, _Props) ->
    lager:debug("WH Authz request received"),
    MsgID = wh_json:get_value(<<"Msg-ID">>, JObj),
    {'ok', AccountId} = wapi_authz:maybe_determine_account_id(wapi_authz:from_jobj(JObj)),
    case couch_mgr:open_cache_doc(wh_util:format_account_id(AccountId, 'encoded'), <<"aaa">>) of
        {'ok', AaaDoc} ->
            % similar check is used for authentication (in the registrar app)
            AuthzServersList = wh_json:get_value(<<"authorization">>, AaaDoc),
            AaaMode = wh_json:get_value(<<"aaa_mode">>, AaaDoc),
            case {AaaMode, length(AuthzServersList)} of
                {<<"inherit">>, 0} ->
                    lager:debug("Authz enabled for this account (inherit mode)"),
                    AppList = wh_json:get_value(<<"authz_apps">>, AaaDoc),
                    TimerRef = erlang:send_after(?AUTHZ_BROADCAST_TIMEOUT, self(), {'authz_broadcast_timeout', MsgID}),
                    lager:debug("Store broadcast request in ETS: ~p", [{MsgID, JObj, AppList, TimerRef}]),
                    ets:insert(?AUTHZ_ETS, {MsgID, JObj, AppList, TimerRef}),
                    lager:debug("Authz broadcast request sent"),
                    JObj1 = wh_json:set_value(<<"Server-ID">>, <<"wh_authz_listener">>, JObj),
                    wapi_authz:publish_authz_req(JObj1);
                {_, 0} ->
                    lager:debug("Authz disabled for this account (no servers in config)"),
                    JObj1 = wh_json:set_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Disabled">>], <<"true">>, JObj),
                    wapi_authz:publish_authz_req(JObj1);
                {<<"on">>, _} ->
                    lager:debug("Authz enabled for this account (on mode)"),
                    AppList = wh_json:get_value(<<"authz_apps">>, AaaDoc),
                    TimerRef = erlang:send_after(?AUTHZ_BROADCAST_TIMEOUT, self(), {'authz_broadcast_timeout', MsgID}),
                    lager:debug("Store broadcast request in ETS: ~p", [{MsgID, JObj, AppList, TimerRef}]),
                    ets:insert(?AUTHZ_ETS, {MsgID, JObj, AppList, TimerRef}),
                    lager:debug("Authz broadcast request sent"),
                    JObj1 = wh_json:set_value(<<"Server-ID">>, <<"wh_authz_listener">>, JObj),
                    wapi_authz:publish_authz_req(JObj1);
                {<<"off">>, _} ->
                    lager:debug("Authz disabled for this account (off mode)"),
                    JObj1 = wh_json:set_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Disabled">>], <<"true">>, JObj),
                    wapi_authz:publish_authz_req(JObj1)
            end;
        {'error', _} ->
            lager:debug("No aaa document for this account"),
            JObj1 = wh_json:set_value([<<"Custom-Auth-Vars">>, <<"AAA-Authz-Disabled">>], <<"true">>, JObj),
            wapi_authz:publish_authz_req(JObj1)
    end.

-spec process_authz_broadcast_response(wh_json:object(), wh_proplist()) -> 'ok'.
process_authz_broadcast_response(JObjResp, _Props) ->
    lager:debug("WH Authz response received: ~p", [JObjResp]),
    JObjMsgID = wh_json:get_value(<<"Msg-ID">>, JObjResp),
    case ets:lookup(?AUTHZ_ETS, JObjMsgID) of
        [] ->
            lager:debug("Unknown message, should be passed"),
            'ok';
        [Ets] ->
            lager:debug("Authz broadcast response received"),
            lager:debug("ETS entry is ~p", [Ets]),
            JObjIsAuthorized = wh_json:get_value(<<"Is-Authorized">>, JObjResp, <<"false">>),
            maybe_app_authorized(JObjResp, Ets, JObjIsAuthorized)
    end.

maybe_app_authorized(JObjResp, {MsgID, JObj, AppList, TimerRef}, <<"true">>) ->
    lager:debug("Application authorized the request"),
    JObjAppName = wh_json:get_value(<<"App-Name">>, JObjResp),
    NewAppList = [App || App <- AppList, App =/= JObjAppName],
    lager:debug("Removed app ~p from applist ~p. Resulting list is ~p", [JObjAppName, AppList, NewAppList]),
    ets:insert(?AUTHZ_ETS, {MsgID, JObj, NewAppList, TimerRef}),
    maybe_send_authz_response(NewAppList, JObjResp, JObj, TimerRef);
maybe_app_authorized(JObjResp, {MsgID, JObj, AppList, TimerRef}, <<"false">>) ->
    lager:debug("Application not authorized the request"),
    % check if this responder is in the app list
    JObjAppName = wh_json:get_value(<<"App-Name">>, JObjResp),
    case lists:member(JObjAppName, AppList) of
        true ->
            % TODO: code for unauthorized response should be deduplicated
            erlang:cancel_timer(TimerRef),
            ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
            JObjResp1 = wh_json:set_values([{<<"Server-ID">>, ServerId}
                                            ,{<<"Is-Authorized">>, <<"false">>}
                                            ,{<<"Event-Category">>, <<"authz">>}
                                            ,{<<"Event-Name">>, <<"authz_resp">>}
                                            ,{<<"Final-Authz-Response">>, <<"true">>}
                                           ], JObjResp),
            wapi_authz:publish_authz_resp(ServerId, JObjResp1),
            ets:delete(?AUTHZ_ETS, MsgID);
        _ ->
            ok
    end.

maybe_send_authz_response([], JObjResp, JObj, TimerRef) ->
    lager:debug("All authz requests were processed. Sending joint result."),
    erlang:cancel_timer(TimerRef),

    CustomAuthVars = wh_json:get_value(<<"Custom-Auth-Vars">>, JObjResp),
    A1 = wh_json:delete_key(<<"Custom-Auth-Vars">>, JObjResp),
    A2 = wh_json:merge_jobjs(A1, CustomAuthVars),
    JObjMsgID = wh_json:get_value(<<"Msg-ID">>, A2),

    ets:delete(?AUTHZ_ETS, JObjMsgID),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    JObjResp1 = wh_json:set_values([{<<"Server-ID">>, ServerId}
                                    ,{<<"Event-Category">>, <<"authz">>}
                                    ,{<<"Event-Name">>, <<"authz_resp">>}
                                    ,{<<"Final-Authz-Response">>, <<"true">>}
                                   ], A2),
    wapi_authz:publish_authz_resp(ServerId, JObjResp1);
maybe_send_authz_response(_, _JObjResp, _JObj, _TimerRef) ->
    lager:debug("Not all authz requests were processed"),
    'ok'.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    ets:new(?AUTHZ_ETS, ['named_table', 'public']),
    {'ok', []}.

handle_call(_, _, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

handle_cast(_, State) ->
    {'noreply', State}.

handle_info({'authz_broadcast_timeout', MsgID}, State) ->
    lager:debug("authz_broadcast_timeout event handled for message ~p", [MsgID]),
    {MsgID, JObj, _AppList, TimerRef} = ets:lookup(?AUTHZ_ETS, MsgID),
    erlang:cancel_timer(TimerRef),
    JObj1 = wh_json:set_values([{<<"Is-Authorized">>, <<"false">>}
                               ,{<<"Event-Category">>, <<"authz">>}
                               ,{<<"Event-Name">>, <<"authz_resp">>}], JObj),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj1),
    wapi_authz:publish_authz_resp_orig(ServerId, JObj1),
    ets:delete(?AUTHZ_ETS, MsgID),
    {'noreply', State};
handle_info(Info, State) ->
    lager:debug("unexpected info message is ~p", [Info]),
    {'noreply', State}.

handle_event(_, _) ->
    {'reply', []}.

terminate(Reason, _State) ->
    lager:debug("listener terminating: ~p", [Reason]),
    MsgList = ets:tab2list(?AUTHZ_ETS),
    lists:foreach(fun({_MsgID, _JObj, _AppList, TimerRef}) -> erlang:cancel_timer(TimerRef) end, MsgList),
    ets:delete(?AUTHZ_ETS).

code_change(_, State, _) ->
    {'ok', State}.
