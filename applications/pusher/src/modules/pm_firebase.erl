%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pm_firebase).
-behaviour(gen_server).

-include("pusher.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-record(state, {tab :: ets:tid()}).
-type state() :: #state{}.

-define(FIREBASE_MAP, [{<<"Alert-Key">>, [<<"alert">>, <<"loc-key">>]}
                      ,{<<"Alert-Params">>, [<<"alert">>, <<"loc-args">>]}
                      ,{<<"Sound">>, [<<"sound">>]}
                      ,{<<"Call-ID">>, [<<"Call-ID">>]}
                      ,{<<"Payload">>, fun kz_json:merge/2}
                      ]).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [],[]).

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    lager:debug("starting server"),
    {'ok', #state{tab=ets:new(?MODULE, [])}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', JObj}, #state{tab=ETS}=State) ->
    lager:debug("process a push"),
    TokenApp = kz_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_fcm(TokenApp, ETS), JObj),
    {'noreply', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info(_Request, State) ->
    {'noreply', State}.

-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, #state{tab=ETS}) ->
    ets:delete(ETS),
    'ok'.

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

-spec maybe_send_push_notification(push_app(), kz_json:object()) -> any().
maybe_send_push_notification('undefined', _JObj) -> lager:debug("no pid to send push");
maybe_send_push_notification({Pid, Envelope}, JObj) ->
    TokenID = kz_json:get_value(<<"Token-ID">>, JObj),
    MessageJObj = kz_json:from_list([{<<"data">>, build_payload(JObj)}]),
    Message = kz_maps:merge(kz_json:to_map(MessageJObj), Envelope),

    lager:debug("pushing to ~p: ~s: ~p", [Pid, TokenID, Message]),

    fcm:push(Pid, [TokenID], Message).

-spec build_payload(kz_json:object()) -> kz_json:object().
build_payload(JObj) ->
    kz_json:foldl(fun map_key/3, kz_json:new(), JObj).

-spec map_key(term(), term(), kz_json:object()) -> kz_json:object().
map_key(K, V, JObj) ->
    case lists:keyfind(K, 1, ?FIREBASE_MAP) of
        'false' -> JObj;
        {_, Fun} when is_function(Fun, 2) -> Fun(V, JObj);
        {_, K1} -> kz_json:set_value(K1, V, JObj)
    end.

-spec get_fcm(kz_term:api_binary(), ets:tid()) -> push_app().
get_fcm('undefined', _) -> 'undefined';
get_fcm(App, ETS) ->
    case ets:lookup(ETS, App) of
        [] -> maybe_load_fcm(App, ETS);
        [{App, Push}] -> Push
    end.

-spec maybe_load_fcm(kz_term:api_binary(), ets:tid()) -> push_app().
maybe_load_fcm(App, ETS) ->
    lager:debug("loading fcm secret for ~s", [App]),
    FCMSecret = kapps_config:get_binary(?CONFIG_CAT, [<<"firebase">>, <<"api_key">>], 'undefined', App),
    EnvelopeJObj = kapps_config:get_json(?CONFIG_CAT, [<<"firebase">>, <<"headers">>], kz_json:new(), App),
    Envelope = kz_json:to_map(EnvelopeJObj),
    maybe_load_fcm(App, ETS, FCMSecret, Envelope).

-spec maybe_load_fcm(kz_term:api_binary(), ets:tid(), kz_term:api_binary(), map()) -> push_app().
maybe_load_fcm(App, _, 'undefined', _) ->
    lager:debug("firebase pusher api_key for app ~s not found", [App]),
    'undefined';
maybe_load_fcm(App, ETS, APIKey, Envelope) ->
    case fcm:start(kz_term:to_atom(<<"fcm_", App/binary>>, 'true'), kz_term:to_list(APIKey)) of
        {'ok', Pid} ->
            ets:insert(ETS, {App, {Pid, Envelope}}),
            {Pid, Envelope};
        {'error', {'already_started', Pid}} ->
            ets:insert(ETS, {App, {Pid, Envelope}}),
            {Pid, Envelope};
        {'error', Reason} ->
            lager:error("error loading fcm ~p", [Reason]),
            'undefined'
    end.
