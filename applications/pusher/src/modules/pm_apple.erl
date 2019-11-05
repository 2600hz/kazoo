%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pm_apple).
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

-define(APNS_MAP, [{<<"Alert-Key">>, [<<"aps">>, <<"alert">>, <<"loc-key">>]}
                  ,{<<"Alert-Params">>, [<<"aps">>, <<"alert">>, <<"loc-args">>]}
                  ,{<<"Sound">>, [<<"aps">>, <<"sound">>]}
                  ,{<<"Call-ID">>, [<<"aps">>, <<"call-id">>]}
                  ,{<<"Payload">>, fun kz_json:merge/2}
                  ]).

-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_server:start_link({'local', ?SERVER}, ?MODULE, [],[]).

-spec init([]) -> {'ok', state()}.
init([]) ->
    kz_log:put_callid(?MODULE),
    {'ok', #state{tab=ets:new(?MODULE, [])}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'push', JObj}, #state{tab=ETS}=State) ->
    kz_log:put_callid(JObj),
    TokenApp = kz_json:get_value(<<"Token-App">>, JObj),
    maybe_send_push_notification(get_apns(TokenApp, ETS), JObj),
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
maybe_send_push_notification('undefined', _) -> 'ok';
maybe_send_push_notification({Pid, ExtraHeaders}, JObj) ->
    TokenID = kz_json:get_value(<<"Token-ID">>, JObj),
    Topic = apns_topic(JObj),
    Headers = kz_maps:merge(#{apns_topic => Topic}, ExtraHeaders),
    Msg = build_payload(JObj),
    lager:debug_unsafe("pushing topic ~s for token-id ~s : ~s", [Topic, TokenID, kz_json:encode(kz_json:from_map(Msg), ['pretty'])]),
    {Result, _Props, _Ignore} = apns:push_notification(Pid, TokenID, Msg, Headers),
    lager:debug("apns result for ~s : ~B", [Topic, Result]).

-spec build_payload(kz_json:object()) -> map().
build_payload(JObj) ->
    kz_json:to_map(kz_json:foldl(fun map_key/3, kz_json:new(), JObj)).

-spec map_key(term(), term(), kz_json:object()) -> kz_json:object().
map_key(K, V, JObj) ->
    case lists:keyfind(K, 1, ?APNS_MAP) of
        'false' -> JObj;
        {_, Fun} when is_function(Fun, 2) -> Fun(V, JObj);
        {_, K1} -> kz_json:set_value(K1, V, JObj)
    end.

-spec get_apns(kz_term:api_binary(), ets:tid()) -> push_app().
get_apns('undefined', _) -> 'undefined';
get_apns(App, ETS) ->
    case ets:lookup(ETS, App) of
        [] -> maybe_load_apns(App, ETS);
        [{App, Push}] -> Push
    end.

-spec maybe_load_apns(kz_term:api_binary(), ets:tid()) -> push_app().
maybe_load_apns(App, ETS) ->
    CertBin = kapps_config:get_ne_binary(?CONFIG_CAT, [<<"apple">>, <<"certificate">>], 'undefined', App),
    Host = kapps_config:get_ne_binary(?CONFIG_CAT, [<<"apple">>, <<"host">>], ?DEFAULT_APNS_HOST, App),
    ExtraHeaders = kapps_config:get_json(?CONFIG_CAT, [<<"apple">>, <<"headers">>], kz_json:new(), App),
    Headers = kz_maps:keys_to_atoms(kz_json:to_map(ExtraHeaders)),
    maybe_load_apns(App, ETS, CertBin, Host, Headers).

-spec maybe_load_apns(kz_term:api_binary()
                     ,ets:tid()
                     ,kz_term:api_ne_binary()
                     ,kz_term:ne_binary()
                     ,map()
                     ) -> push_app().
maybe_load_apns(App, _, 'undefined', _, _) ->
    lager:debug("apple pusher certificate for app ~s not found", [App]),
    'undefined';
maybe_load_apns(App, ETS, CertBin, Host, Headers) ->
    {Key, Cert} = pusher_util:binary_to_keycert(CertBin),
    lager:debug("starting apple push connection for ~s : ~s", [App, Host]),
    Connection = #{name => kz_term:to_atom(App, 'true')
                  ,apple_host => kz_term:to_list(Host)
                  ,apple_port => 443
                  ,certdata => Cert
                  ,keydata => Key
                  ,timeout => 10000
                  ,type => 'certdata'
                  },
    case apns:connect(Connection) of
        {'ok', Pid} ->
            ets:insert(ETS, {App, {Pid, Headers}}),
            {Pid, Headers};
        {'error', {'already_started', Pid}} ->
            apns:close_connection(Pid),
            maybe_load_apns(App, ETS, CertBin, Host, Headers);
        {'error', Reason} ->
            lager:error("error loading apns ~p", [Reason]),
            'undefined'
    end.

-spec apns_topic(kz_json:object()) -> binary().
apns_topic(JObj) ->
    TokenApp = kz_json:get_ne_binary_value(<<"Token-App">>, JObj),
    TokenType = kz_json:get_ne_binary_value(<<"Token-Type">>, JObj),
    case kapps_config:get_ne_binary(<<"pusher">>
                                   ,[TokenType, <<"apns_topic">>]
                                   ,'undefined'
                                   ,TokenApp
                                   )
    of
        'undefined' -> default_apns_topic(TokenApp);
        APNsTopic -> APNsTopic
    end.

%% Retains the old behaviour
-spec default_apns_topic(kz_term:ne_binary()) -> kz_term:ne_binary().
default_apns_topic(TokenApp) ->
    re:replace(TokenApp, <<"\\.(?:dev|prod)$">>, <<>>, [{'return', 'binary'}]).
