%%
%% nksip.hrl: Common types and records definition
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-ifndef(NKSIP_HRL_).
-define(NKSIP_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================

-define(VERSION, "0.4.0").

-define(
    DO_LOG(Level, App, CallId, Text, Opts),
    case CallId of
        <<>> ->
            lager:Level([{app, App}], "~p "++Text, [App|Opts]);
        _ -> 
            lager:Level([{app, App}, {call_id, CallId}], "~p (~s) "++Text, [App, CallId|Opts])
    end).

-define(DO_DEBUG(AppId, CallId, Level, Text, List),
    case AppId:config_debug() of
        false -> ok;
        _ -> AppId:nkcb_debug(AppId, CallId, {Level, Text, List})
    end).


-define(debug(AppId, CallId, Text, List), 
    ?DO_DEBUG(AppId, CallId, debug, Text, List),
    case AppId:config_log_level() >= 8 of
        true -> ?DO_LOG(debug, AppId:name(), CallId, Text, List);
        false -> ok
    end).

-define(info(AppId, CallId, Text, List), 
    ?DO_DEBUG(AppId, CallId, info, Text, List),
    case AppId:config_log_level() >= 7 of
        true -> ?DO_LOG(info, AppId:name(), CallId, Text, List);
        false -> ok
    end).

-define(notice(AppId, CallId, Text, List), 
    ?DO_DEBUG(AppId, CallId, notice, Text, List),
    case AppId:config_log_level() >= 6 of
        true -> ?DO_LOG(notice, AppId:name(), CallId, Text, List);
        false -> ok
    end).

-define(warning(AppId, CallId, Text, List), 
    ?DO_DEBUG(AppId, CallId, warning, Text, List),
    case AppId:config_log_level() >= 5 of
        true -> ?DO_LOG(warning, AppId:name(), CallId, Text, List);
        false -> ok
    end).

-define(error(AppId, CallId, Text, List), 
    ?DO_DEBUG(AppId, CallId, error, Text, List),
    case AppId:config_log_level() >= 4 of
        true -> ?DO_LOG(error, AppId:name(), CallId, Text, List);
        false -> ok
    end).


-define(N(T), lager:notice(T)).
-define(N(T,P), lager:notice(T,P)).
-define(W(T), lager:warning(T)).
-define(W(T,P), lager:warning(T,P)).
-define(E(T), lager:error(T)).
-define(E(T,P), lager:error(T,P)).

-include_lib("kernel/include/inet_sctp.hrl").


%% ===================================================================
%% Types
%% ===================================================================

-type from() :: term().

-type gen_server_time() :: 
        non_neg_integer() | hibernate.

-type gen_server_init(State) ::
        {ok, State} | {ok, State, gen_server_time()} | ignore.

-type gen_server_cast(State) :: 
        {noreply, State} | {noreply, State, gen_server_time()} |
        {stop, term(), State}.

-type gen_server_info(State) :: 
        gen_server_cast(State).

-type gen_server_call(State) :: 
        {reply, term(), State} | {reply, term(), State, gen_server_time()} |
        {stop, term(), term(), State} | gen_server_cast(State).

-type gen_server_code_change(State) ::
        {ok, State}.

-type gen_server_terminate() ::
        ok.



%% ===================================================================
%% Records
%% ===================================================================


-record(sipapp_srv, {
    app_id :: nksip:app_id(),
    args :: term(),
    sipapp_state :: term(),
    meta :: list()
}).


-record(transport, {
    proto = udp :: nksip:protocol(),
    local_ip :: inet:ip_address(),
    local_port :: inet:port_number(),
    remote_ip :: inet:ip_address(),
    remote_port :: inet:port_number(),
    listen_ip :: inet:ip_address(),         % Ip this transport must report as listening
    listen_port :: inet:port_number(),
    sctp_id :: integer(),
    resource = <<>> :: binary()      
}).


-record(sipmsg, {
    id :: nksip_sipmsg:id(),
    class :: {req, nksip:method()} | {resp, nksip:sip_code(), binary()},
    app_id :: nksip:app_id(),
    dialog_id :: nksip_dialog_lib:id(),
    ruri :: nksip:uri(),
    vias = [] :: [nksip:via()],
    from :: {nksip:uri(), FromTag::binary()},
    to :: {nksip:uri(), ToTag::binary()},
    call_id :: nksip:call_id(),
    cseq :: {nksip:cseq(), nksip:method()},
    forwards :: non_neg_integer(),
    routes = [] :: [nksip:uri()],
    contacts = [] :: [nksip:uri()],
    content_type :: nksip:token() | undefined,
    require = [] :: [binary()],
    supported = [] :: [binary()],
    expires :: non_neg_integer() | undefined,
    event :: nksip:token() | undefined,
    headers = [] :: [nksip:header()],
    body = <<>> :: nksip:body(),
    to_tag_candidate = <<>> :: nksip:tag(),
    transport :: nksip_transport:transport(),
    start :: nksip_lib:l_timestamp(),
    meta = [] :: nksip:optslist()   % No current use
}).


-record(reqreply, {
    code = 200 :: nksip:sip_code(),
    headers = [] :: [nksip:header()],
    body = <<>> :: nksip:body(),
    opts = [] :: nksip:optslist()
}).

-record(uri, {
    disp = <<>> :: binary(),
    scheme = sip :: nksip:scheme(),
    user = <<>> :: binary(), 
    pass = <<>> :: binary(), 
    domain = <<"invalid.invalid">> :: binary(), 
    port = 0 :: inet:port_number(),             % 0 means "no port in message"
    path = <<>> :: binary(),
    opts = [] :: nksip:optslist(),
    headers = [] :: [binary()|nksip:header()],
    ext_opts = [] :: nksip:optslist(),
    ext_headers = [] :: [binary()|nksip:header()]
}).

-record(via, {
    proto = udp :: nksip:protocol(),
    domain = <<"invalid.invalid">> :: binary(),
    port = 0 :: inet:port_number(),
    opts = [] :: nksip:optslist()
}).


-record(invite, {
    status :: nksip_dialog:invite_status(),
    answered :: nksip_lib:timestamp(),
    class :: uac | uas | proxy,
    request :: nksip:request(),
    response :: nksip:response(),
    ack :: nksip:request(),
    local_sdp :: nksip_sdp:sdp(),
    remote_sdp :: nksip_sdp:sdp(),
    media_started :: boolean(),
    sdp_offer :: nksip_call_dialog:sdp_offer(),
    sdp_answer :: nksip_call_dialog:sdp_offer(),
    timeout_timer :: reference(),
    retrans_timer :: reference(),
    next_retrans :: integer()
}).


-record(subscription, {
    id :: nksip_subscription_lib:id(),
    event :: nksip:token(),
    expires :: pos_integer(),
    status :: nksip_subscription:status(),
    class :: uac | uas,
    answered :: nksip_lib:timestamp(),
    timer_n :: reference(),
    timer_expire :: reference(),
    timer_middle :: reference(),
    last_notify_cseq :: nksip:cseq()
}).


%% Meta current uses:
%% - {nksip_min_se, MinSE}

-record(dialog, {
    id :: nksip_dialog_lib:id(),
    app_id :: nksip:app_id(),
    call_id :: nksip:call_id(),
    created :: nksip_lib:timestamp(),
    updated :: nksip_lib:timestamp(),
    local_seq :: 0 | nksip:cseq(),
    remote_seq :: 0 | nksip:cseq(),
    local_uri :: nksip:uri(),
    remote_uri :: nksip:uri(),
    local_target :: nksip:uri(),        % Only for use in proxy
    remote_target :: nksip:uri(),
    route_set :: [nksip:uri()],
    blocked_route_set :: boolean(),
    early :: boolean(),
    secure :: boolean(),
    caller_tag :: nksip:tag(),
    invite :: nksip:invite(),
    subscriptions = [] :: [#subscription{}],
    supported = [] :: [nksip:token()],
    meta = [] :: nksip:optslist()
}).



-record(sdp_m, {
    media :: binary(),                  % <<"audio">>, ...
    port = 0 :: inet:port_number(),
    nports = 1 :: integer(),
    proto = <<"RTP/AVP">> :: binary(),      
    fmt = [] :: [binary()],             % <<"0">>, <<"101">> ...
    info :: binary(),
    connect :: nksip_sdp:address(),
    bandwidth = [] :: [binary()],
    key :: binary(),
    attributes = [] :: [nksip_sdp:sdp_a()]
}).

-record(sdp, {
    sdp_vsn = <<"0">> :: binary(),
    user = <<"-">> :: binary(),
    id = 0 :: non_neg_integer(), 
    vsn = 0 :: non_neg_integer(), 
    address = {<<"IN">>, <<"IP4">>, <<"0.0.0.0">>} :: nksip_sdp:address(),
    session = <<"nksip">> :: binary(), 
    info :: binary(),
    uri :: binary(),
    email :: binary(),
    phone :: binary(),
    connect :: nksip_sdp:address(),
    bandwidth = [] :: [binary()],
    time = [] :: [nksip_sdp:sdp_t()],
    zone :: binary(),
    key :: binary(),
    attributes = [] :: [nksip_sdp:sdp_a()],
    medias = [] :: [nksip_sdp:sdp_m()]
}).


-endif.


%% ===================================================================
%% Macros
%% ===================================================================

% Thks to http://rustyklophaus.com/articles/20110209-BeautifulErlangTiming.html
-ifndef(TIMEON).
-define(TIMEON, 
    erlang:put(debug_timer, [now()|
                                case erlang:get(debug_timer) == undefined of 
                                    true -> []; 
                                    false -> erlang:get(debug_timer) end])).
-define(TIMEOFF(Var), 
    io:format("~s :: ~10.2f ms : ~p~n", [
        string:copies(" ", length(erlang:get(debug_timer))), 
        (timer:now_diff(now(), hd(erlang:get(debug_timer)))/1000), Var
    ]), 
    erlang:put(debug_timer, tl(erlang:get(debug_timer)))).
-endif.

-ifndef(P).
-define(P(S,P), io:format(S++"\n", P)).
-define(P(S), ?P(S, [])).
-endif.

-ifndef(I).
-define(I(S,P), lager:info(S++"\n", P)).
-define(I(S), ?I(S, [])).
-endif.

