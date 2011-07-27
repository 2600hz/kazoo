%%%-------------------------------------------------------------------
%%% File    : openid_srv.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Manage OpenID associations
%%%
%%% Created : 21 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(openid_srv).

-behaviour(gen_server).

-include("openid.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
 authReqs,
 assocs,
 pending,
 nonces
}).

% Ten-minute login timeout (milliseconds)
-define(PENDING_TIMEOUT, 600000).

% Ten-minute nonce timeout (seconds)
-define(NONCE_TIMEOUT, 600).


%%====================================================================
%% API
%%====================================================================
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Args) ->
    AuthReqs = ets:new(openid_authreqs, [set, private]),
    Assocs = ets:new(openid_assocs, [set, private]),
    Pending = ets:new(openid_pending, [set, private]),
    Nonces = ets:new(openid_nonces, [set, private]),
    ?DBG({?MODULE, running}),
    {ok, #state{authReqs=AuthReqs, assocs=Assocs, pending=Pending, nonces=Nonces}}.



handle_call({prepare, UUID, Identifier}, From, State) ->
  io:format("Request ~p~n", [Identifier]),
    handle_call({prepare, UUID, Identifier, false}, From, State);

handle_call({prepare, UUID, Identifier, Cache}, _From, State) ->
  io:format("Request ~p~n", [Identifier]),
    Reply = case get_authreq(Identifier, Cache, State) of
                {error, Error} ->
                    {error, Error};
                AuthReq ->
                    Assoc = get_assoc(AuthReq, Cache, State),
                    Login = pend_login(UUID, AuthReq, Assoc, State),
                    {ok, Login}
            end,
    {reply, Reply, State};

handle_call({verify, UUID, ReturnTo, Fields}, _From, State) ->
  io:format("Request ~p~n", [ReturnTo]),
    Reply = verify_return(UUID, ReturnTo, Fields, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
  io:format("Request ~p~n", [_Request]),
    Reply = _Request,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({invalidate_pending, UUID}, State) ->
    %?DBG({invalidating, UUID}),
    ets:delete(State#state.pending, UUID),
    {noreply, State};

handle_info({remove_nonce, Nonce}, State) ->
    %?DBG({removing_nonce, Nonce}),
    ets:delete(State#state.nonces, Nonce),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% XXX Todo -- Yadis expiry
get_authreq(Identifier, Cache, State) ->
    case ets:lookup(State#state.authReqs, Identifier) of
        [] -> discover_authreq(Identifier, Cache, State);
        [{_, AuthReq}] ->
            %?DBG({cache_hit, Identifier, AuthReq}),
            AuthReq
    end.


discover_authreq(Identifier, Cache, State) ->
    %?DBG({discovering, Identifier}),
    case openid:discover(Identifier) of
        none -> {error, discovery_failed};
        AuthReq ->
            %?DBG({discovered, AuthReq}),
            case Cache of
                true -> ets:insert(State#state.authReqs, {Identifier, AuthReq});
                _ -> ok
            end,
            AuthReq
    end.


get_assoc(AuthReq, Cache, State) ->
    [OpURL|_] = AuthReq#openid_authreq.opURLs,
    %initiate_assoc(OpURL, Cache, State).
    case ets:lookup(State#state.assocs, OpURL) of
        [] -> initiate_assoc(OpURL, Cache, State);
        [{_, Assoc}] ->
            %?DBG({cache_hit, OpURL, Assoc#assoc.handle}),
            check_expiry(Assoc, AuthReq, Cache, State)
    end.


initiate_assoc(OpURL, Cache, State) ->
    %?DBG({associating, OpURL}),
    case openid:associate(OpURL) of
        {error, Error} -> {error, Error};
        Assoc ->
            %?DBG({associated, Assoc#assoc.handle, Assoc#assoc.expiresIn}),
            case Cache of
                true ->
                     ets:insert(State#state.assocs, {OpURL, Assoc});
                _ -> ok
            end,
            Assoc
    end.

check_expiry(Assoc, AuthReq, Cache, State) ->
    case timer:now_diff(now(), Assoc#openid_assoc.created) of
        X when X > (Assoc#openid_assoc.expiresIn * 1000000) ->
            %?DBG(assoc_expired),
            [OpURL,_] = AuthReq#openid_authreq.opURLs,
            initiate_assoc(OpURL, Cache, State);
        _ ->
            Assoc
    end.


pend_login(UUID, AuthReq, Assoc, State) ->
    %?DBG({pending, UUID, AuthReq, Assoc#assoc.handle}),
    ets:insert(State#state.pending, {UUID, {AuthReq, Assoc}}),
    timer:send_after(?PENDING_TIMEOUT * 1000, {invalidate_pending, UUID}),
    AuthReq#openid_authreq{assoc=Assoc}.


%%--------------------------------------------------------------------
%%% Internal functions -- verification
%%--------------------------------------------------------------------

verify_return(UUID, ReturnTo, Fields, State) ->
    case check_return_id(ReturnTo, Fields) of
        true -> verify_discovered(UUID, Fields, State);
        false -> {error, "Mismatched return URL"}
    end.

verify_discovered(UUID, Fields, State) ->
    GivenHandle = ?GV("openid.assoc_handle", Fields),
    case ets:lookup(State#state.pending, UUID) of
        [] -> {error, "No pending login"};
        [{UUID, {AuthReq, #openid_assoc{handle=GivenHandle}=Assoc}}] ->
            verify_claimed_id(AuthReq, Assoc, Fields, State);
        _OtherAssoc -> {error, "Invalid association handle"}
    end.

verify_claimed_id(AuthReq, Assoc, Fields, State) ->
    case ?GVD("openid.claimed_id", Fields, none) of
        none -> {error, "No claimed identifier"};
        ClaimedID ->
            case AuthReq#openid_authreq.claimedID of
                ClaimedID ->
                    verify_nonce(ClaimedID, Assoc, Fields, State);
                OtherID ->
                    DiscoveredReq = get_authreq(ClaimedID, false, State),
                    [OpURL|_] = AuthReq#openid_authreq.opURLs,
                    case lists:any(fun(X) -> X == OpURL end, DiscoveredReq#openid_authreq.opURLs) of
                        true -> verify_nonce(OtherID, Assoc, Fields, State);
                        false -> {error, "Invalid OP endpoint discovered"}
                    end
            end
    end.


verify_nonce(ClaimedID, Assoc, Fields, State) ->
    case ?GVD("openid.response_nonce", Fields, none) of
        none -> {error, "No response nonce"};
        Nonce ->
            [TimeBit|_Uniqifier] = string:tokens(Nonce, "Z"),
            case catch (lists:map(fun erlang:list_to_integer/1, string:tokens(TimeBit, ":-T"))) of
                [Year, Month, Day, Hour, Minute, Second] ->
                    NonceSecs = calendar:datetime_to_gregorian_seconds({{Year,Month,Day},{Hour,Minute,Second}}),
                    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
                    case NowSecs - NonceSecs of
                        Age when Age > ?NONCE_TIMEOUT ->
                            {error, "Nonce too old"};
                        _ ->
                            verify_nonce2(ClaimedID, Nonce, Assoc, Fields, State)
                    end;
                _ -> {error, "Invalid response nonce"}
            end
    end.


verify_nonce2(ClaimedID, Nonce, Assoc, Fields, State) -> 
    case ets:lookup(State#state.nonces, Nonce) of
        [] -> 
            ets:insert(State#state.nonces, {Nonce, true}),
            timer:send_after(?NONCE_TIMEOUT * 1000, {remove_nonce, Nonce}),
            verify_signature(ClaimedID, Assoc, Fields);
        _ -> {error, "Nonce reused"}
    end.


verify_signature(ClaimedID, Assoc, Fields) ->
    Invalidate = ?GVD("openid.invalidate_handle", Fields, false),
    verify_signature(ClaimedID, Invalidate, Assoc, Fields).

verify_signature(_, _, none, _Fields) ->
    {error, "Direct verification not implemented yet"};
verify_signature(_ClaimedID, false, #openid_assoc{}=Assoc, Fields) ->
    KV = lists:flatten([[Key,$:,?GV("openid." ++ Key, Fields),$\n]
                        || Key <- string:tokens(?GV("openid.signed", Fields), ",")]),
    MAC = Assoc#openid_assoc.mac,
    Sig = crypto:sha_mac(MAC, KV),
    GivenSig = base64:decode(?GV("openid.sig", Fields)),
    
    case Sig =:= GivenSig of
        true -> {ok, ?GV("openid.claimed_id", Fields)};
        false -> {error, "invalid signature"}
    end;
verify_signature(_, _, _, _) ->    
    {error, "Association invalidated, and direct verification not implemented yet"}.


check_return_id(ReturnTo, Fields) ->
    GivenReturn = ?GV("openid.return_to", Fields),
    % XXX Todo -- do this properly, for people who put fancy stuff in return URLs
    ReturnTo == GivenReturn.
