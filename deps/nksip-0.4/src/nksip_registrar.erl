%% -------------------------------------------------------------------
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

%% @doc NkSIP Registrar Server.
%%
%% This module implements a full registrar implementation according to RFC3261. 
%% "Path" is also supported according to RFC3327.
%% By default, it uses the RAM-only built-in store, but any SipApp can implement 
%% {@link nksip_sipapp:registrar_store/3} callback use any external database.
%% Each started SipApp maintains its fully independent set of registrations.
%%
%% When a new REGISTER request arrives at a SipApp, and if you order to `process' 
%% the request in {@link nksip_sipapp:route/6} callback, NkSIP will try to call 
%% {@link nksip_sipapp:register/3} callback if it is defined. 
%% If it is not defined, but `registrar' option was present in the SipApp's 
%% startup config, NkSIP will process the request automatically. 
%% It will check if `From' and `To' headers differ, and call {@link request/1} if 
%% everything is ok. 
%% If you implement {@link nksip_sipapp:register/3} to customize the registration 
%% process you should call {@link request/1} directly.
%%
%% Use {@link find/4} or {@link qfind/4} to search for a specific registration's 
%% contacts, and {@link is_registered/1} to check if the `Request-URI' of a 
%% specific request is registered.

-module(nksip_registrar).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("nksip.hrl").

-export([find/2, find/4, qfind/2, qfind/4, get_info/4, delete/4, clear/1]).
-export([is_registered/1, request/1]).
-export([internal_get_all/0, internal_clear/0, internal_print_all/0]).

-export_type([reg_contact/0, index/0]).

-define(TIMEOUT, 15000).

-define(AES_IV, <<"12345678abcdefgh">>).


%% ===================================================================
%% Types and records
%% ===================================================================

-type reg_contact() :: #reg_contact{}.

-type index() :: 
    {
        Scheme::sip|sips,
        Proto::nksip:protocol(), 
        User::binary(),
        Domain::binary(), 
        Port::inet:port_number()
    } 
    |
    {ob, Instance::binary(), RegId::binary()}.

-type times() :: {integer(), integer(), integer(), integer(), integer()}.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Gets all current registered contacts for an AOR.
-spec find(term()|nksip:app_id(), nksip:aor() | nksip:uri()) ->
    [nksip:uri()].

find(App, {Scheme, User, Domain}) ->
    find(App, Scheme, User, Domain);

find(App, #uri{scheme=Scheme, user=User, domain=Domain, opts=Opts}) ->
    case nksip:find_app(App) of
        {ok, AppId} ->
            case lists:member(<<"gr">>, Opts) of
                true -> 
                    % It is probably a tmp GRUU
                    case catch decrypt(User) of
                        Tmp when is_binary(Tmp) ->
                            {{Scheme1, User1, Domain1}, InstId, Pos} = binary_to_term(Tmp),
                            [
                                make_contact(Reg) 
                                || #reg_contact{instance_id=InstId1, min_tmp_pos=Min}=Reg 
                                <- get_info(AppId, Scheme1, User1, Domain1), 
                                InstId1==InstId, Pos>=Min
                            ];
                        _ ->
                            ?notice(AppId, <<>>, "private GRUU not recognized: ~p", [User]),
                            find(AppId, Scheme, User, Domain)
                    end;
                false ->
                    case nksip_lib:get_value(<<"gr">>, Opts) of
                        undefined -> 
                            find(AppId, Scheme, User, Domain);
                        InstId ->
                            [
                                make_contact(Reg) 
                                || #reg_contact{instance_id=InstId1}=Reg 
                                <- get_info(AppId, Scheme, User, Domain), InstId1==InstId
                            ]
                    end
            end;
        _ ->
            []
    end.



%% @doc Gets all current registered contacts for an AOR.
-spec find(term()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    [nksip:uri()].

find(App, Scheme, User, Domain) ->
    [make_contact(Reg) || Reg <- get_info(App, Scheme, User, Domain)].


%% @private Gets all current stored info for an AOR.
-spec get_info(term()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    [#reg_contact{}].

get_info(App, Scheme, User, Domain) ->
    case nksip:find_app(App) of
        {ok, AppId} ->
            AOR = {Scheme, nksip_lib:to_binary(User), nksip_lib:to_binary(Domain)},
            case catch callback_get(AppId, AOR) of
                {ok, RegContacts} -> RegContacts;
                _ -> []
            end;
        _ ->
            []
    end.


%% @doc Gets all current registered contacts for an AOR, aggregated on Q values.
%% You can use this function to generate a parallel and/o serial proxy request.
-spec qfind(term()|nksip:app_id(), AOR::nksip:aor()) ->
    nksip:uri_set().

qfind(App, {Scheme, User, Domain}) ->
    qfind(App, Scheme, User, Domain).


%% @doc Gets all current registered contacts for an AOR, aggregated on Q values.
%% You can use this function to generate a parallel and/o serial proxy request.
-spec qfind(term()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    nksip:uri_set().

qfind(App, Scheme, User, Domain) ->
    All = [
        {1/Q, Updated, make_contact(Reg)} || 
        #reg_contact{q=Q, updated=Updated} = Reg 
        <- get_info(App, Scheme, User, Domain)
    ],
    do_qfind(lists:sort(All), []).



%% @doc Deletes all registered contacts for an AOR (<i>Address-Of-Record</i>).
-spec delete(term()|nksip:app_id(), nksip:scheme(), binary(), binary()) ->
    ok | not_found | callback_error.

delete(App, Scheme, User, Domain) ->
    case nksip:find_app(App) of
        {ok, AppId} ->
            AOR = {
                nksip_parse:scheme(Scheme), 
                nksip_lib:to_binary(User), 
                nksip_lib:to_binary(Domain)
            },
            case callback(AppId, {del, AOR}) of
                ok -> ok;
                not_found -> not_found;
                _ -> callback_error
            end;
        _ ->
            not_found
    end.


%% @doc Finds if a request has a <i>From</i> that has been already registered
%% from the same transport, ip and port, or have a registered <i>Contact</i>
%% having the same received transport, ip and port.
-spec is_registered(Req::nksip:request()) ->
    boolean().

is_registered(#sipmsg{class={req, 'REGISTER'}}) ->
    false;

is_registered(#sipmsg{
                app_id = AppId, 
                from = {#uri{scheme=Scheme, user=User, domain=Domain}, _},
                transport=Transport
            }) ->
    case catch callback_get(AppId, {Scheme, User, Domain}) of
        {ok, Regs} -> is_registered(Regs, Transport);
        _ -> false
    end.


%% @doc Process a REGISTER request. 
%% Can return:
%% <ul>
%%  <li>`unsupported_uri_scheme': if R-RUI scheme is not `sip' or `sips'.</li>
%%  <li>`invalid_request': if the request is not valid for any reason.</li>
%%  <li>`interval_too_brief': if <i>Expires</i> is lower than the minimum configured
%%       registration time (defined in `registrar_min_time' global parameter).</li>
%% </ul>
%%
%% If <i>Expires</i> is 0, the indicated <i>Contact</i> will be unregistered.
%% If <i>Contact</i> header is `*', all previous contacts will be unregistered.
%%
%% The requested <i>Contact</i> will replace a previous registration if it has 
%% the same `reg-id' and `+sip_instace' values, or has the same transport scheme,
%% protocol, user, domain and port.
%%
%% If the request is successful, a 200-code `nksip:sipreply()' is returned,
%% including one or more <i>Contact</i> headers (for all of the current registered
%% contacts), <i>Date</i> and <i>Allow</i> headers.
-spec request(nksip:request()) ->
    nksip:sipreply().

request(#sipmsg{app_id=AppId, to={To, _}}=Req) ->
    try
        case nksip_outbound:registrar(Req, []) of
            {ok, Req1, Opts1} -> ok;
            {error, OutError} -> Req1 = Opts1 = throw(OutError)
        end,
        Opts2 = check_gruu(Req, Opts1),
        process(Req1, Opts2),
        {ok, Regs} = callback_get(AppId, aor(To)),
        Contacts1 = [Contact || #reg_contact{contact=Contact} <- Regs],
        ObReq = case 
            lists:member({registrar_outbound, true}, Opts2) andalso
            [true || #reg_contact{index={ob, _, _}} <- Regs] 
        of
            [_|_] -> [{require, <<"outbound">>}];
            _ -> []
        end,
        {ok, [{contact, Contacts1}, date, allow, supported | ObReq]}
    catch
        throw:Throw -> Throw
    end.
 

%% @doc Clear all stored records by a SipApp's core.
-spec clear(term()|nksip:app_id()) -> 
    ok | callback_error | sipapp_not_found.

clear(App) -> 
    case nksip:find_app(App) of
        {ok, AppId} ->
            case callback(AppId, del_all) of
                ok -> ok;
                _ -> callback_error
            end;
        _ ->
            sipapp_not_found
    end.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
-spec do_qfind([{float(), float(), nksip:uri()}], list()) ->
    [[nksip:uri()]].

do_qfind([{Q, _, Contact}|Rest], [{Q, CList}|Acc]) ->
    do_qfind(Rest, [{Q, [Contact|CList]}|Acc]);

do_qfind([{Q, _, Contact}|Rest], Acc) ->
    do_qfind(Rest, [{Q, [Contact]}|Acc]);

do_qfind([], Acc) ->
    [CList || {_Q, CList} <- Acc].


%% @private
-spec is_registered([nksip:uri()], nksip_transport:transport()) ->
    boolean().

is_registered([], _) ->
    false;

is_registered([
                #reg_contact{
                    transport = #transport{proto=Proto, remote_ip=Ip, remote_port=Port}}
                | _ ], 
                #transport{proto=Proto, remote_ip=Ip, remote_port=Port}) ->
    true;

% If a TCP es registered, the transport source port is probably going to be 
% different then the registered, so it is not going to work.
% When outbound is implemented this will be reworked 
is_registered([
                #reg_contact{contact=Contact}|R], 
                #transport{proto=Proto, remote_ip=Ip, remote_port=Port}=Transport) ->
    case nksip_parse:transport(Contact) of
        {Proto, Domain, Port} -> 
            case nksip_lib:to_ip(Domain) of
                {ok, Ip} -> true;
                _ -> is_registered(R, Transport)
            end;
        _ ->
            is_registered(R, Transport)
    end.


%% @private
-spec check_gruu(nksip:request(), nksip_lib:optslist()) ->
    nksip_lib:optslist().

check_gruu(Req, AppOpts) ->
    AppSupp = nksip_lib:get_value(supported, AppOpts, ?SUPPORTED),
    case 
        lists:member(<<"gruu">>, AppSupp) andalso nksip_sipmsg:supported(Req, <<"gruu">>)
    of
        true -> [{registrar_gruu, true}|AppOpts];
        false -> AppOpts
    end.


%% @private
-spec process(nksip:request(), nksip_lib:optslist()) ->
    ok.

process(Req, Opts) ->
    #sipmsg{app_id=AppId, to={#uri{scheme=Scheme}, _}, contacts=Contacts} = Req,
    if
        Scheme==sip; Scheme==sips -> ok;
        true -> throw(unsupported_uri_scheme)
    end,
    {MinTime, MaxTime, DefTime} = AppId:config_registrar_timers(),
    Default = case nksip_sipmsg:field(Req, parsed_expires) of
        D0 when is_integer(D0) -> D0;
        _ -> DefTime
    end,
    Long = nksip_lib:l_timestamp(),
    Times = {
        MinTime, 
        MaxTime,
        Default,
        Long div 1000000,
        Long
    },
    case Contacts of
        [] -> ok;
        [#uri{domain=(<<"*">>)}] when Default==0 -> del_all(Req);
        _ -> update(Req, Times, Opts)
    end.



%% @private
-spec update(nksip:request(), times(), nksip_lib:optslist()) ->
    ok.

update(Req, Times, Opts) ->
    #sipmsg{app_id=AppId, to={To, _}, contacts=Contacts} = Req,
    {_, _, Default, Now, _LongNow} = Times,
    check_several_reg_id(Contacts, Default, false),
    Path = case nksip_sipmsg:header(Req, <<"path">>, uris) of
        error -> throw({invalid_request, "Invalid Path"});
        Path0 -> Path0
    end,
    {_, _, _, Now, _LongNow} = Times,
    AOR = aor(To),
    {ok, Regs} = callback_get(AppId, AOR),
    RegContacts0 = [
        RegContact ||
        #reg_contact{expire=Exp} = RegContact <- Regs, 
        Exp > Now
    ],
    RegContacts = update_regcontacts(Contacts, Req, Times, Path, Opts, RegContacts0),
    case RegContacts of
        [] -> 
            case callback(AppId, {del, AOR}) of
                ok -> ok;
                not_found -> ok;
                _ -> throw({internal_error, "Error calling registrar 'del' callback"})
            end;
        _ -> 
            GlobalExpire = lists:max([Exp-Now||#reg_contact{expire=Exp} <- RegContacts]),
            % Set a minimum expiration check of 5 secs
            case callback(AppId, {put, AOR, RegContacts, max(GlobalExpire, 5)}) of
                ok -> ok;
                _ -> throw({internal_error, "Error calling registrar 'put' callback"})
            end
    end,
    ok.


%% @private Extracts from each contact a index, uri, expire time and q
-spec update_regcontacts([#uri{}], nksip:request(), times(), 
                         [nksip:uri()], nksip_lib:optslist(), [reg_contact()]) ->
    [reg_contact()].

update_regcontacts([Contact|Rest], Req, Times, Path, Opts, Acc) ->
    #uri{scheme=Scheme, user=User, domain=Domain, ext_opts=ExtOpts} = Contact,
    #sipmsg{to={To, _}, call_id=CallId, cseq={CSeq, _}, transport=Transp} = Req,
    update_checks(Contact, Req),
    {Min, Max, Default, Now, LongNow} = Times,
    UriExp = case nksip_lib:get_list(<<"expires">>, ExtOpts) of
        [] ->
            Default;
        Exp1List ->
            case catch list_to_integer(Exp1List) of
                ExpInt when is_integer(ExpInt) -> ExpInt;
                _ -> Default
            end
    end,
    Expires = if
        UriExp==0 -> 0;
        UriExp>0, UriExp<3600, UriExp<Min -> throw({interval_too_brief, Min});
        UriExp>Max -> Max;
        true -> UriExp
    end,
    Q = case nksip_lib:get_list(<<"q">>, ExtOpts) of
        [] -> 
            1.0;
        Q0 ->
            case catch list_to_float(Q0) of
                Q1 when is_float(Q1), Q1 > 0 -> 
                    Q1;
                _ -> 
                    case catch list_to_integer(Q0) of
                        Q2 when is_integer(Q2), Q2 > 0 -> Q2 * 1.0;
                        _ -> 1.0
                    end
            end
    end,
    ExpireBin = list_to_binary(integer_to_list(Expires)),
    ExtOpts1 = nksip_lib:store_value(<<"expires">>, ExpireBin, ExtOpts),
    InstId = case nksip_lib:get_value(<<"+sip.instance">>, ExtOpts) of
        undefined -> <<>>;
        Inst0 -> nksip_lib:hash(Inst0)
    end,
    ObProc = nksip_lib:get_value(registrar_outbound, Opts),
    RegId = case nksip_lib:get_value(<<"reg-id">>, ExtOpts) of
        undefined -> <<>>;
        _ when ObProc == undefined -> <<>>;
        _ when ObProc == false -> throw(first_hop_lacks_outbound);
        _ when InstId == <<>> -> <<>>;
        RegId0 -> RegId0
    end,
    Index = case RegId of
        <<>> -> 
            {Proto, Domain, Port} = nksip_parse:transport(Contact),
            {Scheme, Proto, User, Domain, Port};
        _ -> 
            {ob, InstId, RegId}
    end,

    % Find if this contact was already registered under the AOR
    {Base, Acc1} = case lists:keytake(Index, #reg_contact.index, Acc) of
        false when Expires==0 ->
            {undefined, Acc};
        false ->
            {#reg_contact{min_tmp_pos=0, next_tmp_pos=0, meta=[]}, Acc};
        {value, #reg_contact{call_id=CallId, cseq=OldCSeq}, _} when OldCSeq >= CSeq -> 
            throw({invalid_request, "Rejected Old CSeq"});
        {value, _, Acc0} when Expires==0 ->
            {undefined, Acc0};
        {value, #reg_contact{call_id=CallId}=Base0, Acc0} ->
            {Base0, Acc0};
        {value, #reg_contact{next_tmp_pos=Next0}=Base0, Acc0} ->
            % We have changed the Call-ID for this AOR and index, invalidate all
            % temporary GRUUs
            {Base0#reg_contact{min_tmp_pos=Next0}, Acc0}
    end,
    Acc2 = case Base of
        undefined ->
            Acc1;
        #reg_contact{next_tmp_pos=Next} ->
            ExtOpts2 = case 
                InstId /= <<>> andalso RegId == <<>> andalso 
                Expires>0 andalso lists:member({registrar_gruu, true}, Opts)
            of
                true ->
                    case Scheme of
                        sip -> ok;
                        _ -> throw({forbidden, "Invalid Contact"})
                    end,
                    {AORScheme, AORUser, AORDomain} = aor(To),
                    PubUri = #uri{
                        scheme = AORScheme, 
                        user = AORUser, 
                        domain = AORDomain,
                        opts = [{<<"gr">>, InstId}]
                    },
                    Pub = list_to_binary([$", nksip_unparse:ruri(PubUri), $"]),
                    GOpts1 = nksip_lib:store_value(<<"pub-gruu">>, Pub, ExtOpts1),
                    TmpBin = term_to_binary({aor(To), InstId, Next}),
                    TmpGr = encrypt(TmpBin),
                    TmpUri = PubUri#uri{user=TmpGr, opts=[<<"gr">>]},
                    Tmp = list_to_binary([$", nksip_unparse:ruri(TmpUri), $"]),
                    nksip_lib:store_value(<<"temp-gruu">>, Tmp, GOpts1);
                false ->
                    ExtOpts1
            end,
            RegContact = Base#reg_contact{
                index = Index,
                contact = Contact#uri{ext_opts=ExtOpts2},
                updated = LongNow,
                expire = Now + Expires, 
                q = Q,
                call_id = CallId,
                cseq = CSeq,
                transport = Transp,
                path = Path,
                instance_id = InstId,
                reg_id = RegId,
                next_tmp_pos = Next+1
            },
            [RegContact|Acc1]
    end,
    update_regcontacts(Rest, Req, Times, Path, Opts, Acc2);

update_regcontacts([], _Req, _Times, _Path, _Opts, Acc) ->
    lists:reverse(lists:keysort(#reg_contact.updated, Acc)).


%% @private
update_checks(Contact, Req) ->
    #uri{scheme=Scheme, user=User, domain=Domain, opts=Opts} = Contact,
    #sipmsg{to={To, _}} = Req,
    case Domain of
        <<"*">> -> throw(invalid_request);
        _ -> ok
    end,
    case aor(To) of
        {Scheme, User, Domain} -> throw({forbidden, "Invalid Contact"});
        _ -> ok
    end,
    case lists:member(<<"gr">>, Opts) of
        true ->
            case catch decrypt(User) of
                LoopTmp when is_binary(LoopTmp) ->
                    {{LScheme, LUser, LDomain}, _, _} = binary_to_term(LoopTmp),
                    case aor(To) of
                        {LScheme, LUser, LDomain} -> 
                            throw({forbidden, "Invalid Contact"});
                        _ -> 
                            ok
                    end;
                _ ->
                    ok
            end;
        false ->
            ok
    end.


%% @private
check_several_reg_id([], _Expires, _Found) ->
    ok;

check_several_reg_id([#uri{ext_opts=Opts}|Rest], Default, Found) ->
    case nksip_lib:get_value(<<"reg-id">>, Opts) of
        undefined -> 
            check_several_reg_id(Rest, Default, Found);
        _ ->
            Expires = case nksip_lib:get_list(<<"expires">>, Opts) of
                [] ->
                    Default;
                Expires0 ->
                    case catch list_to_integer(Expires0) of
                        Expires1 when is_integer(Expires1) -> Expires1;
                        _ -> Default
                    end
            end,
            case Expires of
                0 -> 
                    check_several_reg_id(Rest, Default, Found);
                _ when Found ->
                    throw({invalid_request, "Several 'reg-id' Options"});
                _ ->
                    check_several_reg_id(Rest, Default, true)
            end
    end.




%% @priavte
aor(#uri{scheme=Scheme, user=User, domain=Domain}) ->
    {Scheme, User, Domain}.


%% @private Generates a contact value including Path
make_contact(#reg_contact{contact=Contact, path=[]}) ->
    Contact;
make_contact(#reg_contact{contact=Contact, path=Path}) ->
    #uri{headers=Headers} = Contact,
    Route1 = nksip_unparse:uri(Path),
    Routes2 = list_to_binary(http_uri:encode(binary_to_list(Route1))),
    Headers1 = [{<<"route">>, Routes2}|Headers],
    Contact#uri{headers=Headers1, ext_opts=[], ext_headers=[]}.


%% @private
-spec del_all(nksip:request()) ->
    ok | not_found.

del_all(Req) ->
    #sipmsg{app_id=AppId, to={To, _}, call_id=CallId, cseq={CSeq, _}} = Req,
    AOR = aor(To),
    {ok, RegContacts} = callback_get(AppId, AOR),
    lists:foreach(
        fun(#reg_contact{call_id=CCallId, cseq=CCSeq}) ->
            if
                CallId /= CCallId -> ok;
                CSeq > CCSeq -> ok;
                true -> throw({invalid_request, "Rejected Old CSeq"})
            end
        end,
        RegContacts),
    case callback(AppId, {del, AOR}) of
        ok -> ok;
        not_found -> not_found;
        _ -> throw({internal_error, "Error calling registrar 'del' callback"})
    end.


%% @private
callback_get(AppId, AOR) -> 
    case callback(AppId, {get, AOR}) of
        List when is_list(List) ->
            lists:foreach(
                fun(Term) ->
                    case Term of
                        #reg_contact{} -> 
                            ok;
                        _ -> 
                            Msg = "Invalid return in registrar 'get' callback",
                            throw({internal_error, Msg})
                    end
                end, List),
            {ok, List};
        _ -> 
            throw({internal_error, "Error calling registrar 'get' callback"})
    end.


%% @private 
-spec callback(nksip:app_id(), term()) ->
    term() | error.

callback(AppId, Op) -> 
    case 
        nksip_sipapp_srv:sipapp_call_wait(AppId, registrar_store, [Op], [Op], ?TIMEOUT)
    of
        not_exported -> 
            {reply, Reply, none} = 
                nksip_sipapp:registrar_store(AppId, Op, none),
            Reply;
        {reply, Reply} -> 
            Reply;
        _ -> 
            error
    end.


%% @private
encrypt(Bin) ->
    <<Key:16/binary, _/binary>> = nksip_config_cache:global_id(),
    base64:encode(crypto:aes_cfb_128_encrypt(Key, ?AES_IV, Bin)).


%% @private
decrypt(Bin) ->
    <<Key:16/binary, _/binary>> = nksip_config_cache:global_id(),
    crypto:aes_cfb_128_decrypt(Key, ?AES_IV, base64:decode(Bin)).



%% ===================================================================
%% Utilities available only using internal store
%% ===================================================================


% @private Get all current registrations. Use it with care.

% -spec internal_get_all() ->
    % [{nksip:aor(), [{App::nksip:app_id(), URI::nksip:uri(),
    %                  Remaining::integer(), Q::float()}]}].

-spec internal_get_all() ->
    [{nksip:app_id(), nksip:aor(), [#reg_contact{}]}].



internal_get_all() ->
    [
        {AppId, AOR, nksip_store:get({nksip_registrar, AppId, AOR}, [])}
        || {AppId, AOR} <- internal_all()
    ].


%% @private
internal_print_all() ->
    Now = nksip_lib:timestamp(),
    Print = fun({AppId, {Scheme, User, Domain}, Regs}) ->
        ?P("\n --- ~p --- ~p:~s@~s ---", [AppId:name(), Scheme, User, Domain]),
        lists:foreach(
            fun(#reg_contact{contact=Contact, expire=Expire, q=Q}) ->
                ?P("    ~s, ~p, ~p", [nksip_unparse:uri(Contact), Expire-Now, Q])
            end, Regs)
    end,
    lists:foreach(Print, internal_get_all()),
    ?P("\n").


%% @private Clear all stored records for all SipApps, only with buil-in database
%% Returns the number of deleted items.
-spec internal_clear() -> 
    integer().

internal_clear() ->
    Fun = fun(AppId, AOR, _Val, Acc) ->
        nksip_store:del({nksip_registrar, AppId, AOR}),
        Acc+1
    end,
    internal_fold(Fun, 0).


%% @private
internal_all() -> 
    internal_fold(fun(AppId, AOR, _Value, Acc) -> [{AppId, AOR}|Acc] end, []).


%% @private
internal_fold(Fun, Acc0) when is_function(Fun, 4) ->
    FoldFun = fun(Key, Value, Acc) ->
        case Key of
            {nksip_registrar, AppId, AOR} -> Fun(AppId, AOR, Value, Acc);
            _ -> Acc
        end
    end,
    nksip_store:fold(FoldFun, Acc0).





