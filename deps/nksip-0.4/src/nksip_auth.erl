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

%% @doc Authentication management module.

-module(nksip_auth).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([authorize_data/2, realms/1, make_ha1/3]).
-export([make_request/3, make_response/2, get_authentication/2]).

-include("nksip.hrl").
-include("nksip_call.hrl").


-define(RESP_WWW, (<<"www-authenticate">>)).
-define(RESP_PROXY, (<<"proxy-authenticate">>)).
-define(REQ_WWW,  (<<"authorization">>)).
-define(REQ_PROXY,  (<<"proxy-authorization">>)).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Extracts all the realms present in <i>WWW-Authenticate</i> or
%% <i>Proxy-Authenticate</i> headers from a response.
-spec realms(nksip:response()|nksip:handle()) ->
    [Realm::binary()].

realms(#sipmsg{headers=Headers}) ->
    get_realms(Headers, []);

realms(RespId) ->
    Hd1 = case nksip_response:header(RespId, ?RESP_WWW) of
        {ok, WWW} when is_list(WWW) -> [{?RESP_WWW, Data} || Data <- WWW];
        _ -> []
    end,
    Hd2 = case nksip_response:header(RespId, ?RESP_PROXY) of
        {ok, Proxy} when is_list(Proxy) -> [{?RESP_PROXY, Data} || Data <- Proxy];
        _ -> []
    end,
    get_realms(Hd1++Hd2, []).


%% @private
get_realms([{Name, Value}|Rest], Acc) ->
    if
        Name==?RESP_WWW; Name==?RESP_PROXY ->
            case parse_header(Value) of
                {error, _} -> get_realms(Rest, Acc);
                AuthData -> get_realms(Rest, [nksip_lib:get_value(realm, AuthData)|Acc])
            end;
        true ->
            get_realms(Rest, Acc)
    end;
get_realms([], Acc) ->
    lists:usort(Acc).


%% @doc Generates a password hash to use in NkSIP authentication.
%% In order to avoid storing the user's passwords in clear text, you can generate 
%% a `hash' (fom `User', `Pass' and `Realm') and store and use it in
%% {@link nksip_sipapp:get_user_pass/3} instead of the real password.
-spec make_ha1(binary()|string(), binary()|string(), binary()|string()) -> 
    binary().

make_ha1(User, Pass, Realm) ->
    % <<"HA1!">> is a custom header to be detected as a ha1 hash
    <<"HA1!", (md5(list_to_binary([User, $:, Realm, $:, Pass])))/binary>>.


%% @doc Adds an <i>Authorization</i> or <i>Proxy-Authorization</i> header 
%% for a request after receiving a 401 or 407 response.
%% CSeq must be updated after calling this function.
%%
%% Recognized options are `pass', `user', `cnonce' and `nc'.
-spec make_request(Req::nksip:request(), Resp::nksip:response(), nksip:optslist()) ->
    {ok, nksip:request()} | {error, Error}
    when Error :: invalid_auth_header | unknown_nonce | no_pass.

make_request(Req, #sipmsg{headers=RespHeaders}, Opts) ->
    #sipmsg{
        class = {req, Method},
        % app_id = AppId,
        ruri = RUri, 
        from = {#uri{user=User}, _},
        headers = ReqHeaders
    } = Req,
    try
        ReqAuthHeaders = nksip_lib:extract(ReqHeaders, [?REQ_WWW, ?REQ_PROXY]),
        ReqNOnces = [
            case parse_header(ReqAuthHeader) of
                {error, _} -> throw(invalid_auth_header);
                ParsedReqHeader -> nksip_lib:get_value(nonce, ParsedReqHeader)
            end 
            || {_, ReqAuthHeader} <- ReqAuthHeaders
        ],
        RespAuthHeaders = nksip_lib:extract(RespHeaders, [?RESP_WWW, ?RESP_PROXY]),
        case RespAuthHeaders of
            [{RespName, RespData}] -> ok;
            _ -> RespName = RespData = throw(invalid_auth_header)
        end,
        case parse_header(RespData) of
            {error, _} -> AuthHeaderData = throw(invalid_auth_header);
            AuthHeaderData -> ok
        end,
        RespNOnce = nksip_lib:get_value(nonce, AuthHeaderData),
        case lists:member(RespNOnce, ReqNOnces) of
            true -> throw(unknown_nonce);
            false -> ok
        end,
        case nksip_lib:get_value(passes, Opts) of
            undefined -> 
                ReqOpts = throw(no_pass);
            Passes ->
                ReqOpts = [
                    {method, Method}, 
                    {ruri, RUri}, 
                    {user, nksip_lib:get_binary(user, Opts, User)}, 
                    {passes, Passes} 
                    | Opts
                ]
        end,
        case make_auth_request(AuthHeaderData, ReqOpts) of
            error -> 
                throw(invalid_auth_header);
            {ok, ReqData} ->
                ReqName = case RespName of
                    ?RESP_WWW -> ?REQ_WWW;
                    ?RESP_PROXY -> ?REQ_PROXY
                end,
                ReqHeaders1 = [{ReqName, ReqData}|ReqHeaders],
                {ok, Req#sipmsg{headers=ReqHeaders1}}
        end
    catch
        throw:Error -> {error, Error}
    end.


%% @doc Generates a <i>WWW-Authenticate</i> or <i>Proxy-Authenticate</i> header
%% in response to a request.
%% Use this function to answer to a request with a 401 or 407 response.
%%
%% A new `nonce' will be generated to be used by the client in its response, 
%% but will expire after the time configured in global parameter `nonce_timeout'.
%%
-spec make_response(binary(), nksip:request()) ->
    binary().

make_response(Realm, Req) ->
    #sipmsg{
        app_id = AppId, 
        call_id = CallId,
        transport=#transport{remote_ip=Ip, remote_port=Port}
    } = Req,
    Nonce = nksip_lib:luid(),
    Config = nksip_sipapp_srv:config(AppId),
    Timeout = nksip_lib:get_value(nonce_timeout, Config),
    put_nonce(AppId, CallId, Nonce, {Ip, Port}, Timeout),
    Opaque = nksip_lib:hash(AppId),
    list_to_binary([
        "Digest realm=\"", Realm, "\", nonce=\"", Nonce, "\", "
        "algorithm=MD5, qop=\"auth\", opaque=\"", Opaque, "\""
    ]).


%% @doc Extracts digest authentication information from a incoming request.
%% The response can include:
%% <ul>
%%    <li>`{{digest, Realm}, true}': there is at least one valid user authenticated
%%        with this `Realm'.</li>
%%    <li>`{{digest, Realm}, invalid}': there is at least one user offering
%%        an invalid authentication header for this `Realm'</li>
%%    <li>`{{digest, Realm}, false}': there is at least one user offering 
%%        an authentication header for this `Realm', but all of them have
%%        failed the authentication.</li>
%% </ul>
%%
-spec authorize_data(nksip:request(), nksip_call:call()) ->
    [Authorized] 
    when Authorized :: {{digest, Realm::binary()}, true|invalid|false}.

authorize_data(Req, #call{app_id=AppId}=Call) ->
    PassFun = fun(User, Realm) ->
        Args = [User, Realm, Req, Call],
        case AppId:nkcb_call(sip_get_user_pass, Args, AppId) of
            {ok, Reply} -> ok;
            error -> Reply = false
        end,
        ?call_debug("UAS calling get_user_pass(~p, ~p, Req, Call): ~p", 
                    [User, Realm, Reply]),
        Reply
    end,
    get_authentication(Req, PassFun).


%% ===================================================================
%% Private
%% ===================================================================


%% @private Extracts digest authentication information from a incoming request.
%% `Fun' will be called when a password for a pair user, realm is needed.
%% It can return `true' (accepts the request with any password), `false' 
%% (doesn't accept the request) or a `binary()' pasword or hash.
%%
-spec get_authentication(nksip:request(), function()) -> 
    [Authorized] 
    when Authorized :: {{digest, Realm::binary()}, true|invalid|false}.

get_authentication(Req, PassFun) ->
    Fun = fun({Res, _User, Realm}, Acc) ->
        case lists:keyfind(Realm, 1, Acc) of
            false -> [{Realm, Res}|Acc];
            {Realm, true} -> Acc;
            {Realm, _} when Res == true -> nksip_lib:store_value(Realm, Res, Acc);
            {Realm, invalid} -> Acc;
            {Realm, _} when Res == invalid -> nksip_lib:store_value(Realm, Res, Acc);
            {Realm, false} -> Acc
        end
    end,
    [{{digest, Realm}, Res} || 
        {Realm, Res} <- lists:foldl(Fun, [], check_digest(Req, PassFun))].


%% @private Finds auth headers in request, and for each one extracts user and 
%% realm, calling `get_user_pass/3' callback to check if it is correct.
-spec check_digest(Req::nksip:request(), function()) ->
    [{true | invalid | false, User::binary(), Realm::binary()}].

check_digest(#sipmsg{headers=Headers}=Req, Fun) ->
    check_digest(Headers, Req, Fun, []).


%% @private
check_digest([], _Req, _Fun, Acc) ->
    Acc;

check_digest([{Name, Data}|Rest], Req, Fun, Acc) 
                when Name==?REQ_WWW; Name==?REQ_PROXY ->
    case parse_header(Data) of
        {error, _} ->
            check_digest(Rest, Req, Fun, Acc);
        AuthData ->
            Resp = nksip_lib:get_value(response, AuthData),
            User = nksip_lib:get_binary(username, AuthData),
            Realm = nksip_lib:get_binary(realm, AuthData),
            Result = case Fun(User, Realm) of
                true -> true;
                false -> false;
                Pass -> check_auth_header(AuthData, Resp, User, Realm, Pass, Req)
            end,
            check_digest(Rest, Req, Fun, [{Result, User, Realm}|Acc])
    end;
    
check_digest([_|Rest], Req, Fun, Acc) ->
    check_digest(Rest, Req, Fun, Acc).


%% @private Generates a Authorization or Proxy-Authorization header
-spec make_auth_request(nksip:optslist(), nksip:optslist()) ->
    {ok, binary()} | error.

make_auth_request(AuthHeaderData, UserOpts) ->
    QOP = nksip_lib:get_value(qop, AuthHeaderData, []),
    Algorithm = nksip_lib:get_value(algorithm, AuthHeaderData, 'MD5'),
    case Algorithm=='MD5' andalso (QOP==[] orelse lists:member(auth, QOP)) of
        true ->
            case nksip_lib:get_binary(cnonce, UserOpts) of
                <<>> -> CNonce = nksip_lib:luid();
                CNonce -> ok
            end,
            Nonce = nksip_lib:get_binary(nonce, AuthHeaderData, <<>>),  
            Nc = nksip_lib:msg("~8.16.0B", [nksip_lib:get_integer(nc, UserOpts, 1)]),
            Realm = nksip_lib:get_binary(realm, AuthHeaderData, <<>>),
            Passes = nksip_lib:get_value(passes, UserOpts, []),
            Pass = case nksip_lib:get_value(Realm, Passes) of
                undefined -> nksip_lib:get_value(<<>>, Passes, <<>>);
                RealmPass -> RealmPass
            end,
            User = nksip_lib:get_binary(user, UserOpts),
            case Pass of
                <<"HA1!", HA1/binary>> -> ok; %_Pass = <<"hash">>;
                _ -> <<"HA1!", HA1/binary>> = make_ha1(User, Pass, Realm)
            end,
            Uri = nksip_unparse:uri(nksip_lib:get_value(ruri, UserOpts)),
            Method1 = case nksip_lib:get_value(method, UserOpts) of
                'ACK' -> 'INVITE';
                Method -> Method
            end,
            Resp = make_auth_response(QOP, Method1, Uri, HA1, Nonce, CNonce, Nc),
            % ?P("AUTH REQUEST: ~p, ~p, ~p: ~p", [User, _Pass, Realm, Resp]),
            % ?P("AUTH REQUEST: ~p, ~p, ~p, ~p, ~p, ~p, ~p", 
            %               [QOP,  Method1, Uri, HA1, Nonce, CNonce, Nc]),
            Raw = [
                "Digest username=\"", User, "\", realm=\"", Realm, 
                "\", nonce=\"", Nonce, "\", uri=\"", Uri, "\", response=\"", Resp, 
                "\", algorithm=MD5",
                case QOP of
                    [] -> [];
                    _ -> [", qop=auth, cnonce=\"", CNonce, "\", nc=", Nc]
                end,
                case nksip_lib:get_value(opaque, AuthHeaderData) of
                    undefined -> [];
                    Opaque -> [", opaque=\"", Opaque, "\""]
                end
            ],
            {ok, list_to_binary(Raw)};
        false ->
            error
    end.


%% @private
-spec check_auth_header(nksip:optslist(), binary(), binary(), binary(), 
                            binary(), nksip:request()) -> 
    true | invalid | false.

check_auth_header(AuthHeader, Resp, User, Realm, Pass, Req) ->
    #sipmsg{
        class = {req, Method},
        app_id = AppId,
        call_id = CallId,
        transport = #transport{remote_ip=Ip, remote_port=Port}
    } = Req,
    case
        nksip_lib:get_value(scheme, AuthHeader) /= digest orelse
        nksip_lib:get_value(qop, AuthHeader) /= [auth] orelse
        nksip_lib:get_value(algorithm, AuthHeader, 'MD5') /= 'MD5'
    of
        true ->
            ?notice(AppId, CallId, 
                    "received invalid parameters in Authorization Header: ~p", 
                    [AuthHeader]),
            invalid;
        false ->
            % Should we check the uri in the authdata matches the ruri of the request?
            Uri = nksip_lib:get_value(uri, AuthHeader),
            Nonce = nksip_lib:get_value(nonce, AuthHeader),
            Found = get_nonce(AppId, CallId, Nonce),
            if
                Found==not_found ->
                    Opaque = nksip_lib:get_value(opaque, AuthHeader),
                    case nksip_lib:hash(AppId) of
                        Opaque -> ?call_notice("received invalid nonce", []);
                        _ -> ok
                    end,
                    invalid;
                Method=='ACK' orelse Found=={Ip, Port} ->
                    CNonce = nksip_lib:get_value(cnonce, AuthHeader),
                    Nc = nksip_lib:get_value(nc, AuthHeader),
                    case nksip_lib:to_binary(Pass) of
                        <<"HA1!", HA1/binary>> -> ok;
                        _ -> <<"HA1!", HA1/binary>> = make_ha1(User, Pass, Realm)
                    end,
                    QOP = [auth],
                    Method1 = case Method of
                        'ACK' -> 'INVITE';
                        _ -> Method
                    end,
                    ValidResp = make_auth_response(QOP, Method1, Uri, HA1, 
                                                        Nonce, CNonce, Nc),
                    % ?P("AUTH RESP: ~p, ~p, ~p: ~p vs ~p", 
                    %       [User, Pass, Realm, Resp, ValidResp]),
                    % ?P("AUTH RESP: ~p, ~p, ~p, ~p, ~p, ~p, ~p", 
                    %       [QOP, Method1, Uri, HA1, Nonce, CNonce, Nc]),
                    Resp == ValidResp;
                true ->
                    ?call_warning("received nonce from different Ip or Port", []),
                    false
            end
    end.


%% ===================================================================
%% Internal
%% ===================================================================

% %% @private
% get_passes([], Acc) ->
%     lists:reverse(Acc);

% get_passes([Opt|Rest], Acc) ->
%     Acc1 = case Opt of
%         {passes, PassList} -> PassList++Acc;
%         {pass, {P, R}} -> [{nksip_lib:to_binary(P), nksip_lib:to_binary(R)}|Acc];
%         {pass, P} -> [{nksip_lib:to_binary(P), <<>>}|Acc];
%         _ -> Acc
%     end,
%     get_passes(Rest, Acc1).

%% @private Generates a standard SIP Digest Response
-spec make_auth_response([atom()], nksip:method(), binary(), binary(), 
                            binary(), binary(), binary()) -> binary().

make_auth_response(QOP, Method, BinUri, HA1bin, Nonce, CNonce, Nc) ->
    HA1 = nksip_lib:hex(HA1bin),
    HA2_base = <<(nksip_lib:to_binary(Method))/binary, ":", BinUri/binary>>,
    HA2 = nksip_lib:hex(md5(HA2_base)),
    case QOP of
        [] ->
            nksip_lib:hex(md5(list_to_binary([HA1, $:, Nonce, $:, HA2])));
        _ ->    
            case lists:member(auth, QOP) of
                true ->
                    nksip_lib:hex(md5(list_to_binary(
                        [HA1, $:, Nonce, $:, Nc, $:, CNonce, ":auth:", HA2])));
                _ ->
                    <<>>
            end 
    end.


%% @private
-ifdef(old_crypto_hash).
md5(Term) -> crypto:md5(Term).
-else.
md5(Term) -> crypto:hash(md5, Term).
-endif.


% %% @private Extracts password from user options.
% %% The first matching realm is used, otherwise first password without realm
% -spec get_pass([{binary(), binary()}], binary(), binary()) -> 
%     Pass::binary().

% get_pass([], _Realm, FirstPass) -> 
%     FirstPass;
% get_pass([{<<>>, FirstPass}|Rest], Realm, <<>>) -> 
%     get_pass(Rest, Realm, FirstPass);
% get_pass([{Realm, Pass}|_], Realm, _FirstPass) -> 
%     Pass;
% get_pass([_|Rest], Realm, FirstPass) -> 
%     get_pass(Rest, Realm, FirstPass).


%% @private
get_nonce(AppId, CallId, Nonce) ->
    nksip_store:get({nksip_auth_nonce, AppId, CallId, Nonce}).

%% @private
put_nonce(AppId, CallId, Nonce, Term, Timeout) ->
    nksip_store:put({nksip_auth_nonce, AppId, CallId, Nonce}, Term,
                    [{ttl, Timeout}]).


%% @private
-spec parse_header(string() | binary()) ->
    nksip:optslist() | {error, term()}.

parse_header(Bin) when is_binary(Bin) ->
    parse_header(binary_to_list(Bin));

parse_header(List) when is_list(List) ->
    case parse_header_scheme(strip(List), []) of
        {error, Error} -> {error, Error};
        Opts -> lists:reverse(Opts)
    end.


%% @private 
parse_header_scheme([], _Acc) ->
    {error, ?LINE};

parse_header_scheme([Ch|Rest], Acc) when Ch==32; Ch==9; Ch==13 ->
    case Acc of
        [] -> 
            error;
        _ -> 
            Scheme = case string:to_lower(lists:reverse(Acc)) of
                "digest" -> digest;
                "basic" -> basic;
                Other -> list_to_binary(Other)
            end,
            parse_header_key(strip(Rest), [], [{scheme, Scheme}])
    end;

parse_header_scheme([Ch|Rest], Acc) ->
    parse_header_scheme(Rest, [Ch|Acc]).


%% @private
parse_header_key([], _Acc, _Data) ->
    {error, ?LINE};

parse_header_key([$=|Rest], Acc, Data) ->
    Key = lists:reverse(Acc),
    parse_header_value(strip(Rest), Key, [], false, Data);

parse_header_key([Ch|Rest], Acc, Data) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [$=|_]=Rest1 -> parse_header_key(Rest1, Acc, Data);
        _ -> {error, ?LINE}
    end;

parse_header_key([Ch|Rest], Acc, Data) ->
    parse_header_key(Rest, [Ch|Acc], Data).


%% @private
parse_header_value([], Key, Acc, Quoted, Data) ->
    case Acc==[] orelse Quoted of
        true -> {error, ?LINE};
        false -> [parse_header_value_check(Key, lists:reverse(Acc))|Data]
    end;

parse_header_value([92, $"|Rest], Key, Acc, true, Data) ->
    parse_header_value(Rest, Key, [$", 92|Acc], true, Data);

parse_header_value([$"|Rest], Key, Acc, Quoted, Data) ->
    parse_header_value(Rest, Key, [$"|Acc], not Quoted, Data);

parse_header_value([$,|Rest], Key, Acc, false, Data) ->
    case Acc of
        [] -> 
            {error, ?LINE};
        _ ->
            Data1 = [parse_header_value_check(Key, lists:reverse(Acc))|Data],
            parse_header_key(strip(Rest), [], Data1)
    end;

parse_header_value([Ch|Rest], Key, Acc, false, Data) when Ch==32; Ch==9; Ch==13 ->
    case strip(Rest) of
        [] -> parse_header_value([], Key, Acc, false, Data);
        [$,|_]=Rest1 -> parse_header_value(Rest1, Key, Acc, false, Data);
        R -> {error, ?LINE, R}
    end;

parse_header_value([Ch|Rest], Key, Acc, Quoted, Data) ->
    parse_header_value(Rest, Key, [Ch|Acc], Quoted, Data).


%% @private
parse_header_value_check(Key, Val) ->
    Val1 = string:strip(Val, both, $"),
    case string:to_lower(Key) of
        "realm" -> {realm, list_to_binary(string:to_lower(Val1))};
        "nonce" -> {nonce, list_to_binary(Val1)};
        "opaque" -> {opaque, list_to_binary(Val1)};
        "username" -> {username, list_to_binary(Val1)};
        "uri" -> {uri, list_to_binary(Val1)};
        "response" -> {response, list_to_binary(Val1)};
        "cnonce" -> {cnonce, list_to_binary(Val1)};
        "nc" -> {nc, list_to_binary(Val1)};
        "algorithm" -> 
            {algorithm, 
                case string:to_lower(Val1) of
                    "md5" -> 'MD5';
                    A0 -> list_to_binary(A0) 
                end};
        "qop" -> 
            QOP = [
                case string:to_lower(QOPToken) of
                    "auth" -> auth;
                    "auth-int" -> 'auth-int';
                    _ -> list_to_binary(QOPToken)
                end
                || QOPToken <- string:tokens(Val1, " ,")
            ],
            {qop, QOP};
        Other ->
            {list_to_binary(Other), list_to_binary(Val1)}
    end.


%% @private
strip([32|Rest]) -> strip(Rest);
strip([13|Rest]) -> strip(Rest);
strip([10|Rest]) -> strip(Rest);
strip([9|Rest]) -> strip(Rest);
strip(Rest) -> Rest.



%% ===================================================================
%% EUnit tests
%% ===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ha1_test() ->
    HA1 = <<132,147,251,197,59,165,130,251,76,4,76,69,107,220,64,235>>,
    ?assertMatch(<<"HA1!", HA1/binary>>, make_ha1("user", "pass", "realm")),
    ?assertMatch(
        <<"194370e184088fb011b140d770936009">>,
        make_auth_response([], 'INVITE', <<"test@test.com">>, HA1, 
                            <<"gfedcba">>, <<"abcdefg">>, 1)),
    ?assertMatch(
        <<"788a70e3b5d371dc5f9dee5e59bb80cd">>,
        make_auth_response([other, auth], 'INVITE', <<"test@test.com">>, HA1, 
                            <<"gfedcba">>, <<"abcdefg">>, 1)),
    ?assertMatch(<<>>, make_auth_response([other], 'INVITE', <<"any">>, HA1, <<"any">>,
                 <<"any">>, 1)),
    [
        {scheme,digest},
        {realm,<<"av">>},
        {<<"b">>, <<"1, 2\\\"bc\\\" ">>},
        {qop,[auth,'auth-int',<<"other">>]}
    ] = 
        parse_header("   Digest   realm   =   AV,b=\"1, 2\\\"bc\\\" \", "
                      "qop = \"auth,  auth-int,other\"").
-endif.



