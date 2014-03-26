%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

-module(exmpp_sasl_digest).

-export([
	 mech_new/4,
	 mech_client_new/4,
	 mech_step/2]).


%% @type mechstate() = {state, Step, Nonce, Cnonce, Username, Password, AuthzId, GetPassword, CheckPassword, AuthModule, Host, Domain}
%%     Step = 1 | 2 | 3 | 4 | 5
%%     Nonce = string()
%%     Cnonce = string()
%%     Username = string()
%%     Password = string()
%%     AuthzId = string()
%%     GetPassword = function()
%%     AuthModule = atom()
%%     Host = string().
%%     Domain = string().

-record(state, {step, nonce, cnonce, username, password, authzid, get_password, check_password, auth_module,
		host, domain, rspauth}).

%% @spec (Host, GetPassword, CheckPassword, CheckPasswordDigest) -> {ok, State}
%%     Host = string()
%%     GetPassword = function()
%%     CheckPassword = function()
%%     State = mechstate()

mech_new(Host, GetPassword, _CheckPassword, CheckPasswordDigest) ->
    crypto:start(),
    {ok, #state{step = 1,
                nonce = hex(integer_to_list(random:uniform(65536*65536))),
		host = Host,
		get_password = GetPassword,
		check_password = CheckPasswordDigest}}.

mech_client_new(Username, Host, Domain, Password) ->
    crypto:start(),
    {ok, #state{step = 2,
                cnonce = hex(integer_to_list(random:uniform(18446744073709551616))),
                username = Username,
		host = Host,
		domain = Domain,
                password = Password,
                authzid = ""
		}}.

%% @spec (State, ClientIn) -> Ok | Continue | Error
%%     State = mechstate()
%%     ClientIn = string()
%%     Ok = {ok, Props}
%%         Props = [Prop]
%%         Prop = {username, Username} | {authzid, AuthzId} | {auth_module, AuthModule}
%%         Username = string()
%%         AuthzId = string()
%%         AuthModule = atom()
%%     Continue = {continue, ServerOut, New_State}
%%         ServerOut = string()
%%         New_State = mechstate()
%%     Error = {error, Reason} | {error, Reason, Username}
%%         Reason = term()

%% First challenge from server
mech_step(#state{step = 1, nonce = Nonce} = State, _) ->
    {continue,
     "nonce=\"" ++ Nonce ++
     "\",qop=\"auth\",charset=utf-8,algorithm=md5-sess",
     State#state{step = 3}};

%% First  response from client
mech_step(#state{step = 2, username = Username, password=Password, host = Host, domain = Domain, cnonce = Cnonce} = State, ServerOut) ->
    case parse(ServerOut) of
        bad ->
            {error, 'bad-protocol'};
        KeyVals ->
            NC = "00000001",
            Charset = proplists:get_value("charset",KeyVals),
            Nonce = proplists:get_value("nonce", KeyVals),
            Realm = proplists:get_value("realm", KeyVals, Domain),
            DigestUri = case Domain of
                Host -> 
                    "xmpp/" ++ Domain;
                _ ->
                    "xmpp/" ++ Host ++ "/" ++ Domain
            end,
            %% TODO encode Username, Realm, Password inf UTF-8 if Charset is set.
            KeyVals0 = [{"nc",NC}, {"cnonce", Cnonce}, {"realm", Realm}, {"digest-uri", DigestUri} | KeyVals],
            Response = response(KeyVals0, Username, Password, Nonce, "", "AUTHENTICATE"),
            RspAuth = response(KeyVals0, Username, Password, Nonce, "", ""),
            {continue,
             "username=\"" ++ Username ++ "\",nonce=\"" ++ Nonce ++
             "\",qop=\"auth\"" ++ 
             case Charset of
                 undefined ->
                     "";
                 _ ->
                     ",charset=" ++ Charset
             end ++ 
             ",realm=\"" ++ Realm ++"\"" ++
             ",nc=" ++ NC ++ ",cnonce=\"" ++ Cnonce ++
             "\",digest-uri=\"" ++ DigestUri ++ "\",response=\"" ++ Response ++"\"",
             State#state{step = 4, nonce=Nonce, rspauth = RspAuth}}
    end;
%% Server final response
mech_step(#state{step = 3, nonce = Nonce} = State, ClientIn) ->
    case parse(ClientIn) of
	bad ->
	    {error, 'bad-protocol'};
	KeyVals ->
	    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
	    UserName = proplists:get_value("username", KeyVals, ""),
            %% Maybe we should use state.domain instead of host
	    case is_digesturi_valid(DigestURI, State#state.host) of
		false ->
		    {error, 'not-authorized', UserName};
		true ->
		    AuthzId = proplists:get_value("authzid", KeyVals, ""),
		    case (State#state.get_password)(UserName) of
			{false, _} ->
			    {error, 'not-authorized', UserName};
			{Passwd, AuthModule} ->
				case (State#state.check_password)(UserName, "",
					proplists:get_value("response", KeyVals, ""),
					fun(PW) -> response(KeyVals, UserName, PW, Nonce, AuthzId,
						"AUTHENTICATE") end) of
				{true, _} ->
				    RspAuth = response(KeyVals,
						       UserName, Passwd,
						       Nonce, AuthzId, ""),
				    {continue,
				     "rspauth=" ++ RspAuth,
				     State#state{step = 5,
						 auth_module = AuthModule,
						 username = UserName,
						 authzid = AuthzId}};
				false ->
				    {error, 'not-authorized', UserName};
				{false, _} ->
				    {error, 'not-authorized', UserName}
			    end
		    end
	    end
    end;
%% Client authenticates server
mech_step(#state{step = 4, username = UserName, rspauth = RspAuth}, ServerOut) ->
    case parse(ServerOut) of
	bad ->
	    {error, 'bad-protocol'};
	KeyVals ->
            case proplists:get_value("rspauth", KeyVals) of
                RspAuth ->
                    ok;
                _ ->
                    %% Here actually it is the server who was not authenticated
                    {error, 'not-authorized', UserName}
            end
    end;

mech_step(#state{step = 5,
		 auth_module = AuthModule,
		 username = UserName,
		 authzid = AuthzId}, "") ->
    {ok, [{username, UserName}, {authzid, AuthzId},
	  {auth_module, AuthModule}]};
mech_step(_A, _B) ->
    {error, 'bad-protocol'}.

%% @spec (S) -> [{Key, Value}] | bad
%%     S = string()
%%     Key = string()
%%     Value = string()

parse(S) ->
    parse1(S, "", []).

%% @hidden

parse1([$= | Cs], S, Ts) ->
    parse2(Cs, lists:reverse(S), "", Ts);
parse1([$, | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([$\s | Cs], [], Ts) ->
    parse1(Cs, [], Ts);
parse1([C | Cs], S, Ts) ->
    parse1(Cs, [C | S], Ts);
parse1([], [], T) ->
    lists:reverse(T);
parse1([], _S, _T) ->
    bad.

%% @hidden

parse2([$\" | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, Val, Ts);
parse2([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse2([], _, _, _) ->
    bad.

%% @hidden

parse3([$\" | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse3([$\\, C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([C | Cs], Key, Val, Ts) ->
    parse3(Cs, Key, [C | Val], Ts);
parse3([], _, _, _) ->
    bad.

%% @hidden

parse4([$, | Cs], Key, Val, Ts) ->
    parse1(Cs, "", [{Key, lists:reverse(Val)} | Ts]);
parse4([$\s | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, Val, Ts);
parse4([C | Cs], Key, Val, Ts) ->
    parse4(Cs, Key, [C | Val], Ts);
parse4([], Key, Val, Ts) ->
    parse1([], "", [{Key, lists:reverse(Val)} | Ts]).


%% @spec (DigestURICase, JabberHost) -> bool()
%%     DigestURICase = string()
%%     JabberHost = string()
%%
%% @doc Check if the digest-uri is valid.
%% RFC-2831 allows to provide the IP address in Host,
%% however ejabberd doesn't allow that.
%% If the service (for example jabber.example.org)
%% is provided by several hosts (being one of them server3.example.org),
%% then digest-uri can be like xmpp/server3.example.org/jabber.example.org
%% In that case, ejabberd only checks the service name, not the host.

is_digesturi_valid(DigestURICase, JabberHost) ->
    DigestURI = exmpp_stringprep:to_lower(DigestURICase),
    case catch string:tokens(DigestURI, "/") of
	["xmpp", Host] when Host == JabberHost ->
	    true;
	["xmpp", _Host, ServName] when ServName == JabberHost ->
	    true;
	_ ->
	    false
    end.




%% @hidden

digit_to_xchar(D) when (D >= 0) and (D < 10) ->
    D + 48;
digit_to_xchar(D) ->
    D + 87.

%% @hidden

hex(S) ->
    hex(S, []).

%% @hidden

hex([], Res) ->
    lists:reverse(Res);
hex([N | Ns], Res) ->
    hex(Ns, [digit_to_xchar(N rem 16),
	     digit_to_xchar(N div 16) | Res]).


%% @spec (KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) -> string()
%%     KeyVals = [{Key, Value}]
%%         Key = string()
%%         Value = string()
%%     User = string()
%%     Passwd = string()
%%     Nonce = string()
%%     AuthzId = nil() | string()
%%     A2Prefix = string()

response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
    Realm = proplists:get_value("realm", KeyVals, ""),
    CNonce = proplists:get_value("cnonce", KeyVals, ""),
    DigestURI = proplists:get_value("digest-uri", KeyVals, ""),
    NC = proplists:get_value("nc", KeyVals, ""),
    QOP = proplists:get_value("qop", KeyVals, ""),
    A1 = case AuthzId of
	     "" ->
		 binary_to_list(
		   crypto:md5(User ++ ":" ++ Realm ++ ":" ++ Passwd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce;
	     _ ->
		 binary_to_list(
		   crypto:md5(User ++ ":" ++ Realm ++ ":" ++ Passwd)) ++
		     ":" ++ Nonce ++ ":" ++ CNonce ++ ":" ++ AuthzId
	 end,
    A2 = case QOP of
	     "auth" ->
		 A2Prefix ++ ":" ++ DigestURI;
	     _ ->
		 A2Prefix ++ ":" ++ DigestURI ++
		     ":00000000000000000000000000000000"
	 end,
    T = hex(binary_to_list(crypto:md5(A1))) ++ ":" ++ Nonce ++ ":" ++
	NC ++ ":" ++ CNonce ++ ":" ++ QOP ++ ":" ++
	hex(binary_to_list(crypto:md5(A2))),
    hex(binary_to_list(crypto:md5(T))).

