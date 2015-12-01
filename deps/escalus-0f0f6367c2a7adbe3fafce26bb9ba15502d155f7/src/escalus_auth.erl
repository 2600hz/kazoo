%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Module supporting various authentication mechanisms
%%% @end
%%%===================================================================
-module(escalus_auth).

%% Public APi
-export([auth_plain/2,
         auth_xoauth2/2,
         auth_digest_md5/2,
         auth_sasl_anon/2,
         auth_sasl_external/2,
         auth_sasl_scram_sha1/2]).

%% Useful helpers for writing own mechanisms
-export([get_challenge/2,
         wait_for_success/2]).

-include_lib("exml/include/exml.hrl").
-include("no_binary_to_integer.hrl").

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

auth_plain(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = escalus_stanza:auth(<<"PLAIN">>, [base64_cdata(Payload)]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(Username, Conn).

auth_xoauth2(Conn, Props) ->
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Payload = <<0:8,Username/binary,0:8,Password/binary>>,
    Stanza = escalus_stanza:auth(<<"X-OAUTH2">>, [base64_cdata(Payload)]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(Username, Conn).

auth_digest_md5(Conn, Props) ->
    ok = escalus_connection:send(Conn, escalus_stanza:auth(<<"DIGEST-MD5">>)),
    ChallengeData = get_challenge(Conn, challenge1),
    Response = md5_digest_response(ChallengeData, Props),
    ResponseStanza1 = escalus_stanza:auth_response([Response]),
    ok = escalus_connection:send(Conn, ResponseStanza1),
    [{<<"rspauth">>, _}] = get_challenge(Conn, challenge2), %% TODO: validate
    ResponseStanza2 = escalus_stanza:auth_response(),
    ok = escalus_connection:send(Conn, ResponseStanza2),
    wait_for_success(get_property(username, Props), Conn).

auth_sasl_scram_sha1(Conn, Props) ->
    Username = get_property(username, Props),
    Nonce = base64:encode(crypto:rand_bytes(16)),
    ClientFirstMessageBare = csvkv:format([{<<"n">>, Username},
                                           {<<"r">>, Nonce}],
                                          false),
    GS2Header = <<"n,,">>,
    Payload = <<GS2Header/binary,ClientFirstMessageBare/binary>>,
    Stanza = escalus_stanza:auth(<<"SCRAM-SHA-1">>, [base64_cdata(Payload)]),

    ok = escalus_connection:send(Conn, Stanza),

    {Response,
     SaltedPassword,
     AuthMessage} = scram_sha1_response(Conn, GS2Header,
                                        ClientFirstMessageBare, Props),
    ResponseStanza = escalus_stanza:auth_response([Response]),


    ok = escalus_connection:send(Conn, ResponseStanza),

    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    case AuthReply of
        #xmlel{name = <<"success">>, children = [CData]} ->
            Unescaped = exml:unescape_cdata(CData),
            V = get_property(<<"v">>, csvkv:parse(base64:decode(Unescaped))),
            Decoded = base64:decode(V),
            ok = scram_sha1_validate_server(SaltedPassword, AuthMessage,
                                            Decoded);
        #xmlel{name = <<"failure">>} ->
            throw({auth_failed, Username, AuthReply})
    end.


auth_sasl_anon(Conn, Props) ->
    Stanza = escalus_stanza:auth(<<"ANONYMOUS">>),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(get_property(username, Props), Conn).

auth_sasl_external(Conn, Props) ->
    {server, ThisServer} = get_property(endpoint, Props),
    Stanza = escalus_stanza:auth(<<"EXTERNAL">>, [base64_cdata(ThisServer)]),
    ok = escalus_connection:send(Conn, Stanza),
    wait_for_success(ThisServer, Conn).


%%--------------------------------------------------------------------
%% Helpers - implementation
%%--------------------------------------------------------------------

md5_digest_response(ChallengeData, Props) ->
    %% Digest calculated via description at
    %% http://web.archive.org/web/20050224191820/http://cataclysm.cx/wip/digest-hex_md5-crash.html
    Username = get_property(username, Props),
    Password = get_property(password, Props),
    Server = get_property(server, Props),
    Resource = get_property(resource, Props),
    Nonce = get_property(<<"nonce">>, ChallengeData),
    CNonce = base16:encode(crypto:rand_bytes(16)),
    Realm = proplists:get_value(<<"realm">>, ChallengeData, <<>>),
    QOP = get_property(<<"qop">>, ChallengeData),
    NC = <<"00000001">>,
    ServType = <<"xmpp">>,
    DigestUri = <<"xmpp/", Server/binary>>,
    FullJid = <<Username/binary, "@", Server/binary, "/", Resource/binary>>,

    Y = crypto:hash(md5, [Username, $:, Realm, $:, Password]),
    HA1 = hex_md5([Y, $:, Nonce, $:, CNonce, $:, FullJid]),
    HA2 = hex_md5([<<"AUTHENTICATE:">>, DigestUri]),

    %% Digest is the Z from the description above
    Digest = hex_md5([HA1, $:, Nonce, $:, NC, $:, CNonce, $:, QOP, $:, HA2]),

    base64_cdata(csvkv:format([
        {<<"username">>, Username},
        {<<"nonce">>, Nonce},
        {<<"nc">>, NC},
        {<<"cnonce">>, CNonce},
        {<<"qop">>, QOP},
        {<<"serv-type">>, ServType},
        {<<"host">>, Server},
        {<<"digest-uri">>, DigestUri},
        {<<"response">>, Digest},
        {<<"charset">>, <<"utf-8">>},
        {<<"authzid">>, FullJid}
    ])).

scram_sha1_response(Conn, GS2Headers, ClientFirstMessageBare, Props) ->
    Challenge = get_challenge(Conn, challenge1, false),
    ChallengeData = csvkv:parse(Challenge),
    Password = get_property(password, Props),
    Nonce = get_property(<<"r">>, ChallengeData),
    Iteration = binary_to_integer(get_property(<<"i">>, ChallengeData)),
    Salt = base64:decode(get_property(<<"s">>, ChallengeData)),
    SaltedPassword = scram:salted_password(Password, Salt, Iteration),
    ClientKey = scram:client_key(SaltedPassword),
    StoredKey = scram:stored_key(ClientKey),
    GS2Headers64 = base64:encode(GS2Headers),
    ClientFinalMessageWithoutProof = <<"c=",GS2Headers64/binary,",r=", Nonce/binary>>,
    AuthMessage = <<ClientFirstMessageBare/binary,$,,
                    Challenge/binary,$,,
                    ClientFinalMessageWithoutProof/binary>>,
    ClientSignature = scram:client_signature(StoredKey, AuthMessage),
    ClientProof = base64:encode(crypto:exor(ClientKey, ClientSignature)),
    ClientFinalMessage = <<ClientFinalMessageWithoutProof/binary,
                           ",p=", ClientProof/binary>>,
    {base64_cdata(ClientFinalMessage),SaltedPassword, AuthMessage}.

scram_sha1_validate_server(SaltedPassword, AuthMessage, ServerSignature) ->
    ServerKey = scram:server_key(SaltedPassword),
    ServerSignatureComputed = scram:server_signature(ServerKey, AuthMessage),
    case ServerSignatureComputed == ServerSignature of
        true ->
            ok;
        _ ->
            false
    end.

hex_md5(Data) ->
    base16:encode(crypto:hash(md5, Data)).

%%--------------------------------------------------------------------
%% Helpers - actions
%%--------------------------------------------------------------------

get_challenge(Conn, Descr) ->
    get_challenge(Conn, Descr, true).

get_challenge(Conn, Descr, DecodeCsvkv) ->
    Challenge = escalus_connection:get_stanza(Conn, Descr),
    case Challenge of
        #xmlel{name = <<"challenge">>, children=[CData]} ->
            ChallengeData = base64:decode(exml:unescape_cdata(CData)),
            case DecodeCsvkv of
                true ->
                    csvkv:parse(ChallengeData);
                _ ->
                    ChallengeData
            end;
        _ ->
            throw({expected_challenge, got, Challenge})
    end.

wait_for_success(Username, Conn) ->
    AuthReply = escalus_connection:get_stanza(Conn, auth_reply),
    case AuthReply#xmlel.name of
        <<"success">> ->
            ok;
        R when R =:= <<"failure">> orelse R =:= <<"stream:error">> ->
            throw({auth_failed, Username, AuthReply})
    end.

get_property(PropName, Proplist) ->
    case lists:keyfind(PropName, 1, Proplist) of
        {PropName, Value} ->
            Value;
        false ->
            throw({missing_property, PropName})
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

base64_cdata(Payload) ->
    #xmlcdata{content = base64:encode(Payload)}.
