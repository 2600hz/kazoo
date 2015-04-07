%%%-------------------------------------------------------------------
%%% File    : eradius_server_example.erl
%%% Author  : Sean Hinde <sean@Seans-Mac.local>
%%% Description : Example implementation module for eradius server
%%%               Note CHAP is untested as of 26th March 2004, but 
%%%               this is how I thihk it should work !
%%% Created : 26 Mar 2004 by Sean Hinde <sean@Seans-Mac.local>
%%%-------------------------------------------------------------------
-module(eradius_server_example).

-export([test/2, auth/2]).

-include("eradius_lib.hrl").
-include("eradius_dict.hrl").
-include("dictionary.hrl").

%% Minimal (!) example of Access Request handler
test(#rad_pdu{}, #nas_prop{}) ->
    %io:format("Test spawned~n"),
    #rad_accept{}.

%% Example which does a bit more.
%% First tries Pap, then CHAP, then gives up.
auth(#rad_pdu{} = Pdu, #nas_prop{} = Nas) ->
    {request, Attrs} = Pdu#rad_pdu.cmd,
    case lookup(?User_Name, Attrs) of
        {ok, User} ->
            case lookup(?User_Password, Attrs) of
                {ok, Pass} ->
                    pap(User, Pass, Nas#nas_prop.secret, Pdu#rad_pdu.authenticator);
                false ->
                    case lookup(?CHAP_Password, Attrs) of
                        {ok, Chap_pass} ->
                            Challenge = case lookup(?CHAP_Challenge, Attrs) of
                                            {ok, Val} ->
                                                Val;
                                            false ->
                                                Pdu#rad_pdu.authenticator
                                        end,
                            chap(User, list_to_binary(Chap_pass), Challenge);
                        false ->
                            #rad_reject{}
                    end
            end;
        false ->
            #rad_reject{}
    end.

pap(User, Req_pass, Secret, Auth) ->
    case get_user(User) of
        {ok, Passwd} ->
            Enc_pass = eradius_lib:mk_password(Secret, Auth, Passwd),
            Req_pass1 = list_to_binary(Req_pass),
            if Enc_pass == Req_pass1 ->
                    #rad_accept{};
               true ->
                    io:format("PAP~p~n",[{User, Req_pass, Secret, Auth, Enc_pass}]),
                    #rad_reject{}
            end;
        false ->
            #rad_reject{}
    end.

chap(User, <<Chap_id, Chap_pass/binary>>, Chap_challenge) ->
    case get_user(User) of
        {ok, Passwd} ->
            Enc_pass = erlang:md5([Chap_id, Passwd, Chap_challenge]),
            if Enc_pass == Chap_pass ->
                    #rad_accept{};
               true ->
                    #rad_reject{}
            end;
        false ->
             #rad_reject{}
    end.

get_user("sean") -> {ok, <<"Sean_passzektrw8&">>};
get_user("tobbe") -> {ok, <<"qwe123">>};
get_user(_)      -> false.

lookup(Key, [{#attribute{id = Key}, Val}|T]) ->
    {ok, Val};
lookup(Key, [_|T]) ->
    lookup(Key, T);
lookup(_, []) ->
    false.
    
