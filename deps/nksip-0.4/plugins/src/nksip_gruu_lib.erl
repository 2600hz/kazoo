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

%% @doc NkSIP GRUU Plugin Utilities
-module(nksip_gruu_lib).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-include("../include/nksip.hrl").
-include("../include/nksip_call.hrl").
-include("nksip_registrar.hrl").

-export([find/2, update_gruu/1, check_gr/2, update_regcontact/4]).

-define(AES_IV, <<"12345678abcdefgh">>).


%% @private 
-spec update_gruu(nksip:response()) ->
    ok.

update_gruu(#sipmsg{app_id=AppId, contacts=Contacts, class={resp, Code, _}, 
                      cseq={_, Method}}) ->
    case Method=='REGISTER' andalso Code>=200 andalso Code<300 of
        true -> find_gruus(AppId, Contacts);
        false -> ok
    end.


%% @private
find_gruus(AppId, [#uri{ext_opts=Opts}|Rest]) ->
    HasPubGruu = case nksip_lib:get_value(<<"pub-gruu">>, Opts) of
        undefined -> 
            false;
        PubGruu ->
            case nksip_parse:ruris(nksip_lib:unquote(PubGruu)) of
                [PubUri] -> 
                    nksip_config:put({nksip_gruu_pub, AppId}, PubUri),
                    true;
                _ -> 
                    false
            end
    end,
    HasTmpGruu = case nksip_lib:get_value(<<"temp-gruu">>, Opts) of
        undefined -> 
            false;
        TempGruu ->
            case nksip_parse:ruris(nksip_lib:unquote(TempGruu)) of
                [TempUri] -> 
                    nksip_config:put({nksip_gruu_temp, AppId}, TempUri),
                    true;
                _ -> 
                    false
            end
    end,
    case HasPubGruu andalso HasTmpGruu of
        true -> ok;
        false -> find_gruus(AppId, Rest)
    end;

find_gruus(_, []) ->
    ok.


%% @private
-spec find(nksip:app_id(), nksip:uri()) ->
    [nksip:uri()].

find(AppId, #uri{scheme=Scheme, user=User, domain=Domain, opts=Opts}) ->
    case lists:member(<<"gr">>, Opts) of
        true -> 
            % It is probably a tmp GRUU
            case catch decrypt(User) of
                Tmp when is_binary(Tmp) ->
                    {{Scheme1, User1, Domain1}, InstId, Pos} = binary_to_term(Tmp),
                    [
                        nksip_registrar_lib:make_contact(Reg) 
                        || #reg_contact{meta=Meta}=Reg 
                        <- nksip_registrar_lib:get_info(AppId, Scheme1, User1, Domain1), 
                        nksip_lib:get_value(nksip_gruu_instance_id, Meta)==InstId,
                        nksip_lib:get_value(nksip_gruu_tmp_min, Meta, 0)=<Pos
                    ];
                _ ->
                    ?notice(AppId, <<>>, 
                            "private GRUU not recognized: ~p", [User]),
                    nksip_registrar_lib:find(AppId, Scheme, User, Domain)
            end;
        false ->
            case nksip_lib:get_value(<<"gr">>, Opts) of
                undefined -> 
                    nksip_registrar_lib:find(AppId, Scheme, User, Domain);
                InstId ->
                    [
                        nksip_registrar_lib:make_contact(Reg) 
                        || #reg_contact{meta=Meta}=Reg 
                        <- nksip_registrar_lib:get_info(AppId, Scheme, User, Domain), 
                        nksip_lib:get_value(nksip_gruu_instance_id, Meta)==InstId
                    ]
            end
    end.


check_gr(Contact, Req) ->
    #uri{user=User, opts=Opts} = Contact,
    #sipmsg{to={To, _}} = Req,
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
update_regcontact(RegContact, Base, Req, Opts) ->
    #reg_contact{contact=Contact, meta=Meta} = RegContact,
    #reg_contact{call_id=BaseCallId} = Base,
    #sipmsg{to={To, _}, call_id=CallId} = Req,
    Next = nksip_lib:get_value(nksip_gruu_tmp_next, Meta, 0),
    Meta1 = case CallId of
        BaseCallId ->
            Meta;
        _ -> 
            % We have changed the Call-ID for this AOR and index, invalidate all
            % temporary GRUUs
            nksip_lib:store_value(nksip_gruu_tmp_min, Next, Meta)
    end,
    #uri{scheme=Scheme, ext_opts=ExtOpts} = Contact,
    InstId = case nksip_lib:get_value(<<"+sip.instance">>, ExtOpts) of
        undefined -> <<>>;
        Inst0 -> nksip_lib:hash(Inst0)
    end,
    Expires = nksip_lib:get_integer(<<"expires">>, ExtOpts),
    case 
        InstId /= <<>> andalso Expires>0 andalso 
        lists:member({gruu, true}, Opts)
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
            ExtOpts2 = nksip_lib:store_value(<<"pub-gruu">>, Pub, ExtOpts),
            TmpBin = term_to_binary({aor(To), InstId, Next}),
            TmpUri = PubUri#uri{user=encrypt(TmpBin), opts=[<<"gr">>]},
            Tmp = list_to_binary([$", nksip_unparse:ruri(TmpUri), $"]),
            ExtOpts3 = nksip_lib:store_value(<<"temp-gruu">>, Tmp, ExtOpts2),
            Contact3 = Contact#uri{ext_opts=ExtOpts3},
            Meta2 = nksip_lib:store_value(nksip_gruu_instance_id, InstId, Meta1),
            Meta3 = nksip_lib:store_value(nksip_gruu_tmp_next, Next+1, Meta2),
            RegContact#reg_contact{contact=Contact3, meta=Meta3};
        false ->
            RegContact
    end.


%% @private
aor(#uri{scheme=Scheme, user=User, domain=Domain}) ->
    {Scheme, User, Domain}.


%% @private
encrypt(Bin) ->
    <<Key:16/binary, _/binary>> = nksip_config_cache:global_id(),
    base64:encode(do_encrypt(Key, Bin)).


%% @private
decrypt(Bin) ->
    <<Key:16/binary, _/binary>> = nksip_config_cache:global_id(),
    do_decrypt(Key, base64:decode(Bin)).


-ifdef(old_crypto_block).

do_encrypt(Key, Bin) ->
    crypto:aes_cfb_128_encrypt(Key, ?AES_IV, Bin).

do_decrypt(Key, Dec) ->
    crypto:aes_cfb_128_decrypt(Key, ?AES_IV, Dec).

-else.

do_encrypt(Key, Bin) ->
    crypto:block_encrypt(aes_cfb128, Key, ?AES_IV, Bin).

do_decrypt(Key, Dec) ->
    crypto:block_decrypt(aes_cfb128, Key, ?AES_IV, Dec).

-endif.





