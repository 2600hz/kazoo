%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Instructs the switch to receive a fax from the caller
%%% Stores the fax in the database and optionally emails a configured
%%% user(s).
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_receive_fax).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    lager:debug("receiving a fax"),
    whapps_call_command:answer(Call),
    case whapps_call_command:b_receive_fax(Call) of
        {ok, RecvJObj} ->
            lager:debug("rxfax resp: ~p", [RecvJObj]),

            %% store Fax in DB
            case store_fax(Call) of
                {ok, StoreJObj} ->
                    lager:debug("store fax resp: ~p", [StoreJObj]),

                    case wh_json:get_value(<<"emails">>, Data) of
                        undefined ->
                            lager:debug("no emails configured");
                        [] ->
                            lager:debug("no emails configured");
                        Emls ->
                            lager:debug("sending emails to ~p", [Emls])
                    end,

                    cf_exe:continue(Call);
                _E ->
                    lager:debug("store fax other resp: ~p", [_E]),
                    cf_exe:stop(Call)
            end;
        {error, channel_hungup} ->
            lager:debug("rxfax hungup prematurely"),
            cf_exe:stop(Call);
        _Resp ->
            lager:debug("rxfax unhandled: ~p", [_Resp]),
            cf_exe:continue(Call)
    end.

store_fax(Call) ->
    FaxFile = tmp_file(),
    FaxDocId = fax_doc(Call),
    FaxUrl = attachment_url(Call, FaxFile, FaxDocId),

    lager:debug("storing fax to ~s", [FaxFile, FaxUrl]),

    case whapps_call_command:b_store_fax(FaxUrl, Call) of
        {ok, _JObj}=OK ->
            lager:debug("store_fax returned: ~p", [_JObj]),
            OK;
        E ->
            lager:debug("store_fax error: ~p", [E]),
            E
    end.
            

fax_doc(Call) ->
    AccountDb = whapps_call:account_db(Call),

    TStamp = wh_util:current_tstamp(),
    {{Y,M,D},{H,I,S}} = calendar:gregorian_seconds_to_datetime(TStamp),

    Name = list_to_binary(["fax message received at "
                           ,wh_util:to_binary(Y), "-", wh_util:to_binary(M), "-", wh_util:to_binary(D)
                           ," " , wh_util:to_binary(H), ":", wh_util:to_binary(I), ":", wh_util:to_binary(S)
                           ," UTC"
                          ]),

    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<"fax document received">>}
             ,{<<"source_type">>, <<"incoming_fax">>}
            ],

    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), AccountDb
                                       ,[{type, <<"private_media">>}]
                                      ),

    {ok, JObj} = couch_mgr:save_doc(AccountDb, Doc),
    wh_json:get_value(<<"_id">>, JObj).


attachment_url(Call, File, FaxDocId) ->
    AccountDb = whapps_call:account_db(Call),
    _ = case couch_mgr:open_doc(AccountDb, FaxDocId) of
            {ok, JObj} ->
                case wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new())) of
                    [] -> ok;
                    Existing -> [couch_mgr:delete_attachment(AccountDb, FaxDocId, Attach) || Attach <- Existing]
                end;
            {error, _} -> ok
        end,
    Rev = case couch_mgr:lookup_doc_rev(AccountDb, FaxDocId) of
              {ok, R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    list_to_binary([couch_mgr:get_url(), AccountDb, "/", FaxDocId, "/", File, Rev]).


-spec tmp_file/0 :: () -> ne_binary().
tmp_file() ->
     <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".tiff">>.
