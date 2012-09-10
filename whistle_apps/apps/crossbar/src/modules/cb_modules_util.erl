%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Functions shared between crossbar modules
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_modules_util).

-export([pass_hashes/2]).
-export([update_mwi/2]).
-export([get_devices_owned_by/2]).

-include("include/crossbar.hrl").

-spec pass_hashes/2 :: (ne_binary(), ne_binary()) -> {ne_binary(), ne_binary()}.
pass_hashes(Username, Password) ->
    Creds = list_to_binary([Username, ":", Password]),
    SHA1 = wh_util:to_hex_binary(crypto:sha(Creds)),
    MD5 = wh_util:to_hex_binary(erlang:md5(Creds)),
    {MD5, SHA1}.

-spec update_mwi/2 :: ('undefined' | ne_binary(), ne_binary()) -> pid().
update_mwi(OwnerId, AccountDb) ->
    spawn(fun() ->
                  timer:sleep(1000),
                  cf_util:update_mwi(OwnerId, AccountDb) 
          end).

-spec get_devices_owned_by/2 :: (ne_binary(), ne_binary()) -> wh_json:json_objects().
get_devices_owned_by(OwnerID, DB) ->
    case couch_mgr:get_results(DB, <<"cf_attributes/owned">>, [{key, [OwnerID, <<"device">>]}
                                                               ,include_docs
                                                              ]) of
        {ok, JObjs} ->
            lager:debug("Found ~b devices owned by ~s", [length(JObjs), OwnerID]),
            [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs];
        {error, _R} ->
            lager:debug("unable to fetch devices: ~p", [_R]),
            []
    end.
