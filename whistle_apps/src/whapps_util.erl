%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Utilities shared by a subset of whapps
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(whapps_util).

-export([get_db_name/1, get_db_name/2]).

-include_lib("whistle/include/whistle_types.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will verify an account id is valid, and if so return
%% the name of the account database
%% @end
%%--------------------------------------------------------------------
-spec(get_db_name/1 :: (AccountId :: list(binary()) | json_object() | binary()) -> binary()).
-spec(get_db_name/2 :: (AccountId :: list(binary()) | binary() | json_object(), Encoding :: unencoded | encoded | raw) -> binary()).

get_db_name(Doc) -> get_db_name(Doc, unencoded).

get_db_name({struct, _}=Doc, Encoding) ->
    get_db_name([wh_json:get_value(["_id"], Doc)], Encoding);
get_db_name([AccountId], Encoding) ->
    get_db_name(AccountId, Encoding);
get_db_name(AccountId, Encoding) when not is_binary(AccountId) ->
    get_db_name(whistle_util:to_binary(AccountId), Encoding);
get_db_name(<<"accounts">>, _) ->
    <<"accounts">>;
%% unencode the account db name
get_db_name(<<"account/", _/binary>>=DbName, unencoded) ->
    DbName;
get_db_name(<<"account%2F", _/binary>>=DbName, unencoded) ->
    binary:replace(DbName, <<"%2F">>, <<"/">>, [global]);
get_db_name(AccountId, unencoded) ->
    [Id1, Id2, Id3, Id4 | IdRest] = whistle_util:to_list(AccountId),
    whistle_util:to_binary(["account/", Id1, Id2, $/, Id3, Id4, $/, IdRest]);
%% encode the account db name
get_db_name(<<"account%2F", _/binary>>=DbName, encoded) ->
    DbName;
get_db_name(<<"account/", _/binary>>=DbName, encoded) ->
    binary:replace(DbName, <<"/">>, <<"%2F">>, [global]);
get_db_name(AccountId, encoded) when is_binary(AccountId) ->
    [Id1, Id2, Id3, Id4 | IdRest] = whistle_util:to_list(AccountId),
    whistle_util:to_binary(["account%2F", Id1, Id2, "%2F", Id3, Id4, "%2F", IdRest]);
%% get just the account ID from the account db name
get_db_name(<<"account%2F", AccountId/binary>>, raw) ->
    binary:replace(AccountId, <<"%2F">>, <<>>, [global]);
get_db_name(<<"account/", AccountId/binary>>, raw) ->
    binary:replace(AccountId, <<"/">>, <<>>, [global]).
