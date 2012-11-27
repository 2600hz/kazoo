%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_util).

-export([http_method/1
         ,resolve_uri/2
         ,offnet_req/2
        ]).

-include("kzt.hrl").

-define(SUPPORTED_METHODS, [get, post]).

http_method(M) when is_atom(M) ->
    true = lists:member(M, ?SUPPORTED_METHODS),
    M;
http_method(M) when is_list(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower_string(M)));
http_method(M) when is_binary(M) ->
    http_method(wh_util:to_atom(wh_util:to_lower_binary(M))).

-spec resolve_uri/2 :: (nonempty_string() | ne_binary(), nonempty_string() | ne_binary() | 'undefined') -> ne_binary().
resolve_uri(Raw, undefined) -> wh_util:to_binary(Raw);
resolve_uri(_Raw, [$h,$t,$t,$p|_]=Abs) -> Abs;
resolve_uri(_Raw, <<"http", _/binary>> = Abs) -> Abs;
resolve_uri(RawPath, Relative) ->
    lager:debug("taking url ~s and applying path ~s", [RawPath, Relative]),
    PathTokensRev = lists:reverse(binary:split(wh_util:to_binary(RawPath), <<"/">>, [global])),
    UrlTokens = binary:split(wh_util:to_binary(Relative), <<"/">>),

    wh_util:join_binary(
      lists:reverse(
        lists:foldl(fun(<<"..">>, []) -> [];
                       (<<"..">>, [_ | PathTokens]) -> PathTokens;
                       (<<".">>, PathTokens) -> PathTokens;
                       (<<>>, PathTokens) -> PathTokens;
                       (Segment, [LastToken|DirTokens]=PathTokens) ->
                            case filename:extension(LastToken) of
                                <<>> ->
                                    %% no extension, append Segment to Tokens
                                    [Segment | PathTokens];
                                _Ext ->
                                    %% Extension found, append Segment to DirTokens
                                    [Segment|DirTokens]
                            end
                    end, PathTokensRev, UrlTokens)
       ), <<"/">>).

%% see cf_offnet.erl
-spec offnet_req/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
offnet_req(Data, Call) ->
    {ECIDNum, ECIDName} = cf_attributes:caller_id(<<"emergency">>, Call),
    {CIDNumber, CIDName} = cf_attributes:caller_id(<<"external">>, Call),
    CIDNum = case whapps_call:kvs_fetch(dynamic_cid, Call) of
                 undefined -> CIDNumber;
                 DynamicCID -> DynamicCID
             end,
    Req = [{<<"Call-ID">>, whapps_call:call_id(Call)}
           ,{<<"Resource-Type">>, <<"audio">>}
           ,{<<"To-DID">>, whapps_call:request_user(Call)}
           ,{<<"Account-ID">>, whapps_call:account_id(Call)}
           ,{<<"Account-Realm">>, whapps_call:from_realm(Call)}
           ,{<<"Control-Queue">>, whapps_call:control_queue(Call)}
           ,{<<"Application-Name">>, <<"bridge">>}
           ,{<<"Flags">>, wh_json:get_value(<<"flags">>, Data)}
           ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
           ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
           ,{<<"Emergency-Caller-ID-Name">>, ECIDName}
           ,{<<"Emergency-Caller-ID-Number">>, ECIDNum}
           ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
           ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
           ,{<<"Presence-ID">>, cf_attributes:presence_id(Call)}
           ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
           ,{<<"Media">>, wh_json:get_value(<<"Media">>, Data)}
           | wh_api:default_headers(whapps_call:controller_queue(Call), ?APP_NAME, ?APP_VERSION)],
    wapi_offnet_resource:publish_req(Req).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_path_test() ->
    RawPath = <<"http://pivot/script.php">>,
    Relative = <<"script2.php">>,
    RawPath1 = <<"http://pivot/script2.php">>,

    ?assertEqual(RawPath1, resolve_uri(RawPath, Relative)),
    ?assertEqual(RawPath1, resolve_uri(RawPath, RawPath1)),
    ?assertEqual(RawPath1, resolve_uri(RawPath, <<"/", Relative/binary>>)).
-endif.
