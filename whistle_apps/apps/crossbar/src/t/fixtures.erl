%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% This will run through the Quickstart Guide steps as a test of the full Crossbar
%%% system and modules
%%% @end
%%% Created : 13 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(fixtures).

-export([test/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include("fixtures.hrl").

test() ->
    crossbar_module_sup:start_mod(noauthn),
    crossbar_module_sup:start_mod(noauthz),

    Acct0 = new_account(?CB_URL),
    Acct0_1 = edit_account(?CB_URL, Acct0),
    delete_account(?CB_URL, Acct0_1),
    Acct1 = new_account(?CB_URL),

    delete_account(?CB_URL, Acct1),

    crossbar_module_sup:stop_mod(noauthn),
    crossbar_module_sup:stop_mod(noauthz).

%% test GET /accounts, PUT /accounts, and GET /accounts/{ACCT_ID}
-spec(new_account/1 :: (Url :: string()) -> json_object()).
new_account(Url) ->
    ?assert([] =:= list_accounts(Url)),
    AcctJObj = create_account(Url),
    ?assert(length(list_accounts(Url)) =:= 1),

    AcctJObj1 = get_account(Url, AcctJObj),
    ?assert(wh_json:get_value(<<"id">>, AcctJObj) =:= wh_json:get_value(<<"id">>, AcctJObj1)),
    AcctJObj1.

-spec(list_accounts/1 :: (Url :: string()) -> json_objects() | []).
list_accounts(Url) ->
    {_,"200",_,JSON} = ibrowse:send_req(Url, ?IBROWSE_HEADERS, get),
    extract_data(JSON).

-spec(create_account/1 :: (Url :: string()) -> json_object()).
create_account(Url) ->
    {_,"201",_,JSON} = ibrowse:send_req(Url, ?IBROWSE_HEADERS, put, create_json(?ACCT_JOBJ)),
    extract_data(JSON).

-spec(get_account/2 :: (Url :: list(), Acct :: json_object() | binary()) -> json_object()).
get_account(Url, {struct, _}=AcctJObj) ->
    get_account(Url, wh_json:get_value(<<"id">>, AcctJObj));
get_account(Url, Id) when is_binary(Id) ->
    Url1 = wh_util:to_list(list_to_binary([Url, "/", Id])),
    {_,"200",_,JSON} = ibrowse:send_req(Url1, ?IBROWSE_HEADERS, get),
    extract_data(JSON).

-spec(edit_account/2 :: (Url :: string(), AcctJObj :: json_object()) -> json_object()).
edit_account(Url, AcctJObj) ->
    AcctJObj1 = wh_json:set_value(<<"name">>, <<"fixture account edited">>, AcctJObj),
    Url1 = wh_util:to_list(list_to_binary([Url, "/", wh_json:get_value(<<"id">>, AcctJObj)])),
    {ok,"200",_,JSON} = ibrowse:send_req(Url1, ?IBROWSE_HEADERS, post, create_json(AcctJObj1)),
    extract_data(JSON).

-spec(delete_account/2 :: (Url :: string(), AcctJObj :: json_object()) -> json_object() | undefined).
delete_account(Url, AcctJObj) ->
    Url1 = wh_util:to_list(list_to_binary([Url, "/", wh_json:get_value(<<"id">>, AcctJObj)])),
    {ok,"200",_,JSON} = ibrowse:send_req(Url1, ?IBROWSE_HEADERS, delete),
    extract_data(JSON).

-spec(create_json/1 :: (JObj :: json_object()) -> iolist()).
create_json(JObj) ->
    mochijson2:encode({struct, [{<<"data">>, JObj}]}).

-spec(extract_data/1 :: (JSON :: iolist()) -> json_object()).
extract_data(JSON) ->
    JObj = mochijson:decode(JSON),
    test_success(JObj),
    wh_json:get_value(<<"data">>, JObj).

test_success(JObj) ->
    ?assert(<<"success">> =:= wh_json:get_value(<<"status">>, JObj)).
