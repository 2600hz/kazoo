-module(cb_harrys_SUITE).

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% API tests
-export([harrys/1]).

-type config() :: [{atom(), term()}].
-type method() :: get | put | post | delete.

%% =============================================================================
%% CT callbacks
%% =============================================================================
-spec all() ->
  [atom()].
all() ->
  [harrys].

-spec init_per_suite(Config::config()) ->
  config().
init_per_suite(Config) ->
  ok = inets:start(),
  ok = login_user(),
  ok = clean_harrys(),
  Config.

-spec end_per_suite(Config::config()) ->
  config().
end_per_suite(Config) ->
  ok = inets:stop(),
  ok = logout_user(),
  Config.

%% =============================================================================
%% API tests
%% =============================================================================
-spec harrys(Config::config()) ->
  {comment, string()}.
harrys(_Config) ->
  ct:comment("There should be not records yet"),
  {200, Resp} = api_call(get),
  <<"success">> = get_value(<<"status">>, Resp),
  [] = get_value(<<"data">>, Resp),

  ct:comment("Let's create a couple of Harrys"),
  Harry0 = build_harry(<<"Nonie">>, <<"Dormand">>, <<"ndormand0@dion.ne.jp">>),
  {201, Resp0} = api_call(put, jiffy:encode(Harry0)),
  <<"success">> = get_value(<<"status">>, Resp0),
  <<"Nonie">> = get_value(<<"first_name">>, get_value(<<"data">>, Resp0)),

  Harry1 = build_harry(<<"Kippy">>, <<"Guillain">>, <<"kguillain0@freew.com">>),
  {201, Resp1} = api_call(put, jiffy:encode(Harry1)),
  <<"success">> = get_value(<<"status">>, Resp1),
  Data1 = get_value(<<"data">>, Resp1),
  <<"kguillain0@freew.com">> = get_value(<<"email">>, Data1),
  Id1 = get_value(<<"id">>, Data1),

  ct:comment("Let's modify Harry1's email"),
  NewHarry1 = build_harry(<<"Kippy">>, <<"Guillain">>, <<"kippy@guillain.us">>),
  {200, NewResp1} = api_call(post, jiffy:encode(NewHarry1), Id1),
  <<"success">> = get_value(<<"status">>, NewResp1),
  NewData1 = get_value(<<"data">>, NewResp1),
  <<"kippy@guillain.us">> = get_value(<<"email">>, NewData1),
  Id1 = get_value(<<"id">>, NewData1), % match id

  ct:comment("There must be 2 Harrys right now"),
  {200, Resp2} = api_call(get),
  2 = get_value(<<"page_size">>, Resp2),
  2 = length(get_value(<<"data">>, Resp2)),

  ct:comment("Let's delete Harry1"),
  {200, Resp3} = api_call(delete, [], Id1),
  Data3 = get_value(<<"data">>, Resp3),
  true = get_value(<<"deleted">>, get_value(<<"_read_only">>, Data3)),
  Id1 = get_value(<<"id">>, Data3),

  ct:comment("Trying to delete an unexisting Harry should return 404"),
  % Harry1 was already deleted on the previous test
  {404, Resp4} = api_call(delete, [], Id1),
  <<"error">> = get_value(<<"status">>, Resp4),
  <<"bad_identifier">> = get_value(<<"message">>, Resp4),

  ct:comment("There must be only 1 Harry left"),
  {200, Resp5} = api_call(get),
  1 = get_value(<<"page_size">>, Resp5),
  1 = length(get_value(<<"data">>, Resp5)),

  {comment, ""}.

%% =============================================================================
%% Private (helpers)
%% =============================================================================
-spec login_user() ->
  ok.
login_user() ->
  URL = "http://192.168.99.100:8000/v2/user_auth",
  Creds = <<"1141aeae55223480eca2d9e50e3ecb3b">>,
  ID = <<"harenson">>,
  % JSON={"data": {"credentials": "value here", "account_name": "value here"}}.
  ReqBody = jiffy:encode({[{<<"data">>, {[{<<"credentials">>, Creds},
                                          {<<"account_name">>, ID}]} }]}),
  Request = {URL, [], "application/json", ReqBody},
  {ok, {{_Vsn, 201, _HTTPText}, _RespHeaders, RespBody}} =
    httpc:request(put, Request, [], []),
  DecodedResp = jiffy:decode(RespBody),
  Data = get_value(<<"data">>, DecodedResp),
  AccID = get_value(<<"account_id">>, Data),
  Token = get_value(<<"auth_token">>, DecodedResp),
  ok = application:set_env(cb_harrys_tests, account_id, binary_to_list(AccID)),
  ok = application:set_env(cb_harrys_tests, auth_token, binary_to_list(Token)).

-spec logout_user() ->
  ok.
logout_user() ->
  % TODO: perform api call to logout the user.
  ok = application:unset_env(cb_harrys_tests, account_id),
  ok = application:unset_env(cb_harrys_tests, auth_token).

-spec clean_harrys() ->
  ok.
clean_harrys() ->
  {200, Resp} = api_call(get),
  ExistingHarrys = get_value(<<"data">>, Resp),
  ok = lists:foreach(
         fun(Harry) ->
             {200, _} = api_call(delete, <<>>, get_value(<<"id">>, Harry))
         end,
         ExistingHarrys
        ).

-spec account_id() ->
  string().
account_id() ->
  {ok, AccID} = application:get_env(cb_harrys_tests, account_id),
  AccID.

-spec auth_token() ->
  string().
auth_token() ->
  {ok, AuthToken} = application:get_env(cb_harrys_tests, auth_token),
  AuthToken.

-spec build_harry(FName::binary(), LName::binary(), Email::binary()) ->
  jiffy:json_value().
build_harry(FName, LName, Email) ->
  % {"data": {"first_name": "value", "last_name": "value", "email": "value"}}
  {[{<<"data">>, {[{<<"first_name">>, FName},
                   {<<"last_name">>, LName},
                   {<<"email">>, Email}]} }]}.

-spec api_call(Method::method()) ->
  {integer(), jiffy:jiffy_decode_result()}.
api_call(Method) ->
  api_call(Method, <<>>, "").

-spec api_call(Method::method(), ReqBody::binary() | string()) ->
  {integer(), jiffy:jiffy_decode_result()}.
api_call(Method, ReqBody) ->
  api_call(Method, ReqBody, "").

-spec api_call(Method::method(),
               ReqBody::binary() | string(),
               HarryID::string() | binary()) ->
  {integer(), jiffy:jiffy_decode_result()}.
api_call(Method, ReqBody, HarryID) when is_binary(HarryID) ->
  api_call(Method, ReqBody, binary_to_list(HarryID));
api_call(Method, ReqBody, HarryID) ->
  BaseURL = "http://192.168.99.100:8000/v2/accounts/",
  URL = lists:flatten([BaseURL, account_id(), "/harrys/", HarryID]),
  Headers = [{"X-Auth-Token", auth_token()}],
  Request = case Method of
              get -> {URL, Headers};
              _ -> {URL, Headers, "application/json", ReqBody}
            end,
  ct:log("Sending ~p request to ~p with:~nHeaders: ~p~nBody: ~p",
         [Method, URL, Headers, ReqBody]),
  {ok, {{_Vsn, HTTPCode, _HTTPText}, _RespHeaders, RespBody}} =
    httpc:request(Method, Request, [], []),
  DecodedResp = jiffy:decode(RespBody),
  ct:log("Got ~p response with:~nRespBody: ~p~n", [HTTPCode, DecodedResp]),
  {HTTPCode, DecodedResp}.

-spec get_value(Key::binary(), Data::jiffy:json_value()) ->
  term().
get_value(Key, {Data}) ->
  proplists:get_value(Key, Data).
