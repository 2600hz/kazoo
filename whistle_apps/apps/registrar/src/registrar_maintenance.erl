%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(registrar_maintenance).

-export([local_summary/0, local_summary/1, local_summary/2]).

-include("reg.hrl").

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%% look up a cached registration by realm and optionally username
%% @end
%%-----------------------------------------------------------------------------
-spec local_summary() -> 'ok'.
local_summary() ->
    {ok, Registrations} = reg_util:fetch_all_registrations(),
    _ = do_summary(Registrations, fun print_summary/1),
    io:format("~nTotal registrations: ~b~n", [length(Registrations)]),
    ok.

-spec local_summary(ne_binary()) -> 'ok'.
local_summary(Realm) when not is_binary(Realm) ->
    local_summary(wh_util:to_binary(Realm));
local_summary(Realm) ->
    {ok, Registrations} = reg_util:lookup_registrations(Realm),
    _ = do_summary(Registrations, fun print_summary/1),
    io:format("~nTotal registrations: ~b~n", [length(Registrations)]),
    ok.

-spec local_summary(ne_binary(), ne_binary()) -> 'ok'.
local_summary(Realm, Username) when not is_binary(Realm) ->
    local_summary(wh_util:to_binary(Realm), Username);
local_summary(Realm, Username) when not is_binary(Username) ->
    local_summary(Realm, wh_util:to_binary(Username));
local_summary(Realm, Username) ->
    _ = case reg_util:lookup_registration(Realm, Username) of
            {ok, Registration} ->
                do_summary([Registration], fun print_details/1);
            {error, not_found} ->
                io:format("registration not found")
        end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec do_summary(list(), fun((wh_json:json_object()) -> 'ok')) -> ['ok',...].
do_summary(Registrations, PrintFun) ->
    [PrintFun(Registration) || Registration <- Registrations].

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary(wh_json:json_object()) -> 'ok'.
print_summary(Registration) ->
    Username = wh_json:get_value(<<"Username">>, Registration),
    Realm = wh_json:get_value(<<"Realm">>, Registration),
    UserAgent = wh_json:get_value(<<"User-Agent">>, Registration),
    Expires = wh_json:get_integer_value(<<"Expires">>, Registration, 0)
        + wh_json:get_integer_value(<<"Event-Timestamp">>, Registration, 0)
        - wh_util:current_tstamp(),
    Contact = wh_json:get_value(<<"Contact">>, Registration),
    io:format("'~s' '~s' '~s' '~p' '~s'~n", [Username, Realm, UserAgent, Expires, Contact]).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_details(api_terms()) -> ['ok',...].
print_details(Registration) when is_list(Registration) ->
    [io:format("~s: ~s~n", [K, wh_util:to_list(V)]) || {K, V} <- Registration];
print_details(Registration) ->
    print_details(wh_json:to_proplist(Registration)).
