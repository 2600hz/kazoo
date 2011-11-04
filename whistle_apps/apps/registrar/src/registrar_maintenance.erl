%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 1 Nov 2011 by Karl Anderson <karl@2600hz.org>
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
-spec local_summary/0 :: () -> ok.
local_summary() ->
    {ok, Registrations} = reg_util:fetch_all_registrations(),
    do_summary(Registrations, fun print_summary/1),
    ok.

-spec local_summary/1 :: (Realm) -> ok when
      Realm :: binary().
local_summary(Realm) when not is_binary(Realm) ->
    local_summary(wh_util:to_binary(Realm));
local_summary(Realm) ->
    {ok, Registrations} = reg_util:lookup_registrations(Realm),
    do_summary(Registrations, fun print_summary/1),
    ok.

-spec local_summary/2 :: (Realm, Username) -> ok when
      Realm :: binary(),
      Username :: binary().
local_summary(Realm, Username) when not is_binary(Realm) ->
    local_summary(wh_util:to_binary(Realm), Username);
local_summary(Realm, Username) when not is_binary(Username) ->
    local_summary(Realm, wh_util:to_binary(Username));
local_summary(Realm, Username) ->
    case reg_util:lookup_registration(Realm, Username) of
        {ok, Registration} ->
            do_summary([Registration], fun print_details/1);
        {error, not_found} ->
            io:format("registration not found")
    end,
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec do_summary/2 :: (Registrations, PrintFun) -> ok when
      Registrations :: list(),
      PrintFun :: fun().
do_summary(Registrations, PrintFun) ->
    [PrintFun(Registration) || {_, Registration} <- Registrations].

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec print_summary/1 :: (Registration) -> ok when
      Registration :: json_object().
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
-spec print_details/1 :: (Registration) -> ok when
      Registration :: json_object().
print_details(Registration) ->
    [io:format("~s: ~s~n", [K, wh_util:to_list(V)]) || {K, V} <- Registration].
