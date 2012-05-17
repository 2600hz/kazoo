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
        {ok, JObj} ->
            lager:debug("rxfax resp: ~p", [JObj]),

            %% store Fax in DB

            case wh_json:get_value(<<"emails">>, Data) of
                undefined ->
                    lager:debug("no emails configured");
                [] ->
                    lager:debug("no emails configured");
                Emls ->
                    lager:debug("sending emails to ~p", [Emls])
            end,

            cf_exe:continue(Call);
        {error, channel_hungup} ->
            lager:debug("rxfax hungup prematurely"),
            cf_exe:stop(Call);
        _Resp ->
            lager:debug("rxfax unhandled: ~p", [_Resp]),
            cf_exe:continue(Call)
    end.
