%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Brian Davis
%%%-------------------------------------------------------------------
-module(cf_check_cid).

-export([handle/2]).

-include("../callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Number = whapps_call:caller_id_number(Call),
    Regex = wh_json:get_value(<<"regex">>, Data, <<".*">>),
    lager:debug("Comparing caller id ~s against regex ~s", [Number, Regex]),
    case re:run(Number, Regex) of
       {match, _} -> handle_match(Number, Data, Call);
       nomatch -> handle_no_match(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_match(ne_binary(), wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle_match(CallerId, Data, Call) ->
   case wh_json:is_true(<<"use_absolute_mode">>, Data, 'false') of
      true -> 
         case is_callflow_child(CallerId, Call) of
            true ->
               update_caller_identity(Data, Call),
               'ok';
            false -> cf_exe:continue(Call)
         end;
      false -> 
         case is_callflow_child(<<"match">>, Call) of
            true ->
               update_caller_identity(Data, Call),
               'ok';
            false -> cf_exe:continue(Call)
         end
      end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle a caller id "no match" condition
%% @end
%%--------------------------------------------------------------------
-spec handle_no_match(whapps_call:call()) -> 'ok'.
handle_no_match(Call) ->
    case is_callflow_child(<<"nomatch">>, Call) of
       true -> 'ok';
       false -> cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check if the given node name is a callflow child 
%% @end
%%--------------------------------------------------------------------
-spec is_callflow_child(ne_binary(), whapps_call:call()) -> boolean().
is_callflow_child(Name, Call) ->
    lager:debug("Looking for callflow child ~s", [Name]),
    case cf_exe:attempt(Name, Call) of
        {attempt_resp, ok} ->
            lager:debug("found callflow child"),
            true;
        {attempt_resp, {error, _}} ->
            lager:debug("failed to find callflow child"),
            false;
        _ ->
            lager:debug("unrecognized response from attempt"),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc update the caller id and owner information for this call
%% @end
%%--------------------------------------------------------------------
-spec update_caller_identity(wh_json:json_object(), whapps_call:call()) -> 'ok'.
update_caller_identity(Data, Call) ->
    Name = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"name">>], Data),
    Number = wh_json:get_ne_value([<<"caller_id">>, <<"external">>, <<"number">>], Data), 
    UserId =  wh_json:get_ne_value([<<"user_id">>], Data),
    case is_valid_caller_identity(Name, Number, UserId, Call) of
        'true' ->
            lager:info("setting caller id to ~s <~s>", [Number, Name]),
            lager:info("setting owner id to ~s", [UserId]),
            Updates = [ fun(C) -> whapps_call:set_caller_id_number(Number, C) end,
                        fun(C) -> whapps_call:set_caller_id_name(Name, C) end,
                        fun(C) -> whapps_call:kvs_store(owner_id, UserId, C) end
                      ],
            {ok, C} = cf_exe:get_call(Call),
            cf_exe:set_call(whapps_call:exec(Updates, C)),
            'ok';
        'false' ->
            'ok' 
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc validate that all required parameters are defined
%% @end
%%--------------------------------------------------------------------
-spec is_valid_caller_identity(api_binary(), api_binary(), api_binary(), whapps_call:call()) -> boolean().
is_valid_caller_identity('undefined', 'undefined', 'undefined', _) -> 'false';
is_valid_caller_identity('undefined', '_', '_', _) -> 'false';
is_valid_caller_identity('_', 'undefined', '_',  _) -> 'false';
is_valid_caller_identity('_', '_', 'undefined',  _) -> 'false';
is_valid_caller_identity(_, _, _, _) -> 'true'.

