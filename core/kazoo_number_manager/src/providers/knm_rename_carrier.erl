%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle renaming module_name for admins
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_rename_carrier).
-behaviour(knm_gen_provider).

-export([save/1]).
-export([delete/1]).

-include("knm.hrl").

-define(KEY, ?FEATURE_RENAME_CARRIER).

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is saved, and will
%% add the prepend route (for in service numbers only)
%% @end
%%------------------------------------------------------------------------------
-spec save(knm_number:knm_number()) -> knm_number:knm_number().
save(N) ->
    PN = knm_number:phone_number(N),
    Doc = knm_phone_number:doc(PN),
    Value = kz_json:get_ne_value(?KEY, Doc),
    Carrier = maybe_prefix_carrier(Value),
    case is_valid(Carrier, PN) of
        false ->
            Msg = <<"'", Value/binary, "' is not known by the system">>,
            knm_errors:invalid(N, Msg);
        true ->
            NewDoc = kz_json:delete_key(?KEY, Doc),
            Updates = [{fun knm_phone_number:set_module_name/2, Carrier}
                      ,{fun knm_phone_number:reset_doc/2, NewDoc}
                      ],
            {ok, NewPN} = knm_phone_number:setters(PN, Updates),
            knm_number:set_phone_number(N, NewPN)
    end.

%%------------------------------------------------------------------------------
%% @doc This function is called each time a number is deleted, and will
%% remove the prepend route
%% @end
%%------------------------------------------------------------------------------
-spec delete(knm_number:knm_number()) -> knm_number:knm_number().
delete(N) -> N.

maybe_prefix_carrier(<<"knm_", Carrier/binary>>) -> maybe_prefix_carrier(Carrier);
maybe_prefix_carrier(<<"wnm_", Carrier/binary>>) -> maybe_prefix_carrier(Carrier);
maybe_prefix_carrier(Carrier=?NE_BINARY) -> <<"knm_", Carrier/binary>>.

is_valid(Carrier, PN) ->
    case knm_phone_number:is_admin(PN) of
        false -> knm_errors:unauthorized();
        true ->
            ?LOG_DEBUG("allowing setting carrier to ~p", [Carrier]),
            true
    end.
