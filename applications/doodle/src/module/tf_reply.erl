%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Luis Azedo
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tf_reply).

-include("../doodle.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_im:im()) -> 'ok'.
handle(Data, Im) ->
    Body = kz_json:get_value(<<"body">>, Data, <<"empty text">>),
    Funs = [{fun kapps_im:set_from/2, kapps_im:to(Im)}
           ,{fun kapps_im:set_to/2, kapps_im:from(Im)}
           ,{fun kapps_im:set_body/2, Body}
           ,{fun kapps_im:set_message_id/2, kz_binary:rand_hex(16)}
           ,{fun kapps_im:remove_custom_vars/2, [<<"Authorizing-ID">>]}
           ,{fun kapps_im:set_custom_var/3, <<"Authorizing-Type">>, <<"account">>}
           ],
    kapi_im:publish_inbound(kapps_im:to_payload(kapps_im:exec(Funs, Im))),
    tf_exe:stop(Im, 'replied').
