%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_voicemail_keys).

-export([new/0]).
-export([configure/1, configure/2, set_configure/2]).
-export([continue/1, continue/2, set_continue/2]).
-export([delete/1, delete/2, set_delete/2]).
-export([exit/1, exit/2, set_exit/2]).
-export([fastforward/1, fastforward/2, set_fastforward/2]).
-export([hear_new/1, hear_new/2, set_hear_new/2]).
-export([hear_saved/1, hear_saved/2, set_hear_saved/2]).
-export([keep/1, keep/2, set_keep/2]).
-export([listen/1, listen/2, set_listen/2]).
-export([login/1, login/2, set_login/2]).
-export([next/1, next/2, set_next/2]).
-export([operator/1, operator/2, set_operator/2]).
-export([prev/1, prev/2, set_prev/2]).
-export([rec_name/1, rec_name/2, set_rec_name/2]).
-export([rec_unavailable/1, rec_unavailable/2, set_rec_unavailable/2]).
-export([record/1, record/2, set_record/2]).
-export([replay/1, replay/2, set_replay/2]).
-export([return_main/1, return_main/2, set_return_main/2]).
-export([rewind/1, rewind/2, set_rewind/2]).
-export([save/1, save/2, set_save/2]).
-export([set_pin/1, set_pin/2, set_set_pin/2]).

-compile({'no_auto_import', [exit/2]}).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"voicemail_keys">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec configure(doc()) -> kz_term:ne_binary().
configure(Doc) ->
    configure(Doc, <<"5">>).

-spec configure(doc(), Default) -> kz_term:ne_binary() | Default.
configure(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"configure">>], Doc, Default).

-spec set_configure(doc(), kz_term:ne_binary()) -> doc().
set_configure(Doc, Configure) ->
    kz_json:set_value([<<"configure">>], Configure, Doc).

-spec continue(doc()) -> kz_term:api_ne_binary().
continue(Doc) ->
    continue(Doc, 'undefined').

-spec continue(doc(), Default) -> kz_term:ne_binary() | Default.
continue(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"continue">>], Doc, Default).

-spec set_continue(doc(), kz_term:ne_binary()) -> doc().
set_continue(Doc, Continue) ->
    kz_json:set_value([<<"continue">>], Continue, Doc).

-spec delete(doc()) -> kz_term:ne_binary().
delete(Doc) ->
    delete(Doc, <<"7">>).

-spec delete(doc(), Default) -> kz_term:ne_binary() | Default.
delete(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"delete">>], Doc, Default).

-spec set_delete(doc(), kz_term:ne_binary()) -> doc().
set_delete(Doc, Delete) ->
    kz_json:set_value([<<"delete">>], Delete, Doc).

-spec exit(doc()) -> kz_term:ne_binary().
exit(Doc) ->
    exit(Doc, <<"#">>).

-spec exit(doc(), Default) -> kz_term:ne_binary() | Default.
exit(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"exit">>], Doc, Default).

-spec set_exit(doc(), kz_term:ne_binary()) -> doc().
set_exit(Doc, Exit) ->
    kz_json:set_value([<<"exit">>], Exit, Doc).

-spec fastforward(doc()) -> kz_term:api_ne_binary().
fastforward(Doc) ->
    fastforward(Doc, <<"8">>).

-spec fastforward(doc(), Default) -> kz_term:ne_binary() | Default.
fastforward(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"fastforward">>], Doc, Default).

-spec set_fastforward(doc(), kz_term:ne_binary()) -> doc().
set_fastforward(Doc, Fastforward) ->
    kz_json:set_value([<<"fastforward">>], Fastforward, Doc).

-spec hear_new(doc()) -> kz_term:ne_binary().
hear_new(Doc) ->
    hear_new(Doc, <<"1">>).

-spec hear_new(doc(), Default) -> kz_term:ne_binary() | Default.
hear_new(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hear_new">>], Doc, Default).

-spec set_hear_new(doc(), kz_term:ne_binary()) -> doc().
set_hear_new(Doc, HearNew) ->
    kz_json:set_value([<<"hear_new">>], HearNew, Doc).

-spec hear_saved(doc()) -> kz_term:ne_binary().
hear_saved(Doc) ->
    hear_saved(Doc, <<"2">>).

-spec hear_saved(doc(), Default) -> kz_term:ne_binary() | Default.
hear_saved(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"hear_saved">>], Doc, Default).

-spec set_hear_saved(doc(), kz_term:ne_binary()) -> doc().
set_hear_saved(Doc, HearSaved) ->
    kz_json:set_value([<<"hear_saved">>], HearSaved, Doc).

-spec keep(doc()) -> kz_term:ne_binary().
keep(Doc) ->
    keep(Doc, <<"1">>).

-spec keep(doc(), Default) -> kz_term:ne_binary() | Default.
keep(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"keep">>], Doc, Default).

-spec set_keep(doc(), kz_term:ne_binary()) -> doc().
set_keep(Doc, Keep) ->
    kz_json:set_value([<<"keep">>], Keep, Doc).

-spec listen(doc()) -> kz_term:ne_binary().
listen(Doc) ->
    listen(Doc, <<"2">>).

-spec listen(doc(), Default) -> kz_term:ne_binary() | Default.
listen(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"listen">>], Doc, Default).

-spec set_listen(doc(), kz_term:ne_binary()) -> doc().
set_listen(Doc, Listen) ->
    kz_json:set_value([<<"listen">>], Listen, Doc).

-spec login(doc()) -> kz_term:ne_binary().
login(Doc) ->
    login(Doc, <<"*">>).

-spec login(doc(), Default) -> kz_term:ne_binary() | Default.
login(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"login">>], Doc, Default).

-spec set_login(doc(), kz_term:ne_binary()) -> doc().
set_login(Doc, Login) ->
    kz_json:set_value([<<"login">>], Login, Doc).

-spec next(doc()) -> kz_term:ne_binary().
next(Doc) ->
    next(Doc, <<"6">>).

-spec next(doc(), Default) -> kz_term:ne_binary() | Default.
next(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"next">>], Doc, Default).

-spec set_next(doc(), kz_term:ne_binary()) -> doc().
set_next(Doc, Next) ->
    kz_json:set_value([<<"next">>], Next, Doc).

-spec operator(doc()) -> kz_term:ne_binary().
operator(Doc) ->
    operator(Doc, <<"0">>).

-spec operator(doc(), Default) -> kz_term:ne_binary() | Default.
operator(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"operator">>], Doc, Default).

-spec set_operator(doc(), kz_term:ne_binary()) -> doc().
set_operator(Doc, Operator) ->
    kz_json:set_value([<<"operator">>], Operator, Doc).

-spec prev(doc()) -> kz_term:ne_binary().
prev(Doc) ->
    prev(Doc, <<"4">>).

-spec prev(doc(), Default) -> kz_term:ne_binary() | Default.
prev(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"prev">>], Doc, Default).

-spec set_prev(doc(), kz_term:ne_binary()) -> doc().
set_prev(Doc, Prev) ->
    kz_json:set_value([<<"prev">>], Prev, Doc).

-spec rec_name(doc()) -> kz_term:ne_binary().
rec_name(Doc) ->
    rec_name(Doc, <<"2">>).

-spec rec_name(doc(), Default) -> kz_term:ne_binary() | Default.
rec_name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"rec_name">>], Doc, Default).

-spec set_rec_name(doc(), kz_term:ne_binary()) -> doc().
set_rec_name(Doc, RecName) ->
    kz_json:set_value([<<"rec_name">>], RecName, Doc).

-spec rec_unavailable(doc()) -> kz_term:ne_binary().
rec_unavailable(Doc) ->
    rec_unavailable(Doc, <<"1">>).

-spec rec_unavailable(doc(), Default) -> kz_term:ne_binary() | Default.
rec_unavailable(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"rec_unavailable">>], Doc, Default).

-spec set_rec_unavailable(doc(), kz_term:ne_binary()) -> doc().
set_rec_unavailable(Doc, RecUnavailable) ->
    kz_json:set_value([<<"rec_unavailable">>], RecUnavailable, Doc).

-spec record(doc()) -> kz_term:ne_binary().
record(Doc) ->
    record(Doc, <<"3">>).

-spec record(doc(), Default) -> kz_term:ne_binary() | Default.
record(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"record">>], Doc, Default).

-spec set_record(doc(), kz_term:ne_binary()) -> doc().
set_record(Doc, Record) ->
    kz_json:set_value([<<"record">>], Record, Doc).

-spec replay(doc()) -> kz_term:ne_binary().
replay(Doc) ->
    replay(Doc, <<"2">>).

-spec replay(doc(), Default) -> kz_term:ne_binary() | Default.
replay(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"replay">>], Doc, Default).

-spec set_replay(doc(), kz_term:ne_binary()) -> doc().
set_replay(Doc, Replay) ->
    kz_json:set_value([<<"replay">>], Replay, Doc).

-spec return_main(doc()) -> kz_term:ne_binary().
return_main(Doc) ->
    return_main(Doc, <<"0">>).

-spec return_main(doc(), Default) -> kz_term:ne_binary() | Default.
return_main(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"return_main">>], Doc, Default).

-spec set_return_main(doc(), kz_term:ne_binary()) -> doc().
set_return_main(Doc, ReturnMain) ->
    kz_json:set_value([<<"return_main">>], ReturnMain, Doc).

-spec rewind(doc()) -> kz_term:api_ne_binary().
rewind(Doc) ->
    rewind(Doc, <<"5">>).

-spec rewind(doc(), Default) -> kz_term:ne_binary() | Default.
rewind(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"rewind">>], Doc, Default).

-spec set_rewind(doc(), kz_term:ne_binary()) -> doc().
set_rewind(Doc, Rewind) ->
    kz_json:set_value([<<"rewind">>], Rewind, Doc).

-spec save(doc()) -> kz_term:ne_binary().
save(Doc) ->
    save(Doc, <<"1">>).

-spec save(doc(), Default) -> kz_term:ne_binary() | Default.
save(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"save">>], Doc, Default).

-spec set_save(doc(), kz_term:ne_binary()) -> doc().
set_save(Doc, Save) ->
    kz_json:set_value([<<"save">>], Save, Doc).

-spec set_pin(doc()) -> kz_term:ne_binary().
set_pin(Doc) ->
    set_pin(Doc, <<"3">>).

-spec set_pin(doc(), Default) -> kz_term:ne_binary() | Default.
set_pin(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"set_pin">>], Doc, Default).

-spec set_set_pin(doc(), kz_term:ne_binary()) -> doc().
set_set_pin(Doc, SetPin) ->
    kz_json:set_value([<<"set_pin">>], SetPin, Doc).
