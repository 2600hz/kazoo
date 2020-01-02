%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc menu greeting media when referencing other account's media
%%%
%%% 1. create media in account 1
%%% 2. create menu callflow in account 2
%%%   a. set "greeting" to "/{ACCOUNT_1}/{MEDIA_ID}"
%%% 3. fetch media path as ecallmgr would
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_forum_10598).

-export([seq/0
        ,cleanup/0, cleanup/1
        ]).

-include("kazoo_proper.hrl").

-spec seq() -> 'ok'.
seq() ->
    API = pqc_cb_api:init_api(['crossbar', 'media_mgr']
                             ,['cb_media']
                             ),
    ParentAccountId = create_account(API, <<?MODULE_STRING, "_parent">>),
    ChildAccountId = create_account(API, <<?MODULE_STRING, "_child">>),

    CreateMetaResp = pqc_cb_media:create(API, ParentAccountId, pqc_cb_media:new_media_doc()),
    lager:info("created media meta: ~s", [CreateMetaResp]),
    MediaId = kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(CreateMetaResp)),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),
    _UploadResp = pqc_cb_media:update_binary(API, ParentAccountId, MediaId, MP3),
    lager:info("uploaded mp3 to media: ~s", [_UploadResp]),

    CreatedMenuResp = pqc_cb_menus:create(API, ChildAccountId, new_menu(ParentAccountId, MediaId)),
    lager:info("created menu: ~s", [CreatedMenuResp]),
    Menu = kz_json:get_json_value([<<"data">>], kz_json:decode(CreatedMenuResp)),

    GreetingPrompt = kz_media_util:media_path(kzd_menus:media_greeting(Menu), ChildAccountId),
    lager:info("greeting prompt: ~s", [GreetingPrompt]),

    CallId = kz_binary:rand_hex(3),
    PlayCommand = kapps_call_command:play_command(GreetingPrompt, CallId),
    lager:info("play command: ~p", [PlayCommand]),

    MediaName = kz_json:get_ne_binary_value(<<"Media-Name">>, PlayCommand),
    {'ok', MediaPath} = ecallmgr_util:request_media_url(MediaName, 'new', CallId, PlayCommand),
    lager:info("media path for ~s: ~s", [MediaName, MediaPath]),

    {'ok', 200, _, MP3} = kz_http:get(MediaPath),
    lager:info("got mp3 from ~s", [MediaPath]),

    cleanup(API).

-spec new_menu(kz_term:ne_binary(), kz_term:ne_binary()) -> kzd_menus:doc().
new_menu(AccountId, MediaId) ->
    Greeting = <<"/", AccountId/binary, "/", MediaId/binary>>,
    kzd_menus:set_media_greeting(pqc_cb_menus:new_menu(), Greeting).

-spec create_account(pqc_cb_api:state(), kz_term:ne_binary()) -> kz_term:ne_binary().
create_account(API, Name) ->
    AccountResp = pqc_cb_accounts:create_account(API, Name),
    lager:info("created account: ~s", [AccountResp]),

    kz_json:get_ne_binary_value([<<"data">>, <<"id">>], kz_json:decode(AccountResp)).

-spec cleanup() -> 'ok'.
cleanup() ->
    _ = pqc_cb_accounts:cleanup_accounts([<<?MODULE_STRING, "_parent">>, <<?MODULE_STRING, "_child">>]),
    cleanup_system().

-spec cleanup(pqc_cb_api:state()) -> 'ok'.
cleanup(API) ->
    lager:info("CLEANUP TIME, EVERYBODY HELPS"),
    _ = pqc_cb_accounts:cleanup_accounts([<<?MODULE_STRING, "_parent">>, <<?MODULE_STRING, "_child">>]),
    _ = pqc_cb_api:cleanup(API),
    cleanup_system().

cleanup_system() -> 'ok'.
