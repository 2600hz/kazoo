%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_recording).

-export([id/1, id/2
        ,name/1, name/2
        ,origin/1, origin/2
        ,endpoint_id/1, endpoint_id/2
        ,recorder/1, recorder/2
        ,data/1, data/2
        ]).
-export([transfer_destination/1, transfer_destination/2
        ,transfer_method/1, transfer_method/2
        ,file_path/1
        ,response_media/1
        ]).


-include("kz_documents.hrl").

-type event() :: kz_json:object().
-export_type([event/0]).

-type media_directory() :: file:filename_all().
-type media_name() :: file:filename_all().
-type media() :: {media_directory() | 'undefined', media_name()}.
-export_type([media_directory/0
             ,media_name/0
             ,media/0
             ]).

-define(RECORDING_ROOT, <<"Recording">>).

-spec id(event()) -> kz_term:api_ne_binary().
id(JObj) ->
    id(JObj, 'undefined').

-spec id(event(), Default) -> kz_term:ne_binary() | Default.
id(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"ID">>], JObj, Default).

-spec name(event()) -> kz_term:api_ne_binary().
name(JObj) ->
    name(JObj, 'undefined').

-spec name(event(), Default) -> kz_term:ne_binary() | Default.
name(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Name">>], JObj, Default).

-spec origin(event()) -> kz_term:api_ne_binary().
origin(JObj) ->
    origin(JObj, 'undefined').

-spec origin(event(), Default) -> kz_term:ne_binary() | Default.
origin(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Origin">>], JObj, Default).

-spec endpoint_id(event()) -> kz_term:api_ne_binary().
endpoint_id(JObj) ->
    endpoint_id(JObj, 'undefined').

-spec endpoint_id(event(), Default) -> kz_term:ne_binary() | Default.
endpoint_id(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Endpoint-ID">>], JObj, Default).

-spec recorder(event()) -> kz_term:api_ne_binary().
recorder(JObj) ->
    recorder(JObj, 'undefined').

-spec recorder(event(), Default) -> kz_term:ne_binary() | Default.
recorder(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Recorder">>], JObj, Default).

-spec transfer_destination(event()) -> kz_term:api_ne_binary().
transfer_destination(JObj) ->
    transfer_destination(JObj, 'undefined').

-spec transfer_destination(event(), Default) -> kz_term:ne_binary() | Default.
transfer_destination(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Transfer-Destination">>], JObj, Default).

-spec transfer_method(event()) -> kz_term:api_ne_binary().
transfer_method(JObj) ->
    transfer_method(JObj, 'undefined').

-spec transfer_method(event(), Default) -> kz_term:ne_binary() | Default.
transfer_method(JObj, Default) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Transfer-Method">>], JObj, Default).

-spec file_path(event()) -> kz_term:api_ne_binary().
file_path(JObj) ->
    kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"File-Path">>], JObj).

-spec data(event()) -> term().
data(JObj) ->
    data(JObj, 'undefined').

-spec data(event(), term()) -> term().
data(JObj, Default) ->
    case kz_json:get_ne_binary_value([?RECORDING_ROOT, <<"Data">>], JObj) of
        'undefined' -> Default;
        Data -> binary_to_term(base64:decode(Data))
    end.

-spec response_media(kz_json:object()) -> media().
response_media(JObj) ->
    Filename = file_path(JObj),
    {filename:dirname(Filename), filename:basename(Filename)}.
