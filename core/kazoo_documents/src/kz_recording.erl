%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_recording).

-export([id/1, id/2
        ,name/1, name/2
        ,origin/1, origin/2
        ,endpoint_id/1, endpoint_id/2
        ,recorder/1, recorder/2
        ]).
-export([transfer_destination/1, transfer_destination/2
        ,transfer_method/1, transfer_method/2
        ]).


-include("kz_documents.hrl").

-type event() :: kz_json:object().
-export_type([event/0]).

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
