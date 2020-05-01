%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_att_error).

-include("kz_att.hrl").

-export([new/1, new/2]).
-export([fetch_routines/4, put_routines/6]).
-export([db_name/1, set_db_name/2]).
-export([document_id/1, set_document_id/2]).
-export([attachment_name/1, set_attachment_name/2]).
-export([handler_props/1, set_handler_props/2]).
-export([req_url/1, set_req_url/2]).
-export([resp_body/1, set_resp_body/2]).
-export([resp_code/1, set_resp_code/2]).
-export([resp_headers/1, set_resp_headers/2]).
-export([options/1, set_options/2]).

-export([to_json/1]).

-type req_url() :: kz_term:ne_binary().
-type resp_code() :: pos_integer() | atom().
%% HTTP status codes line `400', `404', `409', etc.
-type resp_body() :: binary() | bitstring() | atom().
%% Response body in forms of binary encoded of `map()' or literal binary `<<"example">>' or atoms like `not_found' or `return_id_missing', etc.
-type resp_headers() :: kz_term:proplist().

-type extended_error() :: #{'db_name' => gen_attachment:db_name()
                           ,'document_id' => gen_attachment:doc_id()
                           ,'attachment_name' => gen_attachment:att_name()
                           ,'handler_props' => gen_attachment:handler_props()
                           ,'req_url' => req_url()
                           ,'resp_code' => resp_code()
                           ,'resp_body' => resp_body()
                           ,'resp_headers' => resp_headers()
                           ,'options' => gen_attachment:options() | 'undefined'
                           }.

-type update_routines() :: [{fun((extended_error(), Value) -> extended_error()), Value}].
-type error() :: {'error', kz_datamgr:data_errors(), extended_error()}.
-export_type([error/0
             ,update_routines/0
             ]).

-spec new(kz_datamgr:data_errors()) -> error().
new(Reason) ->
    new(Reason, []).

-spec new(kz_datamgr:data_errors(), update_routines()) -> error().
new(Reason, Routines) ->
    %% Add default values for `required' unset keys. `props:insert_values/2' doesn't
    %% override values, it only add a key/value tuple if that key is not already defined
    %% within the destination proplist, in this case, within `Routines' proplist.
    NewRoutines = props:insert_values(default_routines(Reason), Routines),
    Extended = lists:foldl(fun({F, Value}, M) ->
                                   F(M, Value)
                           end
                          ,#{}
                          ,NewRoutines
                          ),
    {'error', Reason, Extended}.

-spec fetch_routines(gen_attachment:handler_props()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ) -> update_routines().
fetch_routines(HandlerProps, DbName, DocumentId, AttachmentName) ->
    [{fun set_handler_props/2, HandlerProps}
    ,{fun set_db_name/2, DbName}
    ,{fun set_document_id/2, DocumentId}
    ,{fun set_attachment_name/2, AttachmentName}
    ].

-spec put_routines(gen_attachment:settings()
                  ,gen_attachment:db_name()
                  ,gen_attachment:doc_id()
                  ,gen_attachment:att_name()
                  ,gen_attachment:contents()
                  ,gen_attachment:options()
                  ) -> update_routines().
put_routines(Settings, DbName, DocumentId, AttachmentName, _Contents, Options) ->
    [{fun set_handler_props/2, Settings}
    ,{fun set_db_name/2, DbName}
    ,{fun set_document_id/2, DocumentId}
    ,{fun set_attachment_name/2, AttachmentName}
    ,{fun set_options/2, Options}
    ].

-spec db_name(extended_error()) -> gen_attachment:db_name().
db_name(#{'db_name' := DbName}) ->
    DbName.

-spec set_db_name(extended_error(), gen_attachment:db_name()) -> extended_error().
set_db_name(ExtendedError, DbName) ->
    ExtendedError#{'db_name' => DbName}.

-spec document_id(extended_error()) -> gen_attachment:doc_id().
document_id(#{'document_id' := DocumentId}) ->
    DocumentId.

-spec set_document_id(extended_error(), gen_attachment:doc_id()) -> extended_error().
set_document_id(ExtendedError, DocumentId) ->
    ExtendedError#{'document_id' => DocumentId}.

-spec attachment_name(extended_error()) -> gen_attachment:att_name().
attachment_name(#{'attachment_name' := AttachmentName}) ->
    AttachmentName.

-spec set_attachment_name(extended_error(), gen_attachment:att_name()) -> extended_error().
set_attachment_name(ExtendedError, AttachmentName) ->
    ExtendedError#{'attachment_name' => AttachmentName}.

-spec handler_props(extended_error()) -> gen_attachment:handler_props().
handler_props(#{'handler_props' := HandlerProps}) ->
    HandlerProps.

-spec set_handler_props(extended_error(), gen_attachment:handler_props()) -> extended_error().
set_handler_props(ExtendedError, HandlerProps) ->
    ExtendedError#{'handler_props' => HandlerProps}.

-spec req_url(extended_error()) -> req_url().
req_url(#{'req_url' := Url}) ->
    Url.

-spec set_req_url(extended_error(), req_url()) -> extended_error().
set_req_url(ExtendedError, Url) ->
    ExtendedError#{'req_url' => Url}.

-spec resp_body(extended_error()) -> resp_body().
resp_body(#{'resp_body' := Body}) ->
    Body.

-spec set_resp_body(extended_error(), resp_body()) -> extended_error().
set_resp_body(ExtenedError, Body) ->
    ExtenedError#{'resp_body' => Body}.

-spec resp_code(extended_error()) -> resp_code().
resp_code(#{'resp_code' := Code}) ->
    Code.

-spec set_resp_code(extended_error(), resp_code()) -> extended_error().
set_resp_code(ExtenedError, Code) ->
    ExtenedError#{'resp_code' => Code}.

-spec resp_headers(extended_error()) -> resp_headers().
resp_headers(#{'resp_headers' := Headers}) ->
    Headers.

-spec set_resp_headers(extended_error(), resp_headers()) -> extended_error().
set_resp_headers(ExtendedError, Headers) ->
    ExtendedError#{'resp_headers' => [{kz_term:to_binary(K), kz_term:to_binary(V)}|| {K,V} <- Headers]}.

-spec options(extended_error()) -> gen_attachment:options().
options(#{'options' := Options}) ->
    Options.

-spec set_options(extended_error(), gen_attachment:options()) -> extended_error().
set_options(ExtendedError, Options) ->
    ExtendedError#{'options' => Options}.

-spec to_json(extended_error()) -> kz_json:object().
to_json(ExtendedError) ->
    kz_json:from_map(ExtendedError).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc There are some errors for which the attachment handlers don't get an error_code nor a
%% resp_body, so this function tries to return some meaningful default routines based on
%% the error reason. Thus, no need to call `[{fun kz_att_error:set_resp_code/2, 401} | Routines]'
%% in every place we return `oauth_failure' error.
%% @end
%%------------------------------------------------------------------------------
default_routines('oauth_failure') ->
    [{fun set_resp_code/2, 401}
    ,{fun set_resp_body/2, <<>>}
    ,{fun set_resp_headers/2, []}
    ];
default_routines(_) ->
    [{fun set_resp_code/2, 500}
    ,{fun set_resp_body/2, <<>>}
    ,{fun set_resp_headers/2, []}
    ].
