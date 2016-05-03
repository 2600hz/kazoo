%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_media).

-export([new/0
         ,type/0
         ,prompt_id/1, prompt_id/2
         ,is_prompt/1
         ,language/1, language/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(PVT_TYPE, <<"media">>).
-define(PROMPT_ID, <<"prompt_id">>).
-define(LANGUAGE, <<"language">>).

-spec new() -> doc().
new() ->
    kz_json:from_list([{<<"pvt_type">>, type()}]).

-spec type() -> ne_binary().
type() -> ?PVT_TYPE.

-spec prompt_id(doc()) -> api(binary()).
-spec prompt_id(doc(), Default) -> ne_binary() | Default.
prompt_id(Doc) ->
    prompt_id(Doc, 'undefined').
prompt_id(Doc, Default) ->
    kz_json:get_binary_value(?PROMPT_ID, Doc, Default).

-spec is_prompt(doc()) -> boolean().
is_prompt(Doc) ->
    prompt_id(Doc) =/= 'undefined'.

-spec language(doc()) -> api(binary()).
-spec language(doc(), Default) -> ne_binary() | Default.
language(Doc) ->
    language(Doc, 'undefined').
language(Doc, Default) ->
    kz_json:get_binary_value(?LANGUAGE, Doc, Default).
