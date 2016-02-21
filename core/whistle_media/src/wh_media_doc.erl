%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_doc).

-export([single_attachment_url/3]).

-include("whistle_media.hrl").

-spec single_attachment_url/3 :: (ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
single_attachment_url(AccountDb, MediaId, AttachmentName) ->
    _ = case kz_datamgr:open_doc(AccountDb, MediaId) of
            {'ok', JObj} ->
                [begin
                     lager:debug("need to remove ~s/~s/~s first", [AccountDb, MediaId, Attach]),
                     kz_datamgr:delete_attachment(AccountDb, MediaId, Attach)
                 end
                 || Attach <- wh_doc:attachment_names(JObj),
                    Attach =/= AttachmentName
                ];
            {'error', _} -> 'ok'
        end,
    Rev = case kz_datamgr:lookup_doc_rev(AccountDb, MediaId) of
              {'ok', R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    list_to_binary([wh_couch_connections:get_url(), AccountDb, <<"/">>, MediaId, <<"/">>, AttachmentName, Rev]).
