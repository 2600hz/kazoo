%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
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
    _ = case couch_mgr:open_doc(AccountDb, MediaId) of
            {ok, JObj} ->
                [begin
                     lager:debug("need to remove ~s/~s/~s first", [AccountDb, MediaId, Attach]),
                     couch_mgr:delete_attachment(AccountDb, MediaId, Attach)
                 end
                 || Attach <- wh_json:get_keys(wh_json:get_value(<<"_attachments">>, JObj, wh_json:new()))
                        ,Attach =/= AttachmentName
                ];
            {error, _} -> ok
        end,
    Rev = case couch_mgr:lookup_doc_rev(AccountDb, MediaId) of
              {ok, R} -> <<"?rev=", R/binary>>;
              _ -> <<>>
          end,
    list_to_binary([wh_couch_connections:get_url(), AccountDb, <<"/">>, MediaId, <<"/">>, AttachmentName, Rev]).
