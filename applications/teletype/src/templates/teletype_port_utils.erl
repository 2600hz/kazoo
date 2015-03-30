%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(teletype_port_utils).

-export([get_attachments/1]).
-export([fix_email/1]).
-export([fix_port_request_data/1]).

-include("../teletype.hrl").

-spec get_attachments(wh_json:object()) -> attachments().
-spec get_attachments(wh_json:object(), boolean()) -> attachments().
get_attachments(DataJObj) ->
    get_attachments(DataJObj, teletype_util:is_preview(DataJObj)).

get_attachments(_DataJObj, 'true') -> [];
get_attachments(DataJObj, 'false') ->
    PortReqId = wh_json:get_value(<<"port_request_id">>, DataJObj),
    Doc = wh_json:get_value(<<"port_request">>, DataJObj),

    lists:foldl(fun(Name, Acc) -> get_attachment_fold(Name, Acc, PortReqId, Doc) end
                ,[]
                ,wh_doc:attachment_names(Doc)
               ).

-spec get_attachment_fold(wh_json:key(), attachments(), ne_binary(), wh_json:object()) ->
                                 attachments().
get_attachment_fold(Name, Acc, PortReqId, Doc) ->
    {'ok', Attachment} = couch_mgr:fetch_attachment(?KZ_PORT_REQUESTS_DB, PortReqId, Name),
    ContentType = wh_doc:attachment_content_type(Doc, Name),
    [{ContentType, Name, decode_base64(Attachment)}|Acc].

-spec decode_base64(ne_binary()) -> {api_binary(), ne_binary()}.
decode_base64(Base64) ->
    case binary:split(Base64, <<",">>) of
        [Bin] ->
            lager:debug("not split on ','"),
            corrected_base64_decode(Bin);
        [<<"data:", _/binary>>, Bin] ->
             corrected_base64_decode(Bin);
        [_SplitLeft, _SplitRight] ->
            lager:debug("split unexpectedly: ~p/~p", [byte_size(_SplitLeft), byte_size(_SplitRight)]),
            lager:debug("l: ~s", [binary:part(_SplitLeft, byte_size(_SplitLeft), -20)]),
            lager:debug("r: ~s", [binary:part(_SplitRight, byte_size(_SplitRight), -10)]),
            corrected_base64_decode(Base64)
    end.

-spec corrected_base64_decode(ne_binary()) -> ne_binary().
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 3 ->
    base64:mime_decode(<<Base64/binary, "=">>);
corrected_base64_decode(Base64) when byte_size(Base64) rem 4 == 2 ->
    base64:mime_decode(<<Base64/binary, "==">>);
corrected_base64_decode(Base64) ->
    base64:mime_decode(Base64).


-spec fix_email(wh_json:object()) -> wh_json:object().
fix_email(ReqData) ->
    fix_email(ReqData
              ,wh_json:get_value([<<"port_request">>, <<"notifications">>, <<"email">>, <<"send_to">>], ReqData)
             ).

-spec fix_email(wh_json:object(), ne_binary() | ne_binaries()) -> wh_json:object().
fix_email(ReqData, <<_/binary>> = Email) ->
    wh_json:set_value(<<"to">>, [Email], ReqData);
fix_email(ReqData, [_|_]=Emails) ->
    wh_json:set_value(<<"to">>, Emails, ReqData).

-spec fix_port_request_data(wh_json:object()) -> wh_json:object().
fix_port_request_data(JObj) ->
    Routines = [fun fix_numbers/1
                ,fun fix_billing/1
                ,fun fix_comments/1
               ],
    lists:foldl(fun(F, J) -> F(J) end, JObj, Routines).

-spec fix_numbers(wh_json:object()) -> wh_json:object().
fix_numbers(JObj) ->
    Numbers =  wh_json:foldl(fun fix_number_fold/3
                             ,[]
                             ,wh_json:get_value(<<"numbers">>, JObj)
                            ),
    wh_json:set_value(<<"numbers">>, Numbers, JObj).

-spec fix_number_fold(wh_json:object(), _, wh_json:keys()) -> wh_json:keys().
fix_number_fold(Number, _Value, Acc) ->
    [Number|Acc].

-spec fix_billing(wh_json:object()) -> wh_json:object().
fix_billing(JObj) ->
    wh_json:foldl(
      fun fix_billing_fold/3
      ,wh_json:delete_key(<<"bill">>, JObj)
      ,wh_json:get_value(<<"bill">>, JObj)
     ).

-spec fix_billing_fold(wh_json:key(), wh_json:json_term(), wh_json:object()) ->
                              wh_json:object().
fix_billing_fold(Key, Value, Acc) ->
    wh_json:set_value(<<"bill_", Key/binary>>, Value, Acc).

-spec fix_comments(wh_json:object()) -> wh_json:object().
fix_comments(JObj) ->
    Comments =
        lists:foldl(
            fun fix_comment_fold/2
            ,[]
            ,wh_json:get_value(<<"comments">>, JObj)
        ),
    wh_json:set_value(<<"comments">>, Comments, JObj).

-spec fix_comment_fold(wh_json:object(), [wh_proplist(), ...]) -> [wh_proplist(), ...].
fix_comment_fold(JObj, Acc) ->
    [wh_json:to_proplist(JObj)|Acc].
