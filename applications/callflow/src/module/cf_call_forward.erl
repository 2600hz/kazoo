%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_call_forward).

-include("../callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".call_forward">>).

-record(keys, {menu_toggle_cf =
                   whapps_config:get_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_toggle_option">>], <<"1">>)
               ,menu_change_number =
                   whapps_config:get_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_change_number">>], <<"2">>)
              }).
-type keys() :: #keys{}.

-record(callfwd, {keys = #keys{} :: keys()
                  ,doc_id = 'undefined' :: api_binary()
                  ,enabled = 'false' :: boolean()
                  ,number = <<>> :: binary()
                  ,require_keypress = 'true' :: boolean()
                  ,keep_caller_id = 'true' :: boolean()
                 }).
-type callfwd() :: #callfwd{}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case get_call_forward(Call) of
        {'error', _} ->
            catch({'ok', _} = whapps_call_command:b_prompt(<<"cf-not_available">>, Call)),
            cf_exe:stop(Call);
        CF ->
            whapps_call_command:answer(Call),
            CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
            CF1 = case wh_json:get_value(<<"action">>, Data) of
                      <<"activate">> -> cf_activate(CF, CaptureGroup, Call);       %% Support for NANPA *72
                      <<"deactivate">> -> cf_deactivate(CF, Call);                 %% Support for NANPA *73
                      <<"update">> -> cf_update_number(CF, CaptureGroup, Call);    %% Support for NANPA *56
                      <<"toggle">> -> cf_toggle(CF, CaptureGroup, Call);
                      <<"menu">> -> cf_menu(CF, CaptureGroup, Call)
                  end,
            {'ok', _} = update_callfwd(CF1, Call),
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function provides a menu with the call forwarding options
%% @end
%%--------------------------------------------------------------------
-spec cf_menu(callfwd(), ne_binary(), whapps_call:call()) -> callfwd().
cf_menu(#callfwd{keys=#keys{menu_toggle_cf=Toggle
                            ,menu_change_number=ChangeNum
                           }
                 ,enabled=Enabled
                }=CF, CaptureGroup, Call) ->
    lager:info("playing call forwarding menu"),
    Prompt = case Enabled of
                 'true' -> whapps_util:get_prompt(<<"cf-enabled_menu">>, Call);
                 'false' -> whapps_util:get_prompt(<<"cf-disabled_menu">>, Call)
             end,
    _  = whapps_call_command:b_flush(Call),
    case whapps_call_command:b_play_and_collect_digit(Prompt, Call) of
        {'ok', Toggle} ->
            CF1 = cf_toggle(CF, CaptureGroup, Call),
            cf_menu(CF1, CaptureGroup, Call);
        {'ok', ChangeNum} ->
            CF1 = cf_update_number(CF, CaptureGroup, Call),
            cf_menu(CF1, CaptureGroup, Call);
        {'ok', _} ->
            cf_menu(CF, CaptureGroup, Call);
        {'error', _} ->
            CF
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will update the call forwarding enabling it if it is
%% not, and disabling it if it is
%% @end
%%--------------------------------------------------------------------
-spec cf_toggle(callfwd(), 'undefined' | binary(), whapps_call:call()) -> callfwd().
cf_toggle(#callfwd{enabled='false'
                   ,number=Number
                  }=CF, _, Call) when is_binary(Number), size(Number) > 0 ->
    _ = try
            {'ok', _} = whapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = whapps_call_command:b_say(Number, Call)
        catch
            _:_ -> 'ok'
        end,
    CF#callfwd{enabled='true'};
cf_toggle(#callfwd{enabled='false'}=CF, CaptureGroup, Call) ->
    cf_activate(CF, CaptureGroup, Call);
cf_toggle(CF, _, Call) ->
    cf_deactivate(CF, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document to enable call forwarding
%% @end
%%--------------------------------------------------------------------
-spec cf_activate(callfwd(), 'undefined' | binary(), whapps_call:call()) -> callfwd().
cf_activate(CF1, CaptureGroup, Call) when is_atom(CaptureGroup); CaptureGroup =:= <<>> ->
    lager:info("activating call forwarding"),
    CF2 = #callfwd{number=Number} = cf_update_number(CF1, CaptureGroup, Call),
    _ = try
            {'ok', _} = whapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = whapps_call_command:b_say(Number, Call)
        catch
            _:_ -> 'ok'
        end,
    CF2#callfwd{enabled='true'};
cf_activate(CF, CaptureGroup, Call) ->
    lager:info("activating call forwarding with number ~s", [CaptureGroup]),
    _ = try
            {'ok', _} = whapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = whapps_call_command:b_say(CaptureGroup, Call)
        catch
            _:_ -> 'ok'
        end,
    CF#callfwd{enabled='true', number=CaptureGroup}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document to disable call forwarding
%% @end
%%--------------------------------------------------------------------
-spec cf_deactivate(callfwd(), whapps_call:call()) -> callfwd().
cf_deactivate(CF, Call) ->
    lager:info("deactivating call forwarding"),
    catch({'ok', _} = whapps_call_command:b_prompt(<<"cf-disabled">>, Call)),
    CF#callfwd{enabled='false'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document with a new number
%% @end
%%--------------------------------------------------------------------
-spec cf_update_number(callfwd(), 'undefined' | binary(), whapps_call:call()) -> callfwd().
cf_update_number(CF, CaptureGroup, Call) when is_atom(CaptureGroup); CaptureGroup =:= <<>> ->
    EnterNumber = whapps_util:get_prompt(<<"cf-enter_number">>, Call),
    case whapps_call_command:b_play_and_collect_digits(<<"3">>, <<"20">>, EnterNumber, <<"1">>, <<"8000">>, Call) of
        {'ok', <<>>} -> cf_update_number(CF, CaptureGroup, Call);
        {'ok', Number} ->
            _ = whapps_call_command:b_prompt(<<"vm-saved">>, Call),
            lager:info("update call forwarding number with ~s", [Number]),
            CF#callfwd{number=Number};
        {'error', _} -> exit('normal')
    end;
cf_update_number(CF, CaptureGroup, _) ->
    lager:info("update call forwarding number with ~s", [CaptureGroup]),
    CF#callfwd{number=CaptureGroup}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is a helper function to update a document, and corrects the
%% rev tag if the document is in conflict
%% @end
%%--------------------------------------------------------------------
-spec update_callfwd(callfwd(), whapps_call:call()) ->
                            {'ok', wh_json:object()} |
                            {'error', atom()}.
update_callfwd(#callfwd{doc_id=Id
                        ,enabled=Enabled
                        ,number=Num
                        ,require_keypress=_RK
                        ,keep_caller_id=_KCI
                       }=CF, Call) ->
    lager:info("updating call forwarding settings on ~s", [Id]),
    AccountDb = whapps_call:account_db(Call),
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, Id),
    CFObj = wh_json:get_ne_value(<<"call_forward">>, JObj, wh_json:new()),
    Updates = [fun(J) -> wh_json:set_value(<<"enabled">>, Enabled, J) end
               ,fun(J) -> wh_json:set_value(<<"number">>, Num, J) end
              ],
    CFObj1 = lists:foldl(fun(F, Acc) -> F(Acc) end, CFObj, Updates),
    case couch_mgr:save_doc(AccountDb, wh_json:set_value(<<"call_forward">>, CFObj1, JObj)) of
        {'error', 'conflict'} ->
            lager:info("update conflicted, trying again"),
            update_callfwd(CF, Call);
        {'ok', _JObj1}=OK ->
            lager:info("updated call forwarding in db"),
            OK;
        {'error', R}=E ->
            lager:info("failed to update call forwarding in db ~w", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will load the call forwarding record
%% @end
%%--------------------------------------------------------------------
-spec get_call_forward(whapps_call:call()) ->
                              callfwd() |
                              {'error', callfwd()}.
get_call_forward(Call) ->
    AccountDb = whapps_call:account_db(Call),
    AuthorizingId = whapps_call:authorizing_id(Call),
    ViewOptions = [{<<"key">>, AuthorizingId}],
    Id = case couch_mgr:get_results(AccountDb, <<"cf_attributes/owner">>, ViewOptions) of
             {'ok', [Owner]} -> wh_json:get_value(<<"value">>, Owner, AuthorizingId);
             _E -> AuthorizingId
         end,
    case couch_mgr:open_doc(AccountDb, Id) of
        {'ok', JObj} ->
            lager:info("loaded call forwarding object from ~s", [Id]),
            #callfwd{doc_id = wh_json:get_value(<<"_id">>, JObj)
                     ,enabled = wh_json:is_true([<<"call_forward">>, <<"enabled">>], JObj)
                     ,number = wh_json:get_ne_value([<<"call_forward">>, <<"number">>], JObj, <<>>)
                     ,require_keypress = wh_json:is_true([<<"call_forward">>, <<"require_keypress">>], JObj, 'true')
                     ,keep_caller_id = wh_json:is_true([<<"call_forward">>, <<"keep_caller_id">>], JObj, 'true')
                    };
        {'error', R} ->
            lager:info("failed to load call forwarding object from ~s, ~w", [Id, R]),
            {'error', #callfwd{}}
    end.
