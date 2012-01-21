%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_call_forward).

-include("../callflow.hrl").

-export([handle/2]).

-define(GET_CONFIG, fun(Key, Default) ->
                      whapps_config:get_binary(<<"callflow.call_forward">>, Key, Default)
                    end).
                            
-record(prompts, {has_been_enabled =
                      ?GET_CONFIG(<<"has_been_enabled_prompt">>, <<"/system_media/cf-now_forwarded_to">>)
                  ,has_been_disabled = 
                      ?GET_CONFIG(<<"has_been_disabled_prompt">>, <<"/system_media/cf-disabled">>)
                  ,feature_not_available =
                      ?GET_CONFIG(<<"feature_not_available">>, <<"/system_media/cf-not_available">>)
                  ,enter_forwarding_number =
                      ?GET_CONFIG(<<"enter_forwarding_number_prompt">>, <<"/system_media/cf-enter_number">>)
                  ,main_menu_enabled = 
                      ?GET_CONFIG(<<"main_menu_enabled_prompt">>, <<"/system_media/cf-enabled_menu">>)
                  ,main_menu_disabled = 
                      ?GET_CONFIG(<<"main_menu_disabled_prompt">>, <<"/system_media/cf-disabled_menu">>)
                  ,saved = 
                      ?GET_CONFIG(<<"saved_prompt">>, <<"/system_media/vm-saved">>)
                 }).

-record(keys, {menu_toggle_cf = 
                   ?GET_CONFIG(<<"menu_option_toggle_key">>, <<"1">>)
               ,menu_change_number =
                   ?GET_CONFIG(<<"menu_option_change_number_key">>, <<"2">>)
              }).

-record(callfwd, {prompts = #prompts{}
                  ,keys = #keys{}
                  ,doc_id = undefined
                  ,enabled = false
                  ,number = <<>>
                  ,require_keypress = true
                  ,keep_caller_id = true
                 }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> 'ok'.
handle(Data, Call) ->
    case get_call_forward(Call) of
        {error, #callfwd{prompts=Prompts}} ->
            catch({ok, _} = cf_call_command:b_play(Prompts#prompts.feature_not_available, Call)),
            cf_exe:stop(Call);
        CF ->
            cf_call_command:answer(Call),
            CF1 = case wh_json:get_value(<<"action">>, Data) of
                      <<"activate">> -> cf_activate(CF, Call);       %% Support for NANPA *72
                      <<"deactivate">> -> cf_deactivate(CF, Call);   %% Support for NANPA *73
                      <<"update">> -> cf_update_number(CF, Call);    %% Support for NANPA *56
                      <<"toggle">> -> cf_toggle(CF, Call);
                      <<"menu">> -> cf_menu(CF, Call)
                  end,
            {ok, _} = update_callfwd(CF1, Call),
            cf_exe:continue(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function provides a menu with the call forwarding options
%% @end
%%--------------------------------------------------------------------
-spec cf_menu/2 :: (#callfwd{}, #cf_call{}) -> no_return().
cf_menu(#callfwd{prompts=#prompts{main_menu_enabled=EnabledMenu, main_menu_disabled=DisabledMenu}
                 ,keys=#keys{menu_toggle_cf=Toggle, menu_change_number=ChangeNum}}=CF, Call) ->
    ?LOG("playing call forwarding menu"),
    Prompt = case CF#callfwd.enabled of
                 true -> EnabledMenu;
                 false -> DisabledMenu
             end,
    {ok, _} = cf_call_command:b_flush(Call),
    case cf_call_command:b_play_and_collect_digit(Prompt, Call) of
        {ok, Toggle} ->
            CF1 = cf_toggle(CF, Call),
            {ok, _} = update_callfwd(CF1, Call),
            cf_menu(CF1, Call);
        {ok, ChangeNum} ->
            CF1 = cf_update_number(CF, Call),
            {ok, _} = update_callfwd(CF1, Call),
            cf_menu(CF1, Call);
        {ok, _} ->
            cf_menu(CF, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will update the call forwarding enabling it if it is
%% not, and disabling it if it is
%% @end
%%--------------------------------------------------------------------
-spec cf_toggle/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_toggle(CF, #cf_call{capture_group=undefined}=Call) ->
    cf_deactivate(CF, Call);
cf_toggle(CF, Call) ->
    cf_activate(CF, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document to enable call forwarding
%% @end
%%--------------------------------------------------------------------
-spec cf_activate/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_activate(#callfwd{number=undefined}=CF, Call) ->
    cf_activate(cf_update_number(CF, Call), Call);
cf_activate(#callfwd{number=Number, prompts=Prompts}=CF, #cf_call{capture_group=undefined}=Call) ->
    ?LOG("activating call forwarding"),
    _ = try
            {ok, _} = cf_call_command:b_play(Prompts#prompts.has_been_enabled, Call),
            {ok, _} = cf_call_command:b_say(Number, Call)
        catch
            _:_ -> ok
        end,
    CF#callfwd{enabled=true};
cf_activate(#callfwd{prompts=Prompts}=CF, #cf_call{capture_group=CaptureGroup}=Call) ->
    ?LOG("activating call forwarding with number ~s", [CaptureGroup]),
    _ = try
            {ok, _} = cf_call_command:b_play(Prompts#prompts.has_been_enabled, Call),
            {ok, _} = cf_call_command:b_say(CaptureGroup, Call)
        catch
            _:_ -> ok
        end,
    CF#callfwd{enabled=true, number=CaptureGroup}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document to disable call forwarding
%% @end
%%--------------------------------------------------------------------
-spec cf_deactivate/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_deactivate(#callfwd{prompts=Prompts}=CF, Call) ->
    ?LOG("deactivating call forwarding"),
    catch({ok, _} = cf_call_command:b_play(Prompts#prompts.has_been_disabled, Call)),
    CF#callfwd{enabled=false}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document with a new number
%% @end
%%--------------------------------------------------------------------
-spec cf_update_number/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_update_number(#callfwd{prompts=#prompts{saved=Saved, enter_forwarding_number=EnterNumber}}=CF
                 ,#cf_call{capture_group=undefined}=Call) ->
    {ok, Number} = cf_call_command:b_play_and_collect_digits(<<"3">>, <<"20">>, EnterNumber, <<"1">>, <<"8000">>, Call),
    _ = cf_call_command:b_play(Saved, Call),
    ?LOG("update call forwarding number with ~s", [Number]),
    CF#callfwd{number=Number};
cf_update_number(CF, #cf_call{capture_group=CaptureGroup}) ->
    ?LOG("update call forwarding number with ~s", [CaptureGroup]),
    CF#callfwd{number=CaptureGroup}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This is a helper function to update a document, and corrects the
%% rev tag if the document is in conflict
%% @end
%%--------------------------------------------------------------------
-spec update_callfwd/2 :: (#callfwd{}, #cf_call{}) -> {'ok', wh_json:json_object()} | {'error', atom()}.
update_callfwd(#callfwd{doc_id=Id, enabled=Enabled, number=Num, require_keypress=RK, keep_caller_id=KCI}=CF
               ,#cf_call{account_db=Db}=Call) ->
    ?LOG("updating call forwarding settings on ~s", [Id]),
    {ok, JObj} = couch_mgr:open_doc(Db, Id),
    CF1 = {struct, [{<<"enabled">>, Enabled}
                    ,{<<"number">>, Num}
                    ,{<<"require_keypress">>, RK}
                    ,{<<"keep_caller_id">>, KCI}
                   ]},
    case couch_mgr:save_doc(Db, wh_json:set_value(<<"call_forward">>, CF1, JObj)) of
        {error, conflict} ->
            ?LOG("update conflicted, trying again"),
            update_callfwd(CF, Call);
        {ok, JObj1} ->
            ?LOG("updated call forwarding in db"),
            {ok, JObj1};
        {error, R}=E ->
            ?LOG("failed to update call forwarding in db ~w", [R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will load the call forwarding record
%% @end
%%--------------------------------------------------------------------
-spec get_call_forward/1 :: (#cf_call{}) -> #callfwd{} | {'error', #callfwd{}}.
get_call_forward(#cf_call{authorizing_id=AuthId, account_db=Db}) ->
    Id = case couch_mgr:get_results(Db, {<<"cf_attributes">>, <<"owner">>}, [{<<"key">>, AuthId}]) of
             {ok, [Owner]} -> wh_json:get_value(<<"value">>, Owner, AuthId);
             _E -> AuthId
         end,
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            ?LOG("loaded call forwarding object from ~s", [Id]),
            #callfwd{doc_id = wh_json:get_value(<<"_id">>, JObj)
                     ,enabled = wh_json:is_true([<<"call_forward">>, <<"enabled">>], JObj)
                     ,number = wh_json:get_ne_value([<<"call_forward">>, <<"number">>], JObj, <<>>)
                     ,require_keypress = wh_json:is_true([<<"call_forward">>, <<"require_keypress">>], JObj, true)
                     ,keep_caller_id = wh_json:is_true([<<"call_forward">>, <<"keep_caller_id">>], JObj, true)
                    };
        {error, R} ->
            ?LOG("failed to load call forwarding object from ~s, ~w", [Id, R]),
            {error, #callfwd{}}
    end.
