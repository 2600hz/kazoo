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

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".call_forward">>).

-record(keys, {menu_toggle_cf =
                   whapps_config:get_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_toggle_option">>], <<"1">>)
               ,menu_change_number =
                   whapps_config:get_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_change_number">>], <<"2">>)
              }).

-record(callfwd, {keys = #keys{}
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
        {error, _} ->
            catch({ok, _} = cf_call_command:b_prompt(<<"cf-not_available">>, Call)),
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
cf_menu(#callfwd{keys=#keys{menu_toggle_cf=Toggle, menu_change_number=ChangeNum}}=CF, Call) ->
    ?LOG("playing call forwarding menu"),
    Prompt = case CF#callfwd.enabled of
                 true -> cf_util:get_prompt(<<"cf-enabled_menu">>);
                 false -> cf_util:get_prompt(<<"cf-disabled_menu">>)
             end,
    _  = cf_call_command:b_flush(Call),
    case cf_call_command:b_play_and_collect_digit(Prompt, Call) of
        {ok, Toggle} ->
            CF1 = cf_toggle(CF, Call),
            cf_menu(CF1, Call);
        {ok, ChangeNum} ->
            CF1 = cf_update_number(CF, Call),
            cf_menu(CF1, Call);
        {ok, _} ->
            cf_menu(CF, Call);
        {error, _} ->
            CF
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will update the call forwarding enabling it if it is
%% not, and disabling it if it is
%% @end
%%--------------------------------------------------------------------
-spec cf_toggle/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_toggle(#callfwd{enabled=false, number=Number}=CF, Call) when is_binary(Number), size(Number) > 0 ->
    _ = try
            {ok, _} = cf_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {ok, _} = cf_call_command:b_say(Number, Call)
        catch
            _:_ -> ok
        end,
    CF#callfwd{enabled=true};    
cf_toggle(#callfwd{enabled=false}=CF, Call) ->
    cf_activate(CF, Call);
cf_toggle(CF, Call) ->
    cf_deactivate(CF, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document to enable call forwarding
%% @end
%%--------------------------------------------------------------------
-spec cf_activate/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_activate(CF1, #cf_call{capture_group=CG}=Call) when is_atom(CG); CG =:= <<>> ->
    ?LOG("activating call forwarding"),
    CF2 = #callfwd{number=Number} = cf_update_number(CF1, Call),
    _ = try
            {ok, _} = cf_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {ok, _} = cf_call_command:b_say(Number, Call)
        catch
            _:_ -> ok
        end,
    CF2#callfwd{enabled=true};
cf_activate(CF, #cf_call{capture_group=CaptureGroup}=Call) ->
    ?LOG("activating call forwarding with number ~s", [CaptureGroup]),
    _ = try
            {ok, _} = cf_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
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
cf_deactivate(CF, Call) ->
    ?LOG("deactivating call forwarding"),
    catch({ok, _} = cf_call_command:b_prompt(<<"cf-disabled">>, Call)),
    CF#callfwd{enabled=false}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will udpate the call forwarding object on the owner
%% document with a new number
%% @end
%%--------------------------------------------------------------------
-spec cf_update_number/2 :: (#callfwd{}, #cf_call{}) -> #callfwd{}.
cf_update_number(CF, #cf_call{capture_group=CG}=Call) when is_atom(CG); CG =:= <<>> ->
    EnterNumber = cf_util:get_prompt(<<"cf-enter_number">>),
    case cf_call_command:b_play_and_collect_digits(<<"3">>, <<"20">>, EnterNumber, <<"1">>, <<"8000">>, Call) of
        {ok, <<>>} -> cf_update_number(CF, Call);
        {ok, Number} ->
            _ = cf_call_command:b_prompt(<<"vm-saved">>, Call),
            ?LOG("update call forwarding number with ~s", [Number]),
            CF#callfwd{number=Number};
        {error, _} -> exit(normal)
    end;
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
