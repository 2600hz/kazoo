%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Callflow action to control call forwarding feature.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>The action to be done: `activate', `deactivate', `update', `toggle' and `menu'.</dd>
%%% </dl>
%%%
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_call_forward).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".call_forward">>).
-define(KEY_LENGTH, 1).

-define(MIN_CALLFWD_NUMBER_LENGTH, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_callfwd_number_length">>, 3)).
-define(MAX_CALLFWD_NUMBER_LENGTH, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_callfwd_number_length">>, 20)).
-define(CALLFWD_NUMBER_TIMEOUT, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"callfwd_number_timeout">>, 8000)).

-record(keys, {menu_toggle_cf =
                   kapps_config:get_ne_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_toggle_option">>], <<"1">>)
               :: kz_term:ne_binary()
              ,menu_change_number =
                   kapps_config:get_ne_binary(?MOD_CONFIG_CAT, [<<"keys">>, <<"menu_change_number">>], <<"2">>)
               :: kz_term:ne_binary()
              }).
-type keys() :: #keys{}.

-record(callfwd, {keys = #keys{} :: keys()
                 ,doc_id :: kz_term:api_ne_binary()
                 ,enabled = 'false' :: boolean()
                 ,number = <<>> :: binary()
                 ,require_keypress = 'true' :: boolean()
                 ,keep_caller_id = 'true' :: boolean()
                 ,interdigit_timeout = kapps_call_command:default_interdigit_timeout() :: pos_integer()
                 }).
-type callfwd() :: #callfwd{}.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case get_call_forward(Call) of
        {'error', _} ->
            catch({'ok', _} = kapps_call_command:b_prompt(<<"cf-not_available">>, Call)),
            cf_exe:stop(Call);
        CF ->
            {'ok', _} = kapps_call_command:b_answer(Call),
            CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),

            CF1 = case kz_json:get_ne_binary_value(<<"action">>, Data) of
                      <<"activate">> -> cf_activate(CF, CaptureGroup, Call);       %% Support for NANPA *72
                      <<"deactivate">> -> cf_deactivate(CF, Call);                 %% Support for NANPA *73
                      <<"update">> -> cf_update_number(CF, CaptureGroup, Call);    %% Support for NANPA *56
                      <<"toggle">> -> cf_toggle(CF, CaptureGroup, Call);
                      <<"menu">> -> cf_menu(CF, CaptureGroup, Call);
                      _Action ->
                          lager:info("improper action in Data: ~p", [_Action]),
                          'undefined'
                  end,
            {'ok', _} = update_callfwd(CF1, Call),
            cf_exe:continue(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc This function provides a menu with the call forwarding options
%% @end
%%------------------------------------------------------------------------------
-spec cf_menu(callfwd(), kz_term:ne_binary(), kapps_call:call()) -> callfwd().
cf_menu(#callfwd{keys=#keys{menu_toggle_cf=Toggle
                           ,menu_change_number=ChangeNum
                           }
                ,enabled=Enabled
                ,interdigit_timeout=Interdigit
                }=CF, CaptureGroup, Call) ->
    lager:info("playing call forwarding menu"),

    Prompt = case Enabled of
                 'true' ->
                     kapps_call:get_prompt(Call, <<"cf-enabled_menu">>);
                 'false' ->
                     kapps_call:get_prompt(Call, <<"cf-disabled_menu">>)
             end,
    _  = kapps_call_command:b_flush(Call),

    NoopId = kapps_call_command:play(Prompt, Call),

    case kapps_call_command:collect_digits(?KEY_LENGTH
                                          ,?CALLFWD_NUMBER_TIMEOUT
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
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

%%------------------------------------------------------------------------------
%% @doc This function will update the call forwarding enabling it if it is
%% not, and disabling it if it is
%% @end
%%------------------------------------------------------------------------------
-spec cf_toggle(callfwd(), kz_term:api_binary(), kapps_call:call()) -> callfwd().
cf_toggle(#callfwd{enabled='false'
                  ,number=Number
                  }=CF, _, Call) when is_binary(Number), Number =/= <<>> ->
    _ = try
            {'ok', _} = kapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = kapps_call_command:b_say(Number, Call)
        catch
            _:_ -> 'ok'
        end,
    CF#callfwd{enabled='true'};
cf_toggle(#callfwd{enabled='false'}=CF, CaptureGroup, Call) ->
    cf_activate(CF, CaptureGroup, Call);
cf_toggle(CF, _, Call) ->
    cf_deactivate(CF, Call).

%%------------------------------------------------------------------------------
%% @doc This function will update the call forwarding object on the owner
%% document to enable call forwarding
%% @end
%%------------------------------------------------------------------------------
-spec cf_activate(callfwd(), kz_term:api_binary(), kapps_call:call()) -> callfwd().
cf_activate(CF1, CaptureGroup, Call) when is_atom(CaptureGroup); CaptureGroup =:= <<>> ->
    lager:info("activating call forwarding to '~s'", [CaptureGroup]),
    CF2 = #callfwd{number=Number} = cf_update_number(CF1, CaptureGroup, Call),
    _ = try
            {'ok', _} = kapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = kapps_call_command:b_say(Number, Call)
        catch
            _:_ -> 'ok'
        end,
    CF2#callfwd{enabled='true'};
cf_activate(CF, CaptureGroup, Call) ->
    lager:info("activating call forwarding with number ~s", [CaptureGroup]),
    _ = try
            {'ok', _} = kapps_call_command:b_prompt(<<"cf-now_forwarded_to">>, Call),
            {'ok', _} = kapps_call_command:b_say(CaptureGroup, Call)
        catch
            _:_ -> 'ok'
        end,
    CF#callfwd{enabled='true', number=CaptureGroup}.

%%------------------------------------------------------------------------------
%% @doc This function will update the call forwarding object on the owner
%% document to disable call forwarding
%% @end
%%------------------------------------------------------------------------------
-spec cf_deactivate(callfwd(), kapps_call:call()) -> callfwd().
cf_deactivate(CF, Call) ->
    lager:info("deactivating call forwarding"),
    catch({'ok', _} = kapps_call_command:b_prompt(<<"cf-disabled">>, Call)),
    CF#callfwd{enabled='false'}.

%%------------------------------------------------------------------------------
%% @doc This function will update the call forwarding object on the owner
%% document with a new number
%% @end
%%------------------------------------------------------------------------------
-spec cf_update_number(callfwd(), kz_term:api_binary(), kapps_call:call()) -> callfwd().
cf_update_number(#callfwd{interdigit_timeout=Interdigit}=CF, CaptureGroup, Call)
  when is_atom(CaptureGroup); CaptureGroup =:= <<>> ->
    EnterNumber = kapps_call:get_prompt(Call, <<"cf-enter_number">>),

    NoopId = kapps_call_command:play(EnterNumber, Call),
    Min = ?MIN_CALLFWD_NUMBER_LENGTH,

    case kapps_call_command:collect_digits(?MAX_CALLFWD_NUMBER_LENGTH
                                          ,?CALLFWD_NUMBER_TIMEOUT
                                          ,Interdigit
                                          ,NoopId
                                          ,Call
                                          )
    of
        {'ok', Short} when byte_size(Short) < Min ->
            lager:debug("too short of input(~p): '~s'", [Min, Short]),
            cf_update_number(CF, CaptureGroup, Call);
        {'ok', Number} ->
            _ = kapps_call_command:b_prompt(<<"vm-saved">>, Call),
            lager:info("update call forwarding number with ~s", [Number]),
            CF#callfwd{number=Number};
        {'error', _E} ->
            lager:debug("error collecting digits: ~p", [_E]),
            exit('normal')
    end;
cf_update_number(CF, CaptureGroup, _) ->
    lager:info("update call forwarding number with '~s'", [CaptureGroup]),
    CF#callfwd{number=CaptureGroup}.

%%------------------------------------------------------------------------------
%% @doc This is a helper function to update a document, and corrects the
%% rev tag if the document is in conflict
%% @end
%%------------------------------------------------------------------------------
-spec update_callfwd(callfwd(), kapps_call:call()) ->
                            {'ok', kz_term:api_object()} |
                            {'error', atom()}.
update_callfwd('undefined', _Call) -> {'ok', 'undefined'};
update_callfwd(#callfwd{doc_id=Id
                       ,enabled=Enabled
                       ,number=Num
                       ,require_keypress=_RK
                       ,keep_caller_id=_KCI
                       }=CF
              ,Call) ->
    lager:info("updating call forwarding settings on ~s", [Id]),
    AccountDb = kapps_call:account_db(Call),
    {'ok', JObj} = kz_datamgr:open_doc(AccountDb, Id),
    CFObj = kz_json:get_ne_value(<<"call_forward">>, JObj, kz_json:new()),
    Updates = [fun(J) -> kz_json:set_value(<<"enabled">>, Enabled, J) end
              ,fun(J) -> kz_json:set_value(<<"number">>, Num, J) end
              ],
    CFObj1 = lists:foldl(fun(F, Acc) -> F(Acc) end, CFObj, Updates),
    case kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"call_forward">>, CFObj1, JObj)) of
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

%%------------------------------------------------------------------------------
%% @doc This function will load the call forwarding record
%% @end
%%------------------------------------------------------------------------------
-spec get_call_forward(kapps_call:call()) ->
                              callfwd() |
                              {'error', callfwd()}.
get_call_forward(Call) ->
    AuthorizingId = kapps_call:authorizing_id(Call),

    OwnerId =
        case kz_attributes:owner_id(AuthorizingId, Call) of
            'undefined' -> AuthorizingId;
            UserId -> UserId
        end,
    maybe_get_call_forward(Call, OwnerId).

-spec maybe_get_call_forward(kapps_call:call(), kz_term:api_binary()) ->
                                    callfwd() |
                                    {'error', callfwd()}.
maybe_get_call_forward(_Call, 'undefined') ->
    lager:debug("cannot get call forwarding from undefined"),
    {'error', #callfwd{}};
maybe_get_call_forward(Call, OwnerId) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), OwnerId) of
        {'ok', UserJObj} ->
            lager:info("loaded call forwarding object from ~s", [OwnerId]),
            #callfwd{doc_id = kz_doc:id(UserJObj)
                    ,enabled = kz_json:is_true([<<"call_forward">>, <<"enabled">>], UserJObj)
                    ,number = kz_json:get_ne_value([<<"call_forward">>, <<"number">>], UserJObj, <<>>)
                    ,require_keypress = kz_json:is_true([<<"call_forward">>, <<"require_keypress">>], UserJObj, 'true')
                    ,keep_caller_id = kz_json:is_true([<<"call_forward">>, <<"keep_caller_id">>], UserJObj, 'true')
                    };
        {'error', R} ->
            lager:info("failed to load call forwarding object from ~s, ~w", [OwnerId, R]),
            {'error', #callfwd{}}
    end.
