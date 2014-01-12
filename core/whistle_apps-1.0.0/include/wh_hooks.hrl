-ifndef(WH_HOOKS_HRL).

-define(HOOK_EVT(AccountId, EventType, JObj), {'wh_hook'
                                               ,AccountId
                                               ,EventType
                                               ,JObj
                                              }).

-define(WH_HOOKS_HRL, 'true').
-endif.
