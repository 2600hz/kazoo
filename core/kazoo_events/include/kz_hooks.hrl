-ifndef(KZ_HOOKS_HRL).

-define(HOOKS_CACHE_NAME, 'kz_hooks_cache').

-define(HOOK_EVT(AccountId, EventType, JObj)
       ,{'kz_hook', AccountId, EventType, JObj}
       ).

-define(KZ_HOOKS_HRL, 'true').
-endif.
