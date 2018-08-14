# Kazoo Services *Service Plans*
Planning your services since 2012


## Applications

To add applications to your service plans. You need to add them under the category `ui_apps`.

Once this is done, kazoo will start pulling the apps available to you from the service plan (the one you are subscribing to),
so make sure to add all the apps you used to have...

**Under each app you need to provide the `app_id` but also the `account_db` (where the app is).**


Ex:
```
"ui_apps": {
   "numbers": {
       "enabled": true, //if false the app will not appear as available
       "name": "Number manager",
       "as": "numbers",
       "account_db": "{{account_db}}",
       "app_id": "{{application_id}}",
       "activation_charge": 1,
       "cascade": true,
       "rate": 2
   },
   "branding": {
       "enabled": false, //if false the app will not appear as available
       "name": "Branding",
       "as": "branding",
       "app_id": "{{application_id}}",
       "account_db": "{{account_db}}",
       "activation_charge": 3,
       "cascade": true,
       "rate": 5
   },
   "accounts": {
       "enabled": true, //if false the app will not appear as available
       "name": "Account Manager",
       "as": "accounts",
       "app_id": "{{application_id}}",
       "account_db": "{{account_db}}",
       "activation_charge": 5,
       "cascade": true,
       "rate": 3
   }
}
```

Kazoo will start charging for an application **only if it is turned on and used by the account.**


This is located under each the account document:

```
{
    "apps": {
        "{{application_id}}": {
            "name": "accounts",
            "allowed_users": "specific",
            "users": []
        },
        "{{application_id}}": {
            "allowed_users": "specific",
            "users": [
                "{{user_id_1}}",
                "{{user_id_2}}"
            ]
        },
        "{{application_id}}": {
            "allowed_users": "admin",
            "users": []
        },
        "{{application_id}}": {
            "allowed_users": "all",
            "users": []
        }
    }
}
```


| Allowed Users  | Will be charged |
| ------------- | ------------- |
| Specific with **no user**  | NO  |
| Specific with **user(s)**  | YES  |
| All  | YES  |
| Admin | YES  |

Kazoo is not charging on a user base, having one or all users enabled will not affect this amount charged.
As application can only be charged once (at least for now).

