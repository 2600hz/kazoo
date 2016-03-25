

Configuring the `system_config/number_manager` document for European Union-based installations.

#### Reconcile Regex

    "reconcile_regex": "^\\+?[1-9]\\d{5,}$|^0\\d{5,}$|^00\\d{5,}$"

#### E164 Convertors

    "e164_converters": {
        "^00(\\d*)$": {
            "prefix": "+"
        },
        "^([1-9]\\d{7,})$": {
            "prefix": "+"
        }
    }
