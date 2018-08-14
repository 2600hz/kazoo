# Netherlands (Amsterdam)

Dialplan object for devices/users/accounts calling from Amsterdam

    "dial_plan": {
	    "^00(\\d{8,})$": {
		    "description": "international",
			"prefix": "+"
        },
        "^(0800\\d{4,7})$": {
		    "description": "NL 0800 8-11 digits",
			"prefix": "+31800"
		},
		"^0(\\d{9,})$": {
		    "description": "NL national and mobile",
			"prefix": "+31"
		},
		"^([1-9]\\d{8,9})$": {
		    "description": "NL Amsterdam local",
			"prefix": "+3120"
		}
    }
