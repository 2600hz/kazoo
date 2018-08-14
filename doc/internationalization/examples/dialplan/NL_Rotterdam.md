# Netherlands (Rotterdam)

Dialplan object for devices/users/accounts calling from Rotterdam

    "dial_plan": {
	    "^0\\d{6,7})$": {
		    "description": "NL-Rotterdam-Lokaal",
			"prefix": "+3110"
        },
        "^0(800\\d{4,7})$": {
		    "description": "NL-Gratis",
			"prefix": "+31"
		},
		"^^0(90\\d{5,8}|8[47]\\d{7})$": {
		    "description": "NL-Premium",
		    "prefix": "+31"
		},
		"^0(6\\d{8})$": {
		    "description": "NL-Mobiel",
			"prefix": "+31"
		},
		"^0(([1-57]\\d{4,8}|8[58]\\d{7}))(\\D+\\d+)?$": {
		    "description": "NL-Nationaal",
			"prefix": "+31"
		},
		"^(112|14\\d{2,4})$": {
		    "description": "NL-Service",
			"prefix": "+"
		},
		"^(?:00)((1[2-9]\\d\\d[2-9]\\d{6})|([2-9]\\d{6,14}))(\\D+\\d+)?$": {
		    "description": "NL-Internationaal",
			"prefix": "+"
		}
    }
