
### Groups

#### Get groups for a given account

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/groups`
- Payload: None

##### Response

```
{
    "data": [{
        "id": "18ccfd6cea456cbdd38133e5aa726ec4",
        "name": "Group Name",
        "features": [],
        "endpoints": 2
    }],
    "status": "success"
}
```

#### Create a group for a given account

##### Request

- Verb: `PUT`
- Url: `/accounts/{{ACCOUNT_ID}}/groups`
- Payload:

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        }
    },
    "verb": "PUT"
}
```

##### Response

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        },
        "id": "1743724cd775bf6994380dbc79c1af09"
    },
    "status": "success"
}
```

#### Get a group for a given account

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/groups/{{GROUP_ID}}`
- Payload: None

##### Response

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        },
        "ui_metadata": {
            "ui": "kazoo-ui"
        },
        "id": "1743724cd775bf6994380dbc79c1af09"
    },
    "status": "success"
}
```

#### Update a group for a given account

##### Request

- Verb: `POST`
- Url: `/accounts/{{ACCOUNT_ID}}/groups/{{GROUP_ID}}`
- Payload:

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group 2",
        "id": "1743724cd775bf6994380dbc79c1af09",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        }
    },
    "verb": "POST"
}
```

##### Response

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group 2",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        },
        "ui_metadata": {
            "ui": "kazoo-ui"
        },
        "id": "1743724cd775bf6994380dbc79c1af09"
    },
    "status": "success"
}
```

#### Update a group for a given account

##### Request

- Verb: `DELETE`
- Url: `/accounts/{{ACCOUNT_ID}}/groups/{{GROUP_ID}}`
- Payload: None

##### Response

```
{
    "data": {
        "music_on_hold": {},
        "name": "Test group 2",
        "id": "1743724cd775bf6994380dbc79c1af09",
        "endpoints": {
            "df9274b450ea6795cdb381055c3f9b45": {
                "type": "user",
                "weight": 1
            },
            "dd03d7442a4bec5c092ea6a0e6d579ef": {
                "type": "device",
                "weight": 2
            }
        }
    },
    "status": "success"
}
```
