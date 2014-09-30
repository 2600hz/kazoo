/*
Section: Pivot
Title: Bridging
Language: en-US
*/

# Overview

Kazoo JSON offers a plethora of ways to call out to various endpoints!

## Devices

Dial a single Kazoo device

    {"module":"device"
     ,"data":{"id":"device_id"}
    }

## Users

Dial a Kazoo user (any devices owned by the user)

    {"module":"user"
     ,"data":{"id":"user_id"}
    }

## Ring Group

Ring groups are ultra-flexible in what types of endpoints you can combine: devices, users, or groups! You need only include the IDs you want to ring and Kazoo will build the appropriate list of endpoints.

    {"module":"ring_group"
     ,"data":{"endpoints":["device_1_id", "device_2_id", "user_1_id, "user_2_id", "group_1_id", "group_2_id"]}
    }

You are free to mix/match devices, users, and groups based on the needs of this particular call.
