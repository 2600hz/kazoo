
### Call Queues

When you have more callers than agents to handle those calls, you can create a call queue to put the callers on hold while agents process callers in the order they arrived in the call queue.

#### cURL examples

##### List queues

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues

##### Create a queue

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues -d '{"data":{"name":"Support Queue"}}'

##### List queues stats

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/stats

##### List queue

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}

##### List queue stats

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/stats

##### List queue roster (what agents are assigned to the queue)

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster

##### Set the queue roster

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster -d '{"data": ["f3ced8ea7bccc352a2124e8a34351e81", "e154a97ec2942599865a1591a477fd19"]}'

##### Clear a queue's roster

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/queues/{QUEUE_ID}/roster
