# Ping (pong)

This module receives a ping, and responds with a pong. It takes no arguments.
This can be used to test a socket is up, or keep it alive by preventing it from becoming idle.

## Example Javascript

```javascript
// Put your Blackhole url here
var socket = new WebSocket(url)
var socketResponseListeners = {};

// From https://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript
function getUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  });
}

// Ping the websocket, if we don't get a pong within a second then reopen the websocket
function checkOpen() {
  var receivedResponse = false;
  var data = {
    request_id: getUUID(),
    action: 'ping'
  };
  socketResponseListeners[data.request_id] = function(resp) {
    if (resp.data.response === 'pong') {
      receivedResponse = true;
    }
  }
  // Expect response within one second
  setTimeout(function() {
    if (!receivedResponse) {
      console.log('Socket is down!');
      // You'll probably want to handle reopening logic here!
    }
  }, 1000);
  socket.send(JSON.stringify(data));
}

socket.onmessage = function(message_event) {
  // Parse message data
  var data = JSON.parse(message_event.data);

  if (data.action === "reply") {
    // Send response to requestor, if there is one
    if (socketResponseListeners[data.request_id]) {
      socketResponseListeners[data.request_id](data);
      delete socketResponseListeners[data.request_id];
    }
  }
};
```
