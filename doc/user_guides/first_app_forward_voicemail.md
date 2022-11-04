# A First App: Let’s Forward Voicemail

One of the things that we at 2600Hz pride ourselves on is the fact that the open core means we are use the same APIs to develop applications. One of our longtime engineers, James Aimonetti, had this to say once about building on KAZOO versus other platforms:

> KAZOO offers the [open core](https://en.wikipedia.org/wiki/Open_core) model, so the majority of typical APIs are built into the open source project. Some of the MonsterUI applications are closed [source], true, but the APIs they build on are open for you to build on as well. Nothing in KAZOO checks whether it’s MonsterUI making the API call vs another client. So anything you might see demoed [by 2600Hz] is possible.

With that in mind, let’s walk through an example application that allows us to view, save to local disk, or delete voicemails from an account, with just the click of a couple buttons. We’re going to keep the code as simple as possible, and then get into more complex tasks in the future.


NOTE: Be warned that deleting voicemails is a permanent action. Only delete test voicemails! Saving VMs is non-destructive, but we still recommend only using test voicemails for this guide.


## Prerequisites

---

This guide assumes that you have:

1. A KAZOO account
2. A created user
3. A registered device that can make outbound calls
4. An auth token (see [The KAZOO API Primer](../user_guides/first_app_forward_voicemail.md) for how to get one)

If you need help with any of the prerequisites, you can check out our [Community](https://forums.2600hz.com/forums) for guides and discussions about troubleshooting issues.

## Skeleton Code

Provided is an HTML file to render the SMS that we receive from the KAZOO API, and also giving us clickable buttons that allow the user to forward a voicemail to the inputted address or delete the voicemail. The file calls a script, `api.js`, that we will define in the below section.

**index.html**

```html
<head>
  <script rel="./api.js">
  <style>
		body { font-family: Arial; }
    .mockphone-container { display: flex; }
    .mockphone-row { height: 100px; display: flex; flex-direction: column; }
		.mockphone-save-btn { height: 50%; width: 50%; border-style: solid; border-color: green;}
		.mockphone-del-btn { height: 50%; width: 50%; border-style: solid; border-color: red; }
    .mockphone-error-container { color: red; }
  </style>
</head>
<body>
  <div id="mockphone-container">
  </div>
</body>
```

The HTML code isn’t important to the API, and gets mutated by our `api.js` script anyway, so we’ll omit any explanation.

## The Starter KAZOO API Code

We’re using the simplest, most widely-supported approach to this application in order to get you upstarted quickly. In future guides, we’ll likely be using different programming languages, frameworks, and runtime environments to build more complex applications. Because our script only needs to make a few calls to the API to work, we’re going to use the most barebones JavaScript code possible and then talk through what it’s doing in relation to KAZOO.

- **api.js**

    ```javascript
    var serverURL = "your_server:port"; // fill this in
    var accountId = "your-32-char-account-id"; // this too
    var authToken = "your-auth-token"; // this too!

    // It will be helpful to have our container element hanging around to access :-)
    var container = document.getElementById("mockphone-container");

    function sendKazooRequest(method, url) {
      var req = new XMLHttpRequest();
    	req.open(method, url, false);
    	req.setRequestHeader('Content-Type', 'application/json');
    	req.setRequestHeader('X-Auth-Token', authToken);
    	req.send();

    	return req;
    }

    function saveHandler(id) {
    	var url = `${serverURL}/v2/accounts/${accountId}/vmboxes/${id}`
    	var data = sendKazooRequest("GET", url);
    	var downloadUrl = URL.createObjectURL(data.response);
    	// This is a very quick-and-dirty way to download from click.
      // Only useful for personal use
      var a = document.createElement("a");
      a.style = "display: none";
      document.body.appendChild(a);
      a.href = downloadUrl;
      a.download = "id";
      a.click();
    }

    function deleteHandler(idx, id) {
    	var url = `${serverURL}/v2/accounts/${accountId}/vmboxes/${id}`
    	sendKazooRequest("DELETE", url);
    	var row = document.getElementById(`row-${idx}`).remove();
    }

    // Our first request to Crossbar, where we query all the account's VM boxes
    var allBoxesURL = `${serverURL}/v2/accounts/${accountId}/vmboxes`;
    var boxes = sendKazooRequest("GET", allBoxesURL);

    // We need to get the box's ID that we want to actually query messages from
    var boxId = null;
    if (boxes.length > 0) {
      boxId = boxes[0].id; // We use the first box, you can change this as needed
    } else {
    	container.innerHTML = `
        <div class="mockphone-error-container">
          <h1>No voicemail boxes found for accound ${accountId}.
        </div>
      `;
    }
    
    // Now that we have the VM box's ID, let's get the VM box's messages
    var messagesURL = `${allBoxesURL}/${boxId}/messages`;
    var vmData = sendKazooRequest("GET", messagesURL);
    
    // For each message, let's give it a row inside our app's container
    for (var i = 0; i < data.length; i++) {
    	var idx = i + 1; // just a nice way to do row 1, 2, 3, etc for ids
      var vmObj = data[i]; // the actual message object
    	// now we create an actual html row element for the message
      var dv = document.createElement("div");
    	dv.id = `row-${idx}`;
      dv.class = "mockphone-row";
    	dv.innerHTML = `	
    		<p>${idx}</p>
    		<p>${vmObj.date}</p>
    		<p>${vmObj.from}</p>
    		<div id="save-btn${idx}" class="mockphone-save-btn"></div>
    		<div id="del-btn${idx}" class="mockphone-del-btn"></div>
    	`;
    
      // Once we've created our row, let's append it to our container
      container.appendChild(dv);

    	// Give each row a save and delete button, where by clicking, it fires off
    	// a call to the API to either download the message or delete it.
    	saveBtn = document.getElementById(`save-btn${idx}`);
    	saveBtn.onclick = function() { saveHandler(vmObj.media_id) };
    	delBtn = document.getElementById(`del-btn${idx}`);
    	delBtn.onclick = function() { deleteHandler(vmObj.media_id) };
    }
    ```

You can save the two files above (make sure they’re in the same directory!) and then open `index.html` in a browser. Depending on the KAZOO server that you’re requesting, it may take a few seconds for your browser to render our app. This is due to the script using `XMLHttpRequest`'s synchronous mode.

![vm_ex.png](https://appex-server-public-us-west-1.s3.us-west-1.amazonaws.com/public/vm_ex_b228e780a3.png?updated_at=2022-11-04T17:20:02.817Z)

## The Explanation

Let’s break down how our very simple application actually interacts with the KAZOO API, and what we should expect to receive from the API based on what we request.

In our code, we’re initializing and then sending multiple HTTP requests to our Crossbar server. To cut down on repetitive code, let’s first define a very simple `sendKAZOORequest` function, which takes a `method` string and a `url` string. We’ll create a new `XMLHTTPRequest` object, and pass it our `method` and `url`.

```bash
function sendKazooRequest(method, url) {
  var req = new XMLHttpRequest();
	req.open(method, url, false);
	req.setRequestHeader('Content-Type', 'application/json');
	req.setRequestHeader('X-Auth-Token', authToken);
	req.send();

	return req;
}
```

Looks good! Now, let’s construct the URL for the first request we need to send to Crossbar. We know from the Voicemail documentation that to fetch an account’s VM boxes, we need to make a request to `/v2/accounts/{accountId}/vmboxes`.

```javascript
var allBoxesURL = `${serverURL}/v2/accounts/${accountId}/vmboxes`;
```

Our response to the `vmboxes` endpoint should look something like this:

```javascript
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "3a63acc3694ba189947235ae4727941b",
            "name": "VMBox 0",
            "mailbox": "3000",
            "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
            "messages": 4
        }
    ],
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

Now that we have queried the VM boxes for an account, let’s grab the first one. the next part of our script will then extract our VM box’s ID, since we’ll need it to tell KAZOO what voicemail box to fetch messages from. We’ll also do a check that if the account has no voicemail boxes, we’ll update the HTML to display some sort of `No VM Boxes` message.

```javascript
var boxId = null;
var boxes = allVMs.response.data;
if (boxes.length > 0) {
  boxId = boxes[0].id;
} else {
  document.getElementById('mockphone-container').innerHTML = "<p>No voicemail boxes to fetch from!</p>"
}
```

Once we’ve gotten the voicemail box ID, we’re ready to get its messages, which we accomplish with a `GET` to `/v2/accounts/{accountId}/vmboxes/{boxId}/messages`. 

```javascript
var messagesURL = `${allBoxesURL}/${boxId}/messages`;
var vmData = sendKazooRequest("GET", messagesURL);
```

Once we’ve made the request to KAZOO, something similar should be returned:

```javascript
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "0e820108c0f4ca391500f3be1b02bdfa": {
                "timestamp": 63630058722,
                "from": "1001@aeac33.sip.2600hz.com",
                "to": "1000@aeac33.sip.2600hz.com",
                "caller_id_number": "1001",
                "caller_id_name": "userb userb",
                "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
                "folder": "new",
                "length": 3140,
                "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
            }
        },
        {
            "0e820108c0f4ca391500f3be1b02bdfa": {
                "timestamp": 63630058413,
                "from": "1002@aeac33.sip.2600hz.com",
                "to": "1000@aeac33.sip.2600hz.com",
                "caller_id_number": "1002",
                "caller_id_name": "userd userd",
                "call_id": "79959MmNiMmJiMTIxODhjZjk0ZDhmOGNkMjJkN2MwNGQyNWY",
                "folder": "new",
                "length": 5500,
                "media_id": "201605-f0c3c16551a5ff7b5753a381892e2e01"
            }
        }
    ],
    "next_start_key": [],
    "page_size": 50,
    "revision": "{REVERSION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

Great! We’ve received the metadata for the messages in our voicemail box. Now, we can use that metadata to populate rows in our application. Since we’re using vanilla Javascript, we’ll accomplish this by editing the innerHTML of our parent element, `mockphone-container`.

```javascript
var container = document.getElementById("mockphone-container");
for (var i = 0; i < data.length; i++) {
	var idx = i + 1; // just a nice way to do row 1, 2, 3, etc for ids
  var vmObj = data[i]; // the actual message object
	// now we create an actual html row element for the message
  var dv = document.createElement("div");
	dv.id = `row-${idx}`;
  dv.class = "mockphone-row";
	dv.innerHTML = `
		<p>${idx}</p>
		<p>${vmObj.date}</p>
		<p>${vmObj.from}</p>
		<div id="save-btn${idx}" class="mockphone-save-btn"></div>
		<div id="del-btn${idx}" class="mockphone-del-btn"></div>
	`;

	saveBtn = document.getElementById(`save-btn${idx}`);
	saveBtn.onclick = function() { forwardHandler(vmObj.media_id) };
	delBtn = document.getElementById(`del-btn${idx}`);
	delBtn.onclick = function() { deleteHandler(vmObj.media_id) };
}
```

The `data` array is the result of the last API request we sent to KAZOO. From there, we iterate through, accessing each individual voicemail message with `data[i]`.

Lastly, let’s make two buttons: one for saving, and one for deleting a voicemail. When we delete a voicemail, we’ll issue a `DELETE` to: `/v2/accounts/accountId/vmboxes/{boxId}/messages/{voicemailId}`. Then, we’ll delete the HTML element in our app representing the row in our voicemail in order to signify that it’s been deleted!

```javascript
function deleteHandler(idx, boxId, msgId) {
	var url = `${serverURL}/v2/accounts/${accountId}/vmboxes/${boxId}/messages/${messageId}`;
	sendKazooRequest("DELETE", url);
	var row = document.getElementById(`row-${idx}`).remove();
}
```

To save a message, we will also need to make an additional KAZOO API request. To download a raw voicemail media file, we can issue a `GET` to: `/v2/accounts/accountId/vmboxes/{boxId}/messages/{voicemailId}/raw`.

```javascript
function saveHandler(id) {
	var url = `${serverURL}/v2/accounts/${accountId}/vmboxes/${boxId}/messages/${messageId}`;
	var data = sendKazooRequest("GET", url);
	var downloadUrl = URL.createObjectURL(data.response);
```

The omitted part of the `saveHandler` function is simply [borrowed](https://stackoverflow.com/questions/20830309/download-file-using-an-ajax-request) code meant to add the download feature to this example app in the least amount of code. You may need to modify it to fit your browser needs.

## Conclusion

In this guide, we've constructed a simple application that can download and delete voicemails from a voicemail box associated with any KAZOO account. By creating an `index.html` file, as well as an `api.js` file in the same directory furnished with the code above (and the addition of your own KAZOO account credentials and authorization token), opening `index.html` in the web browser will then display the application.