//  The contents of this file are subject to the Mozilla Public License
//  Version 1.1 (the "License"); you may not use this file except in
//  compliance with the License. You may obtain a copy of the License
//  at http://www.mozilla.org/MPL/
//
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
//  the License for the specific language governing rights and
//  limitations under the License.
//
//  The Original Code is RabbitMQ.
//
//  The Initial Developer of the Original Code is VMware, Inc.
//  Copyright (c) 2007-2013 VMware, Inc.  All rights reserved.
//

var JsonRpc;
var RabbitMQ;
(function () {
     var Support = {
	 extend: Object.extend,
	 bindEvent: function (elt, event, cb) {
	     Event.observe(elt, event, cb);
	 },
	 each: function (collection, callback) {
	     return collection.each(callback);
	 },
	 ajaxPost: function (url, customHeaders, bodyString, callback) {
	     var headers = ['Content-type', 'application/json',
			    'Accept', 'application/json'];
	     for (var header in customHeaders) {
		 if (customHeaders.hasOwnProperty(header)) {
		     headers.push(header, customHeaders[header]);
		 }
	     }
	     return new Ajax.Request(url, { method: 'post',
					    requestHeaders: headers,
					    postBody: bodyString,
					    onComplete: callback });
	 }
     };

     JsonRpc = JsonRpc_ModuleFactory(Support);
     RabbitMQ = RabbitMQ_ModuleFactory(JsonRpc, Support);
 })();
