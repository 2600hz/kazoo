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
	 extend: jQuery.extend,
	 bindEvent: function (elt, event, cb) {
	     jQuery(elt).bind(event, cb);
	 },
	 each: function (collection, callback) {
	     jQuery.each(collection, function (key, value) { callback(value) });
	 },
	 ajaxPost: function (url, customHeaders, bodyString, callback) {
	     return new jQuery.ajax({ url: url,
				      type: 'post',
				      beforeSend: function (xhr) {
					  for (var header in customHeaders) {
					      if (customHeaders.hasOwnProperty(header)) {
						  xhr.setRequestHeader(header,
								       customHeaders[header]);
					      }
					  }
				      },
				      contentType: 'application/json',
				      accepts: {json: 'application/json'},
				      data: bodyString,
				      complete: callback });
	 }
     };

     JsonRpc = JsonRpc_ModuleFactory(Support);
     RabbitMQ = RabbitMQ_ModuleFactory(JsonRpc, Support);
 })();
