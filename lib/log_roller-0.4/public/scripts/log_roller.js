var g_requests = new Array();

Event.observe(window, 'load', page_load);
Event.observe(document, 'keypress',  key_pressed);

function key_pressed(event) { 
    if(event.keyCode == 13) { 
		refresh();
	} /*else if (event.keyCode == 102) {
	    toggle_filters();
	} else if (event.keyCode == 91) {
	    prev_tab();
	} else if (event.keyCode == 93) {
        next_tab();
	}*/
}

function page_load() {
	new Ajax.Request("/servers/" + $('server').value, {
		method: 'get',
		onSuccess: function(tabs) {
			$('tab_bar').innerHTML = tabs.responseText;
		}
	});
	
	new Ajax.Request("/nodes/" + $('server').value, {
		method: 'get',
		onSuccess: function(nodes) {
			$('sl_node').innerHTML = nodes.responseText;
			if($('sl_node').length > 4) {
			    $('sl_node').size = $('sl_node').length;
		    } else {
				$('sl_node').size = 4;
			}
		}
	});
	
	refresh();
}

function get_params() {
    return {   
        server: $('server').getValue(),
		type: $('sl_type').getValue(), 
		node: $('sl_node').getValue(),
		grep: $('grep').getValue(),
		max: $('max').getValue() 
	};
}

function refresh() {
	$('log_frame').innerHTML = '';
	$('int_log_frame').innerHTML = '';
	
	new Ajax.Request("/logs", {
		method: 'post',
		parameters: get_params(),
		onCreate: function(request) {
			stop_existing_requests();
			g_requests.push(request);
			$('ajax-loader').style.display = 'inline';
		},
		onInteractive: function(logs) {
			$('int_log_frame').innerHTML = logs.responseText;
		},
		onComplete: function(logs) {
			$('log_frame').innerHTML += logs.responseText;
			$('int_log_frame').innerHTML = '';
			$('ajax-loader').style.display = 'none';
            if(logs.responseText == '') {
                $('log_frame').innerHTML = 'no logs met your search criteria';
            }
		}
	});
}

function tail() {
    if($('btn_tail').value == 'Stop Tailing') {
        $('btn_tail').value = 'Tail Log';
        stop_existing_requests();
        return;
    }
    
    var log_frame = $('log_frame');
    
    var max_size = 10;
    
    log_frame.innerHTML = '';
    $('btn_tail').value = 'Stop Tailing';
	
	new Ajax.Request("/tail", {
		method: 'post',
		parameters: get_params(),
		onCreate: function(request) {
			stop_existing_requests();
			g_requests.push(request);
			$('ajax-loader').style.display = 'inline';
		},
		onInteractive: function(logs) {
		    var tmp = new Element('div');
		    tmp.innerHTML = logs.responseText;
		    var c = tmp.childElements();
			if(c.length > max_size) {
			    log_frame.innerHTML = '';
			    for(var i=c.length-max_size; i<c.length; i++) {
			        log_frame.insert(c[i]);
			    }
		    } else {
		        log_frame.innerHTML = tmp.innerHTML;
		    }
			//$('end').scrollIntoView(true);
		},
		onComplete: function(logs) {
			$('ajax-loader').style.display = 'none';
			$('btn_tail').value = 'Tail Log';
		}
	});
}

function stop_existing_requests() {
    for(var i=0; i<g_requests.length; i++) {
		try { g_requests[i].transport.abort(); } catch(e) {}
	}
}

function switch_server(server_name) {
	$('server').value = server_name;
	$('sl_type').selectedIndex = -1;
	$('grep').value = '';
	$('max').value = '20';
	stop_existing_requests();
	page_load();
}

function toggle_filters() {
    if($('filter_bar_open').style.display == 'none') {
        $('filter_bar_open').style.display = 'block';
        $('filter_bar_closed').style.display = 'none';
    } else {
        $('filter_bar_open').style.display = 'none';
        $('filter_bar_closed').style.display = 'block';
    }
}

function prev_tab() {
    var tabs = document.getElementsByName('tab');
    var is_next = false;
    for(var i=tabs.length-1; i>=0; i--) {
        if(is_next) {
            switch_server(tabs[i].childNodes[0].innerHTML);
            i = 0;
        } else if(tabs[i].className == 'active_tab') {
            if(i == 0) {
                switch_server(tabs[tabs.length-1].childNodes[0].innerHTML);
            } else {
                is_next = true;
            }
        } else if(i == 0) {
            switch_server(tabs[tabs.length-1].childNodes[0].innerHTML);
        }
    }
}

function next_tab() {
    var tabs = document.getElementsByName('tab');
    var is_next = false;
    for(var i=0; i<tabs.length; i++) {
        if(is_next) {
            switch_server(tabs[i].childNodes[0].innerHTML);
            i = tabs.length;
        } else if(tabs[i].className == 'active_tab') {
            if(i == (tabs.length-1)) {
                switch_server(tabs[0].childNodes[0].innerHTML);
            } else {
                is_next = true;
            }
        } else if(i == (tabs.length-1)) {
            switch_server(tabs[0].childNodes[0].innerHTML);
        }
    }    
}