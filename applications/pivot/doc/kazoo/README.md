# Kazoo JSON Overview

Sometimes Kazoo's callflow builder doesn't match your needs, integrate with your applications, or provide the experience for your caller that you desire. Fortunately, the building blocks are available for you to play with as you see fit!

By routing a call to a Pivot callflow action, Kazoo will make an HTTP request to your web server asking for the callflow to process for that specific call (versus the one-size-fits-all approach in the normal callflow builder).

This could be best illustrated with an example, no?

## Building Time-based routing

We want to add time-based rules when deciding how to route our office's main number (this exists with the `temporal_route` callflow action already, but we're trying to make a point!). Following a "typical" office's hours, we are open 9am-5pm (9:00 to 17:00). During that time, we'd like to ring the front desk's phone. Outside of those hours, we'd like calls to go directly to the company's voicemail box.

### Build the Kazoo Things

Based on the above goals, we need to build:

* Front Desk's device
* Company Voicemail box
* Main number callflow

Once you've made those, note the IDs for the device and voicemail box (using the developer tool is a good way to find those).

The main number callflow should be:

```
[NUMBER] -> Pivot
            Url: http://your.webserver.com/path/to/main_number_tod.php
```

Now, whenever this number is called, Kazoo will query your URL for callflow to execute.

### Build `main_number_tod.php`

The first thing needed is to set the content-type to application/json.

```php
<?php

    header('content-type:application/json');
```

Now we need to know what time it is and determine what to do:

```php
$now = time();

$hour = date("G", $now);

if ( $hour >= 9 && $hour < 17 ) {
  business_hours();
} else {
  after_hours();
}
```

Now we have two functions to build the JSON for the callflow to execute:

* `business_hours()`:
    ```php
    function business_hours() {
    ?>
      {"module":"device"
       ,"data":{"id":"{FRONT_DESK_DEVICE_ID}"}
       ,"children":{
         "_":{
           "module":"voicemail"
           ,"data":{"id":"{COMPANY_VM_BOX_ID}"}
         }
       }
    <?php
    }
    ```

* `after_hours()`:
    ```php
    function after_hours() {
    ?>
      {"module":"voicemail"
       ,"data":{"id":"{COMPANY_VM_BOX_ID}"}
      }
    <?php
    }
    ```

Bring it all together:

```php
<?php
    header('content-type:application/json');
    $now = time();
    $hour = date("G", $now);

    if ( $hour > 8 && $hour < 17 ) {
      business_hours();
    } else {
      after_hours();
    }

    function business_hours() {
    ?>
      {"module":"device"
       ,"data":{"id":"{FRONT_DESK_DEVICE_ID}"}
       ,"children":{
         "_":{
           "module":"voicemail"
           ,"data":{"id":"{COMPANY_VM_BOX_ID}"}
         }
       }
    <?php
    }

    function after_hours() {
    ?>
      {"module":"voicemail"
       ,"data":{"id":"{COMPANY_VM_BOX_ID}"}
      }
    <?php
    }
?>
```
