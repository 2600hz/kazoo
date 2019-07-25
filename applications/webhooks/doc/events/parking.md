# Call parking webhook event

This event triggers when call get parked, or retrieved from a parking slot or abandoned.

## Info

* **Event name:** parking
* **Friendly name:** Call Parking

## Modifiers

_None._

## Samples

All parking events have same payload, the only difference is "event_name" which can be:

* PARK_PARKED
* PARK_RETRIEVED
* PARK_ABANDONED

```json
{
    "account_id": "5a2d994fbae69b1d6b01eb9f0e7dfe62",
    "call_id": "OWU4NzEwOTgyZWNiMjM0MzI0NjRkZDc4MWVmMjEyOWI",
    "callee_id_name": "Test Name",
    "callee_id_number": "5355543456",
    "caller_id_name": "Superman",
    "caller_id_Number": "+15555432345",
    "event_name": "PARK_PARKED",
    "parking_slot": 1
}
```
