#### Public exports

* utc_to_local(DateTime, Timezone) - converts UTC time to local according to specified Timezone
* local_to_utc(DateTime, Timezone) - converts local time to UTC
* local_to_local(DateTime, TimezoneFrom, TimezoneTo) - converts local time to local
* tz_name(DateTime, Timezone) - returns a timezone name (E.g. MSK, MSD, etc)
* tz_shift(DateTime, Timezone) - returns time difference between local datetime and GMT
* tz_shift(DateTime, TimezoneFrom, TimezoneTo) - returns time difference between local datetime and required timezone
Where
DateTime = {date(), time()}
TimeZone(To, From) = String(). E.g. “Europe/Moscow”, “America/NewYork”. Or abbreviations "MSK", "MSD", etc. Note:
abbreviation is just used to find appropriate timezone name. If you want to convert "MSK" -> "PDT", but source timezone
is not in daylight saving, it will be corrected by library and "MSK" -> "PST" conversion will be made.

#### Examples of usage

Converts UTC time to local one
>localtime:utc_to_local({{2010, 7, 22}, {17, 56, 23}, "Europe/Moscow").
>
>{{2010,10,10},{21,56,23}}

Converts local time to UTC one
>localtime:local_to_utc({{2010, 10, 10}, {21, 56, 23}}, "Europe/Moscow").
>
>{{2010,10,10},{17,56,23}}

Converts time from one local timezone to another local one
>localtime:local_to_local({{2010, 10, 10}, {21, 56, 23}}, "Europe/Moscow", "Australia/Sydney").
>
>{{2010,10,11},{3,56,23}}

Returns timezone name
>localtime:tz_name({{2010, 10, 10}, {21, 56, 23}}, "Europe/Moscow").
>
>{"MSK","MSK"}

>localtime:tz_name({{2010,10,11},{3,56,23}}, "Australia/Sydney").
>
>{"EST","EST"}

Calculates time difference between UTC and local one
>localtime:tz_shift({{2013, 01, 22}, {18, 17, 00}}, "Europe/Moscow").
>
>{'+',4,0}

>localtime:tz_shift({{2013, 01, 22}, {18, 17, 00}}, "America/New York").
>
>{'-',5,0}

Calculates time difference between two local timezones
>localtime:tz_shift({{2013, 01, 22}, {18, 17, 00}}, "America/New York", "Europe/Moscow").
>
>{'+',9,0}
