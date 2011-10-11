This library contains next public exported methods:

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
