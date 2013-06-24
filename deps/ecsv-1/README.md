#ecsv 0.2

**2011 (c) Nicolas R Dufour <nicolas.dufour@nemoworld.info>**

**ecsv** is a simple Erlang CSV parser able to read a file or string and sending back to an erlang process events when a line is parsed.

ecsv is under MIT. See NOTICE file for more details.

##Requirements

* Erlang/OTP R13/R14
* GNU Make

##Design

Ecsv has 3 components:

- a reader [*ecsv_reader*]: read a file or a string and sends a stream of characters to the parser, and eof when it's the end.
- a parser [*ecsv_parser*]: parse the character stream and sends to the processor each parsed line.
- a processor (function given to *ecsv*): function given to *ecsv* to process a parsed line.

##How to use it

Create a function that will accept 2 arguments:

- the new parsed row
- a current state (defaulted to [])

Example: how to count the lines:

    MyFun = fun(_NewLine, Counter) ->
        % NewLine contains an array of strings
        Counter + 1.

Then call ecsv with the default state set to `0`:

    {ok, IoDevice} = file:open("/path/to/my.csv", [read]),
    {ok, FinalCounter} = ecsv:process_csv_file_with(IoDevice, MyFun, 0)

`FinalCounter` will have the number of parsed lines.

Take a look at the examples in the directory `examples`. A basic benchmark accepting one argument as the csv filename will parse it and display the number of parsed lines.

##Notes

This parser is based on the blog post written by *Andy Till* located
here [http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html](http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html).

This parser supports well formed csv files which are

- a set of lines ending with a `\n`
- each line contains a set of fields separated with a comma (`,`)
- each field value can be enclosed with single (`'`) or double quote (`"`)
- each field value can be empty
- any `\r` is ignored

Example:

    SC_Group_ID,SC_Group_Desc,SC_GroupCommod_ID,SC_GroupCommod_Desc,SC_Geography_ID,SortOrder,SC_GeographyIndented_Desc,SC_Commodity_ID,SC_Commodity_Desc,SC_Attribute_ID,SC_Attribute_Desc,SC_Unit_ID,SC_Unit_Desc,Year_ID,SC_Frequency_ID,SC_Frequency_Desc,Timeperiod_ID,Timeperiod_Desc,Amount
    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1960,3,"Annual",114,"MY Oct-Sep",248
    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1961,3,"Annual",114,"MY Oct-Sep",326

Please note:

- This parser has no failsafe mechanism if the file is badly formed!
  But the line `a,,,,,\n` is perfectly fine.
- This parser doesn't allow a return (`\n`) in a field value!

