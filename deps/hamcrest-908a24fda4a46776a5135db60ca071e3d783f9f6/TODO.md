todo list
=========

- improve error reporting
  - change semantics of 'matchspec'.desc field
    - return a description of the matcher *only*
    - modify assert_that to handle construction of failure messages (?)
  - support for matchers to return information about match failures
	  - actual input
	  - indirect input/fixture data
	  - explanatory message
  - defaults for matchers not returning information about match failures
  - progress reports
	  - *real time* progress logging
	  - support for _stdout_ or _MFA_ (to support _ct:pal/2_ for example)
	  - quiet mode (turn it off!)
	  - provide full _textual_ log post processing (?)