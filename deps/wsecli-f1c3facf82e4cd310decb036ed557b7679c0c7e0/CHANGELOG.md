#Change Log

### 1.1.1
  * Update rebar.confi to fetch wsock 1.1.5

### 1.1.0
  * Typespec fixes
  * Include makefile and deprecate rake tasks

### 1.0.0
  * New wsecli:start/2 function that takes a WebSockets URI as parameter.
  * The use of the 'anon' atom to request a non registered process has been deprecated. Now use the {register, true | false | atom()} option.
  * Support connections to SSL capable endpoints.

### 0.2.1
  * Don't do builds for R14B0X in Travis as they are broken.

### 0.2.0
  * Ensure the version for the dependencies using wether a git-tag or a commit sha1.

### 0.1.1
  * Set the version for ```wsock``` to its newest version. This version fixes a bug  ```wsock``` where ```message``` record was not exported.

