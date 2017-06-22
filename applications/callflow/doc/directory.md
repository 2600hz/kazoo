## Directory

### About Directory

Match DTMF keypresses to the directory's listing to find callees.

#### Schema

Validator for the directory callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`id` | Directory ID | `string()` |   | `false`


### Processing Flow

The basic flow of a directory call:

1. Prompt: Please enter the first few letters of the person's
    a. First entry in sort order (first or last name)
2. Receive MIN_DTMF dtmf tones
   a. If timeout occurs:
       1. Prompt: You need to specify a minimum of
           a. MIN_DTMF
           b. Prompt: letters of the person's name
       2. go back into main #2
3. After receiving MIN_DTMF, filter table
4. Go into a next_dtmf wait loop
   a. if timeout, prompt with # of matches, option to hear matches or continue pressing keys
   b. if continue, go into next_dtmf wait loop
   c. else go to play_matches
5. play_matches: play hd(matches), options to hear more or connect or continue pressing keys
