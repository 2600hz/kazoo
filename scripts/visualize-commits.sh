#!/bin/bash
gource -1280x720 --highlight-all-users --file-filter ".*\.beam" --user-scale 2 --file-extensions --seconds-per-day .5 --auto-skip-seconds .5 --file-idle-time 0 -hide bloom,dirnames,filenames,mouse --date-format "%B %e, %G" --camera-mode overview --caption-file caption.log --caption-size 26 --caption-duration 5
