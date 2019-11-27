EMSCSV Downloader
=================

To download the csvs, make a folder `certs` with the files `cert.key`, `cert.crt`, `stp.wien.gv.at.crt` in it.
`cert.{crt,key}` should be able to access the csv files.

Then run

    ./download-files.sh

The script prints some useful diagnostic info and creates folders `file-lists` and `csvs`.
The script may be run repeatedly, it saves its outputs in folders named after the run-date.

File structure

    .
    ├── certs
    │   ├── cert.crt
    │   ├── cert.key
    │   └── stp.wien.gv.at.crt
    ├── csvs
    │   └── $DATE
    │       └── emscsv
    │           └── EXPORT
    │               └── files appear here
    ├── download-files.sh
    ├── extract-links.py
    └── file-lists
        └── $DATE
                ├── files.html
                └── urls.txt


Dependencies
------------

 - python3
 - python3 module Beautiful Soup v4
 - cut
 - bash
 - curl

On ubuntu, running `apt install python3 python3-bs4` should be sufficient.
