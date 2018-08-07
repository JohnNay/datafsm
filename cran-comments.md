## Test environments
* local build Windows 10, R 3.5.1
* ubuntu 18.04, R 3.4.4
* ubuntu 16.04, R 3.4.4
* ubuntu 16.04, R-devel  (2018-06-20 r74924 -- "Unsuffered Consequences") with ASAN, gcc
* ubuntu 14.04.5 R 3.5.0
* ubuntu 14.04.5 R-devel 2018-08-06 r75070 with valgrind


## R CMD check results
There were no ERRORs, no WARNINGs, and no NOTES.

## ASAN gcc results
There were no reported memory access errors

## Downstream dependencies
None.

## Additional comments

The vignette FRD_vignette.Rmd takes a very long time to build.
