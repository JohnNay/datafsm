## Test environments
* local build Windows 10, R 3.5.1
* ubuntu 18.04, R 3.4.4
* ubuntu 16.04, R 3.4.4
* ubuntu 16.04, R-devel  (2018-06-20 r74924 -- "Unsuffered Consequences") with ASAN, gcc

## R CMD check results
There were no ERRORs, no WARNINGs, and no NOTES.

## ASAN gcc results
There were no reported memory access errors

## Downstream dependencies
None.

## Notes

The vignette FRD_vignette.Rmd takes a very long time to build, so I 
am including a pre-compiled version of the vignette.
