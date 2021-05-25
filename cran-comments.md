## Test environments

* local build Windows 10, R 4.1.0
* ubuntu 20.04, R 4.1.0
* R-hub builder:
    * ubuntu 18.04, R-release GCC
    * ubuntu 18.04, R-devel GCC
    * macOS 10.11 El Capitan, R-release
    * debian, R-release GCC
    * debian, R-devel GCC
    * debian, R-patched GCC
* winbuilder devel, release, oldrelease.

## R CMD check results

* r-hub Debian R-devel build:

    0 errors | 0 warnings | 1 note

* All other builds:

    0 errors | 0 warnings | 0 notes

## NOTES


## Downstream dependencies

None.

## Additional comments

* The vignette FRD_vignette.Rmd takes a very long time to build.
* This submission is an update to version 0.2.4

    I fixed errors in the package tests that were caught by the new
    CHECK_MATRIX_DATA tests in the new r-devel under Debian.
