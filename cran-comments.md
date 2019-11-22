## Test environments

* local build Windows 10, R 3.6.1
* ubuntu 18.04, R 3.
* ubuntu 16.04, R-release GCC on R-hub builder
* ubuntu 16.04, R-devel GCC on R-hub builder
* macOS 10.11 El Capitan, R-release on R-hub builder
* debian, R-release GCC  on R-hub builder
* debian, R-devel GCC  on R-hub builder
* debian, R-patched GCC  on R-hub builder
* winbuilder devel, release, oldrelease.

## R CMD check results

* Debian R-hub builder:

    0 errors | 0 warnings | 1 note

* All other builds:

    0 errors | 0 warnings | 0 notes

## NOTES

**debian rhub** builds gave NOTEs for false-positives for possible spelling 
errors on the words "interpretable" and "stochasticity". Both words are spelled
correctly. No NOTEs from other builds.

## Downstream dependencies

None.

## Additional comments

The vignette FRD_vignette.Rmd takes a very long time to build.
