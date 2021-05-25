## Test environments

* local build Windows 10, R 4.1.0
* local build ubuntu 20.04.2 LTS, R 4.1.0
* GitHub Actions builds:
  * ubuntu 20.04 LTS, R-release
  * ubuntu 20.04 LTS, R-devel
  * macOS-latest, R-release
  * windows-latest, R-release
* R-hub builder:
    * ubuntu 20.04.1 LTS, R-release GCC
    * ubuntu 20.04.1 LTS, R-devel GCC
    * macOS 10.13.6 High Sierra, R-release CRAN's setup
    * fedora, R-devel GCC
    * fedora, R-devel clang, gfortran
    * debian, R-release GCC
    * debian, R-devel GCC
    * debian, R-devel clang, ISO-8859-15 locale
* winbuilder devel, release, oldrelease.

## R CMD check results

* All builds:

    0 errors | 0 warnings | 0 notes

## Downstream dependencies

None.

## Additional comments

* The vignette FRD_vignette.Rmd takes a very long time to build.
* This submission is an update to version 0.2.4

  I fixed errors in the package tests that were caught by the new
  `CHECK_MATRIX_DATA` tests in the new r-devel under Debian.
