## Test environments

* local build Windows 10, R 4.1.0
* local build ubuntu 20.04.2 LTS, R 4.1.0
* GitHub Actions builds:
  * ubuntu-20.04: 20.04.2 LTS, R-release
  * ubuntu-20.04: 20.04.2 LTS, R-devel
  * macOS-latest: Catalina 10.15.7, R-release
  * windows-latest: Server 2019 10.0.17763, R-release
* R-hub builder:
  * ubuntu 20.04.1 LTS, R-release GCC
  * ubuntu 20.04.1 LTS, R-devel GCC
  * macOS 10.13.6 High Sierra, R-release CRAN's setup
  * macOS 10.13.6 High Sierrs, R-release
  * fedora, R-devel GCC
  * fedora, R-devel clang, gfortran
  * debian, R-release GCC
  * debian, R-devel GCC
  * debian, R-devel clang
  * debian, R-patched GCC
  * windows, server 2008 R2 SP1, R-release
  * windows, server 2008 R2 SP1, R-oldrel
  * windows, server 2008 R2 SP1, R-patched
* win-builder builds:
  * R-release
  * R-devel
  * R-oldrelease

## R CMD check results

* All builds except win-builder:

  0 errors | 0 warnings | 0 notes

* Win-builder R-devel and R-oldrelease
  
  0 errors | 0 warnings | 1 note
  
* Win-builder R-release

  1 error | 1 warning | 1 note
  
### Comments on win-builder results

* The **note** in R-release, R-devel, and R-oldrelease is a harmless note about 
  changing maintainer (because I changed the arguments to person() in the 
  `Authors@R` section of `DESCRIPTION` because the `middle` argument has 
  been depracated). The maintainer remains the same, but the check observes a
  change from `person(last="Gilligan", first="Jonathan", middle="M.", ...)` to
  `person(last="Gilligan", given = c("Jonathan", "M."), ...)`.
  
* The **error** and **warning** in R-release are false-positives resulting 
  from win-builder not having the CRAN `caret` package available, even though
  `caret` is current on CRAN and has binaries available for Windows.
  I also observe that the `caret` package is listed as an import in 
  `DESCRIPTION` but this error occurs only during testing with `testthat`
  but there were no errors about the dependency during installation.
  For these reasons I am confident that the error and warning reported by 
  win-builder for R-release are false-positives.
  
  The error reported by win-builder with R-release is:
  ```
  * checking for unstated dependencies in 'tests' ... OK
  * checking tests ...
  ** running tests for arch 'i386' ... [7s] OK
    Running 'testthat.R' [6s]
  ** running tests for arch 'x64' ... [6s] ERROR
    Running 'testthat.R' [6s]
  Running the tests in 'tests/testthat.R' failed.
  Complete output:
    > library(testthat)
    > library(datafsm)
    > 
    > test_check("datafsm")
    == Failed tests ================================================================
    -- Error (test_mainfunc.R:7:9): evolve_model() returns correct type of object --
    Error: DLL 'caret' not found: maybe not installed for this architecture?
    Backtrace:
        x
     1. +-datafsm::evolve_model(cdata, cv = FALSE) test_mainfunc.R:7:8
     2. \-base::loadNamespace(x)
     3.   \-base::library.dynam(lib, package, package.lib)
    -- Error (test_mainfunc.R:14:3): evolve_model() returns warnings and errors ----
    Error: DLL 'caret' not found: maybe not installed for this architecture?
    Backtrace:
        x
     1. +-testthat::expect_warning(evolve_model(cdata, cv = FALSE), "did not supply a data.frame") test_mainfunc.R:14:2
     2. | \-testthat:::quasi_capture(enquo(object), label, capture_warnings)
     3. |   +-testthat:::.capture(...)
     4. |   | \-base::withCallingHandlers(...)
     5. |   \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
     6. +-datafsm::evolve_model(cdata, cv = FALSE)
     7. \-base::loadNamespace(x)
     8.   \-base::library.dynam(lib, package, package.lib)

    [ FAIL 2 | WARN 0 | SKIP 0 | PASS 3 ]
    Error: Test failures
    Execution halted
  ```
  and the warning is:
  ```
  * checking for unstated dependencies in vignettes ... OK
  * checking package vignettes in 'inst/doc' ... OK
  * checking re-building of vignette outputs ... [10s] WARNING
  Error(s) in re-building vignettes:
  --- re-building 'datafsm_introduction.Rmd' using rmarkdown
  Quitting from lines 173-175 (datafsm_introduction.Rmd) 
  Error: processing vignette 'datafsm_introduction.Rmd' failed with diagnostics:
  DLL 'caret' not found: maybe not installed for this architecture?
  --- failed re-building 'datafsm_introduction.Rmd'

  --- re-building 'FRD_vignette.Rmd' using rmarkdown_notangle
  --- finished re-building 'FRD_vignette.Rmd'
  ```


## Downstream dependencies

None.

## Additional comments

* This submission updates the package to version 0.2.4

  I fixed errors in the package tests that were caught by the new
  `CHECK_MATRIX_DATA` tests in r-devel under Debian.
  
  I changed the calls to `person()` in the `Authors@R` section of `DESCRIPTION` 
  because the parameter `middle` is now deprecated.
  This produces a harmless **note** in some `R CMD check` runs about a new
  maintainer.
