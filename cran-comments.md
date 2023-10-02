Sent to win-builder:

* using R version 4.3.1 (2023-06-16 ucrt)
...
* DONE
Status: OK

and:

* using log directory 'd:/RCompile/CRANguest/R-devel/surveil.Rcheck'
* using R Under development (unstable) (2023-10-01 r85245 ucrt)
...
* DONE
Status: OK


Built and check on Ubuntu 22.04:

── R CMD check results ──────────────────────────────────── surveil 0.2.2 ────
Duration: 8m 15.5s

❯ checking installed package size ... NOTE
    installed size is 80.3Mb
    sub-directories of 1Mb or more:
      libs  79.0Mb

❯ checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    ‘RcppParallel’ ‘rstantools’
    All declared Imports should be used.

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

