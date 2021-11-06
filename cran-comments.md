# R CMD check results

## win-builder

win-builder R-release produced 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Connor Donegan <connor.donegan@gmail.com>'

## devtools::check()

On my Linux machine this produced the following:

There were no ERRORs or WARNINGs.

R CMD check results ────────────────────────────────────── surveil 0.1.0 ────
Duration: 3m 28.4s

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Connor Donegan <connor.donegan@gmail.com>’
  
  New submission

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-march=native’

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
