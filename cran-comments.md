# R CMD check results

On my Linux machine there were no unavoidable notes; I confirmed that the "(possibly) invalid URL" is valid, it is "blocked" by an application on my web browser. I also checked that the 'Possible spelling errors' are indeed correct spellings of surnames.

`devtools::check(manual = TRUE, incoming = TRUE, remote = TRUE)`
...
There were no ERRORs or WARNINGs.
── R CMD check results ────────────────────────────────────── surveil 0.1.0 ────
Duration: 9m 17.5s

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Connor Donegan <connor.donegan@gmail.com>’
  
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    Conceicao (12:857)
    Theil (12:981)
    Theil's (12:274)
  
  Found the following (possibly) invalid URLs:
    URL: https://www.jstor.org/stable/40326064
      From: DESCRIPTION
            man/surveil-package.Rd
      Status: 403
      Message: Forbidden

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-march=native’

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
