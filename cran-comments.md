A previous submission had a long build time for vignettes (12m). 

Time to rebuild vignettes is now 30s on Ubuntu, 37s for Windows development version and 70s for Windows release.

From devtools::check_win_release():

```
* checking re-building of vignette outputs ... [70s] OK
* checking PDF version of manual ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: OK
```

From devtools::check_win_devel():

```
* checking re-building of vignette outputs ... [37s] OK
```

from devtools::check():

```
✔  checking re-building of vignette outputs (20.9s)
...
── R CMD check results ────────────────────────────────────── surveil 0.2.0 ────
Duration: 5m 34.7s

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-march=native’

0 errors ✔ | 0 warnings ✔ | 2 notes ✖
```

