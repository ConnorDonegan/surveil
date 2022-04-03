A previous submission had a long build time for vignettes (12m). 

Time to rebuild vignettes is now 37s for Windows development version and 70s for Windows release.

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
