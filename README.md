<!-- README.md is generated from README.Rmd. Please edit that file -->

Surveil
=======

Public Health Surveillance
--------------------------

<img src="man/figures/logo.png" align="right" width="160" />

The **surveil** R package provides time series models for routine public
health surveillance tasks: model time trends in mortality or disease
incidence rates to make inferences about health inequalities, cumulative
and period percent change, and age-standardized rates.

**surveil** is an interface to [Stan](https://mc-stan.org), a
state-of-the-art platform for Bayesian inference.

### Installation

This project is still under development.

### Usage

Model time series data of mortality or disease incidence by loading the
**surveil** package into R together with disease surveillance data.
Tables exported from CDC WONDER are automatically in the correct format.

``` r
library(surveil)
library(knitr)
data(cancer)

kable(head(cancer), 
      booktabs = TRUE,
      caption = "Table 1. A glimpse of cancer surveillance data")
```

|  Year| Age   |  Count|  Population|
|-----:|:------|------:|-----------:|
|  1999| &lt;1 |    866|     3708753|
|  1999| 1-4   |   2959|    14991152|
|  1999| 5-9   |   2226|    20146188|
|  1999| 10-14 |   2447|    19742631|
|  1999| 15-19 |   3875|    19585857|
|  1999| 20-24 |   5969|    18148795|

Model trends in risk and easily obtain functions of risk estimates, such
as cumulative percent change:

``` r
fit <- stan_rw(data = cancer,
               time = Year,
               group = Age)
```

    ## [1] "Setting normal prior(s) for eta_1: "
    ##  location scale
    ##        -5    10
    ## [1] "\nSetting half-normal prior for sigma: "
    ##  location scale
    ##         0     1
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 5.7e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.57 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 4.85199 seconds (Warm-up)
    ## Chain 1:                1.99421 seconds (Sampling)
    ## Chain 1:                6.8462 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 4.3e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.43 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 5.03155 seconds (Warm-up)
    ## Chain 2:                2.02755 seconds (Sampling)
    ## Chain 2:                7.0591 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 4.7e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.47 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 5.19795 seconds (Warm-up)
    ## Chain 3:                2.03429 seconds (Sampling)
    ## Chain 3:                7.23224 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 4.6e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.46 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 4.92575 seconds (Warm-up)
    ## Chain 4:                2.13442 seconds (Sampling)
    ## Chain 4:                7.06018 seconds (Total)
    ## Chain 4:

``` r
fit_apc <- apc(fit)
plot(fit_apc, cumulative = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Citation

> Donegan, Connor (2021). surveil: Public health surveillance. R package
> version 0.1.0.,
> <a href="https://connordonegan.github.io/surveil/" class="uri">https://connordonegan.github.io/surveil/</a>

    bibentry(
      bibtype = "Manual",
      title= "surveil: Public Health Surveillance",
      author= "Donegan, Connor",
      url = "https://connordonegan.github.io/surveil/",
      year = 2021,
      note = "R package version 0.1.0"
    )

All **surveil** models were built using Stan:

> Stan Development Team. 2021. Stan Modeling Language Users Guide and
> Reference Manual, 2.28.
> <a href="https://mc-stan.org" class="uri">https://mc-stan.org</a>

    bibentry(
      bibtype = "Manual",
      title= "Stan Modeling Language Users Guide and Reference Manual",
      author= "{Stan Development Team}",
      url = "https://mc-stan.org",
      year = 2021,
      note = "Version 2.28"
    )
