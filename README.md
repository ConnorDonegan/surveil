<!-- README.md is generated from README.Rmd. Please edit that file -->

# Surveil

<img src="man/figures/logo.png" align="right" width="160" />

### Public health surveillance

#### Measure and monitor health inequalities

#### Cumulative and period percent change

#### Age standardization methods

#### Fully Bayesian inference

### Installation

From **R** (\>= version 4.05), install **surveil** using:

``` r
if (!require(drat)) install.packages("drat")
drat::addRepo("connordonegan")
install.packages("surveil")
```

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

| Year | Age   | Count | Population |
|-----:|:------|------:|-----------:|
| 1999 | \<1   |   866 |    3708753 |
| 1999 | 1-4   |  2959 |   14991152 |
| 1999 | 5-9   |  2226 |   20146188 |
| 1999 | 10-14 |  2447 |   19742631 |
| 1999 | 15-19 |  3875 |   19585857 |
| 1999 | 20-24 |  5969 |   18148795 |

Table 1. A glimpse of cancer surveillance data

Model trends in risk and easily view functions of risk estimates, such
as cumulative percent change:

``` r
fit <- stan_rw(data = cancer,
               time = Year,
               group = Age)
```

    ## [1] "Setting normal prior(s) for eta_1: "

    ## Distribution: normal

    ##  location scale
    ##        -5    10
    ## [1] "\nSetting half-normal prior for sigma: "

    ## Distribution: normal

    ##  location scale
    ##         0     1
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 5e-05 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.5 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 1: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 5.20563 seconds (Warm-up)
    ## Chain 1:                1.99068 seconds (Sampling)
    ## Chain 1:                7.19632 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 4.5e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.45 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 2: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 5.40392 seconds (Warm-up)
    ## Chain 2:                2.04437 seconds (Sampling)
    ## Chain 2:                7.44829 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 8.3e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.83 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 3: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 6.60441 seconds (Warm-up)
    ## Chain 3:                2.46238 seconds (Sampling)
    ## Chain 3:                9.06679 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'poissonRW' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 5.1e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.51 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 3000 [  0%]  (Warmup)
    ## Chain 4: Iteration: 1500 / 3000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1501 / 3000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 3000 / 3000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 6.95378 seconds (Warm-up)
    ## Chain 4:                2.55067 seconds (Sampling)
    ## Chain 4:                9.50446 seconds (Total)
    ## Chain 4:

``` r
fit_apc <- apc(fit)
plot(fit_apc, cumulative = TRUE)
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Citation

> Donegan, Connor (2021). surveil: Public health surveillance. R package
> version 0.1.0., <https://connordonegan.github.io/surveil/>

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
> Reference Manual, 2.28. <https://mc-stan.org>

    bibentry(
      bibtype = "Manual",
      title= "Stan Modeling Language Users Guide and Reference Manual",
      author= "{Stan Development Team}",
      url = "https://mc-stan.org",
      year = 2021,
      note = "Version 2.28"
    )
