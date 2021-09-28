
library(tidyverse)

iter = 50
data(msa)

dfw <- dplyr::filter(msa,
                     str_detect(MSA, "Dallas")          
                         )

context("stan_rw")
test_that("cor = FALSE works with stan_rw", {    
    fit <- stan_rw(
        data = filter(dfw, Race == "Hispanic"),
        time = Year,
        iter = iter,
        cor = FALSE,
        chains = 1
    )
    expect_s3_class(fit, "surveil")
})

test_that("cor = TRUE works with stan_rw", {
    fit2 <- stan_rw(
        data = dfw,
        cor = TRUE,
        group = Race,
        time = Year,
        iter = iter,
        chains = 1
    )
    
    expect_s3_class(fit2, "surveil")
})

test_that("loo methods work", {
        fit <- stan_rw(
            data = filter(dfw, Race == "Hispanic"),
            time = Year,
            iter = iter,
            chains = 1
        )
       IC <- loo(fit)
        expect_s3_class(IC, "psis_loo")
       IC2 <- waic(fit)
        expect_s3_class(IC2, "waic")        
})

test_that("group_diff works", {
    fit2 <- stan_rw(        
        dfw,
        group = Race,
        time = Year,
        iter = iter,
        chains = 1
        )
        x <- group_diff(fit2, "Black or African American", "White")
        expect_s3_class(x, "list")
    })
