
iter = 100
data(msa)

dfw <- msa[grep("Dallas", msa$MSA), ]

context("stan_rw")
test_that("cor = FALSE works with stan_rw", {    
    fit <- stan_rw(
        data = dfw[grep("Hispanic", dfw$Race), ],
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

test_that("priors work", {
    prior <- list()
    prior$eta_1 = student_t(location = -5, scale = 5)
        fit <- stan_rw(
            data = dfw[grep("Hispanic", dfw$Race), ],
            prior = prior,
            time = Year,
            iter = iter,
            chains = 1
        )
    expect_s3_class(fit, "surveil")
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


test_that("theil.surveil works", {
    fit <- stan_rw(        
        dfw,
        group = Race,
        time = Year,
        iter = iter,
        chains = 1
    )
    x <- theil(fit)
    expect_s3_class(x, "theil")
    })
test_that("theil.list works", {
    fit <- stan_rw(        
        dfw,
        group = Race,
        time = Year,
        iter = iter,
        chains = 1
    )
    flist <- list(fit, fit, fit)
    x <- theil(flist)
    expect_s3_class(x, "theil_list")
})
