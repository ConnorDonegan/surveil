#' Time series models for mortality and disease incidence 

#' @description Model time-varying incidence rates given a time series of case (or death) counts and population at risk.
#'
#' @param data A `data.frame` containing the following columns: \describe{
#'  \item{Count}{Number of cases or deaths; this column must be named 'Count'.}
#'  \item{Population}{Size of population at risk; this column must be named 'Population'.}
#'  \item{time}{Time period indicator. (Provide the (unquoted) column name using the `time` argument.)}
#'  \item{group}{Optional grouping variable. (Provide the (unquoted) column name using the `group` argument.)}
#' }
#'
#' @param time Specify the (unquoted) name of the time variable in `data`, as in `time = Year`. This variable must be numeric-alike (i.e., `as.numeric(data$time)` will not fail).
#' 
#' @param group If `data` is aggregated by demographic group, provide the (unquoted) name of the column in `data` containing the grouping structure, such as age brackets or race-ethnicity. E.g., if `data` has column names `Year`, `Race`, `Cases`, and `Population`, then you would provide `group = Race`. 
#'
#' @param cor  For correlated random walks use `cor = TRUE`; default value is `FALSE`. Note this only applies when the `group` argument is used.
#'
#' @param family The default specification is a Poisson model with log link function (`family = poisson()`). For a Binomial model with logit link function, use `family = binomial()`.
#' 
#' @param prior Optionally provide a named `list` with prior parameters. If any of the following items are missing, default priors will be assigned and printed to the console.
#'
#' \describe{
#' \item{eta_1}{The first value of log-risk in each series must be assigned a Gaussian prior probability distribution. Provide the location and scale parameters for each demographic group in a list, where each parameter is a `k`-length vector.
#'
#' For example, with `k=2` demographic groups, the following code will assign priors of `normal(-6.5, 5)` to the starting values of both series: `prior = list(eta_1 = normal(location = -6.5, scale = 5, k = 2)`. Note, `eta` is the log-rate, so centering the prior for `eta_1` on `-6.5` is similar to centering the prior rate on `exp(-6.5)*100,000 = 150` cases per 100,000 person-years at risk. Note, however, that the translation from log-rate to rate is non-linear.}
#'
#' \item{sigma}{Each demographic group has a scale parameter assigned to its log-rate. This is the scale of the annual deviations from the previous year's log-rate. The scale parameters are assigned independent half-normal prior distributions (these `half` normal distributions are restricted to be positive-valued only).}
#'
#' \item{omega}{If `cor = TRUE`, an LKJ prior is assigned to the correlation matrix, Omega.}
#' }
#'
#' 
#' @param chains Number of independent MCMC chains to initiate (passed to \code{\link[rstan]{sampling}}).
#'
#' @param cores The number of cores to use when executing the Markov chains in parallel (passed to \code{\link[rstan]{sampling}}).
#' 
#' @param iter Total number of MCMC iterations. Warmup draws are automatically half of `iter`.
#' @param refresh How often to print the MCMC sampling progress to the console.
#' @param control A named list of parameters to control Stan's sampling behavior. The most common parameters to control are `adapt_delta`, which may be raised to address divergent transitions, and `max_treedepth`. For example, `control = list(adapt_delta = 0.99, max_treedepth = 13)`, may be a reasonable specification to address a divergent transitions or maximum treedepth warning from Stan.
#'
#' @param ... Other arguments passed to \code{\link[rstan]{sampling}}.
#'
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#'
#' @return
#' The function returns a list, also of class `surveil`, containing the following elements:
#' \describe{
#' 
#'   \item{summary}{A `data.frame` with posterior means and 95 percent credible intervals, as well as the raw data (Count, Population,  time period, grouping variable if any, and crude rates).}
#'
#' \item{samples}{A `stanfit` object returned by \code{\link[rstan]{sampling}}. This contains the MCMC samples from the posterior distribution of the fitted model.}
#'
#' \item{cor}{Logical value indicating if the model included a correlation structure.}
#'
#' \item{time}{A list containing the name of the time-period column in the user-provided data and a `data.frame` of observed time periods and their index.}
#'
#' \item{group}{If a grouping variable was used, this will be a list containing the name of the grouping variable and a `data.frame` with group labels and index values.}
#'
#' \item{family}{The user-provided `family` argument.}
#' }
#' 
#' @details
#'
#' By default, the models have Poisson likelihoods for the case counts, with log link function. Alternatively, a Binomial model with logit link function can be specified using the `family` argument (`family = binomial()`).
#' 
#' For time t = 1,...n, the models assign Poisson probability distribution to the case counts, given log-risk `eta` and population at tirks P; the log-risk is modeled using the first-difference (or random-walk) prior:
#' 
#' ```
#'  y_t ~ Poisson(p_t * exp(eta_t))
#'  eta_t ~ Normal(eta_{t-1}, sigma)
#'  eta_1 ~ Normal(-6, 5) (-Inf, 0)
#'  sigma ~ Normal(0, 1) (0, Inf)
#' ```
#' This style of model has been discussed in Bayesian (bio)statistics for quite some time. See Clayton (1996).
#'
#' The above model can be used for multiple distinct groups; in that case, each group will have its own independent time series model.
#'
#' It is also possible to add a correlation structure to that set of models. Let `Y_t` be a k-length vector of observations for each of k groups at time t (the capital letter now indicates a vector), then:
#'
#' ```
#'  Y_t ~ Poisson(P_t * exp(Eta_t))
#'  Eta_t ~ MVNormal(Eta_{t-1}, Sigma)
#'  Eta_1 ~ Normal(-6, 5)  (-Inf, 0)
#'  Sigma = diag(sigma) * Omega * diag(sigma)
#'  Omega ~ LKJ(2)
#'  sigma ~ Normal(0, 1) (0, Inf)
#' ```
#' where `Omega` is a correlation matrix and `diag(sigma)` is a diagonal matrix with scale parameters on the diagonal. This was adopted from Brandt and Williams (2007); for the LKJ prior, see the Stan Users Guide and Reference Manual.
#'
#' If the binomial model is used instead of the Poisson, then the first line of the model specifications will be:
#'
#' ```
#'  y_t ~ binomial(P_t, inverse_logit(eta_t))
#' ```
#' All else is remains the same. The logit function is `log(r/(1-r))`, where `r` is a rate between zero and one; the inverse-logit function is `exp(x)/(1 + exp(x))`.
#' 
#' @source
#'
#' Brandt P, Williams JT. Multiple time series models. Thousand Oaks, CA: SAGE Publications, 2007. 
#' 
#' Clayton, DG. Generalized linear mixed models. In: Gilks WR, Richardson S, Spiegelhalter DJ, editors. Markov Chain Monte Carlo in Practice: Interdisciplinary Statistics. Boca Raton, FL: CRC Press, 1996. p. 275-302. 
#'
#' Stan Development Team. Stan Modeling Language Users Guide and Reference Manual, 2.28. 2021. https://mc-stan.org
#' 
#' @examples
#' \donttest{
#' library(rstan)
#' data(msa)
#' austin <- msa[grep("Austin", msa$MSA), ]
#'
#' fit <- stan_rw(austin,
#'                time = Year,
#'                group = Race,
#'                chains = 2, iter = 900) # for speed only
#' 
#' ## MCMC diagnostics
#' rstan::stan_mcse(fit$samples)
#' rstan::stan_rhat(fit$samples)
#' print(fit$samples)
#' 
#' ## print the surveil object
#' print(fit)
#' head(fit$summary)
#'
#' ## plot time trends
#' plot(fit, style = 'lines')
#'
#' ## age-specific rates and cumulative percent change
#' data(cancer)
#' fit <- stan_rw(cancer, time = Year, group = Age,
#'               chains = 2, iter = 900) # for speed only
#' fit_apc <- apc(fit)
#' plot(fit_apc, cumulative = TRUE)
#'
#' # age-standardized rates
#' data(standard)
#' fit_stands <- standardize(fit,
#'                           label = standard$age,
#'                           standard_pop = standard$standard_pop)
#' print(fit_stands)
#' plot(fit_stands)
#' fit_stands_apc <- apc(fit_stands)
#' plot(fit_stands_apc)
#' }
#'
#' @seealso \code{vignette("demonstration", package = "surveil")} \code{vignette("age-standardization", package = "surveil")} \code{\link[surveil]{apc}} \code{\link[surveil]{standardize}}
#' 
#' @export
#' @importFrom parallel detectCores
#' @importFrom dplyr `%>%` mutate arrange full_join distinct select group_by ungroup n select
#' @importFrom tidyr pivot_wider
#' @importFrom stats quantile poisson 
#' @md
stan_rw <- function(data,
                    group,
                    time,
                    cor = FALSE,
                    family = poisson(),
                    prior = list(),
                    chains = 4,
                    cores = 1,
                    iter = 3e3,
                    refresh = 1500,
                    control = list(adapt_delta = 0.98),
                    ...
                    ) {
    stopifnot(inherits(data, "data.frame"))
    stopifnot(all(c("Count", "Population") %in% names(data)))
    stopifnot(rlang::as_label(dplyr::enquo(time)) %in% names(data))
    stopifnot(inherits(family, "family"))
    stopifnot(family$family %in% c("poisson", "binomial"))    
    stopifnot(min(data$Count)>=0)
    stopifnot(min(data$Population)>=0)
    stopifnot(inherits(prior, "list"))
    stopifnot(inherits(cor, "logical"))
    if (!missing(group)) {
        tmp.df <- dplyr::distinct(data, {{ group }})
        if (!(nrow(tmp.df) > 1)) stop("Grouping variable (group) must contain at least two groups.")       
    }
    ## prep data
    ## force correct order by time
    data <- dplyr::mutate(data, time = as.numeric({{ time }}))
    data <- dplyr::arrange(data, .data$time)
    time_labels <- unique(data$time)
    time_df <- data.frame(time.index = 1:length(time_labels),
                          time.label = time_labels)
    time_list <- list(time = rlang::as_label(dplyr::enquo(time)),
                      time.df = time_df)
    if (missing(group)) {
        cases <- as.matrix(data$Count, ncol = 1)
        at_risk <- as.matrix(data$Population, ncol = 1)
        group.ids <- group.labels <- "1"
        group.df <- data.frame(group.index = group.ids,
                               group.label = group.labels)
        
    } else {
        cases <- data %>%
            as.data.frame %>%
            dplyr::select({{ group }}, .data$Count) %>%
            dplyr::group_by({{ group }}) %>%
            dplyr::mutate(id = 1:dplyr::n()) %>%
            tidyr::pivot_wider(
                .data$id,
                names_from = {{ group }},
                values_from = .data$Count
            ) %>%
            dplyr::select(-c(.data$id))
        at_risk <- data %>%
            as.data.frame %>%
            dplyr::select({{ group }}, .data$Population) %>%
            dplyr::group_by({{ group }}) %>%
            dplyr::mutate(id = 1:dplyr::n()) %>%            
            tidyr::pivot_wider(
                .data$id,
                names_from = {{ group }},
                values_from = .data$Population
            ) %>%
            dplyr::select(-c(.data$id))
        group.labels <- colnames(cases)
        group.ids <- 1:length(group.labels)
        group.df <- data.frame(group.index = group.ids,
                               group.label = group.labels)            
    }
    ## fill in missing priors
    if (!"eta_1" %in% names(prior)) {   
        prior$eta_1 <- normal(
            location = -6,
            scale = 5,
            k = ncol(cases)
        )
        print("Setting normal prior(s) for eta_1: ")
                print(normal(-6, 5))                
    }    
    if (!"sigma" %in% names(prior)) {
        prior$sigma <- normal(
            location = 0,
            scale = 1,
            k = ncol(cases)
        )
        print("\nSetting half-normal prior for sigma: ")
        print(normal(0, 1))
    }
    if (cor && !"omega" %in% names(prior)) {
        prior$omega <- lkj(2)
        print(paste("\nSetting LKJ prior on correlation matrix: "))
        print(lkj(2))
    }
    standata <- list(
        TT = nrow(cases),
        K = ncol(cases),
        y = cases, #/# no transpose 
        log_E = log(at_risk),  #/# no transpose
        population = at_risk, #/# no transpose
        is_poisson = as.integer(family$family == "poisson"),
        is_binomial = as.integer(family$family == "binomial"),
        prior_eta_1_location = as.array(prior$eta_1$location),
        prior_eta_1_scale = as.array(prior$eta_1$scale),
        prior_sigma_location = as.array(prior$sigma$location),
        prior_sigma_scale = as.array(prior$sigma$scale),
        prior_omega = prior$omega$eta
    )
    if (cor) {
        samples <- rstan::sampling(stanmodels$RWCorr,
                                   data = standata,
                                   pars = c("rate", "sigma", "Omega", "log_lik"),
                                   iter = iter,
                                   chains = chains,
                                   cores = cores,
                                   refresh = refresh,
                                   control = control, ...)
    } else {  
        standata$y <- t(standata$y) #/# transpose
        standata$log_E <- t(standata$log_E)  #/# transpose
        standata$population <- t(standata$population)
        samples <- rstan::sampling(stanmodels$RW,
                                   data = standata,
                                   pars = c("rate", "sigma", "log_lik"),
                                   iter = iter,
                                   chains = chains,
                                   cores = cores,
                                   refresh = refresh,
                                   control = control,
                                   ...)
    }
    group.names <- colnames(cases)
    eta = rstan::extract(samples, pars = "rate")$rate
    K <- dim(eta)[2]
    list.df <- list()
    for (j in 1:K) {
        M <- eta[,j,]
        j.mu <- apply(M, 2, mean)
        j.ci <- apply(M, 2, stats::quantile, probs = c(0.025, 0.975))
        j.df <- data.frame(
            time = time_labels,
            mean = j.mu,
            lwr_2.5 = j.ci["2.5%",],
            upr_97.5 = j.ci["97.5%",]
        )
        if (length(group.labels) > 1) j.df$group = group.labels[j]
        list.df[[j]] <- j.df
    }
    eta.df <- do.call("rbind", list.df)
    data.tmp <- data %>%
        dplyr::group_by({{ group }}) %>%
        dplyr::mutate(
            Crude = .data$Count / .data$Population
        )
    if (!missing(group)) {
        group_char <- rlang::as_label(dplyr::enquo(group))
        names(eta.df)[which(names(eta.df) == "group")] <- group_char
        join.vars <- c("time", group_char)
    } else {
        join.vars <- "time"
    }   
    res.df <- dplyr::full_join(eta.df, data.tmp, by = join.vars)
    out <- list(summary = res.df,
                samples = samples,
                cor = cor,
                time = time_list,
                family = family,
                data = list(cases = cases,
                            at_risk = at_risk,
                            prior = prior,
                            time = time_labels)
                )
    if (!missing(group)) out$group <- list(group = group_char, group.df = group.df)
    class(out) <- append("surveil", class(out))    
    return( out )    
}


