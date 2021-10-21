#' Time series models for mortality and disease incidence 

#' @description Model time-varying disease risk given time series of case (or death) counts and population at risk.
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
#' @param prior Optionally provide a named `list` with prior parameters. If any of the following items are missing, default priors will be assigned and printed to the console.
#'
#' \describe{
#' \item{eta_1}{The first value of log-risk in each series must be assigned a Student's t prior probability distribution. Provide the degrees of freedom (df), location, and scale parameters for each demographic group in a list, where each parameter is a `k`-length vector.
#'
#' For example, with `k=2` demographic groups, the following code will assign priors of `student_t(10, -5, 5)` to the starting values of both series: `prior = list(eta_1 = student_t(df = c(10, 10), location = c(-6.5, -6.5), scale = c(5, 5))`. Note, `eta` is the log-rate, so centering the prior for `eta_1` on `-5` is equivalent to centering the prior rate on `exp(-6.5)*100,000 = 150` cases per 100,000 person-years at risk.}
#'
#' \item{sigma}{Each demographic group has a scale parameter assigned to its log-rate. This is the scale of the annual deviations from the previous year's log-rate. The scale parameters are assigned independent half-Student's t prior distributions (these `half` t distributions are restricted to be positive-valued only).}
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
#' @examples
#' \dontrun{
#' data(msa)
#' dfw <- msa[grep("Dallas", msa$MSA), ]
#' fit <- stan_rw(dfw, time = Year, group = Race)
#'
#' print(fit)
#' head(fit$summary)
#' 
#'  plot(fit)
#'
#' # Inequality analysis
#' Ti <- theil(fit)
#' plot(Ti)
#' print(Ti)
#' 
#' gd <- group_diff(fit, target = "Black or African American", reference = "White")
#' plot(gd)
#' print(gd, scale = 10e3)
#'
#' # age-specific rates and cumulative percent change
#' data(cancer)
#' fit <- stan_rw(cancer,
#'                time = Year,
#'                group = Age
#'                )
#' fit_apc <- apc(fit)
#' plot(fit_apc, cumulative = TRUE)
#'
#' # age-standardized rates
#' data(standard)
#' fit_stands <- standardize(fit, label = standard$age, standard_pop = standard$standard_pop)
#' print(fit_stands)
#' plot(fit_stands)
#' fit_stands_apc <- apc(fit_stands)
#' plot(fit_stands_apc)
#' }
#' @export
#' @importFrom parallel detectCores
#' @importFrom dplyr `%>%` mutate arrange full_join distinct select group_by ungroup n select
#' @importFrom tidyr pivot_wider
#' @importFrom stats quantile
#' @md
stan_rw <- function(data,
                    group,
                    time,
                    cor = FALSE,
                    prior = list(),
                    chains = 4,
                    cores = 1,
                    iter = 3e3,
                    refresh = 1500,
                    control = list(adapt_delta = 0.975),
                    ...
                    ) {
    stopifnot(inherits(data, "data.frame"))
    stopifnot(all(c("Count", "Population") %in% names(data)))
    stopifnot(rlang::as_label(dplyr::enquo(time)) %in% names(data))
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
                          label = time_labels)
    time_list <- list(time = rlang::as_label(dplyr::enquo(time)),
                      time.df = time_df)
    if (missing(group)) {
        cases <- as.matrix(data$Count, ncol = 1)
        at_risk <- as.matrix(data$Population, ncol = 1)
        group.ids <- group.labels <- "1"
        group.df <- data.frame(group = group.ids, label = group.labels)
        
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
        group.df <- data.frame(group.index = group.ids, label = group.labels)            
    }
    ## fill in missing priors
    if (!"eta_1" %in% names(prior)) {   
        prior$eta_1 <- student_t(
            df = rep(20, ncol(cases)),
            location = rep(-5, ncol(cases)),
            scale = rep(10, ncol(cases))
        )
        print("Setting Student t prior(s) for eta_1: ")
                print(student_t(20, -5, 10))                
    }    
    if (!"sigma" %in% names(prior)) {
        prior$sigma <- student_t(df = rep(20, times = ncol(cases)),
                            location = rep(0, times = ncol(cases)), 
                            scale = rep(1, times = ncol(cases))
                            )
        print("\nSetting half-Student t prior for sigma: ")
        print(student_t(20, 0, 1))
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
        prior_eta_1_df = as.array(prior$eta_1$df),
        prior_eta_1_location = as.array(prior$eta_1$location),
        prior_eta_1_scale = as.array(prior$eta_1$scale),
        prior_sigma_df = as.array(prior$sigma$df),
        prior_sigma_location = as.array(prior$sigma$location),
        prior_sigma_scale = as.array(prior$sigma$scale),
        prior_omega = prior$omega$eta
    )                
    if (cor) {
        samples <- rstan::sampling(stanmodels$poissonRWCorr,
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
        samples <- rstan::sampling(stanmodels$poissonRW,
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
                data = list(cases = cases,
                            at_risk = at_risk,
                            prior = prior,
                            time = time_labels)
                )
    if (!missing(group)) out$group <- list(group = group_char, group.df = group.df)
    class(out) <- append("surveil", class(out))    
    return( out )    
}


