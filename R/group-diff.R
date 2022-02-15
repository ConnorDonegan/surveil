#' Measures of pairwise inequality
#'
#' @description Calculate pairwise measures of health inequality from a fitted `surveil` time series model, with credible intervals and MCMC samples. Calculates absolute and fractional rate differences (RD and population attributable risk (PAR)), rate ratios, and excess cases.
#'
#' @param x Either a fitted `surveil` time series model, or a list of two `stand_surveil` objects (i.e., `surveil` models with age-standardized rates, as returned by \code{\link[surveil]{standardize}}). If `x` is a list of `stand_surveil` objects, see details below and note that the models must contain the same number of MCMC samples---to ensure this is the case, when using `stan_rw` set `iter` and `chains` to the same values for each of the two models.
#'
#' @param target The name (character string) of the disadvantaged group that is the target of inference. If `x` is a list of `stand_surveil` objects, the `target` argument is ignored and the first listed model will serve as the `target` group.
#'
#' @param reference The name (character string) of the reference group to which `target` will be compared. If `x` is a list of `stand_surveil` objects, the `reference` argument is ignored and the second listed model will serve as the `reference` group.
#'
#' @return
#'
#' A list, also of class "surveil_diff", with the following elements: \describe{
#' \item{summary}{A tibble with a summary of posterior distributions (mean and 95 percent cred. intervals) for the target group incidence rate, the RD, RR, PAR, and excess cases.}
#' \item{cumulative_cases}{Summary of the posterior distribution for the cumulative number of excess cases and the PAR (mean and 95 percent cred. intervals)}
#' \item{groups}{Character string with target and reference population names}
#' \item{samples}{A data frame of MCMC samples for each quantity of interest (target and reference rates, RD, RR, PAR, and EC, as well as `Trend_Cases = Rate * Population`). Indexed by time.}
#' \item{cum_samples}{MCMC samples of the cumulative number of excess cases.}
#' }
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#'
#' @examples
#'
#' \donttest{
#' data(msa)
#' houston <- msa[grep("Houston", msa$MSA), ]
#' fit <- stan_rw(houston, time = Year, group = Race,
#'                chains = 2, iter = 900) # low iter for speed only
#' gd <- group_diff(fit, "Black or African American", "White")
#' print(gd, scale = 100e3)
#' plot(gd, scale = 100e3)
#' }
#' 
#' @details
#'
#' ## Comparing incidence rates
#' 
#' For the following calculations, the terms `reference` and `target` refer to incidence rates for the respective groups; `p` is the size of the target population. (Target is the group that is the 'target' of our inferences, so that it is the numerator in rate ratios, etc.) The following measures are calculated by `group_diff`:
#' 
#' ```
#' # rate difference
#' RD = target - reference
#' # population attributable fraction
#' PAR = RD/target = (RR - 1)/RR
#' # rate ratio
#' RR = target/reference
#' # excess cases
#' EC = RD * p
#' ```
#' As the math communicates, the PAR is the rate difference expressed as a fraction of total risk for the target population. This could also be read as the fraction of risk in the target population that would have been removed had the target rate equaled the reference rate (Menvielle et al. 2017).
#'
#' ## Comparing age-standardized rates
#' 
#' If the user provides a list of `stand_surveil` objects with age-standardized rates (instead of a single `surveil` model), then the exact calculations will be completed as follows. The RR is simply the ratio of age-standardized rates, and the rate difference is similarly the difference between age-standardized rates. However, excess cases is calculated for each age group separately, and the total excess cases across all age groups is returned. Similarly, the attributable risk is calculated by taking the total excess cases across all age groups per year and dividing by the total risk (i.e., by the sum of the whole number of cases across all age groups). Cumulative excess cases is the sum of the time-period specific total number of excess cases. (Notice that the PAR is not equal to (RR-1)/RR when the PAR is derived from a number of age-specific rates and the RR is based on age-standardized rates.)
#'
#' 
#' @source
#' 
#' Menvielle, G, Kulhanaova, I, Machenbach, JP. Assessing the impact of a public health intervention to reduce social inequalities in cancer. In: Vaccarella, S, Lortet-Tieulent, J, Saracci, R, Conway, D, Straif, K, Wild, CP, editors. Reducing Social Inequalities in Cancer: Evidence and Priorities for Research. Geneva, Switzerland: WHO Press, 2017:185-192.
#'
#' @seealso \code{\link[surveil]{plot.surveil_diff}} \code{\link[surveil]{print.surveil_diff}} \code{\link[surveil]{theil}}
#' 
#' @export
#' @md
#' 
group_diff <- function(x, target, reference) {
    UseMethod("group_diff", x)
}

#' group_diff.surveil
#' @importFrom ggdist mean_qi
#' @importFrom stats quantile
#' @importFrom dplyr group_by mutate `%>%`
#' @importFrom rstan extract
#' @md
#' @rdname group_diff
#' @method group_diff surveil
#' @export
group_diff.surveil <- function(x, target, reference) {
    stopifnot(inherits(target, "character"))
    stopifnot(inherits(reference, "character"))
    stopifnot(all(c(target, reference) %in% names(x$data$at_risk)))
    lambda = rstan::extract(x$samples, pars = "rate")$rate
    ref.idx <- which(names(x$data$at_risk) == reference)
    target.idx <- which(names(x$data$at_risk) == target)
    Time.index <- unique(x$summary$time)
    T <- 1:length(Time.index)
    res.list <- list()
    for (t in seq_along(T)) {
        ref.rate <- lambda[, ref.idx, t]        
        at.risk <- as.numeric(x$data$at_risk[t, target.idx])
        target.rate <- lambda[, target.idx, t]
        M <- length(target.rate)
        RD <- target.rate - ref.rate
        RR <- target.rate / ref.rate
        PAR <- RD / target.rate
        actual.cases <- target.rate * at.risk
        excess.cases <- RD * at.risk
        res <- data.frame(.draw = 1:M,
                          Rate = target.rate,
                          Reference_Rate = ref.rate,
                          RD = RD,
                          RR = RR,
                          PAR = PAR,
                          EC = excess.cases,
                          Trend_Cases = actual.cases
                          )
        res.list[[t]] <- res
        }
    res.df <- dplyr::bind_rows(res.list, .id = "time")
    res.df$time <- as.numeric(res.df$time)
    res.summary <- res.df %>%
        dplyr::group_by(.data$time) %>%
        ggdist::mean_qi(.data$Rate, .data$RD, .data$RR, .data$PAR, .data$EC) %>%
        dplyr::mutate(time = Time.index)
    ec.tmp <- lapply(res.list, function(l) matrix(l$EC, ncol = 1))
    ec.tmp <- do.call("cbind", ec.tmp)
    EC.cumulative.samples <- apply(ec.tmp, 1, sum)
    cases.tmp <- lapply(res.list, function(l) matrix(l$Trend_Cases, ncol = 1))
    cases.tmp <- do.call("cbind", cases.tmp)
    cumulative.cases.samples <- apply(cases.tmp, 1, sum)
    PAR.samples <- EC.cumulative.samples / cumulative.cases.samples
    EC.cumulative.summary <- c(EC = mean(EC.cumulative.samples), stats::quantile(EC.cumulative.samples, probs = c(0.025, 0.975)),
                               PAR = mean(PAR.samples), stats::quantile(PAR.samples, probs =c(0.025, 0.975)))
    return.list <- list(
        summary = res.summary,
        cumulative_cases = EC.cumulative.summary,
        samples = res.df,
        cum_samples = EC.cumulative.samples,        
        groups = c(target = target, reference = reference)
    )
    class(return.list) <- append("surveil_diff", class(return.list))
    return( return.list )
}


#' group_diff.list
#' @importFrom dplyr `%>%` inner_join transmute group_by summarise mutate
#' @importFrom stats quantile
#' @importFrom rstan extract
#' @md
#' @rdname group_diff
#' @method group_diff list
#' @export
group_diff.list <- function(x) {
    stopifnot( length(x) == 2 )
    stopifnot( all(unlist(lapply(x, inherits, "stand_surveil"))) )
    # check number of MCMC samples is same
    stopifnot( nrow(x[[1]]$standard_samples) ==  nrow(x[[2]]$standard_samples) )
    # check that age-group order is identical for both models
    stopifnot( all( names(x[[1]]$data$at_risk) == names(x[[2]]$data$at_risk) ) )
    # check that time label is the same for both models
    stopifnot( all( names(x[[1]]$time$time.label) == names(x[[2]]$time$time.label) ) )
    x.a <- x[[1]]
    x.b <- x[[2]]
    S.a <- x.a$standard_samples
    S.b <- x.b$standard_samples
    S <- dplyr::inner_join(S.a, S.b,
                    by = c(".draw", "time_index"),
                    suffix = c("_a", "_b")
                    )
    # get RD and RR from standardize rates (samples, then summary)
    S.df.1 <- S %>%
        dplyr::transmute(
                   .draw = .data$.draw,
                   time = .data$time_label_a,
                   Rate = .data$stand_rate_a,
                   Reference_Rate = .data$stand_rate_b,
                   RD.s = (.data$Rate - .data$Reference_Rate),
                   RR.s = .data$Rate / .data$Reference_Rate
        )    
    res.df.1 <- S.df.1 %>%
        dplyr::group_by(.data$time) %>%    
        dplyr::summarise(
                   Rate = mean(.data$Rate),
                   RD = mean(.data$RD.s),                
                   RD.lower = stats::quantile(.data$RD.s, probs = 0.025),
                   RD.upper = stats::quantile(.data$RD.s, probs = 0.975),
                   RR = mean(.data$RR.s),                
                   RR.lower = stats::quantile(.data$RR.s, probs = 0.025),
                   RR.upper = stats::quantile(.data$RR.s, probs = 0.975)
        )
    ## get EC and AR from age-specific rates (samples, then summary)
    groups <- names(x.a$data$at_risk)
    a.lambda = rstan::extract(x.a$samples, pars = "rate")$rate
    b.lambda = rstan::extract(x.b$samples, pars = "rate")$rate
    Time.label <- x.a$time$time.df$time.label
    T <- 1:length(Time.label)
    res.list <- list()
    for (t in seq_along(T)) {
        for (g in seq_along(groups)) {
            ref.rate <- b.lambda[, g, t]
            at.risk <- as.numeric(x.a$data$at_risk[t, g])
            target.rate <- a.lambda[, g, t]
            M <- length(target.rate)
            RD <- target.rate - ref.rate
            actual.cases <- target.rate * at.risk
            excess.cases <- RD * at.risk
            res <- data.frame(.draw = 1:M,
                              t = Time.label[t],
                              g = g,
                              EC = excess.cases,
                              AC = actual.cases
                              )
            res.list <- c(res.list, list(res))
        }
    }
    suppressMessages(    
        S.df.2 <- do.call("rbind", res.list)  %>%
            dplyr::mutate(time = .data$t) %>%
            dplyr::group_by(.data$time, .data$.draw) %>%
            dplyr::summarise(
                       AC.s = sum(.data$AC),
                       PAR.s = sum(.data$EC) / sum(.data$AC),
                       EC.s = sum(.data$EC)
                   )
    )
    suppressMessages(
        res.df.2 <- S.df.2 %>%
            dplyr::group_by(time) %>%
            dplyr::summarise(
                       EC = mean(.data$EC.s),        
                       EC.lower = stats::quantile(.data$EC.s, probs = 0.025),
                       EC.upper = stats::quantile(.data$EC.s, probs = 0.975),
                       PAR = mean(.data$PAR.s),
                       PAR.lower = stats::quantile(.data$PAR.s, probs = 0.025),
                       PAR.upper = stats::quantile(.data$PAR.s, probs = 0.975)        
                   ) 
        )
        ## main output: summary, samples
    summary.df <- dplyr::inner_join(res.df.1, res.df.2, by = "time") 
    samples.df <- dplyr::inner_join(S.df.1, S.df.2, by = c(".draw", "time")) %>%
        dplyr::transmute(
                   time = .data$time,
                   .draw = .data$.draw,
                   Rate = .data$Rate,
                   Reference_Rate = .data$Reference_Rate,
                   RD = .data$RD.s,
                   RR = .data$RR.s,
                   PAR = .data$PAR.s,
                   EC = .data$EC.s,
                   Trend_Cases = .data$AC.s        
               )
    ## aggregate excess cases and AR (samples, summary): summing across all years and age groups
    xs.cases.samples <- do.call("rbind", res.list) %>%
        dplyr::group_by(.data$.draw) %>%
        dplyr::summarise(
                   AC.s = sum(.data$AC),
                   EC.s = sum(.data$EC),                   
                   PAR.s = sum(.data$EC) / sum(.data$AC)
            ) %>%
        dplyr::ungroup()
    EC.cumulative.summary <- c(
        EC = mean(xs.cases.samples$EC.s),
        stats::quantile(xs.cases.samples$EC.s, probs = c(0.025, 0.975)),
        PAR = mean(xs.cases.samples$PAR.s),
        stats::quantile(xs.cases.samples$PAR.s, probs = c(0.025, 0.975))
        )
        

    if ( is.null(names(x)) ) names(x) <- c("x[[1]]", "x[[2]]")
    group_names <- c(target = names(x)[[1]], reference = names(x)[[2]])
    return.list <- list(
        summary = summary.df,
        cumulative_cases = EC.cumulative.summary,
        samples = samples.df,
        cum_samples = xs.cases.samples,
        groups = group_names
    )
    class(return.list) <- append("surveil_diff", class(return.list))    
    return ( return.list )
}


#' Methods for `surveil_diff` objects
#'
#' @param x Object of class `surveil_diff`, as returned by calling `group_diff` on a fitted `surveil` model
#' @param style If `style = "mean_qi"`, then the posterior mean and 95 percent credible interval will be plotted; if `style = "lines"`, then `M` samples from the joint probability distribution of the annual rates will be plotted.
#' @param M If `style = "lines"`, then `M` is the number of samples from the posterior distribution that will be plotted; the default is `M = 250`.
#' @param col Line color
#' @param fill Fill color for credible intervals, passed to `geom_ribbon`
#' @param plot If `plot = FALSE`, a list of `ggplot`s will be returned
#' @param scale Scale rates by this amount (`rate * scale`)
#' @param lwd Linewidth
#' @param alpha transparency; for `style = "mean_qi", controls the credible interval shading; for `style = "lines"`, this is applied to the lines
#' @param PAR Return population attributable risk? IF `FALSE`, then the rate ratio will be used instead of PAR.
#' @param ncol Number of columns for the plotting device. If `ncol = 1`, the three plots will be aligned vertically in one column; if `ncol = 3` they will b aligned horizontally in one row.
#' @param base_size Passed to `theme_classic` to control size of plot elements (e.g., text)
#' @param ... additional plot arguments passed to \code{\link[ggplot2]{theme}}
#'
#' @return
#'
#' ### plot.surveil_diff
#'
#' By default or whenever `plot = TRUE`, the plot method draws a series of plots to the current plotting device using \code{\link[gridExtra]{grid.arrange}}. If `plot = FALSE`, then a list of `ggplot`s is returned.
#' 
#' @method plot surveil_diff
#' @importFrom scales comma
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @name surveil_diff
#' @export
plot.surveil_diff <- function(x,
                              style = c("mean_qi", "lines"),
                              M = 250,                              
                              col = "black",
                              fill = "gray80",
                              lwd,
                              alpha,
                              plot = TRUE,
                              scale = 100e3,
                              PAR = TRUE,
                              ncol = 3,
                              base_size = 14,
                              ...) {
    style <- match.arg(style, c("mean_qi", "lines"))
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)
    message("Rate differences (RD) are per ", scales::comma(scale), " at risk")    
    if (style == "lines") {
        time_df <- data.frame(time = 1:nrow(x$summary),
                              label = x$summary$time)
        s_df <- x$samples
        s_df$RD <- s_df$RD * scale        
        TT <- max(s_df$time)
        n_samples <- nrow(s_df) / TT
        if (PAR) {
            cols <- c("time", ".draw", "RD", "PAR", "EC")
        } else {
            cols <- c("time", ".draw", "RD", "RR", "EC")
        }
        s_df <- s_df[,cols]        
        s_df <- tidyr::pivot_longer(s_df,
                                    -c(.data$time, .data$.draw),
                                    names_to = "measure",
                                    values_to = "value"
                                    )
        s_df <- arrange(s_df, .data$time)
        s_df <- left_join(s_df, time_df, by = "time")
        n_samples <- max(s_df$.draw)        
        s_df <- s_df[which(s_df$.draw %in% sample(n_samples, size = M)),]
        gg <- ggplot(s_df) +
            geom_line(
   #             aes(.data$label, .data$value,
                aes(.data$time, .data$value,
                    group = factor(.data$.draw)),
                lwd = lwd,
                col = col,
                alpha = alpha
            ) +
            facet_wrap(~ measure,
                       scales = "free",
                       ncol = ncol) +
            labs(x = NULL,
                 y = NULL) +
            theme_classic(base_size = base_size) +
            theme(...)
        return (gg)
    }    
    gg.ec <- ggplot(x$summary) +
        geom_ribbon(aes(.data$time,
                        ymin = .data$EC.lower,
                        ymax = .data$EC.upper),
                    fill = fill,
                    alpha = alpha) +
        geom_line(aes(.data$time,
                      .data$EC),
                  lwd = lwd,
                  col = col
                  ) +
        labs(
            x = NULL,
            y = NULL,
            subtitle = "EC"
        ) +
        #geom_hline(yintercept = 0) +
        theme_classic(base_size = base_size) +
        theme(...)
    if (PAR) {
        gg.relative <- ggplot(x$summary) +
            geom_ribbon(aes(.data$time,
                            ymin = .data$PAR.lower,
                            ymax = .data$PAR.upper),
                        fill = fill,
                        alpha = alpha) +
            geom_line(aes(.data$time, .data$PAR),
                      col = col,
                      lwd = lwd
                      ) +
            labs(
                x = NULL,
                y = NULL,
                subtitle = "PAR"
            ) +
            #geom_hline(yintercept = 0) +
            theme_classic(base_size = base_size) +
            theme(...)
    } else {
        gg.relative <- ggplot(x$summary) +
            geom_ribbon(aes(.data$time,
                            ymin = .data$RR.lower,
                            ymax = .data$RR.upper),
                        fill = fill,
                        alpha = alpha
                        ) +
            geom_line(aes(.data$time, .data$RR),
                      col = col,
                      lwd = lwd
                      ) +
            labs(
                x = NULL,
                y = NULL,
                subtitle = "RR"
            ) +
           # geom_hline(yintercept = 0) +            
            theme_classic(base_size = base_size) +
            theme(...)
    }
    f.lab <- function(brks) scale * brks
    gg.rd <- ggplot(x$summary) +
        geom_ribbon(aes(.data$time,
                        ymin = .data$RD.lower,
                        ymax = .data$RD.upper),
                    fill = fill,
                    alpha = alpha) +
        geom_line(aes(.data$time, .data$RD),
                  col = col,
                  lwd = lwd
                  ) +
        labs(
            x = NULL,
            y = NULL,
            subtitle = "RD"
        ) +
        #geom_hline(yintercept = 0) +        
        theme_classic(base_size = base_size) +
        scale_y_continuous(labels = f.lab) +
        theme(...)
    if (plot) {
        return ( gridExtra::grid.arrange(gg.rd,  gg.relative, gg.ec, ncol = ncol) )
    } else return (list(RD = gg.rd, Relative = gg.relative, EC = gg.ec))
}


#' print surveil_diff objects for analyses of inequality
#' @param scale Print rates and rate differences as per `scale` at risk, e.g., per 10,000 at risk.
#' @param ... additional print arguments
#'
#' @return
#'
#' ### print.surveil_diff
#'
#' The print method returns nothing and prints a summary of results to the console.
#' @method print surveil_diff
#' @importFrom scales comma percent
#' @name surveil_diff
#' @export
#' @md 
print.surveil_diff <- function(x, scale = 1, ...) {    
    message("Summary of Pairwise Inequality")    
    message("Target group: ", x$groups["target"])
    message("Reference group: ", x$groups["reference"])
    message("Time periods observed: ", length(x$summary$time))
    if (scale != 1) {
        message("Rate scale: per ", scales::comma(scale))
        x$summary$Rate <- x$summary$Rate * scale
        x$summary$RD <- x$summary$RD * scale
    }
    xs <- x$cumulative_cases
    xs[1:3] <-  round(xs[1:3])
    xs[4:6] <- round(xs[4:6], 2)
    message("Cumulative excess cases (EC): ", scales::comma(xs[1]), " [", xs[2], ", ", xs[3], "]")
    message("Cumulative EC as a fraction of group risk (PAR): ", xs["PAR"], " [", xs[5], ", ", xs[6], "]")
    x$summary$EC <- round(x$summary$EC)
    print(as.data.frame(x$summary)[,c("time", "Rate", "RD", "PAR", "RR", "EC")], digits = 2, row.names = FALSE)
}
    
