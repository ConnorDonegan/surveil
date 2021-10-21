#' Measures of pairwise inequality
#'
#' @description Calculate pairwise measures of health inequality from a fitted `surveil` time series model, with credible intervals and MCMC samples. Calculates absolute and fractional rate differences (RD and population attributable risk (PAR)), rate ratios, and excess cases.
#'
#' @param fit A fitted `surveil` time series model
#'
#' @param target The name (character string) of the disadvantaged group that is the target of inference. 
#'
#' @param reference The name (character string) of the reference group to which `target` will be compared.
#'
#' @param samples If `TRUE`, return MCMC samples for the inequality measures. The default is `FALSE`.
#'
#' @return
#'
#' A list, also of class "surveil_diff", with the following elements: \describe{
#' \item{summary}{A tibble with a summary of posterior distributions (mean and 95 percent cred. intervals) for the target group incidence rate, the RD, RR, PAR, and excess cases.}
#' \item{cumulative_cases}{Summary of the posterior distribution for the cumulative number of excess cases and the PAR (mean and 95 percent cred. intervals)}
#' \item{groups}{Character string with target and reference population names}
#' }
#' If `samples = TRUE`, then, in addition to the above items, a list called `samples` is returned; it contains:
#' \describe{
#' \item{annual}{A data frame of MCMC samples for each quantity of interest (target and reference rates, RD, RR, PAR, and EC, as well as `Trend_Cases = Rate * Population`. Indexed by time.}
#' \item{cumulative_cases}{MCMC samples of the cumulative number of excess cases.}
#' }
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#'
#' @details
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
#' As the math concisely communicates, the PAR is defined as the fraction of risk in the target population that would have been removed had the target rate equaled the reference rate (Menvielle et al. 2017).
#' 
#' @source
#' 
#' Menvielle, G, Kulhanaova, I, Machenbach, JP. Assessing the impact of a public health intervention to reduce social inequalities in cancer. In: Vaccarella, S, Lortet-Tieulent, J, Saracci, R, Conway, D, Straif, K, Wild, CP, editors. Reducing Social Inequalities in Cancer: Evidence and Priorities for Research. Geneva, Switzerland: WHO Press, 2017:185-192.
#'
#' @seealso \code{\link[surveil]{plot.surveil_diff}} \code{\link[surveil]{print.surveil_diff}} \code{\link[surveil]{theil}}
#' 
#' @importFrom ggdist mean_qi
#' @importFrom stats quantile
#' @importFrom dplyr group_by mutate `%>%`
#' @export
#' @md
#' 
group_diff <- function(fit, target, reference, samples = FALSE) {
    stopifnot(inherits(fit, "surveil"))
    stopifnot(inherits(target, "character"))
    stopifnot(inherits(reference, "character"))
    stopifnot(all(c(target, reference) %in% names(fit$data$at_risk)))
    stopifnot(inherits(as.logical(samples), "logical"))
    lambda = rstan::extract(fit$samples, pars = "rate")$rate
    ref.idx <- which(names(fit$data$at_risk) == reference)
    target.idx <- which(names(fit$data$at_risk) == target)
    Time.index <- unique(fit$summary$time)
    T <- 1:length(Time.index)
    res.list <- list()
    for (t in seq_along(T)) {
        ref.rate <- lambda[, ref.idx, t]        
        at.risk <- as.numeric(fit$data$at_risk[t, target.idx])
        target.rate <- lambda[, target.idx, t]
        RD <- target.rate - ref.rate
        RR <- target.rate / ref.rate
        PAR <- RD / target.rate
        actual.cases <- target.rate * at.risk
        excess.cases <- RD * at.risk
        res <- data.frame(Rate = target.rate,
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
        groups = c(target = target, reference = reference)
    )
    if (samples) return.list$samples <- list(annual = res.df, cumulative_cases = EC.cumulative.samples)
    class(return.list) <- append("surveil_diff", class(return.list))
    return( return.list )
}

#' Methods for `surveil_diff` objects
#'
#' @param x Object of class `surveil_diff`, as returned by calling `group_diff` on a fitted `surveil` model
#' @param col Line color
#' @param fill Fill color for credible intervals, passed to `geom_ribbon`
#' @param plot If `plot = FALSE`, a list of `ggplot`s will be returned
#' @param scale Scale rates by this amount (`rate * scale`)
#' @param PAR Return population attributable risk? IF `FALSE`, then the rate ratio will be used instead of PAR.
#' @param base_size Passed to `theme_classic` to control size of plot elements (e.g., text)
#' @param ... additional plot arguments passed to \code{\link[ggplot2]{theme}}
#' @method plot surveil_diff
#' @importFrom scales comma
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @name surveil_diff
#' @export
plot.surveil_diff <- function(x, col = "black", fill = "gray80", plot = TRUE, scale = 100e3, PAR = TRUE, base_size = 14, ...) {
    gg.ec <- ggplot(x$summary) +
        geom_ribbon(aes(.data$time, ymin = .data$EC.lower, ymax = .data$EC.upper),
                    fill = fill,
                    alpha = 0.5) +
        geom_line(aes(.data$time, .data$EC), lwd = 1,
                  col = col
                  ) +
        labs(
            x = NULL,
            y = "EC"
        ) +
        geom_hline(yintercept = 0) +
        theme_classic(base_size = base_size) +
        theme(...)
    if (PAR) {
        gg.relative <- ggplot(x$summary) +
            geom_ribbon(aes(.data$time, ymin = .data$PAR.lower, ymax = .data$PAR.upper),
                        fill = fill,
                        alpha = 0.5) +
            geom_line(aes(.data$time, .data$PAR),
                      col = col,
                      lwd = 1
                      ) +
            labs(
                x = NULL,
                y = "PAR"
            ) +
            geom_hline(yintercept = 0) +
            theme_classic(base_size = base_size) +
            theme(...)
    } else {
        gg.relative <- ggplot(x$summary) +
            geom_ribbon(aes(.data$time, ymin = .data$RR.lower, ymax = .data$RR.upper),
                        fill = fill,
                        alpha = 0.5) +
            geom_line(aes(.data$time, .data$RR),
                      col = col,
                      lwd = 1
                      ) +
            labs(
                x = NULL,
                y = "RR"
            ) +
            geom_hline(yintercept = 0) +            
            theme_classic(base_size = base_size) +
            theme(...)
    }
    f.lab <- function(brks) scale * brks
    gg.rd <- ggplot(x$summary) +
        geom_ribbon(aes(.data$time, ymin = .data$RD.lower, ymax = .data$RD.upper),
                    fill = fill,
                    alpha = 0.5) +
        geom_line(aes(.data$time, .data$RD),
                  col = col,
                  lwd = 1
                  ) +
        labs(
            x = NULL,
            y = "RD"
        ) +
        geom_hline(yintercept = 0) +        
        theme_classic(base_size = base_size) +
        scale_y_continuous(labels = f.lab) +
        theme(...)
    message("Rate differences (RD) are per ", scales::comma(scale), " at-risk")    
    if (plot) {
        return ( gridExtra::grid.arrange(gg.rd,  gg.relative, gg.ec, ncol = 1) )
    } else return (list(RD = gg.rd, Relative = gg.relative, EC = gg.ec))
}


#' print surveil_diff objects for analyses of inequality
#' @param scale Print rates and rate differences as per `scale` at risk, e.g., per 10,000 at risk.
#' @param ... additional print arguments
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
    
