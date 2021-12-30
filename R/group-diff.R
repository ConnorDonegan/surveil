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
#'                iter = 900) # low iter for speed only
#' gd <- group_diff(fit, "Black or African American", "White")
#' print(gd, scale = 100e3)
#' plot(gd, scale = 100e3)
#' }
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
#' As the math communicates, the PAR is the rate difference expressed as a fraction of total risk for the target population. This could also be read as the fraction of risk in the target population that would have been removed had the target rate equaled the reference rate (Menvielle et al. 2017).
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
group_diff <- function(fit, target, reference) {
    stopifnot(inherits(fit, "surveil"))
    stopifnot(inherits(target, "character"))
    stopifnot(inherits(reference, "character"))
    stopifnot(all(c(target, reference) %in% names(fit$data$at_risk)))
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
        cols <- c("time", ".draw", "RD", "EC")
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
                aes(.data$label, .data$value,
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
    
