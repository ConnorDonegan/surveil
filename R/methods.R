#' Plot disease risk time trends
#'
#' @description Plotting method for `surveil` model results
#' @return a `ggplot` object
#' @seealso \code{\link[surveil]{stan_rw}}
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#'    data(msa)
#'    dfw <- msa[grep("Dallas", msa$MSA), ]
#'    fit <- stan_rw(dfw, time = Year, group = Race)
#'
#'   ## plot time-varying risk with 95\% credible intervals
#'  plot(fit)
#'  plot(fit, legend.text = element_text(size = 12))
#' 
#'  ## as a ggplot, you can customize the output
#' plot(fit) +
#'  scale_x_continuous(breaks = 1:19, 
#'                     labels = 1999:2017,
#'                     name = NULL
#'                     )
#' }
#' @param x A fitted `surveil` model
#' @param base_size Passed to `theme_classic()` to control size of plot components (text).
#' @param scale Scale the rates by this amount; e.g., `scale = 100e3` will print rates per 100,000 at risk.
#' @param ... additional arguments will be passed to `\code{\link[ggplot2]{theme}}
#' @export 
#' @import graphics
#' 
#'
#' @rdname surveil
#' @method plot surveil
#' @importFrom scales comma
#' @import ggplot2
#' @importFrom rlang parse_expr
plot.surveil <- function(x, scale = 1, base_size = 14, ...) {
    if (inherits(x$group, "list")) {
        group <- rlang::parse_expr(x$group$group)    
        gg <- ggplot(x$summary,
                     aes(group = {{ group }},
                         col = {{ group }} )
                     )
    } else {
        gg <- ggplot(x$summary)
    }
    if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
    gg +
        geom_ribbon(aes(.data$time,
                        ymin = scale * .data$lwr_2.5,
                        ymax = scale * .data$upr_97.5
                        ),
                    alpha = 0.5,
                    fill = 'gray80',
                    col = 'gray80',
                    lwd = 0
                    ) +
        geom_line(aes(.data$time, scale * .data$mean)) +
        geom_point(aes(.data$time, scale * .data$Crude),
                   alpha = 0.75
                   ) +
        scale_x_continuous(name = NULL) +
        scale_y_continuous(name = NULL) +
        scale_color_brewer(
            palette = "Dark2",
            type ="qual",
            name = NULL
        ) +     
        theme_classic(base_size = base_size) +
        theme(legend.position = "bottom",
              ...)
}

#' Print `surveil` model results
#'
#' @description Print a summary of `surveil` model results to the console
#'
#' @param x A fitted `surveil` model.
#' @param scale Scale the rates by this amount; e.g., `scale = 100e3` will print rates per 100,000 at risk.
#' @param ... additional arguments passed to \code{\link[base]{print.data.frame}}
#' @seealso \code{\link[surveil]{stan_rw}} \code{\link[surveil]{plot.surveil}}
#'
#' @method print surveil
#' @export
#' @rdname surveil
print.surveil <- function(x, scale = 1, ...) {    
    message("Summary of surveil model results")
    message("Time periods: ", nrow(x$data$cases))
    cols <- "time"
    if (inherits(x$group, "list")) {
        message("Grouping variable: ", x$group$group)
        cols <- c(cols, x$group$group)
        message("Correlation matrix: ", x$cor)
    }

    data.cols <- c("mean", "lwr_2.5", "upr_97.5")
    x$summary[,data.cols] <- x$summary[,data.cols] * scale
    cols <- c(cols, data.cols)
    print(x$summary[ , cols], ...)
}

#' WAIC
#'
#' @description Widely Application Information Criteria (WAIC) for model comparison
#' 
#' @param fit An \code{surveil} object or any Stan model with a parameter named "log_lik", the pointwise log likelihood of the observations.
#' @param pointwise Logical (defaults to `FALSE`); if `pointwide = TRUE`, a vector of values for each observation will be returned. 
#' @param digits Round results to this many digits.
#' 
#' @return A vector of length 3 with \code{WAIC}, a rough measure of the effective number of parameters estimated by the model \code{Eff_pars}, and log predictive density \code{Lpd}. If \code{pointwise = TRUE}, results are returned in a \code{data.frame}.
#'
#' @examples
#' 
#' \dontrun{
#' data(msa)
#' austin <- msa[grep("Austin", msa$MSA), ]
#' fit <- stan_rw(austin, time = Year, group = Race)
#' waic(fit)
#' }
#' @source
#'
#' Watanabe, S. (2010). Asymptotic equivalence of Bayes cross validation and widely application information criterion in singular learning theory. Journal of Machine Learning Research 11, 3571-3594.
#' @importFrom stats var
#' @export
waic <- function(fit, pointwise = FALSE, digits = 2) {
  ll <- as.matrix(fit, pars = "log_lik")
  nsamples <- nrow(ll)
  lpd <- apply(ll, 2, log_sum_exp) - log(nsamples)
  p_waic <- apply(ll, 2, var)
  waic <- -2 * (lpd - p_waic)
  if(pointwise) return(data.frame(waic = waic, eff_pars = p_waic, lpd = lpd))
  res <- c(WAIC = sum(waic), Eff_pars = sum(p_waic), Lpd = sum(lpd))
  return(round(res, digits))
}

#' Log sum of exponentials
#' @noRd
#' @details Code adapted from Richard McElreath's Rethinking package, and other sources.
#' 
log_sum_exp <- function(x) {
  xmax <- max(x)
  xsum <- sum( exp( x - xmax ) )
  xmax + log(xsum)
}
