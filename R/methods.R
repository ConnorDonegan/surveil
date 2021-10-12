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
#' 
#' @export 
#' @import graphics
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


#' Standardized rates
#' @description Convert `surveil` model results to age standardized rates using a fixed age distribution
#'
#' @param x A fitted `surveil` model
#' @param label Labels (character strings) for the age groups that correspond to the values of `stand_pop`. The labels must match the grouping variable used to fit the model (i.e., `all(label %in% names(x$data$cases))` must be true).
#' @param standard_pop Standard population values corresponding to the age groups specified by `label`
#'
#' @return A list, also of class "stand_surveil", containing a summary data frame (mean and 95 percent credible intervals) (`summary`), a data frame of MCMC samples of standardized rates per time period (`samples`), and the user-provided labels and standard population sizes (`label`, `standard_pop`).
#' 
#' @examples
#' data(cancer)
#' data(standard)
#' 
#' head(standard)
#' head(cancer)
#'
#' \dontrun{
#' fit <- stan_rw(cancer,
#'               time = Year,
#'               group = Label
#'               )
#'
#' stands <- standardize(fit, label = standard$age, standard_pop = standard$standard_pop)
#' print(stands)
#' plot(stands)
#' }
#' @md
#' @export
#' @importFrom dplyr `%>%` left_join select group_by summarise
#' @importFrom tidybayes gather_draws
#' @importFrom ggdist mean_qi
standardize <- function(x, label, standard_pop) {
    stopifnot(all(label %in% names(x$data$cases)))
    rate <- group_index <- time_index <- NULL # global bindings
    ids <- 1:ncol(x$data$cases)
    # proper order
    stand_df <- data.frame(label=label, standard_pop = standard_pop)
    id_df <- data.frame(group_index = ids, label = names(x$data$cases))
    stand_df <- dplyr::left_join(id_df, stand_df, by = "label")
    # time period labels
    time_labels <- x$data$time
    time_df <- data.frame(time_index = 1:length(time_labels), time_label = time_labels)
    # samples of standardized rates
    stand_samples <- x$samples %>%
        tidybayes::gather_draws(rate[group_index, time_index]) %>%
        dplyr::select(.data$group_index, .data$time_index, .data$.draw, .data$.value) %>%
        dplyr::left_join(stand_df, by = c("group_index")) %>%
        dplyr::group_by(.data$.draw, .data$time_index) %>%
        dplyr::summarise(stand_rate = standardize_rate(.data$.value, .data$standard_pop))
    # summary of marginal posterior distributions
    stand_summary <- stand_samples %>%
        dplyr::group_by(.data$time_index) %>%
        ggdist::mean_qi(.data$stand_rate) %>%
        dplyr::left_join(time_df, by ="time_index")
    res.list <- list(standard_summary = stand_summary,
                     standard_samples = stand_samples,
                     standard_label = label,
                     standard_pop = standard_pop)
    res.list <- c(res.list, x)
    class(res.list) <- append("stand_surveil", class(res.list))
    return( res.list )
}

standardize_rate <- function(rate, stand_pop) sum(rate * stand_pop) / sum(stand_pop)


#' stand_surveil methods
#'
#' @description Print and plot methods for `stand_surveil` (standardized rates obtained from a fitted `surveil` model)
#'
#' @param x An object of `stand_surveil` obtained by calling \code{\link[surveil]{standardize}} on a fitted `surveil` model
#' @param scale Scale the rates by this amount; e.g., `scale = 100e3` will print rates per 100,000 at risk.
#' @param digits Number of digits to print
#' @param ... additional arguments
#' @details
#' 
#' ### print.stand_surveil
#'
#' Any additional arguments (`...`) will be  passed to \code{\link[base]{print.data.frame}}
#' 
#' @seealso \code{\link[surveil]{standardize}} \code{\link[surveil]{stan_rw}}
#' 
#' @importFrom scales comma
#' @method print stand_surveil
#' @export
#' @md
#' @rdname stand_surveil
print.stand_surveil <- function(x, scale = 1, digits = 3, ...) {    
    message("Summary of age-standardized surveil model results")
    if (scale != 1) message("As rate per ", scales::comma(scale), " at risk")
    message("Time periods: ", nrow(x$standard_summary))
    cols <- c("stand_rate", ".lower", ".upper")
    x$standard_summary[,cols] <- x$standard_summary[,cols] * scale
    print.data.frame(x$standard_summary[ , c("time_label", cols)], row.names = FALSE, digits = digits, ...)
}


#' 
#' @param base_size Passed to `theme_classic()` to control size of plot components (text).
#' @param col Line color
#' @param fill Fill color for the 95 percent credible intervals
#'
#' @details
#' 
#' ### plot.stand_surveil
#' 
#' Any additional arguments (`...`) will be passed to `\code{\link[ggplot2]{theme}}.
#'
#' @importFrom scales comma
#' @import ggplot2
#' @import graphics
#' @export
#' @md
#' @rdname stand_surveil
#' @method plot stand_surveil
#' 
plot.stand_surveil <- function(x, scale = 1, base_size = 14, col = 'black', fill = 'gray80', ...) {
    if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
    ggplot(x$standard_summary) +
        geom_ribbon(aes(.data$time_label,
                        ymin = scale * .data$.lower,
                        ymax = scale * .data$.upper
                        ),
                    alpha = 0.5,
                    fill = fill,
                    col = fill,
                    lwd = 0
                    ) +
        geom_line(aes(.data$time_label, scale * .data$stand_rate),
                  col = col) +
        scale_x_continuous(name = NULL) +
        scale_y_continuous(name = NULL) +
        theme_classic(base_size = base_size) +
        theme(...)
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
  ll <- as.matrix(fit$samples, pars = "log_lik")
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


