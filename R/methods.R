#' Methods for fitted `surveil` models
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
#' @param facet Logical value to indicate how groups are differentiated. If \code{facet = TRUE}, \code{\link[ggplot2]{facet_wrap}} will be used instead of differentiating by line color.
#' @param palette For multiple groups, choose the color palette. For a list of options, see \code{\link[ggplot2]{scale_color_brewer}}. The default is `palette = "Dark2"`.
#' @param M If `style = "lines"`, then `M` is the number of samples from the posterior distribution that will be plotted; the default is `M = 250`.
#' @param alpha For `style = "lines"`; numeric value from zero to one indicating transparency of lines. Passed to \code{\link[ggplot2]{geom_line}}
#' @param lwd For `style = "lines"`; numeric value indicating linewidth. Passed to \code{\link[ggplot2]{geom_line}}
#' @param ... additional arguments will be passed to `\code{\link[ggplot2]{theme}}
#' 
#' @export 
#' @import graphics
#' @import ggplot2
#' @rdname surveil
#' @method plot surveil
plot.surveil <- function(x,
                         scale = 1,
                         style = c("mean_qi", "lines"),
                         facet = FALSE,
                         base_size = 14,
                         palette = "Dark2",
                         M = 250,
                         alpha = 0.7,
                         lwd = 0.05,
                         ...) {
    stopifnot(is.logical(facet))    
    style <- match.arg(style, c("mean_qi", "lines"))
    if (style == "lines") return(plot_lines(x, scale, facet, base_size, palette, M, alpha, lwd, ...))
    if (style == "mean_qi") return(plot_mean_qi(x, scale, facet, base_size, palette, ...))
}

#' @import ggplot2
#' @importFrom rlang parse_expr
plot_mean_qi <- function(x, scale, facet, base_size, palette, M, ...) {
    if (!inherits(x$group, "list")) {
        gg <- ggplot(x$summary)
    } else {
        group <- rlang::parse_expr(x$group$group)
        x$summary <- dplyr::mutate(x$summary,
                                   group = factor({{ group }}, levels = unique({{ group }}), ordered = TRUE))
        if (facet) {
            gform <- as.formula(paste0("~ ", x$group$group))
                gg <- ggplot(x$summary) +
                    facet_wrap(gform, scale = "free" )
            } else {            
                gg <- ggplot(x$summary,
                             aes(group = {{ group }},
                                 col = {{ group }} )
                             )
                if (length(x$group$group.df$group.index) > 8) {
                    warning("You have too many groups to use scale_color_brewer palettes; you may want to use facet = TRUE")
                } else {
                    gg <- gg +
                        scale_color_brewer(
                            palette = palette,
                            name = NULL
                        )
                }
            }
    } 
    if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
    gg <- gg +
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
                   alpha = 0.75,
                   size = 0.5
                   ) +
        scale_x_continuous(name = NULL) +
        scale_y_continuous(name = NULL) +
        theme_classic(base_size = base_size) +
        theme(legend.position = "bottom",
              ...)
    return (gg)

    }

#' @importFrom scales comma
#' @importFrom rstan extract
#' @importFrom dplyr %>% mutate left_join
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @noRd
plot_lines <- function(x, scale, facet, base_size, palette, M, alpha, lwd, ...) {
    eta <- rstan::extract(x$samples, pars = "rate")$rate
        K <- dim(eta)[2]
        TT <- dim(eta)[3]
        n_samples <- dim(eta)[1]
        draw_id_start <- 1
        draw_id_end <- M
        list_df <- list()
        for (j in 1:K) {    
            Sj <- eta[sample(n_samples, M),j,]
            Sj <- Sj %>%
                as.data.frame() %>% 
                tidyr::pivot_longer(
                           everything(),        
                           names_to = "time.index",
                           values_to = "rate"
                       ) %>%
                dplyr::mutate(group.index = j,
                              time.index = as.numeric(gsub("[A-z]", "", .data$time.index)),
                              draw = rep(draw_id_start:draw_id_end, each = TT)
                              )
            list_df[[j]] <- Sj    
            draw_id_start <- draw_id_end + 1
            draw_id_end <- M * (j + 1)
        }
        s_df <- do.call("rbind", list_df)
        if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
        s_df$rate <- s_df$rate * scale
        if (K == 1) {
            gg <- ggplot(s_df, aes(.data$time.index, .data$rate,
                                   group = factor(.data$draw))
                         ) +
                facet_wrap(~ .data$label, scale = "free" ) +
                geom_line(alpha = alpha, lwd = lwd) +
                scale_x_continuous(
                    breaks = x$time$time.df$time.index,
                    labels = x$time$time.df$label,
                    name = NULL
                ) +
                scale_y_continuous(name = NULL) +    
                theme_classic() +
                theme(...)
            return (gg)
        }        
        s_df <- dplyr::left_join(s_df, x$group$group.df, by = "group.index")
        s_df$label <- factor(s_df$label, ordered = TRUE, levels = unique(s_df$label))
        if (facet) {            
            gg <- ggplot(s_df, aes(.data$time.index, .data$rate,
                                   group = factor(draw))
                         ) +
                geom_line(alpha = alpha, lwd = lwd) +
            scale_x_continuous(
                breaks = x$time$time.df$time.index,
                labels = x$time$time.df$label,
                name = NULL
            ) +
                scale_y_continuous(name = NULL) +    
                theme_classic() +
                theme(...) +
                facet_wrap(~ label, scale = "free" )
            return(gg)
        }        
        gg <- ggplot(s_df, aes(.data$time.index, .data$rate,
                               group = factor(.data$draw),
                               col = .data$label)) +
            geom_line(alpha = alpha, lwd = lwd) +
            scale_x_continuous(
                breaks = x$time$time.df$time.index,
                labels = x$time$time.df$label,
                name = NULL
            ) +
            scale_y_continuous(name = NULL) +    
            theme_classic() +
            guides(color = guide_legend(override.aes = list(size = 2))) +
            theme(legend.position = "bottom",
                  ...)
        if (K < 8) {
            gg <- gg +
                scale_color_brewer(palette = palette,
                                   name = NULL)
        } else {
            warning("You have too many groups to use scale_color_brewer palettes; you may want to use facet = TRUE")
        }
        return (gg)
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
#' @return A list, also of class "stand_surveil", containing a summary data frame (mean and 95 percent credible intervals) (named `summary`), a data frame of MCMC samples of standardized rates per time period (named `samples`), and the user-provided labels and standard population sizes (named `label` and `standard_pop`). In addition, all of the items from the user-provided `surveil` model are automatically appended to the list.
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
#'               group = Age
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
    time_labels <- x$time$time.df$label
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


#' Methods for standardized rates
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



#' Widely Applicable Information Criteria
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


