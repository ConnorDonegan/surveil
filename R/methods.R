#' Methods for fitted `surveil` models
#'
#' @description Print and plot methods for `surveil` model results
#' 
#' @return The plot method returns a `ggplot` object; the print method returns nothing but prints a summary of results to the R console. If `x` is a list of `stand_surveil` objects, the plotted lines will be labeled using the names returned by `names(x)`; if elements of the list are not named, plotted lines will simply be numbered.
#' 
#' @seealso \code{\link[surveil]{stan_rw}}
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#' @examples
#'
#' \donttest{
#' data(msa)
#' houston <- msa[grep("Houston", msa$MSA), ]
#' fit <- stan_rw(houston, time = Year, group = Race,
#'               chains = 2, iter = 900) # for speed only
#'
#' print(fit)
#' 
#' ## plot probability distribution for disease risk
#' plot(fit, style = "lines")
#' plot(fit, facet = TRUE, scale = 100e3)
#' 
#'  ## as a ggplot, you can customize the output
#' library(ggplot2)
#' plot(fit) + theme_bw()
#' }
#' 
#' @param x A fitted `surveil` model, or a list of `stand_surveil` objects (as produced by \code{\link[surveil]{standardize}}). 
#' @param base_size Passed to `theme_classic()` to control size of plot components (text).
#' @param scale Scale the rates by this amount; e.g., `scale = 100e3` will print rates per 100,000 at risk.
#' @param style If `style = "mean_qi"`, then the posterior mean and 95 percent credible interval will be plotted; if `style = "lines"`, then `M` samples from the joint probability distribution of the annual rates will be plotted.
#' @param M If `style = "lines"`, then `M` is the number of samples from the posterior distribution that will be plotted; the default is `M = 250`.
#' @param facet If \code{facet = TRUE}, \code{\link[ggplot2]{facet_wrap}} will be used instead of differentiating by line color.
#' @param facet_scales When \code{facet = TRUE}, this argument controls behavior of the scales for each sub-plot. See the `scales` argument to \code{\link[ggplot2]{facet_wrap}}.
#' @param ncol Number of columns for the plotting device; optional and only used if `facet = TRUE`. If `ncol = 1`, the three plots will be aligned vertically in one column; if `ncol = 3` they will b aligned horizontally in one row. Defaults to `ncol = NULL` to allow \code{\link[ggplot2]{facet_wrap}} to automatically determine the number of columns.
#' @param palette For multiple groups, choose the color palette. For a list of options, see \code{\link[ggplot2]{scale_color_brewer}}. The default is `palette = "Dark2"`. Not used if `facet = TRUE`.
#' 
#' @param alpha Numeric value from zero to one. When `style = "lines"`,  this controls transparency of lines; passed to \code{\link[ggplot2]{geom_line}}. For `style = "mean_qi", this controls the transparency of the shaded credible interval; passed to \code{\link[ggplot2]{geom_ribbon}}.
#' @param lwd Numeric value indicating linewidth. Passed to \code{\link[ggplot2]{geom_line}}
#' @param fill Color for the shaded credible intervals; only used when `style = "mean_qi"`.
#' @param size Positive numeric value. For `style = "mean_qi"`, this controls the size of the points representing crude rates. To exclude these points from the plot altogether, use `size = 0`.
#' @param ... For the plot method, additional arguments will be passed to `\code{\link[ggplot2]{theme}}; for the print method, additional arguments will be passed to \code{\link[base]{print.data.frame}}.
#' 
#' @name plot.surveil
NULL

#' @rdname plot.surveil
#' @method print surveil
#' @export
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

#' @rdname plot.surveil
#' @method plot surveil
#' @import graphics
#' @import ggplot2
#' @export
plot.surveil <- function(x,
                         scale = 1,
                         style = c("mean_qi", "lines"),
                         facet = FALSE,
                         facet_scales = c("fixed", "free"),
                         ncol = NULL,
                         base_size = 14,
                         palette = "Dark2",
                         M = 250,
                         alpha,
                         lwd,
                         fill = "gray80",
                         size = 1.5,
                         ...) {
    stopifnot(is.logical(facet))    
    style <- match.arg(style, c("mean_qi", "lines"))
    facet_scales <- match.arg(facet_scales, c("fixed", "free"))    
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)
    if (style == "lines") return(plot_lines(x=x,
                                            scale=scale,
                                            facet=facet,
                                            ncol=ncol,                                            
                                            facet_scales=facet_scales,
                                            base_size=base_size,
                                            palette=palette,
                                            M=M,
                                            alpha=alpha,
                                            lwd=lwd,
                                            ...))
    if (style == "mean_qi") return(plot_mean_qi(x=x,
                                                scale=scale,
                                                facet=facet,
                                                ncol=ncol,                                                
                                                facet_scales=facet_scales,                                                
                                                base_size=base_size,
                                                palette=palette,
                                                alpha=alpha,
                                                lwd=lwd,
                                                fill=fill,
                                                size=size,
                                                ...))
}

#' @import ggplot2
#' @importFrom rlang parse_expr
#' @importFrom stats as.formula
plot_mean_qi <- function(x,
                         scale,
                         facet,
                         ncol,
                         facet_scales,                         
                         base_size,
                         palette,
                         alpha,
                         lwd,
                         fill,
                         size,
                         ...) {
    if (!inherits(x$group, "list")) {
        gg <- ggplot(x$summary)
    } else {
        group <- rlang::parse_expr(x$group$group)
        x$summary <- dplyr::mutate(x$summary,
                                   group = factor({{ group }}, levels = unique({{ group }}), ordered = TRUE))
        if (facet) {
                gg <- ggplot(x$summary) +
                    facet_wrap(~ group, #gform,
                               scales = facet_scales,
                               ncol = ncol)
            } else {            
                gg <- ggplot(x$summary,
                             aes(group = group, 
                                 col = group)   
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
                    alpha = alpha,
                    fill = fill,
                    lwd = 0
                    ) +
        geom_line(aes(.data$time, scale * .data$mean),
                  lwd = lwd) +
        labs(x = NULL,
             y = NULL) +
        theme_surveil(base_size = base_size, ...)
    if (size) {
        gg <- gg +
            geom_point(aes(.data$time, scale * .data$Crude),
                       alpha = 0.75,
                       size = size
                       )
    }
    return (gg)

    }

#' @importFrom scales comma
#' @importFrom rstan extract
#' @importFrom dplyr %>% mutate left_join
#' @importFrom tidyr pivot_longer everything
#' @import ggplot2
#' @noRd
plot_lines <- function(x,
                       scale,
                       facet,
                       ncol,
                       facet_scales,                       
                       base_size,
                       palette,
                       M,
                       alpha,
                       lwd,
                       ...) {    
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
                       tidyr::everything(),        
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
    s_df <- dplyr::left_join(s_df, x$time$time.df, by = "time.index")
    if (K == 1) {
        gg <- ggplot(s_df, aes(.data$time.label, .data$rate,
                               group = factor(.data$draw))
                     ) +
            geom_line(alpha = alpha, lwd = lwd) +
            labs(y = NULL,
                 x = NULL) +
            theme_surveil(base_size = base_size, ...)
        return (gg)
    }        
    s_df <- dplyr::left_join(s_df, x$group$group.df, by = "group.index")
    s_df$group.label <- factor(s_df$group.label, ordered = TRUE, levels = unique(s_df$group.label))
    if (facet) {            
        gg <- ggplot(s_df, aes(.data$time.label, .data$rate,
                               group = factor(.data$draw))
                         ) +
            geom_line(alpha = alpha,
                      lwd = lwd) +
            labs(x = NULL,
                 y = NULL) +
            facet_wrap(~ group.label,
                       scales = facet_scales,
                       ncol = ncol) +
            theme_surveil(base_size = base_size, ...)             
        return(gg)
    }        
    gg <- ggplot(s_df, aes(.data$time.label, .data$rate,
                           group = factor(.data$draw),
                           col = .data$group.label)) +
        geom_line(alpha = alpha,
                  lwd = lwd) +
        labs(x = NULL,
             y = NULL) +
        guides(color = guide_legend(override.aes = list(size = 2))) +
        theme_surveil(base_size = base_size, ...) 
    if (K < 8) {
        gg <- gg +
            scale_color_brewer(palette = palette,
                               name = NULL)
    } else {
        warning("You have too many groups to use scale_color_brewer palettes; you may want to use facet = TRUE")
    }
        return (gg)
}


#' @importFrom dplyr bind_rows left_join filter
#' @importFrom scales comma
#' @import ggplot2
#' @import graphics
#' @rdname plot.surveil
#' @method plot list
#' @export
plot.list <- function(x,
                      scale = 1,
                      style = c("mean_qi", "lines"),
                      facet = FALSE,
                      ncol,
                      facet_scales = c("fixed", "free"),
                      M = 250,
                      base_size = 14,
                      palette = "Dark2",
                      fill = 'gray80',
                      size = 1.5,
                      alpha,
                      lwd,
                      ...) { 
    stopifnot( all(unlist(lapply(x, inherits, "stand_surveil"))) )
    stopifnot(is.logical(facet))     
    style <- match.arg(style, c("mean_qi", "lines"))
    facet_scales <- match.arg(facet_scales, c("fixed", "free"))
    if (missing(ncol)) ncol <- length(x)
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)
    if (scale != 1) message("Plotted rates are per ", scales::comma(scale)) 
    if (style == "mean_qi") { 
        x2 <- lapply(x, function(x) x$standard_summary)
        df <- dplyr::bind_rows(x2, .id = "Group")
        time.df <- x[[1]]$time$time.df
        time.df$time_index <- time.df$time.index
        df <- dplyr::left_join(df, time.df, by = "time_index")
        if (facet) {
            gg <- ggplot(df) +
                facet_wrap(~ .data$Group,
                           scales = facet_scales,
                           ncol = ncol)            
        } else {            
                gg <- ggplot(df,
                             aes(group = .data$Group,
                                 col = .data$Group )
                             )
                if (length(unique(df$Group)) > 8) {
                    warning("You have too many groups to use scale_color_brewer palettes; you may want to use facet = TRUE")
                } else {
                    gg <- gg +
                        scale_color_brewer(
                            palette = palette,
                            name = NULL
                        )
                }
        }    
        if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
        gg <- gg +
            geom_ribbon(aes(.data$time_label,
                            ymin = scale * .data$.lower,
                            ymax = scale * .data$.upper
                            ),
                        alpha = alpha,
                        fill = fill,
                        lwd = 0
                    ) +
            geom_line(aes(.data$time_label,
                          scale * .data$stand_rate),
                      lwd = lwd) +
            labs(x = NULL,
                 y = NULL) +
            theme_surveil(base_size = base_size, ...)
    return (gg)
}
        x2 <- lapply(x, function(l) dplyr::filter(l$standard_samples, .data$.draw <= M))        
        df <- dplyr::bind_rows(x2, .id = "Group")
        df$Group <- factor(df$Group, ordered = TRUE, levels = names(x))
        if (facet) {
            gg <- ggplot(df, aes(x = .data$time_label,
                                 y = scale * .data$stand_rate,
                                 group = factor(paste(.data$.draw, .data$Group))
                                 )
                         ) +
                geom_line(alpha = alpha,
                          lwd = lwd
                          ) +
                labs(x = NULL,
                     y = NULL) +
                facet_wrap(~ .data$Group,
                           scales = facet_scales,
                           ncol = ncol
                           ) +                 
                theme_surveil(base_size = base_size, ...)
            return (gg)
        }
        gg <- ggplot(df, aes(x = .data$time_label,
                       y = scale * .data$stand_rate,
                       group = factor(paste(.data$.draw, .data$Group))
                       )
                     ) +
            geom_line(aes(col = .data$Group),
                      alpha = alpha,
                      lwd = lwd) +
            labs(x = NULL,
                 y = NULL) +
            guides(color = guide_legend(override.aes = list(size = 2))) +
            theme_surveil(base_size = base_size, ...)            
    K <- length(unique(df$Group))
    if (K < 8) {
        gg <- gg +
            scale_color_brewer(palette = palette,
                               name = NULL)
    } else {
        warning("You have too many groups to use scale_color_brewer palettes; you may want to use facet = TRUE")
    }    
    return (gg)
}


#' Age-standardized rates
#' @description Convert `surveil` model results to age standardized rates using a fixed age distribution
#'
#' @param x A fitted `surveil` model
#' @param label Labels (character strings) for the age groups that correspond to the values of `stand_pop`. The labels must match the grouping variable used to fit the model (i.e., `all(label %in% names(x$data$cases))` must be true).
#' @param standard_pop Standard population values corresponding to the age groups specified by `label`
#'
#' @return A list, also of class "stand_surveil", containing the entire contents of the user-provided `surveil` model plus the following:
#' \describe{
#'  \item{standard_summary}{summary data frame of standardized rates (means and 95 percent credible intervals)}
#'
#' \item{standard_samples}{a data frame of Markov chain Monte Carlo (MCMC) samples from the posterior probability distribution for the standardized rates}
#'
#' \item{standard_label}{user-provided age-group labels}
#'
#' \item{standard_pop}{user-provided standardized population sizes (ordered as `standard_label`)}
#' }
#'
#' 
#' @examples
#' data(cancer)
#' data(standard)
#' 
#' head(standard)
#' head(cancer)
#'
#' \donttest{
#' fit <- stan_rw(cancer,
#'               time = Year,
#'               group = Age,
#'               chains = 2, iter = 900 # for speed only
#'               )
#'
#' stands <- standardize(fit,
#'                       label = standard$age,
#'                       standard_pop = standard$standard_pop)
#' print(stands)
#' plot(stands, style = "lines")
#' }
#' @seealso \code{vignette("age-standardization", package = "surveil")} \code{\link[surveil]{stan_rw}}  \code{\link[surveil]{plot.stand_surveil}} \code{\link[surveil]{print.stand_surveil}} 
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
    stand_df <- data.frame(label = label, standard_pop = standard_pop)
    id_df <- data.frame(group_index = ids, label = names(x$data$cases))
    stand_df <- dplyr::left_join(id_df, stand_df, by = "label")
    # time period labels
    time_labels <- x$time$time.df$time.label
    time_df <- data.frame(time_index = 1:length(time_labels), time_label = time_labels)
    # samples of standardized rates
    suppressMessages(
    stand_samples <- x$samples %>%
        tidybayes::gather_draws(rate[group_index, time_index]) %>%
        dplyr::select(.data$group_index, .data$time_index, .data$.draw, .data$.value) %>%
        dplyr::left_join(stand_df, by = c("group_index")) %>%
        dplyr::group_by(.data$.draw, .data$time_index) %>%
        dplyr::summarise(stand_rate = standardize_rate(.data$.value, .data$standard_pop)) %>%
        dplyr::left_join(time_df, by ="time_index")
    )
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

#' Methods for age-standardized rates
#'
#' @description Print and plot methods for `stand_surveil` (standardized rates obtained from a fitted `surveil` model)
#'
#' @param x An object of `stand_surveil` obtained by calling \code{\link[surveil]{standardize}} on a fitted `surveil` model
#' @param scale Scale the rates by this amount; e.g., `scale = 100e3` will print rates per 100,000 at risk.
#' @param digits Number of digits to print
#' @param ... additional arguments
#' @details
#'
#'  Calling `standardize` on a fitted `surveil` model will create a new object that contains the `surveil` model results as well standardized rates. This new `stand_surveil` object has its own methods for printing and plotting.
#'
#' ### print.stand_surveil
#'
#' Any additional arguments (`...`) will be  passed to \code{\link[base]{print.data.frame}}
#'
#' @return
#'
#' ### print.stand_surveil
#'
#' The print method returns nothing but prints a summary of results to the console. 
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


#' @param style If `style = "mean_qi"`, then the posterior means and 95 percent credible intervals will be plotted; if `style = "lines"`, then `M` samples from the joint posterior distribution will be plotted.
#' @param M Number of samples to plot when `style = "lines"`
#' @param base_size Passed to `theme_classic()` to control size of plot components (text).
#' @param col Line color
#' @param fill Fill color for the 95 percent credible intervals
#' @param alpha For `style = "mean_qi"`, this controls the transparency for the credible interval (passed to \code{\link[ggplot2]{geom_ribbon}}) and defaults to `alpha = 0.5`; for `style = "lines"`, this controls the transparency of the lines and defaults to `alpha = 0.7`.
#' @param lwd Line width; for `style = "mean_qi"`, the default is `lwd = 1`; for `style = "lines"`, the default is `lwd = 0.05`.
#' @details
#' 
#' ### plot.stand_surveil
#' 
#' Any additional arguments (`...`) will be passed to `\code{\link[ggplot2]{theme}}.
#'
#' @return
#'
#' ### plot.stand_surveil
#'
#' The plot method returns an object of class `ggplot`.
#' 
#' @importFrom scales comma
#' @import ggplot2
#' @import graphics
#' @export
#' @md
#' @rdname stand_surveil
#' @method plot stand_surveil
#' 
plot.stand_surveil <- function(x,
                               scale = 1,
                               style = c("mean_qi", "lines"),
                               M = 250,
                               base_size = 14,
                               col = 'black',
                               fill = 'gray80',
                               alpha,
                               lwd,
                               ...) {
    style <- match.arg(style, c("mean_qi", "lines"))
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)
    if (scale != 1) message("Plotted rates are per ", scales::comma(scale))
    if (style == "mean_qi") {
        gg <- ggplot(x$standard_summary) +
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
            labs(x = NULL,
                 y = NULL) +
            theme_surveil(base_size = base_size, ...)            
        return (gg)
    }
    if (style == "lines") {
        df <- x$standard_samples
        df <- dplyr::filter(df, .data$.draw <= M)
        TT <- max(df$time_index)        
        df$draw <- rep(1:M, each = TT)
        gg <- ggplot(df, aes(.data$time_label, scale * .data$stand_rate,
                             group = factor(.data$draw))
                     ) +
            geom_line(col = col,
                      alpha = alpha,
                      lwd = lwd) +
            labs(x = NULL,
                 y = NULL) +
            theme_surveil(base_size = base_size, ...)
        return (gg)
    }
}



#' Widely Applicable Information Criteria
#'
#' @description Widely Application Information Criteria (WAIC) for model comparison
#' 
#' @param fit An \code{surveil} object
#' @param pointwise Logical (defaults to `FALSE`); if `pointwise = TRUE`, a vector of values for each observation will be returned. 
#' @param digits Round results to this many digits.
#' 
#' @return A vector of length 3 with \code{WAIC}, a rough measure of the effective number of parameters estimated by the model \code{Eff_pars}, and log predictive density \code{Lpd}. If \code{pointwise = TRUE}, results are returned in a \code{data.frame}.
#'
#' @examples
#' 
#' data(msa)
#' austin <- msa[grep("Austin", msa$MSA), ]
#' austin.w <- austin[grep("White", austin$Race),]
#' fit <- stan_rw(austin.w, time = Year,
#'                chains = 2, iter = 1200) # for speed only
#' waic(fit)
#'  
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


