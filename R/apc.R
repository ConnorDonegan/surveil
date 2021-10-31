#' Annual and cumulative percent change
#'
#' @description Summarize annual and cumulative percent change in risk
#'
#' @param x A fitted `surviel` model, or standardized rates (a `stand_surveil` object). 
#'
#' @return An `apc_ls` (list) object containing the following data frames:
#' \describe{
#'  \item{apc}{A data frame containing a summary of the posterior distribution for period-specific percent change. This contains the posterior mean (`apc`) 95 percent credible intervals (`lwr` and `upr` bounds).}
#' \item{cpc}{A data frame containing a summary of the posterior distribution for the cumulative percent change in risk at each time period. This contains the posterior mean (`cpc`) and 95 percent credible interval (`lwr` and `upr` bounds).}
#' \item{apc_samples}{MCMC samples from the posterior distribution for period percent change}
#' \item{cpc_samples}{MCMC samples from the posterior distribution for cumulative percent change}
#' }
#'
#' @examples
#' data(cancer)
#' \dontrun{
#'  fit <- stan_rw(cancer, time = Year, group = Age)
#'  x <- apc(fit)
#'  print(x, rate = 10e3)
#'  plot(x, cumulative = TRUE)
#' }
#' @seealso \code{\link[surveil]{stan_rw}} \code{\link[surveil]{standardize}}
#' @md
#' @export
#' @rdname apc 
apc <- function(x) UseMethod("apc", x)

#' @method apc surveil
#' @importFrom tidyr pivot_longer
#' @export
#' @rdname apc
apc.surveil <- function(x) {
    ## S x G x T array
    a.samples <- rstan::extract(x$samples, pars = "rate")$rate
    time_label <- x$time$time.df$time.label
    GG <- dim(a.samples)[2]
    if (GG > 1) {
        group_label <- colnames(x$data$cases)
    } else {
        group_label <- 1
    }
    apc_list <- list()
    cpc_list <- list()
    s_apc_list <- list()
    s_cpc_list <- list()
    for (g in 1:GG) {
        g.samples <- a.samples[,g,]
        res.apc <- matrix(nrow = nrow(g.samples), ncol = ncol(g.samples) - 1)
        res.cum <- matrix(nrow = nrow(g.samples), ncol = ncol(g.samples) - 1)
        for (i in 2:ncol(g.samples)) {
            res.apc[,i-1] <- 100 * (g.samples[,i] / g.samples[,i-1] - 1)
            res.cum[,i-1] <- 100 * (g.samples[,i] / g.samples[,1] - 1)
        }
        apc_summary <- data.frame(
            time = time_label[-1],
            group = group_label[g],
            apc = apply(res.apc, 2, mean),
            lwr = apply(res.apc, 2, quantile, probs = 0.025),
            upr = apply(res.apc, 2, quantile, probs = 0.975)
        )
        cpc_summary <- data.frame(
            time = time_label[-1],
            group = group_label[g],
            cpc = apply(res.cum, 2, mean),
            lwr = apply(res.cum, 2, quantile, probs = 0.025),
            upr = apply(res.cum, 2, quantile, probs = 0.975)
        )        
        apc_list[[g]] <- apc_summary
        cpc_list[[g]] <- cpc_summary

        apc_samples <- as.data.frame(res.apc)
        names(apc_samples) <- time_label[-1]
        apc_samples$.draw <- 1:nrow(apc_samples)
        apc_samples$group <- group_label[g]
        apc_samples <- tidyr::pivot_longer(apc_samples,
                                  -c(.data$.draw, .data$group),
                                  names_to = "time",
                                  values_to = "value")
        apc_samples$time <- as.numeric(apc_samples$time)
        s_apc_list[[g]] <- apc_samples

        cpc_samples<- as.data.frame(res.cum)
        names(cpc_samples) <- time_label[-1]
        cpc_samples$.draw <- 1:nrow(cpc_samples)
        cpc_samples$group <- group_label[g]
        cpc_samples <- tidyr::pivot_longer(cpc_samples,
                                  -c(.data$.draw, .data$group),
                                  names_to = "time",
                                  values_to = "value")
        cpc_samples$time <- as.numeric(cpc_samples$time)        
        s_cpc_list[[g]] <- cpc_samples
    }
    apc_df <- do.call("rbind", apc_list)
    cpc_df <- do.call("rbind", cpc_list)
    apc_samples <- do.call("rbind", s_apc_list)
    cpc_samples <- do.call("rbind", s_cpc_list)
    if (GG == 1) apc_df$group <- cpc_df$group <- NULL
    res <- list(apc = apc_df,
                cpc = cpc_df,
                apc_samples = apc_samples,
                cpc_samples = cpc_samples,
                time = x$time,
                group = x$group)
    class(res) <- append("apc_ls", class(res))
    return (res)
}

#' @importFrom tidyr pivot_wider
#' @rdname apc
#' @method apc stand_surveil
#' @export
apc.stand_surveil <- function(x) {
    time_label <- x$time$time.df$time.label
    s.wide <- tidyr::pivot_wider(x$standard_samples,
                                 id_cols = .data$.draw,
                                 names_from = .data$time_index,
                                 values_from = .data$stand_rate
                                 )
    s.wide$.draw <- NULL                   
    s.wide <- as.matrix(s.wide)
    res.apc <- matrix(NA, nrow = nrow(s.wide), ncol = ncol(s.wide) - 1)
    res.cum <- matrix(NA, nrow = nrow(s.wide), ncol = ncol(s.wide) - 1)    
    for (i in 2:ncol(s.wide)) {
        res.apc[,i-1] <- 100 * ((s.wide[,i] / s.wide[,i-1]) - 1)
        res.cum[,i-1] <- 100 * ((s.wide[,i] / s.wide[,1]) - 1)
    }    
    apc_summary <- data.frame(
        time = time_label[-1],
        apc = apply(res.apc, 2, mean),
        lwr = apply(res.apc, 2, quantile, probs = 0.025),
        upr = apply(res.apc, 2, quantile, probs = 0.975)
    )
    cpc_summary <- data.frame(
        time = time_label[-1],
        cpc = apply(res.cum, 2, mean),
        lwr = apply(res.cum, 2, quantile, probs = 0.025),
        upr = apply(res.cum, 2, quantile, probs = 0.975)
    )        
    res <- list(apc = apc_summary,
                cpc = cpc_summary,
                apc_samples = res.apc,
                cpc_samples = res.cum,
                time = x$time)
    class(res) <- append("apc_ls", class(res))
    return (res)
    }


#' Methods for APC objects
#' @param x An `apc_ls` object returned by \code{\link[surveil]{apc}}
#' @param digits Print this many digits (passed to \code{\link[base]{print.data.frame}})
#' @param max Print this many rows
#' @param ... additional arguments
#' @details
#' 
#' ### print.apc_ls
#'
#' Any additional arguments (`...`) to the print method will be  passed to \code{\link[base]{print.data.frame}}
#' 
#' @param max Maximum number of time periods (rows) to print 
#' @importFrom scales comma
#' @importFrom tidyr pivot_wider
#' @method print apc_ls
#' @export
#' @md
#' @rdname apc_ls
print.apc_ls <- function(x, digits = 1, max = 10, ...) {    
    message("Summary of cumulative and per-period percent change")
    message("Time periods: ", length(unique(x$time$time.df$time.label)))
    GG <- !is.null(x$apc$group)
    if (GG > 1) {
        x$apc <- tidyr::pivot_wider(x$apc,
                                     id_cols = .data$time,
                                     names_from = .data$group,
                                     values_from = .data$apc
                                     )
        x$cpc <- tidyr::pivot_wider(x$cpc,
                                       id_cols = .data$time,
                                       names_from = .data$group,
                                       values_from = .data$cpc
                                    )
        message("Cumulative percent change:")
        print.data.frame(x$cpc[nrow(x$cpc), -which(names(x$cpc) == "time")], digits = digits, row.names = FALSE, ...)        
        message("\nPeriod percent change")
        print.data.frame(x$apc, digits = digits, max = max * ncol(x$apc), row.names = FALSE, ...)        
    } else {
        c.est <- round(x$cpc$cpc[nrow(x$cpc)], digits)
        lwr <- round(x$cpc$lwr[nrow(x$cpc)], digits)
        upr <- round(x$cpc$upr[nrow(x$cpc)], digits)
        message("Cumulative percent change: ", c.est, " [", lwr, ", ", upr, "]")  
        message("Period percent change:")
        print.data.frame(x$apc, digits = digits, max = max * ncol(x$apc), row.names = FALSE, ...)
    }
}

#' @param cumulative Plot cumulative percent change? Defaults to `cumulative = FALSE`
#' @param style If `style = "mean_qi"`, then the posterior mean and 95 percent credible interval will be plotted; if `style = "lines"`, then `M` samples from the joint probability distribution will be plotted.
#' @param M If `style = "lines"`, then `M` is the number of samples from the posterior distribution that will be plotted; the default is `M = 250`.
#' @param col Line color
#' @param fill Fill color for the 95 percent credible interval
#' @param alpha For `style = "mean_qi"`, this controls the transparency for the credible interval (passed to \code{\link[ggplot2]{geom_ribbon}}) and defaults to `alpha = 0.5`; for `style = "lines"`, this controls the transparency of the lines and defaults to `alpha = 0.7`.
#' @param lwd Line width; for `style = "mean_qi"`, the default is `lwd = 1`; for `style = "lines"`, the default is `lwd = 0.05`.
#' @param base_size Size of plot attributes, passed to `\code{\link[ggplot2]{theme_classic}}
#' @param lwd Line width
#' @param ... Additional arguments
#' @md
#' @import ggplot2
#' @method plot apc_ls
#' @rdname apc_ls
#' @export
#' 
#' @details
#' ### plot.apc_ls
#' 
#' Any additional arguments (`...`) will be passed to \code{\link[ggplot2]{theme}}.
#' 
plot.apc_ls <- function(x,
                        cumulative = FALSE,
                        style = c("mean_qi", "lines"),
                        M = 250,                        
                        col = 'black',
                        fill = 'black',
                        alpha,
                        lwd,
                        base_size = 14,
                        ...
                        ) {
    style <- match.arg(style, c("mean_qi", "lines"))
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)    
    ylab <- ifelse(cumulative, "Cumulative % Change", "APC")
    if (style == "lines") {
        if (cumulative) {
            s_df <- x$cpc_samples
        } else {
            s_df <- x$apc_samples
        }
        s_df <- dplyr::filter(s_df, .data$.draw %in% sample(max(.data$.draw), size = M))
        if (length(unique(s_df$group)) > 1) s_df$group <- factor(s_df$group, ordered = TRUE, levels = unique(s_df$group))
        gg <- ggplot(s_df, aes(.data$time, .data$value,
                               group = factor(.data$.draw))
                     ) +
            geom_line(col = col,
                      alpha = alpha,
                      lwd = lwd) +
            geom_hline(yintercept = 0) +
            labs(y = ylab,
                 x = NULL) +
            theme_classic(base_size = base_size) +
            theme(...)
        if (length(unique(s_df$group)) > 1) {
            gg <- gg +
                facet_wrap(~ .data$group )
        }
        return (gg)
    }
    if (length(unique(x$apc$group)) > 1) {
        x$apc$group <- factor(x$apc$group, levels = unique(x$apc$group), ordered = TRUE)
        x$cpc$group <- factor(x$cpc$group, levels = unique(x$cpc$group), ordered = TRUE)
    }
    if (cumulative) {        
        gg <- ggplot(x$cpc,
                     aes(.data$time,
                         y = .data$cpc,
                         ymin = .data$lwr,
                         ymax = .data$upr)
                     ) +
            labs(x = NULL,
                 y = "Cumulative % Change")
    } else {
        gg <- ggplot(x$apc,
                     aes(.data$time,
                         y = .data$apc,
                         ymin = .data$lwr,
                         ymax = .data$upr)
                     ) +
            labs(x = NULL,
                 y = "APC")
    }
    gg <- gg +
        geom_hline(yintercept = 0) +
        geom_ribbon(alpha = alpha,
                    fill = fill) +    
        geom_line(col = col,
                  lwd = lwd) +
        theme_classic(base_size = base_size)
    if (length(unique(x$apc$group)) > 1) {
        gg <- gg + facet_wrap(~ .data$group)
    }    
    gg +
        theme(...)
}
