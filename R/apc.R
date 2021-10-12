#' Annual and cumulative percent change
#'
#' @description Summarize annual and cumulative percent change in risk
#'
#' @param x For the `apc` function, `x` may be a fitted `surviel` model, or standardized rates (a `stand_surveil` object). For print and plot methods, `x` is the `apc_ls` object returned by `apc`.
#'
#' @return A data frame containing a summary of the annual and cumulative percent changes in risk. The means of posterior distributions are reported (`apc`, `cumulative`) with 95 percent credible intervals (`apc_lwr`, `apc_upr` and `cum_lwr`, `cum_upr`).
#'
#' @examples
#' data(cancer)
#' \dontrun{
#'  fit <- stan_rw(cancer, time = Year, group = Label)
#'  x <- apc(fit)
#'  print(x, rate = 10e3)
#'  plot(x, cumulative = TRUE)
#' }
#' @seealso \code{\link[surveil]{stan_rw}} \code{\link[surveil]{standardize}}
#' @md
#' @rdname apc 
apc <- function(x) UseMethod("apc", x)


#' @method apc surveil
#' @export
#' @rdname apc
apc.surveil <- function(x) {
    ## S x G x T array
    a.samples <- rstan::extract(x$samples, pars = "rate")$rate
    time_label <- x$data$time
    GG <- dim(a.samples)[2]
    if (GG > 1) {
        group_label <- colnames(x$data$cases)
    } else {
        group_label <- 1
    }
    apc_list <- list()
    cpc_list <- list()
    for (g in 1:GG) {
        g.samples <- a.samples[,g,]
        res.s <- matrix(nrow = nrow(g.samples), ncol = ncol(g.samples) - 1)
        for (i in 2:ncol(g.samples)) {
            res.s[,i-1] <- 100 * (g.samples[,i] / g.samples[,i-1] - 1)
        }
        cum.pc = apply(res.s, 1, cumsum)
        apc_summary <- data.frame(
            time = time_label[-1],
            group = group_label[g],
            apc = apply(res.s, 2, mean),
            lwr = apply(res.s, 2, quantile, probs = 0.025),
            upr = apply(res.s, 2, quantile, probs = 0.975)
        )
        cpc_summary <- data.frame(
            time = time_label[-1],
            group = group_label[g],
            cumulative = apply(cum.pc, 1, mean),
            lwr = apply(cum.pc, 1, quantile, probs = 0.025),
            upr = apply(cum.pc, 1, quantile, probs = 0.975)
        )        
        apc_list[[g]] <- apc_summary
        cpc_list[[g]] <- cpc_summary
    }
    apc_df <- do.call("rbind", apc_list)
    cpc_df <- do.call("rbind", cpc_list)
    if (GG == 1) apc_df$group <- cpc_df$group <- NULL
    res <- list(apc = apc_df, cpc = cpc_df)
    class(res) <- append("apc_ls", class(res))
    return (res)
}

#' @importFrom tidyr pivot_wider
#' @rdname apc
#' @method apc stand_surveil
#' @export
apc.stand_surveil <- function(x) {
    time_label <- x$data$time
    s.wide <- tidyr::pivot_wider(x$standard_samples,
                                 id_cols = .data$.draw,
                                 names_from = .data$time_index,
                                 values_from = .data$stand_rate
                                 )
    s.wide$.draw <- NULL                   
    s.wide <- as.matrix(s.wide)
    res.s <- matrix(NA, nrow = nrow(s.wide), ncol = ncol(s.wide) - 1)
    for (i in 2:ncol(s.wide)) {
        res.s[,i-1] <- 100 * ((s.wide[,i] / s.wide[,i-1]) - 1)
    }    
    cum.pc = apply(res.s, 1, cumsum)
    apc_summary <- data.frame(
        time = time_label[-1],
        apc = apply(res.s, 2, mean),
        lwr = apply(res.s, 2, quantile, probs = 0.025),
        upr = apply(res.s, 2, quantile, probs = 0.975)
    )
    cpc_summary <- data.frame(
        time = time_label[-1],
        cumulative = apply(cum.pc, 1, mean),
        lwr = apply(cum.pc, 1, quantile, probs = 0.025),
        upr = apply(cum.pc, 1, quantile, probs = 0.975)
    )        
    res <- list(apc = apc_summary, cpc = cpc_summary)
    class(res) <- append("apc_ls", class(res))
    return (res)
    }



#' @param digits Print this many digits (passed to \code{\link[base]{print.data.frame}})
#' @param max Print this many rows
#' @param ... additional arguments
#' @details
#' 
#' ### print.apc_df
#'
#' Any additional arguments (`...`) will be  passed to \code{\link[base]{print.data.frame}}
#' 
#' @param max Maximum number of time periods (rows) to print 
#' @importFrom scales comma
#' @importFrom tidyr pivot_wider
#' @method print apc_ls
#' @export
#' @md
#' @rdname apc
print.apc_ls <- function(x, digits = 0, max = 10, ...) {    
    message("Summary of per-period and cumulative percent change")
    message("Time periods: ", length(unique(x$apc$time)))    
    GG <- length(unique(x$apc$group))
    if (GG > 1) {
        x$apc <- tidyr::pivot_wider(x$apc,
                                     id_cols = .data$time,
                                     names_from = .data$group,
                                     values_from = .data$apc
                                     )
        x$cpc <- tidyr::pivot_wider(x$cpc,
                                       id_cols = .data$time,
                                       names_from = .data$group,
                                       values_from = .data$cumulative
                                    )
        message("Cumulative percent change:")
        print.data.frame(x$cpc[nrow(x$cpc), -which(names(x$cpc) == "time")], digits = digits, row.names = FALSE, ...)
        message("\nPeriod percent change")
        print.data.frame(x$apc, digits = digits, max = max * ncol(x$apc), row.names = FALSE, ...)        
    } else {
        c.est <- round(x$cpc$cumulative[nrow(x$cpc)], digits)
        lwr <- round(x$cpc$lwr[nrow(x$cpc)], digits)
        upr <- round(x$cpc$upr[nrow(x$cpc)], digits)
        message("Cumulative percent change: ", c.est, " [", lwr, ", ", upr, "]")  
        message("Period percent change:")
        print.data.frame(x$apc, digits = digits, max = max * ncol(x$apc), row.names = FALSE, ...)
    }
}

#' @param cumulative Plot cumulative percent change? Defaults to `cumulative = FALSE`
#' @param col Line color
#' @param fill Fill color for the 95 percent credible interval
#' @param alpha Transparency (defaults to `alpha = 0.5`) for the credible interval (passed to \code{\link[ggplot2]{geom_ribbon}}).
#' @param base_size Size of plot attributes, passed to `\code{\link[ggplot2]{theme_classic}}
#' @param lwd Line width
#' @param ... Additional arguments
#' @md
#' @import ggplot2
#' @method plot apc_ls
#' @rdname apc
#' @export
#' @details
#' ### plot.apc_ls
#' 
#' Any additional arguments (`...`) will be passed to \code{\link[ggplot2]{theme}}.
#' 
plot.apc_ls <- function(x,
                        cumulative = FALSE,
                        col = 'black',
                        fill = 'black',
                        alpha = 0.5,
                        base_size = 14,
                        lwd = 1,
                        ...
                        ) {
    ylab <- ifelse(cumulative, "Cumulative % Change", "APC")
    if (cumulative) {        
        gg <- ggplot(x$cpc,
                     aes(.data$time,
                         y = .data$cumulative,
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
        geom_ribbon(alpha = alpha, fill = fill) +    
        geom_line(col = col, lwd = lwd) +
        theme_classic(base_size = base_size)    
    if (length(unique(x$apc$group)) > 1) gg <- gg + facet_wrap(~ .data$group)    
    gg +
        theme(...)
}
