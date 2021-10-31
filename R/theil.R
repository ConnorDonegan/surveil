
#' @title Theil's inequality index
#' 
#' @description Theil's entropy-based index of inequality 
#' @param x A fitted `surveil` model, from \code{\link[surveil]{stan_rw}}; or, a list of fitted `surveil` models, where each model represents a different geographic area (e.g., states).
#' 
#' @details
#'
#' Theil's index is a good index of inequality in disease and mortality burdens when multiple groups are being considered. It provides a summary measure of inequality across a set of demographic groups that may be tracked over time. Also, it is interesting because it is additive, and thus admits of simple decompositions. 
#'
#' The index measures discrepancies between a population's share of the disease burden, `omega`, and their share of the population, `eta`. A situation of zero inequality would imply that each population's share of cases is equal to its population share, or, `omega=eta`. Each population's contribution to total inequality is calculated as:
#' ```
#'              T_i = omega_i * [log(omega_i/eta_i)],
#' ```
#' the log-ratio of case-share to population-share, weighted by their share of cases. Theil's index for all areas is the sum of each area's T_i:
#' ```
#'              T = sum_(i=1)^n T_i.
#' ```
#' Theil's T is thus a weighted mean of log-ratios of case shares to population shares, where each log-ratio (which we may describe as a raw inequality score) is weighted by its share of total cases. The index has a minimum of zero and a maximum of `log(N)`, where `N` is the number of units (e.g., number of counties).

#' Theil's index is based on Shannon's information theory and Theil used it to study a variety of topics, including income inequality and racial segregation.
#'
#' Theil's index is often of great interest because it is additive across multiple scales. Surveillance data often consist of nested population structures, such as demographic groups nested within states. 
#'
#' @source
#'
#' Conceicao, P. and P. Ferreira (2000). The young person's guide to the Theil Index: Suggesting intuitive interpretations and exploring analytical applications. University of Texas Inequality Project. UTIP Working Paper Number 14. Accessed May 1, 2021 from \url{https://utip.gov.utexas.edu/papers.html}
#'
#' Conceicao, P, Galbraith, JK, Bradford, P. (2001). The Theil Index in sequences of nested and hierarchic grouping structures: implications for the measurement of inequality through time, with data aggregated at different levels of industrial classification. *Eastern Economic Journal*. 27(4): 491-514.
#' 
#' Theil, Henri (1972). *Statistical Decomposition Analysis.* Amsterdan, The Netherlands and London, UK: North-Holland Publishing Company.
#'
#' Shannon, Claude E. and Weaver, Warren (1963). *The Mathematical Theory of Communication*. Urbana and Chicago, USA: University if Illinois Press.
#'
#' @seealso \code{\link[surveil]{plot.theil}} \code{\link[surveil]{print.theil}} \code{\link[surveil]{plot.theil_list}}
#'
#' @examples
#' 
#' \dontrun{
#'  dfw <- msa[grep("Dallas", msa$MSA), ]
#'  fit <- stan_rw(dfw, time = Year, group = Race)
#'  theil.dfw <- theil(fit)
#'  plot(theil.dfw)
#' }
#' 
#' 
#' @export
#' @md
theil <- function(x) {
    UseMethod("theil", x)
}



#' @param Count Case counts, integers
#' @param Population Population at risk, integers
#' @param rates If `Count` is not provided, then `rates` must be provided (`Count = rates * Population`).
#' @param total If `total = TRUE`, Theil's index will be returned. Each unit contributes to Theil's index; if `total = FALSE`, all of the elements that sum to Theil's index will be returned.
#'
#' @return
#' 
#' ### theil2
#'
#' If `total = TRUE` (the default), `theil2` returns Theil's index as a numeric value. Else, `theil2` returns a vector of values that sum to Theil's index.
#' 
#' @examples
#'
#' Count <- c(10, 12, 3, 111)
#' Pop <- c(1000, 1200, 4000, 9000)
#' theil2(Count, Pop)
#' theil2(Count, Pop, total = FALSE)
#' 
#' @export
#' @rdname theil
theil2 <- function(Count, Population, rates, total = TRUE) {
    if (missing(Count)) Count <- rates * Population
    omega = Count / sum(Count)
    eta = Population / sum( Population )
    T = omega * log (omega / eta)
    T[is.na(T)] <- 0
    if (total) T = sum( T )
    return (Theil = T)
}

#' 
#' @return
#' ### theil.surveil
#' 
#' A named list with the following elements: \describe{
#'
#' \item{summary}{ A `data.frame` summarizing the posterior probability distribution for Theil's T, including the mean and 95 percent credible interval for each time period}
#' \item{samples}{A `data.frame` with MCMC samples for Theil's T}
#' }
#' 
#' 
#' @importFrom ggdist mean_qi
#' @importFrom tidybayes gather_draws
#' @importFrom dplyr `%>%` distinct select inner_join left_join mutate group_by ungroup summarise
#' @md
#' @rdname theil
#' @method theil surveil
#' @export
theil.surveil <- function(x) {
    if (inherits(x$group, "NULL")) stop("There is no grouping variable stored in the model (x$group); the purpose of Theil's T is to measure inequality across group.")
    rate <- group.index <- time.index <- NULL # global binding
    group.df <- x$group$group.df
    group.df$group.index <- as.integer(group.df$group.index)
    time.var <- unique(x$summary$time)
    time.index <- 1:length(time.var)
    time.df <- data.frame(time = time.var, time.index = time.index)
    
    names(x$summary)[grep(x$group$group, names(x$summary))] <- "group.label"    
    pop.df <- dplyr::distinct(x$summary, .data$time, .data$group.label, .data$Population) %>%
        dplyr::left_join(group.df, by = "group.label") %>%
        dplyr::left_join(time.df, by = "time")
    suppressMessages(
        theil.samples <- x$samples %>%
            tidybayes::gather_draws(rate[group.index, time.index]) %>%
            dplyr::select(.data$group.index, .data$time.index, .data$.draw, .data$.value) %>%
            dplyr::inner_join(pop.df, by = c("time.index", "group.index")) %>%
            dplyr::mutate(Count = .data$.value * .data$Population) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(.data$time, .data$.draw) %>%    
            dplyr::summarise(Theil = theil2(Count = .data$Count, Population = .data$Population)) %>%
            dplyr::ungroup()          
    )
    theil.df <- theil.samples %>%
        dplyr::group_by(.data$time) %>%
        ggdist::mean_qi(.data$Theil)
    res <- list(summary = theil.df,
                samples = theil.samples,
                group = x$group.label,
                time = x$time)
    class(res) <- append("theil", class(res))
    return (res)
}


#' @return
#' ### theil.list
#'
#' A list (also of class `theil_list`) containing a summary data frame and a `tbl_df` containing MCMC samples for Theil's index at each time period.
#'
#' The summary data frame includes the following columns:
#' \describe{
#' \item{time}{time period}
#' \item{Theil}{Posterior mean for Theil's index; equal to the sum of `Theil_between` and `Theil_within`.}
#' \item{Theil_between}{The between-areas component to Theil's indequality index}
#' \item{Theil_within}{The within-areas component to Theil's inequality index}
#' }
#' Additional columns contain the upper and lower limits of the 95 percent credible intervals for each component of Theil's index. 
#' 
#' The data frame of samples contains the following columns: \describe{
#' \item{time}{Time period indicator}
#' \item{.draw}{An id for each MCMC sample; note that samples are from the joint distribution}
#' \item{Theil_between}{The between-geographies component of Thiel's index}
#' \item{Theil_within}{The within-geographies component of Theil's index}
#' \item{Theil}{Theil's indequality index (T = Between + Within)}.
#' }
#'
#' 
#' @importFrom dplyr bind_rows inner_join `%>%` ungroup group_by mutate summarise
#' @importFrom stats aggregate
#' @export
#' @method theil list
#' @rdname theil
#' @md
theil.list <- function(x) {
    Population <- time <- NULL # global binding
    stopifnot(all(unlist(lapply(x, function(xi) inherits(xi, "surveil")))))
    geog.pop.list <- lapply(x, function(x) {
    stats::aggregate(Population ~ time, FUN = sum, x$summary)
    })
    geog.pop.df <- dplyr::bind_rows(geog.pop.list, .id = "geog")
    tw.i.list <- lapply(x, function(x) theil(x)$samples)
    tw.i.df <- dplyr::bind_rows(tw.i.list, .id = "geog")
    cases.list <- lapply(x, make_cases)
    cases.df <- dplyr::bind_rows(cases.list, .id = "geog")
    suppressMessages(
        theil_samples <- cases.df %>%
            dplyr::inner_join(tw.i.df, by = c("geog", "time", ".draw")) %>%    
            dplyr::inner_join(geog.pop.df, by = c("geog", "time")) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(.data$time, .data$.draw) %>%
            dplyr::mutate(total.count = sum(.data$Count),
                          total.pop = sum(.data$Population)
                          ) %>%
       dplyr::mutate(omega = .data$Count / .data$total.count,
                     eta = .data$Population / .data$total.pop,
                     T_between_contribution = .data$omega * log(.data$omega / .data$eta),
                     T_within_contribution = .data$omega * .data$Theil) %>%
       dplyr::summarise(
                  Theil_between = sum(.data$T_between_contribution),
                  Theil_within = sum(.data$T_within_contribution),
                  ) %>%
       dplyr::mutate(Theil = .data$Theil_between + .data$Theil_within) %>%
       dplyr::ungroup()
    )
    theil_summary <- theil_samples %>%
        dplyr::group_by(.data$time) %>%
        ggdist::mean_qi(.data$Theil, .data$Theil_between, .data$Theil_within)
    out <- list(summary = theil_summary, samples = theil_samples, areas = length(x))
    class(out) <- append("theil_list", class(out))
    return (out)    
}

#' @noRd
#' @importFrom dplyr distinct `%>%` left_join mutate select inner_join ungroup group_by
#' @importFrom tidybayes gather_draws
make_cases <- function(x) {
    rate <- group.index <- time.index <- NULL # global binding
    stopifnot(inherits(x, "surveil"))
    group.df <- x$group$group.df
    group.df$group.index <- as.integer(group.df$group.index)
    time.var <- unique(x$summary$time)
    time.df <- data.frame(time = time.var, time.index = 1:length(time.var))    
    names(x$summary)[grep(x$group$group, names(x$summary))] <- "group.label"    
    pop.df <- dplyr::distinct(x$summary, .data$time, .data$group.label, .data$Population) %>%
        dplyr::left_join(group.df, by = "group.label") %>%
        dplyr::left_join(time.df, by = "time") %>%
        dplyr::mutate(group.index = as.integer(.data$group.index))            
    cases.samples <- x$samples %>%
        tidybayes::gather_draws(rate[group.index, time.index]) %>%
        dplyr::select(.data$group.index, .data$time.index, .data$.draw, .data$.value) %>%
        dplyr::inner_join(pop.df, by = c("time.index", "group.index")) %>%
        dplyr::mutate(Count = .data$.value * .data$Population) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$time, .data$.draw) %>%
        dplyr::mutate(Count = sum(.data$Count)) %>% # count.per.geog
        dplyr::ungroup() %>%
        select(.data$time, .data$.draw, .data$Count)
    return (cases.samples)
    }




#' Methods for Theil's index
#' 
#' @description Printing and plotting methods for Theil's inequality index
#' @export
#' @md
#' @param x An object of class `thiel' or `theil_list`, as returned by calling `theil` on a list of fitted `surveil` models
#' @param style If `style = "mean_qi"`, then the posterior mean and 95 percent credible interval will be plotted; if `style = "lines"`, then `M` samples from the joint probability distribution will be plotted.
#' @param M If `style = "lines"`, then `M` is the number of samples from the posterior distribution that will be plotted; the default is `M = 250`.
#' @param alpha For `style = "mean_qi"`, this controls the transparency for the credible interval (passed to \code{\link[ggplot2]{geom_ribbon}}) and defaults to `alpha = 0.5`; for `style = "lines"`, this controls the transparency of the lines and defaults to `alpha = 0.7`.
#' @param lwd Line width; for `style = "mean_qi"`, the default is `lwd = 1`; for `style = "lines"`, the default is `lwd = 0.05`.
#' @param scale Scale Theil's index by `scale`
#' @param digits number of digits to print (passed to \code{\link[base]{print.data.frame}})
#' @param fill Fill color
#' @param col Line color
#' @param labels x-axis labels (time periods)
#' @param base_size Passed to `theme_classic` to control size of plot elements (e.g., text)
#' @param ... additional arguments
#' @examples
#'  \dontrun{
#'  dfw <- msa[grep("Dallas", msa$MSA), ]
#'  fit <- stan_rw(dfw, time = Year, group = Race)
#'  theil.dfw <- theil(fit)
#'  plot(theil.dfw)
#' }
#' @method plot theil
#' @import ggplot2
#' @rdname theil_methods
plot.theil <- function(x,
                       style = c("mean_qi", "lines"),
                       M = 250,                      
                       col = "black",
                       fill = "black",
                       alpha,
                       lwd,         
                       base_size = 14,
                       scale = 100,
                       labels = x$summary$time,
                       ...) {
    style <- match.arg(style, c("mean_qi", "lines"))
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)    
    if (style == "lines") {
        s_df <- x$samples
        max_M <- max(s_df$.draw)
        M <- min(M, max_M)
        idx <- which(s_df$.draw %in% sample(max_M, M))
        s_df <- s_df[idx,]
        gg <- ggplot(s_df, aes(.data$time, .data$Theil,
                         group = .data$.draw
                         )
               ) +
            geom_line(alpha = alpha,
                      lwd = lwd,
                      col = col) +
            labs(x = NULL,
                 y = NULL) +
            theme_classic() +
            theme(...)
        return (gg)
    }
    ggplot(x$summary) +
        geom_ribbon(
            aes(.data$time,
                ymin= scale * .data$.lower,
                ymax= scale * .data$.upper),
            fill = fill,
            alpha = alpha
        ) +
        geom_line(aes(.data$time, scale * .data$Theil),
                  col = col,                
                  lwd = 0.75
                  ) +
        labs(x = NULL,
             y = paste0("Theil x ", scale)) +
        theme_classic(base_size = base_size) +
        theme(...)
}


#' @param plot If `FALSE`, return a list of `ggplot`s. Not used when `style = "lines"`.
#' @param between_title Plot title for the between geography component of Theil's T; defaults to "Between".
#' @param within_title Plot title for the within geography component of Theil's T; defaults to "Within".
#' @param total_title Plot title for Theil's index; defaults to "Total".
#' 
#' @method plot theil_list
#' @import ggplot2
#' @importFrom ggdist mean_qi
#' @importFrom dplyr `%>%` group_by mutate summarise filter
#' @importFrom tidyr pivot_longer
#' @importFrom gridExtra grid.arrange
#' @seealso \code{\link[surveil]{theil}}
#' @export
#' @rdname theil_methods
#' @md
plot.theil_list <- function(x,
                            style = c("mean_qi", "lines"),
                            M = 250,
                            col = "black",
                            fill = "black",
                            alpha,
                            lwd,
                            between_title = "Between",
                            within_title = "Within",
                            total_title = "Total",
                            scale = 100,
                            plot = TRUE,
                            base_size = 14,
                            ...) {
    style <- match.arg(style, c("mean_qi", "lines"))
    if (missing(lwd)) lwd <- ifelse(style == "mean_qi", 1, 0.05)
    if (missing(alpha)) alpha <- ifelse(style == "mean_qi", 0.5, 0.7)
    if (scale != 1) message("y-axis scale is T times ", scale)    
    if (style == "lines") {
        s_df <- x$samples
        s_df <- dplyr::filter(s_df, .data$.draw %in% sample(max(.data$.draw), size = M))
        s_df <- tidyr::pivot_longer(s_df,
                                    -c(.data$time, .data$.draw),
                                    names_to = "component",
                                    values_to = "value"
                                    )
        s_df$component <- gsub("Theil_within", "Within", s_df$component)
        s_df$component <- gsub("Theil_between", "Between", s_df$component)
        s_df$component <- gsub("Theil", "Total", s_df$component)
        s_df$component <- factor(s_df$component, ordered = TRUE, levels = c("Within", "Between", "Total"))                                   
        gg <- ggplot(s_df, aes(.data$time, .data$value * scale,
                               group = factor(.data$.draw))
                     ) +
            geom_line(alpha = alpha,
                      lwd = lwd,
                      col = col) +
            labs(x = NULL,
                 y = NULL) +
            theme_classic(base_size = base_size) +
            facet_wrap(~ component, scales = "free") +
            theme(...)    
        return(gg)
    }    
    #max.val <- x$summary %>%
     #   dplyr::summarise(max.val = max(.data$Theil.upper))
    #max.val <- as.numeric(max.val$max.val) * scale
    ## between geography inequality
    g1 <- ggplot(x$summary,
                 aes(x = .data$time,                     
                     y = .data$Theil_between * scale,
                     ymin = .data$Theil_between.lower * scale,
                     ymax = .data$Theil_between.upper * scale)
                     ) +
        geom_line(col = col,
                  lwd = lwd) +
        geom_ribbon(alpha = alpha,
                    fill = fill
                    ) +
       # scale_y_continuous(limits = c(0, max.val)) +
        labs(subtitle = between_title,
             x  = NULL,
             y = NULL) +
        theme_classic(base_size = base_size) +
        theme(...)
    ## within geography inequality
    g2 <- ggplot(x$summary,
                 aes(.data$time,
                     .data$Theil_within * scale)) +
        geom_line(col = col,
                  lwd = lwd) +
        geom_ribbon(aes(ymin = .data$Theil_within.lower * scale,
                        ymax = .data$Theil_within.upper * scale),
                    alpha = alpha,
                    fill = fill
                    ) +
       # scale_y_continuous(limits = c(0, max.val)) +
        labs(
            subtitle = within_title,
            x = NULL,
            y = NULL
        ) +
        theme_classic(base_size = base_size) +
        theme(...)
    ## total inequality
    g3 <- ggplot(x$summary,
                 aes(.data$time,
                     .data$Theil * scale)) +
        geom_line(col = col,
                  lwd = lwd) +
        geom_ribbon(aes(ymin = .data$Theil.lower * scale,
                        ymax = .data$Theil.upper * scale),
                    alpha = alpha,
                    fill = fill
                    ) +
        #scale_y_continuous(limits = c(0, max.val)) +
        labs(subtitle = total_title,
             x  = NULL,
             y = NULL) +
        theme_classic(base_size = base_size) +
        theme(...)    
    if (!plot) {
        glist <- list(between = g1, within = g2, total = g3)
        return (glist)
    } else {
        gridExtra::grid.arrange(g1, g2, g3, nrow = 1)
    }
}



#' @method print theil
#' @rdname theil_methods
#' @importFrom scales comma percent
#' @export
print.theil <- function(x, scale = 100, digits = 3, ...) {    
    message("Summary of Theil's Inequality Index")    
    message("Groups: ", paste(x$group$group.df$group.label, collapse = ", "))
    message("Time periods observed: ", length(x$summary$time))
    pdf <- as.data.frame(x$summary)
    pdf <- pdf[,c("time", "Theil", ".lower", ".upper")]
    if (scale != 1) {
        pdf$Theil <- pdf$Theil * scale
        pdf$.lower <- pdf$.lower * scale
        pdf$.upper <- pdf$.upper * scale
        message("Theil's T (times ", scale, ") with 95% credible intervals")
    } else {
        message("Theil's T with 95% credible intervals")
    }
    print(pdf, digits = digits, row.names = FALSE, ...)
}


#' print Theil's index
#' @method print theil_list
#' @importFrom scales comma percent
#' @export
#' @rdname theil_methods
print.theil_list <- function(x, scale = 100, digits = 3, ...) {    
    message("Summary of Theil's Inequality Index for Nested Population Structure")    
    message("Areas: ", x$areas) 
    message("Time periods: ", length(x$summary$time))
    pdf <- as.data.frame(x$summary)
    pdf <- pdf[,c("time", "Theil_between", "Theil_within", "Theil")]
    if (scale != 1) {
        pdf$Theil <- pdf$Theil * scale
        pdf$Theil_between <- pdf$Theil_between * scale
        pdf$Theil_within <- pdf$Theil_within * scale
        message("Theil's T (times ", scale, ")")
    } else {
        message("Theil's T: within geographies, between geographies, and total inequality")
    }
    print(pdf, digits = digits, row.names = FALSE, ...)
}
