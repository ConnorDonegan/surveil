#' Calculate summary measures of inequality: rate ratios, rate differences, excess cases
#'
#' @description Calculate pairwise measures of health inequality from a fitted `surveil` time series model, with credible intervals and MCMC samples.
#'
#' @param fit A fitted `surveil` time series model
#'
#' @param target The name (character string) of the disadvantaged group that is the target of inference. 
#'
#' @param reference The name (character string) of the reference group to which `target` will be compared.
#'
#' @param samples If `TRUE`, return MCMC samples for the inequality measures. The default is `FALSE`.
#'
#' @return A list, also of class "surveil_diff", with the following elements ...
#' @author Connor Donegan (Connor.Donegan@UTSouthwestern.edu)
#' 
#' @importFrom tidybayes mean_qi 
#' 
#' @export
#' 
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
    res.list <- lapply(T, function(t) {
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
        return(res)
    })
    res.df <- dplyr::bind_rows(res.list, .id = "time")
    res.df$time <- as.numeric(res.df$time)
    res.summary <- res.df %>%
        dplyr::group_by(time) %>%
        tidybayes::mean_qi(Rate, RD, RR, PAR, EC) %>%
        dplyr::mutate(time = Time.index)
    ec.tmp <- lapply(res.list, function(l) matrix(l$EC, ncol = 1))
    ec.tmp <- do.call("cbind", ec.tmp)
    EC.cumulative.samples <- apply(ec.tmp, 1, sum)
    cases.tmp <- lapply(res.list, function(l) matrix(l$Trend_Cases, ncol = 1))
    cases.tmp <- do.call("cbind", cases.tmp)
    cumulative.cases.samples <- apply(cases.tmp, 1, sum)
    PAR.samples <- EC.cumulative.samples / cumulative.cases.samples
    EC.cumulative.summary <- c(EC = mean(EC.cumulative.samples), quantile(EC.cumulative.samples, probs = c(0.025, 0.975)),
                               PAR = mean(PAR.samples), quantile(PAR.samples, probs =c(0.025, 0.975)))
    return.list <- list(
        summary = res.summary,
        cumulative_cases = EC.cumulative.summary,
        groups = c(target = target, reference = reference)
    )
    if (samples) return.list$samples <- list(annual = res.df, cumulative_cases = EC.cumulative.samples)
    class(return.list) <- append("surveil_diff", class(return.list))
    return( return.list )
}


#' Theil's inequality index
#'
#' @param x A fitted `surveil` model, from \code{\link[surveil]{stan_rw}}.
#' 
#' @return A named list with the following elements: \code{summary}, A `data.frame` summarizing the posterior probability distribution for Theil's T, including the mean and 95\% credible interval for each time period; and \code{samples}, A `data.frame` with MCMC samples for Theil's T.
#' 
#' @md
#' @export
#' @rdname theil
#' @method theil surveil
#' @importFrom tidybayes mean_qi gather_draws
#' @importFrom dplyr `%>%` distinct select inner_join mutate group_by ungroup summarise
#' 
theil.surveil <- function(x) {
    if (inherits(x$group, "NULL")) stop("There is no grouping variable stored in x$group; Theil's T calculates inequality across population sub-groups.")
    
    group.df <- x$group$group.df
    group.df$group.index <- as.integer(group.df$group.index)
    time.var <- unique(x$summary$time)
    time.index <- 1:length(time.var)
    time.df <- data.frame(time = time.var, time.index = time.index)
    
    names(x$summary)[grep(x$group$group, names(x$summary))] <- "label"    
    pop.df <- dplyr::distinct(x$summary, time, label, Population) %>%
        dplyr::left_join(group.df, by = "label") %>%
        dplyr::left_join(time.df, by = "time")
    suppressMessages(
        theil.samples <- x$samples %>%
            tidybayes::gather_draws(rate[group.index, time.index]) %>%
            dplyr::select(group.index, time.index, .draw, .value) %>%
            dplyr::inner_join(pop.df, by = c("time.index", "group.index")) %>%
            dplyr::mutate(Count = .value * Population) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(time, .draw) %>%    
            dplyr::summarise(Theil = theil2(Count = Count, Population = Population)) %>%
            dplyr::ungroup()          
    )
    theil.df <- theil.samples %>%
        dplyr::group_by(time) %>%
        tidybayes::mean_qi(Theil)
    res <- list(summary = theil.df,
                samples = theil.samples,
                groups = x$group$group.df$label,
                group_var = x$group$group)
    class(res) <- append("theil", class(res))
    return (res)
}


#' Theil index
#'
#' @export
#' @md
#' @method theil list
#' @rdname theil
theil.list <- function(x) {
    geog.pop.list <- lapply(x, function(x) {
    aggregate(Population ~ time, FUN = sum, x$summary)
    })
    geog.pop.df <- bind_rows(geog.pop.list, .id = "geog")
    tw.i.list <- lapply(x, function(x) theil(x)$samples)
    tw.i.df <- bind_rows(tw.i.list, .id = "geog")
    cases.list <- lapply(x, make_cases)
    cases.df <- bind_rows(cases.list, .id = "geog")

   theil.df <- cases.df %>%
       dplyr::inner_join(tw.i.df, by = c("geog", "time", ".draw")) %>%    
       dplyr::inner_join(geog.pop.df, by = c("geog", "time")) %>%
       dplyr::ungroup() %>%
       dplyr::group_by(time, .draw) %>%
       dplyr::mutate(total.count = sum(Count),
                     total.pop = sum(Population)
                     ) %>%
       dplyr::mutate(omega = Count / total.count,
                     eta = Population / total.pop,
                     T_between_contribution = omega * log(omega / eta),
                     T_within_contribution = omega * Theil) %>%
       dplyr::summarise( # groups: time, .draw
#                  geog = unique(geog),
                  Theil_between = sum(T_between_contribution),
                  Theil_within = sum(T_within_contribution),
                  ) %>%
       dplyr::mutate(Theil = Theil_between + Theil_within) %>%
       dplyr::ungroup() 
    out <- theil.df
    class(out) <- append("theil_list", class(out))
    return (out)    
}

make_cases <- function(x) {
    stopifnot(inherits(x, "surveil"))
    group.df <- x$group$group.df
    group.df$group.index <- as.integer(group.df$group.index)
    time.var <- unique(x$summary$time)
    time.index <- 1:length(time.var)
    time.df <- data.frame(time = time.var, time.index = time.index)    
    names(x$summary)[grep(x$group$group, names(x$summary))] <- "label"    
    pop.df <- dplyr::distinct(x$summary, time, label, Population) %>%
        dplyr::left_join(group.df, by = "label") %>%
        dplyr::left_join(time.df, by = "time") %>%
        dplyr::mutate(group.index = as.integer(group.index))            
    cases.samples <- x$samples %>%
        tidybayes::gather_draws(rate[group.index, time.index]) %>%
        dplyr::select(group.index, time.index, .draw, .value) %>%
        dplyr::inner_join(pop.df, by = c("time.index", "group.index")) %>%
        dplyr::mutate(Count = .value * Population) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(time, .draw) %>%
        dplyr::mutate(Count = sum(Count)) %>% # count.per.geog
        dplyr::ungroup() %>%
        select(time, .draw, Count)
    return (cases.samples)
    }


#' Theil's inequality index
#' 
#' @description Calculates Theil's entropy-based measure of inequality for a given set of disease incidence rates and populations at risk. For nested data structures, within-group (`W`) and between-group (`B`) components of inequality are calculated, as well as total inequality (`T = W + B`).
#'
#' @return The results depend on the input. For a non-nested data structure, a numeric value is returned for each time period of observation. 
#'
#' 
#' @details
#'
#' Theil's index is a good index of inequality in disease and mortality burdens when multiple groups are being considered, as is typical of geospatial analysis. It provides a summary measure of inequality across a set of demographic groups that may be tracked over time. Also, it is interesting because it is additive, and thus admits of simple decompositions. 
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

#' Theil's index is based on Shannon's information theory, he used it to study a variety of topics, including income inequality and racial segregation. Theil's index is often of great interest because it is additive across multiple scales, such as when the data has a nested structure to it (e.g., demographic groups within states). The Texas Inequality Project provides introductions to, and examples of using, the Theil index (Conceicao and Ferreira, 2000). However, this `R` function is just a simple implementation for `flat' or non-nested data structures (e.g., a set of counties). 
#'
#' @source
#'
#' Conceicao, P. and P. Ferreira (2000). The young person's guide to the Theil Index: Suggesting intuitive interpretations and exploring analytical applications. University of Texas Inequality Project. UTIP Working Paper Number 14. Accessed May 1, 2021 from \url{https://utip.gov.utexas.edu/papers.html}
#'
#' Theil, Henri (1972). *Statistical Decomposition Analysis.* Amsterdan, The Netherlands and London, UK: North-Holland Publishing Company.
#'
#' Shannon, Claude E. and Weaver, Warren (1963). *The Mathematical Theory of Communication*. Urbana and Chicago, USA: University if Illinois Press.
#'
#' @seealso \code{\link[surveil]{theil2}}
#'
#' @export
#' @md
#' 
theil <- function(x, group, time, detail = FALSE) {
    UseMethod("theil", x)
}

#' Thiel's T for non-nested data structure
#'
#' @examples
#'
#' Count <- c(10, 12, 3, 111)
#' Pop <- c(1000, 1200, 4000, 9000)
#' theil2(Count, Pop)
#' @export
#' @rdname theil
#' @seealso \code{\link[surveil]{theil}}
theil2 <- function(Count, Population, rates, total = TRUE) {
    if (missing(Count)) Count <- rates * Population
    omega = Count / sum(Count)
    eta = Population / sum( Population )
    T = omega * log (omega / eta)
    T[is.na(T)] <- 0
    if (total) T = sum( T )
    return (Theil = T)
}



#' Theil's inequality index
#' 
#' @param x A data.frame with columns "Count" (for disease case count or deaths) and "Population" (for population at risk), and optional columns for "time" and "geog" (geographic grouping variable, such as state or city name).
#'
#' If `x` is a `data.frame` and it has a geographic grouping variable (as indicated by a column named "geog"), a between-geographies component of inequality will be added to the index, such that Theil's T will be the sum of within-geography inequality and between-georgaphy inequality. Providing a `geog` variable that contains only one value (a single georgraphic area) is the same as not providing any `geog` variable, such that Theil's T indexes the within-geography inequality only.
#'
#' If `x` is a `data.frame` and it has a column named "time", then Theil's index will be calculated separately for each time period.
#' 
#' @param detail If `detail = TRUE`, the return value will be a list with each component of Theil's T. Theil'T is the sum of all its parts.
#' 
#' @export
#' @method theil data.frame
#' @importFrom dplyr  `%>%`  group_by mutate summarise full_join select enquo
#' @importFrom rlang as_label
#' @rdname theil
theil.data.frame <- function(x, detail = FALSE) {
    stopifnot(all(c("Count", "Population") %in% names(x)))
    stopifnot(min(x$Count)>=0)
    stopifnot(min(x$Population)>=0)
    has_time <- ifelse("time" %in% names(x), 1, 0)
    has_geog <- ifelse("geog" %in% names(x), 1, 0)
    if (!has_geog) {
        if (!has_time) {
            theil2(x$Count, x$Population, total = !detail)
        } else {
            d.tmp <- x %>%
                dplyr::group_by(time)
            T <- d.tmp %>%
                dplyr::summarise(
                    T = theil2(Count, Population)
                    )
            if (!detail) return(T)            
            d.dets <- d.tmp %>%
                mutate(
                    T_contribution = theil2(Count, Population, total = FALSE)
                )
            L <- list(detail = d.dets,
                      T = T
                      )
            return (L)                
            }
    }
    if (!has_time) x$time = 1
  ## individual level
    level_1_detail <- x %>%
        dplyr::group_by(time, geog) %>%
        dplyr::mutate(
            raw_T_contribution = theil2(Count, Population, total = FALSE),
            case_share = Count / sum(Count),
            pop_share = Population / sum(Population)
        )
  ## unweighted within-group inequality
    level_1 <- level_1_detail %>%
        dplyr::summarise(
            raw_T_within = sum(raw_T_contribution),
            Count = sum(Count),
            Population = sum(Population)
        ) %>%
        dplyr::mutate(
            case_share = Count / sum( Count ),
            pop_share = Population / sum( Population )
        )
  ## within-group inequality
    within <- level_1 %>%
        dplyr::summarise(
            T_within = sum( case_share * raw_T_within )
        )
    ## between-msa inequality
    between <- level_1 %>%
        dplyr::mutate(
            T_between = theil2(Count, Population)
        ) %>%
        dplyr::summarise(
            T_between = sum(T_between)
        )
  ## total inequality
    if (!has_time) {
        T.df <- cbind(within, between)
        T.df$Theil <- T.df$T_within + T.df$T_between
        T.df <- T.df[,c("T_within", "T_between", "Theil")]
    } else {
        T.df <- dplyr::full_join(within, between, by = "time") %>%
            dplyr::mutate(
                Theil = T_within + T_between
            )
    }
    if (!detail) return(T.df)
    ## group-level detail
    group_detail <- level_1 %>%
        dplyr::mutate(
            weighted_T_within = case_share * raw_T_within,
            T_between = theil2(Count, Population)
        ) %>%
        dplyr::select(time, geog,
               raw_T_within, weighted_T_within, T_between, case_share, pop_share, Count, Population)
    det.res <- list(
        level_1_detail = level_1_detail,
        group_detail = group_detail,
        total = T.df
    )
    return (det.res)
}

