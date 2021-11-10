#' The 'surveil' package 
#'
#' @description Fits time series models for routine disease surveillance tasks and returns probability distributions for a variety of quantities of interest, including measures of health inequality, period and cumulative percent change, and age-standardized rates. Calculates Theil's index to measure inequality among multiple groups, and can be extended to measure inequality across multiple groups nested within geographies. Inference is completed using Markov chain Monte Carlo via the Stan modeling language. The models are appropriate for rare disease incidence and mortality data, employing a Poisson likelihood and first-difference (random-walk) prior for unknown risk, and optional covariance matrix for multiple correlated time series models. References: Brandt and Williams (2007, ISBN:978-1-4129-0656-2); Clayton (1996, ISBN-13:978-0-412-05551-5); Conceicao and Galbraith (2001) <https://www.jstor.org/stable/40326064>; Stan Development Team (2021) <https://mc-stan.org>; Theil (1972, ISBN:0-444-10378-3).
#'
#' @docType package
#' @name surveil-package
#' @aliases surveil
#' @useDynLib surveil, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#'
#' Brandt P, Williams JT. Multiple time series models. Thousand Oaks, CA: SAGE Publications, 2007. ISBN:9781412906562
#' 
#' Clatyon, DG. Generalized linear mixed models. In: Gilks WR, Richardson S, Spiegelhalter DJ, editors. Markov chain Monte Carlo in practice. Boca Raton, FL: CRC Press, 1996. p. 275-302. ISBN:9780412055515
#'
#' Conceicao P, Galbraith JK, Bradford P. The Theil Index in sequences of nested and hierarchic grouping structures: implications for the measurement of inequality through time, with data aggregated at different levels of industrial classification. Eastern Economic Journal 2001;27(4):491-514.
#' 
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
#'
#' Theil H. Statistical decomposition analysis. Amsterdam, The Netherlands: North-Holland Publishing Company, 1972. ISBN:0444103783
#' 
NULL
