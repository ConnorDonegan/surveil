#' Prior distributions
#'
#' @param location Location parameter (numeric)
#' @param scale Scale parameter (positive numeric)
#' @param k Optional; number of groups for which priors are needed. This is a shortcut to avoid using the `rep` function to repeat the same prior for each group, as in: `normal(location = rep(0, times = 3), scale = rep(1, times = 3)`. To provide distinct priors for each group, simply specify them individually, as in `normal(location = c(-5, -6, -8), scale = c(2, 2, 2))`.
#' 
#' @details
#' 
#' The prior distribution functions are used to set the values of prior parameters.
#'
#' Users can control the values of the parameters, but the distribution (model) itself is fixed. The first log-rate (`eta[t]`, `t=1`) and the scale parameters (sigma) are assigned Gaussian (`normal`) prior distribution. (The scale parameter, sigma, is constrained to be positive, making it a half-normal prior.) For correlated time series, the correlation matrix is assigned the LKJ prior.
#' 
#' ### Parameterizations
#'
#' For details on how any distribution is parameterized, see the Stan Language Functions Reference document: \url{https://mc-stan.org/users/documentation/}.
#'
#' @return An object of class `prior` which will be used internally by **surveil** to set parameters of prior distributions. 
#' 
#' @examples
#'
#' # note there are three groups in the data, each requires a prior
#' prior <- list()
#' prior$eta_1 <- normal(location = -6, scale = 4, k = 3)
#' ## by default, location = 0
#' prior$sigma <- normal(scale = 1, k = 3)
#' prior$omega <- lkj(2)
#'
#' \donttest{
#' dfw <- msa[grep("Dallas", msa$MSA), ]
#' fit <- stan_rw(dfw, time = Year, group = Race, prior = prior, iter = 1200)
#' plot(fit)
#' }
#'
#' @name priors
NULL

#' @rdname priors
#' @md
#' @export
#'
normal <- function(location = 0, scale, k = 1) {
    validate_positive_parameter(scale)
    if (k > 1) {
        location <- rep(location, times = k)
        scale <- rep(scale, times = k)
    }
    out <- list(dist = "normal", location = location, scale = scale)
    class(out) <- append("prior", class(out))
    return (out)
}


#' @param eta The shape parameter for the LKJ prior
#' 
#' @details
#'
#' ### LKJ prior
#'
#' The LKJ prior for correlation matrix has a single parameter, eta (eta > 0). If `eta=1`, then you are placing a uniform prior on any K-by-K correlation matrix. For eta > 1, there is a higher probability on the identity matrix, such that as eta increases beyond 1, you are expressing greater scepticism towards large correlations. If 0 < eta < 1, then you will be expressing scepticism towards correlations of zero and favoring non-zero correlations. See Stan documentation: \url{https://mc-stan.org/docs/2_27/functions-reference/lkj-correlation.html}.
#' 
#' @source
#' Stan Development Team. Stan Functions Reference Version 2.27. \url{https://mc-stan.org/docs/2_27/functions-reference/lkj-correlation.html}
#' @rdname priors
#' @export
#' @md
lkj <- function(eta) {
    validate_positive_parameter(eta)
    out <- list(dist = "lkj", eta = eta)
    class(out) <- append("prior", class(out))
    return (out)
}

#' @export
#' @noRd
#' @param x a prior distribution object
#' @param digits number of digits to print
#' @param ... additional arguments passed to \code{\link[base]{print.data.frame}}
#' @method print prior
print.prior <- function(x, digits = 2, ...) {
    nm <- x$dist
    message("Distribution: ", nm)
    if (nm == "normal") df <- as.data.frame(x[c('location', 'scale')])
    if (nm == "lkj") df <- data.frame(eta = x$eta)
    print(df, digits = digits, row.names = FALSE, ...)           
}

#' @noRd
#' @param x The value to check.
#' @return Either an error is thrown or \code{TRUE} is returned invisibly.
validate_positive_parameter <- function(x) {
  nm <- deparse(substitute(x))
  if (!is.null(x)) {
    if (!is.numeric(x)) 
      stop(nm, " should be numeric", call. = FALSE)
    if (any(x <= 0)) 
      stop(nm, " should be positive", call. = FALSE)
  }
  invisible(TRUE)
}

