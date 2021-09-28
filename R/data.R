
#' Colorectal cancer incidence by Texas MSA, 1999-2017, ages 50-79
#'
#' @source 
#'
#' United States Cancer Statistics--Incidence: 1999-2017, WONDER Online Database. United States Department of Health and Human Services, Centers for Disease Control and Prevention and National Cancer Institute; 2020. Accessed at [http://wonder.cdc.gov/cancer-v2017.html] on Nov 9, 2020 2:59:24 PM.
#'
#' @description Annual counts of colorectal cancer (cancer of colon or rectum), ages 50-79, for Texas's top four metropolitan statistical areas (MSAs), with population at risk estimates, by race-ethnicity (non-Hispanic White, non-Hispanic Black, Hispanic/Latino).
#' 
#' @format A `tibble` with the following attributes:
#' \describe{
#'  \item{Year}{Year of diagnosis}
#'  \item{Race}{Race-ethnicity designation}
#'  \item{MSA}{Metropolitan statistical area}
#'  \item{Count}{Number of CRC cases}
#'  \item{Population}{Age-specific population estimate}
#' }
#' @examples
#' \dontrun{
#' data(msa)
#' }
"msa"
