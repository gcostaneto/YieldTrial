#' Maize hybrids over yield trials
#'
#' Data from a multienvironment framework for summer hybrid testing from a private breeding program in Brazil.
#' A total of 99 hybrids were evaluated across 17 environments in 4 years (2011-2014) in Central Brazil summer season
#'
#' @docType data
#' @usage data(MET_maize)
#' @format data.frame
#' \describe{
#' \item{YIELD}{Grain yield data (tons per hectare)}
#' \item{GEN}{Name of the each 99 evaluated hybrids}
#' \item{ENV}{Name of each 17 trials (environments)}
#' \item{REP}{treatments replicaiton (1 and 2) for each trial}
#' \item{YEAR}{Year in which the trial was conducted (2011, 2012, 2013 and 2014)}
#' \item{SITE}{Site where the trial was conducted over 4 states in Brazil)}
#'
#' }
#' @examples
#' data(MET_maize)
#' boxplot(YIELD~GEN,MET_maiz)
#'

"MET_maize"



