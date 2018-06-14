#' AMMI (Additive Main effects and Multiplicative Interaction)
#'
#' Returns a dataframe containing outputs results from two-by-two analysis using
#' mixed model REML/BLUP (assuming random genotypic effects and fixed block).
#' Perform a analysis on the variety connectivity (number of the same genotypes among trials),
#' calculate the
#' Indicates what type of genoytpe x environment interaction are predominant.
#'
#'
#' @param df dataframe object
#' @param trials factor type column associated to environment levels within df object
#' @param gen  factor type column associated to genotype levels within df object
#' @param rep factor type column associated to replication or blocks levels within df object
#' @param y numeric type column associated to evaluated trait within df object
#' @param GE TRUE or FALSE, return a plot of the predominant type of GxE interaction, genotypic correaltion across trials and variety connectivity
#' @author Adapted from Umesh R Rosyara by Germano Costa Neto
#' @examples data(MET_maize)
#' env.corr<-envcorrelation(y = "YIELD", trials = "ENV",
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(env.corr)
#' @references Eberhart S.A., Russell W.A. (1966) Stability parameters for comparing varieties. Crop Sci. 6: 36-40.
#' @references Singh R.K., Chaudhary B.D.(1985) Biometrical Methods in Quantitative Genetics Analysis, Kalyani Publishers

AMMIf<-function(){

}
