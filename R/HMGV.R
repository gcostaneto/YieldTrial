#' Harmomic Mean of Genotypic Values (HMGV)
#'
#' Returns a dataframe containing outputs results from two-by-two analysis using
#' mixed model REML/BLUP (assuming random genotypic effects and fixed block).
#' Perform a analysis on the variety connectivity (number of the same genotypes among trials),
#' calculate the
#' Indicates what type of genoytpe x environment interaction are predominant.
#'
#'
#' @param df dataframe object containing genotypic values or means for each genotype across the environmentsdf object
#' @param y numeric type column associated to evaluated trait within df object
#' @param gen  factor type column associated to genotype levels within df object
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(MET_maize)
#' env.corr<-envcorrelation(y = "YIELD", trials = "ENV",
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(env.corr)
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.


HMGV<-function(df, y, gen){


  dfa<-data.frame(y=as.numeric(df[,y]), gen=as.factor(df[,gen]))

  result<-data.frame(HMGV=NA,gen=NA)

  for(i in 1:length(levels(dfa$gen))){

    g<-droplevels(subset(dfa,gen == levels(dfa$gen)[i]))
    cat("HMGV for",levels(dfa$gen)[i],"genotype","\n")
    result[i,2] <-levels(dfa$gen)[i]
    result[i,1] <-psych::harmonic.mean(g$y)}
  return(result)}

