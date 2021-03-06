% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HMGV.R
\name{HMGV}
\alias{HMGV}
\title{Harmomic Mean of Genotypic Values (HMGV)}
\usage{
HMGV(df, y, gen)
}
\arguments{
\item{df}{dataframe object containing genotypic values or means for each genotype across the environmentsdf object}

\item{y}{numeric type column associated to evaluated trait within df object}

\item{gen}{factor type column associated to genotype levels within df object}
}
\description{
Returns a dataframe containing outputs results from two-by-two analysis using
mixed model REML/BLUP (assuming random genotypic effects and fixed block).
Perform a analysis on the variety connectivity (number of the same genotypes among trials),
calculate the
Indicates what type of genoytpe x environment interaction are predominant.
}
\examples{
data(MET_maize)
env.corr<-envcorrelation(y = "YIELD", trials = "ENV",
                         gen = "GEN", rep = "REP", df = MET.maize)
head(env.corr)
}
\references{
1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
}
\author{
Germano Martins F. Costa Neto  <germano.cneto@usp.br>
}
