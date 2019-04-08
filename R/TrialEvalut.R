#' Mixed model analysis of individuals trials
#'
#' Perform a mixed model analysis of each individual trial within a MET dataset.
#'
#'
#' @param df dataframe object
#' @param trials factor, column associated to environment levels within df object
#' @param gen  factor, column associated to genotype levels within df object
#' @param rep factor, column associated to replication or blocks levels within df object
#' @param y numeric, column associated to evaluated trait within df object
#' @param accuracy numeric, a single value (from 0 to 1) indicating the expected threshold accuracy (we recommend the use of 0.7)
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(MET_maize)
#' trial.evalut-trialEvalut(y = "YIELD", trials = "ENV", accuracy=0.7,
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(trial.evalut)
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
#' @references 2. Resende MDV, Duarte JB. Precisão e controle de qualidade em experimentos de avaliação de cultivares. Pesquisa Agropecuária Tropical. 2007;37(3):182–94.

TE1<-function(df,y,rep, gen, trials, accuracy,print=FALSE){

  y <- df[, y]
  g <- df[, gen]
  r <- df[, rep]
  e <- df[, trials]
  dfa <- data.frame(y, g, r, e)
  Ntrials <- length(unique(dfa$e))
  results <- data.frame(matrix(nrow = Ntrials, ncol = 10))
  names(results) <- c("Trial", "mean", "Vg", "Vres", "Vf", 
                      "h2", "rgg", "CVg", "CVe", "Diagnostic")
  for (i in 1:Ntrials) {
    cat("Running ", i, "Trial ", levels(dfa$e)[i], "\n")
    A <- subset(dfa, e == levels(e)[i])
    r <- length(unique(A$r))
    modelo <- lme4::lmer(y ~ 1 + r + (1 | g), A)
    results$Trial[i] <- levels(as.factor(A$e))[i]
    results$Vg[i] <- round(lme4::VarCorr(modelo)[[1]][1], 
                           5)
    results$Vres[i] <- round(sigma(modelo)^2, 5)
    results$Vf[i] <- (results$Vg[i] + results$Vres[i])
    results$h2[i] <- round(results$Vg[i]/(results$Vg[i] + 
                                            (results$Vres[i]/r)), 4)
    results$mean[i] <- fixef(modelo)[[1]]
    results$CVg[i] <- round(100 * (sqrt(results$Vg[i])/results$mean[i]), 
                            3)
    results$CVe[i] <- round(100 * (sqrt(results$Vf[i])/results$m[i]), 
                            3)
    results$rgg[i] <- round(sqrt(1 - (1/(1 + r * (results$CVg[i]/results$CVe[i])^2))), 
                            3)
    results$Diagnostic[i] <- ifelse(results$rgg[i] < accuracy, 
                                    "Remove", "-")
  }
  if(isTRUE(print)){print(knitr::kable(results))}
  return(results)
}
