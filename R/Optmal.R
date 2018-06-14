#' Optimal number of trials
#'
#' Identify the optimal number of trials.
#'
#'
#' @param Nenv dataframe object
#' @param rgg numeric, a single value (from 0 to 1) indicating the expected threshold accuracy (we recommend the use of 0.7)
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(MET_maize)
#' trial.evalut-trialEvalut(y = "YIELD", trials = "ENV", accuracy=0.7,
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(trial.evalut)
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
#' @references 2. Resende MDV, Duarte JB. Precisão e controle de qualidade em experimentos de avaliação de cultivares. Pesquisa Agropecuária Tropical. 2007;37(3):182–94.


ONT<-function(Nenv,rgg){
  Ef<-sqrt(env/(1+(env-1)*rgg))
  return(Ef)
}
