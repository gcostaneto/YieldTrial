#' Correcting phenotypic datasets for GS and GWAS using BLUEs
#'
#' Perform a mixed model analysis of each individual trial within a MET dataset.
#'
#'
#' @param df dataframe object
#' @param trials factor, column associated to environment levels within df object
#' @param gen  factor, column associated to genotype levels within df object
#' @param rep factor, column associated to replication or blocks levels within df object
#' @param traits character, list of columns associated to evaluated trait within df object
#' @param accuracy numeric, a single value (from 0 to 1) indicating the expected threshold accuracy (we recommend the use of 0.7)
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(MET_maize)
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
#' @references 2. Resende MDV, Duarte JB. Precisão e controle de qualidade em experimentos de avaliação de cultivares. Pesquisa Agropecuária Tropical. 2007;37(3):182–94.



BLUEC<-function(gen,env,rep,traits,df){

  output<-vector("list",length(traits))
  names(output)<-traits
  dfa<-data.frame(gen=df[,gen],
                  env=df[,env],
                  rep=df[,rep])
  traits<-traits


  for(i in 1:length(traits)){
    dfa$y<-df[,traits[i]]
    solF <- breedR::remlf90(fixed = y ~ env + gen,
                            random = ~ rep,
                            method = "em", data = dfa)

    output[[i]]<-data.frame(blue=data.frame(solF$fixed$gen)$value,
                            se=data.frame(solF$fixed$gen)$s.e.,
                            gen=row.names(data.frame(solF$fixed$gen)))
  }
  cat("Phenotype correction using fixed genotypic effects","\n",
      "---------------------------------------------------------------------","\n","\n")
  return(output)

}
