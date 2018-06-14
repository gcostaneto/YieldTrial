#' Correcting phenotypic datasets for GS and GWAS using deregressed BLUPs
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

DBLUPC<-function(gen,env,rep,traits,df){
  library(breedR)
  output<-vector("list",length(traits))
  names(output)<-traits
  dfa<-data.frame(gen=df[,gen],env=df[,env],rep=df[,rep])
  traits<-traits

  for(i in 1:length(traits)){
    dfa$y<-df[,traits[i]]
    solR <- remlf90(fixed = y ~ env , random = ~ rep + gen, method = "em", data = dfa)

    blups<-data.frame(gen=row.names(data.frame(solR$ranef$gen)),
                      blup=data.frame(solR$ranef$gen)$value,
                      se=data.frame(solR$ranef$gen)$s.e.)

    blups$rel <- 1 - (blups$se)^2/solR$var[2]
    blups$dblup <- blups$blup/blups$rel

    c <- 0.5 # this constant is amount of variation that we expect not be explained by markers
    r<-unique(length(dfa$rep))
    (blups$hg <- round(solR$var[2] / (solR$var[2] + solR$var[3]/r),2)) # heritability
    (blups$w <- (1 -blups$hg)/(c + (1 - blups$rel) / blups$rel*blups$hg))

    output[[i]]<-blups
  }
  cat("Phenotype correction using random genotypic effects","\n",
      "---------------------------------------------------------------------","\n","\n")
  return(output)


}
