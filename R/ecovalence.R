#' Fixed effects of G, GE, G+GE and Ecovalence
#'
#' Returns a dataframe containing outputs results from two-by-two analysis using
#' mixed model REML/BLUP (assuming random genotypic effects and fixed block).
#' Perform a analysis on the variety connectivity (number of the same genotypes among trials),
#' calculate the
#' Indicates what type of genoytpe x environment interaction are predominant.
#'
#'
#' @param df dataframe object
#' @param digits numeric, digits for round ouputs
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(corsten.interaction)
#' m1 <- melt(corsten.interaction, measure.var='yield')
#' dmat <- acast(m1, loc~gen)
#' dmat<-ecovalence(as.matrix(dmat),digits=3)
#' dmat$Ecovalence
#' dmat$GA.matrix
#' dmat$Y.j
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
#' @references 2. Smith AB, Ganesalingam A, Kuchel H, Cullis BR. Factor analytic mixed models for the provision of grower information from national crop variety testing programs. Theor Appl Genet. 2014;128(1):55–72.


GGAW<-function(df, digits, plot=TRUE){
  require(gplots)
  (Y<-as.matrix(df))
  dim(Y)
  (Ymi<-as.matrix(colMeans(Y)))
  (Ymj<-as.matrix(rowMeans(Y)))

  (J <- matrix(1, nrow = 1 , ncol = dim(Y)[1]))
  (J. <- matrix(1, nrow = dim(Y)[2] , ncol = 1))

  (L <- Ymi%*%J)

  (C <- J.%*%t(Ymj))
  (M<-mean(Y)*matrix(1, dim(Y)[2],dim(Y)[1]))
  (GA<-t(Y)-L-C+M)
  GGE<-t(Y)-M
  (GA2<-GA^2)
  w<-rowSums(GA2)
  SQ<-sum(GA2)
  wp<-(w/SQ)*100
  MG<-Ymi-M[1,1]

  if(plot == T){
     heatmap.2(GA)
    return(list(GA.matrix = round(GA, digits=digits),
                GGE.matrix= round(GGE, digits=digits),
                Ecovalence = data.frame(w2=round(w, digits=digits),
                                        Relative.Ecovalence=round(wp, digits=2),
                                        Yi.=Ymi,Vg=MG),Y..=M[1,1], Y.j=Ymj))}
  if(plot == F){
    return(list(GA.matrix = round(GA, digits=digits),
                GGE.matrix= round(GGE, digits=digits),
                Ecovalence = data.frame(w2=round(w, digits=digits),
                                        Relative.Ecovalence=round(wp, digits=2),
                                        Yi.=Ymi,Vg=MG),Y..=M[1,1], Y.j=Ymj))}
}
