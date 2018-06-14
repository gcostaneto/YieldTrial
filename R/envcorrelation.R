#' Genotypic Correlation across Environments
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
#' @author Germano Martins F. Costa Neto  <germano.cneto@usp.br>
#' @examples data(MET_maize)
#' env.corr<-envcorrelation(y = "YIELD", trials = "ENV",
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(env.corr)
#' @references 1. Colombari Filho JM, de Resende MDV, de Morais OP, de Castro AP, Guimarães ÉP, Pereira JA, et al. Upland rice breeding in Brazil: A simultaneous genotypic evaluation of stability, adaptability and grain yield. Euphytica. 2013;192(1):117–29.
#' @references 2. Smith AB, Ganesalingam A, Kuchel H, Cullis BR. Factor analytic mixed models for the provision of grower information from national crop variety testing programs. Theor Appl Genet. 2014;128(1):55–72.

ENVCOR<-function(df, trials, gen, rep, y, plot=TRUE){

  y <- df[,y]
  g <- df[,gen]
  r <- df[,rep]
  e <- df[,trials]
  dfa<-data.frame(y,g,r,e)


  Ntrials <-length(unique(dfa$e))

  results<-data.frame(Trials=levels(dfa$e),
                      Trials.=rep(levels(dfa$e),each=Ntrials),
                      matrix(ncol=10))

  names(results)<-c("Trial","Trial.","Vg","Vga","Vres","Vf","h2","rg","mean","r_ge","GE_type","Connectivity")

  for(i in 1:nrow(results)){

    t1<-droplevels(subset(dfa, e == results$Trial[i]))
    genoj<-levels(t1$g)

    t2<-droplevels(subset(dfa, e == results$Trial.[i]))
    genoj.<-levels(t2$g)
    veri <- genoj == genoj.
    cat("Running correlation: Trial",levels(t1$e),"vs. Trial.",levels(t2$e),'\n')

    t<-rbind(t1,t2)

    r <- length(unique(t$r))
    a <- length(unique(t$e))

    if(results$Trial[i] == results$Trial.[i]){
      results[i,c(3:12)]<-NA
      results$r_ge[i]<-1
      results$Connectivity[i]<-sum(ifelse(veri == "TRUE",1,0))
    }else{

      modelo<-lmer(y~1+(1|e:r)+(1|g)+(1|g:e),t)

      vg<-round(VarCorr(modelo)[[2]][1],5)
      vga<-round(VarCorr(modelo)[[1]][1],5)
      vres<-round(sigma(modelo)^2,5)

      results$Vga[i]<-vga
      results$Vg[i]<-vg
      results$Vres[i]<-vres

      results$Vf[i]<-round(VarCorr(modelo)[[1]][1],5)+round(VarCorr(modelo)[[2]][1],5)+round(sigma(modelo)^2,5)
      results$h2[i]<-round((vg/(vg+(vres/r)+(vga/a*r))),5)
      results$rg[i]<-round(sqrt(vg/(vg+(vres/r)+(vga/a*r))),5)
      results$mean[i]<-round(fixef(modelo)[[1]],3)
      results$r_ge[i]<-round(vg/(vg+vga),5)
      results$GE_type[i]<-ifelse(results$r_ge[i] > 0.95, "non-interaction",
                                 ifelse(results$r_ge[i] < 0.20, "crossover","noncrossover"))
      results$Connectivity[i]<-sum(ifelse(veri == "TRUE",1,0))
    }
  }
  if(plot == TRUE){
    require(viridis)
    require(ggplot2)
    plotGE<-ggplot(results, aes(Trial, Trial., z= r_ge)) +
      geom_tile(aes(fill = GE_type)) +
      theme_bw() + xlab(" ")+ylab(" ")+ ggtitle("Predominant type of GxE interaction")+
      scale_fill_manual(values=c("red2","yellow2","darkgreen","white"),
                        name= "GxE type")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(family = "Arial",
                                      color="black", face="bold", size=15, hjust=0))
      print(plotGE)

      plotCOR<-ggplot(results, aes(Trial, Trial., z= r_ge)) +
        geom_tile(aes(fill = r_ge)) +
        theme_bw() + xlab(" ")+ylab(" ")+ggtitle("Genotypic correlation")+
        scale_fill_viridis(option = "viridis",name=expression(rg/(rg+rge)))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(plot.title = element_text(family = "Arial",
                                        color="black", face="bold", size=15, hjust=0))
      print(plotCOR)

      plotCONNE<-ggplot(results, aes(Trial, Trial., z= Connectivity)) +
        geom_tile(aes(fill = Connectivity)) +
        theme_bw() + xlab(" ")+ylab(" ")+ ggtitle("Variety Connectivity")+
        scale_fill_viridis(option = "plasma",name="Nº of Genotypes")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(plot.title = element_text(family = "Arial",
                                        color="black", face="bold", size=15, hjust=0))
      print(plotCONNE)

      plots<-list(GxE=plotGE,Corr=plotCOR,Conne=plotCONNE)

    return(list(results=results,plots=plots))}
  if(plot == FALSE){
    return(results)
  }

}

