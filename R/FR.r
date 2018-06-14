#' Factorial Regression (FR) using additional information
#'
#' Returns a dataframe containing outputs results from two-by-two analysis using
#' mixed model REML/BLUP (assuming random genotypic effects and fixed block).
#' Perform a analysis on the variety connectivity (number of the same genotypes among trials),
#' calculate the
#' Indicates what type of genoytpe x environment interaction are predominant.
#'
#'
#' @param df.y dataframe contaning response values (e.g., GE matrix)
#' @param df.cov dataframe contaning predictor values (e.g., covariate matrix)
#' @param scale  FALSE (default) or TRUE if scale df.y is required
#' @author  Germano Costa Neto
#' @examples data(MET_maize)
#' env.corr<-envcorrelation(y = "YIELD", trials = "ENV",
#'                          gen = "GEN", rep = "REP", df = MET.maize)
#' head(env.corr)
#' @references Van Eeuwijk FA. Linear and bilinear models for the analysis of multi-environment trials: I. An inventory of models. Euphytica. 1995;84(1):1–7.
#' @references Brancourt-Hulmel M, Denis JB, Lecomte C. Determining environmental covariates which explain genotype environment interaction in winter wheat through probe genotypes and biadditive factorial regression. Theor Appl Genet. 2000;100(2):285–98.
#' @references Balfourier F, Oliveira JA, Charmet G, Arbones E. Factorial regression analysis of genotype by environment interaction in ryegrass populations, using both isozyme and climatic data as covariates. Euphytica. 1997;98(1):37–46.
#' @references Baril CP, Denis J-B, Wustman R, Van Eeuwijk FA. Analysing genotype by environment interaction in Dutch potato variety trials using factorial regression. Euphytica. 1995;84(1):23–9.
#' @references Vargas M, Crossa J, Van Eeuwijk FA, Ramírez ME, Sayre K. Using partial least squares regression, factorial regression, and AMMI models for interpreting genotype x environment interaction. Crop Sci. 1999;39(4):955–67.
#' @references Costa-Neto GMF. Integrating environmental covariates and thematic maps into genotype by environment interaction analysis in upland rice. Master degree Thesis in Genetics and Plant Breeding, Agronomy School, Federal University of Goiás. Brazil, 2017. 122f.

FR<-function (df.y, df.cov,scale=FALSE){
  na<-colnames(df.cov)
  n <- ncol(df.y)

  # creating output objects
  fres <- data.frame(matrix(NA,nrow=n,ncol=(1+ncol(df.cov))))
  sqres<-vector("list",length(colnames(df.y)))
  SQR<-data.frame(Sum.sq=rep(0,(1+ncol(df.cov))),VarExp=0)
  row.names(SQR)<-c(na,"Residual")
  names(fres)<-c(paste("b.",na,sep=""),"genotype")

  if(isTRUE(scale == TRUE)){df.y<-scale(df.y)}
  # creating formula
  fo<-c()
  for(i in 1:length(na)){fo[i]<-paste(na[i])}
  fo<-c("0",fo);fmla<-as.formula(paste("y~",paste(fo, collapse= "+")))

  for(i in 1:n){
    dfa <- data.frame(y=df.y[,i],df.cov)
    fit<-lm(fmla,data=dfa)
    sqres[[i]]<-data.frame(anova(fit))
    SQR[,1]<-SQR[,1]+data.frame(anova(fit))[,2]
    for(j in 1:length(na)){fres[i,j]<-coef(fit)[j]}
    fres[i,ncol(fres)]<-colnames(df.y)[i]}
  names(sqres)<-colnames(df.y)
  SQR[(nrow(SQR)+1),1]<-round(sum(SQR[1,1]:SQR[(nrow(SQR)-1),1]),digits=3)
  SQR[(nrow(SQR)+1),1]<-sum(SQR[(nrow(SQR)-1),1],SQR[(nrow(SQR)-2),1])
  row.names(SQR)[(nrow(SQR)-1):nrow(SQR)]<-c("Regression","Total")
  SQR[,2][1:(nrow(SQR)-3)]<-round(100*((SQR[,1][1:(nrow(SQR)-3)])/(SQR[(nrow(SQR)),1])),digits=3)
  SQR[which(row.names(SQR) == "Residual"),2]<-100-sum(SQR[(1:(nrow(SQR)-3)),2])
  SQR[which(row.names(SQR) == "Regression"),2]<-100-SQR[which(row.names(SQR) == "Residual"),2]

  return(list(Coefficients=fres,Sum.sq=SQR,Sum.sq.G=sqres))

}
