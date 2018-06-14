#' Elite advanced lines of Wheat
#'
#' Grain yield (kg/ha) data from Sukumaran et al (2017).
#' The WAMI population was assembled from the elite advanced wheat
#' nurseries distributed through the International Wheat Improvement
#' Network (IWIN). It consists of 287 diverse elite lines selected from
#' nurseries bred for high yield potential environments.
#'
#' @docType data
#' @usage data(WheatI)
#' @format data.frame
#' \describe{
#' \item{YLD}{Grain yield data (tons per hectare)}
#' \item{Entry}{Name of the each 99 evaluated hybrids}
#' \item{Env}{Name of each 17 trials (environments)}
#' \item{Rep}{treatments replicaiton}
#' \item{Block}{blocks}
#'
#' }
#' @examples
#' data(WheatI)
#' boxplot(YLD~Env,WheatI, col="green")
#'
#' @references Sukumaran S, Crossa J, Jarquin D, Lopes M, Reynolds MP. Genomic Prediction with Pedigree and Genotype × Environment Interaction in Spring Wheat Grown in South and West Asia, North Africa, and Mexico. G3. 2017;7(2):481–95. Available from: http://g3journal.org/lookup/doi/10.1534/g3.116.036251

WheatI<-read.csv("/home/germano/Dropbox/Statistical Analysis/Pacote/Multienvironment/data-raw/WheatI.csv", header=T,sep=",", dec=".")
str(WheatI)
WheatI$Rep<-as.factor(WheatI$Rep)
WheatI$YLD<-as.numeric(WheatI$YLD)
WheatI$Entry<-as.factor(WheatI$Entry)
WheatI$Block<-as.factor(WheatI$Block)
