import.vcu = function(.path,.pattern,.id="id",.names,.sep=";",.dec=","){
  setwd(.path)
  LISTF <- list.files(pattern = .pattern)
  LEG <- length(LISTF)
  cat(paste("------------------------------------------------------------------------","\n",sep=" "))
  cat(paste("Total of  ",LEG," files in the directorie:","\n",sep=""))
  cat(paste(.path,"\n"))
  output <- c()
  for(L in 1:LEG){
    output <- rbind(output,
                    data.frame(read.csv(LISTF[L],header=T,sep=.sep,dec=.dec)[,.names],.id = LISTF[[L]]))
  }
  names(output)[dim(output)[2]]<-.id
  cat(paste("Data frame containing ",dim(output)[1]," rows and ", dim(output)[2],"  columns \n",sep=""))
  cat(paste("------------------------------------------------------------------------","\n",sep=" "))
  cat(paste("Imported vectors:\n",sep=" "))
  cat(paste(.names,sep=" "))
  return(output)
  
  
}
