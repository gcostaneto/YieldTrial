r.confid = function(value,yhat,round=3){
  n = length(value)-2
  r = round(cor(value,yhat,use="complete.obs"),round)
  se = 1/sqrt(n)
  ci= round(tanh(atanh(r)+c(1,-1)*qnorm(.95)*se),round)
  return(data.frame(r=r,upper=ci[1], lower = ci[2]))
}
