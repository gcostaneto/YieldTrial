# Obtains the double entry table of genotypes and environments through different models


TwoWay<-function(y, gen, env, rep, df, mod){
        require(reshape2)
        require(lsmeans)
        require(lme4)
        
        y <- df[,y]
        g <- df[,gen]
        r <- df[,rep]
        e <- df[,env]
        dfa<-data.frame(y,g,r,e)
        
        
        if(isTRUE(mod == 2)){
                cat("Running mixed effects model: fixed (environment, replication),random (genetic and gxe interaciton)","\n")
                model<-lmer(y~r/e+e+(1|g)+(1|g:e), dfa)
                g<-as.data.frame(ranef(model)[2])
                g<-data.frame(g.hat=g$X.Intercept., g=row.names(g))
                ge<-data.frame(ranef(model)[1])
                ge<-data.frame(colsplit(row.names(ge), ":",c("g","e")),ge.hat=ge$X.Intercept.)
                Y.hat<-merge(g,ge, by="g")
                e_hat <-data.frame(summary(lsmeans(model, ~"e")))
                e_hat <- e_hat[,c(1:2)]
                Y.hat<-merge(Y.hat, e_hat, by ="e")
                cat("estimated means: Y = blue (intercept + environment) + blups (genotype + gxe interaction)","\n")
                Y.hat$y.hat <- Y.hat$g.hat + Y.hat$ge.hat + Y.hat$lsmean
                TW<-acast(Y.hat,e~g, mean, value.var="y.hat")
                return(list(Means=Y.hat,Yhat=TW))}
        if(isTRUE(mod == 1)){
                cat("Running full fixed model","\n")
                model<-lm(y~r:e + g*e, dfa)
                Y.hat<-data.frame(summary(lsmeans(model, ~g*e)))
                TW<-acast(Y.hat,e~g, mean, value.var="lsmean")
                return(list(Means=Y.hat,Yhat=TW))} 
}
